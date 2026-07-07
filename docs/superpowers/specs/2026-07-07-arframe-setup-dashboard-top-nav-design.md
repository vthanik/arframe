# Setup → sectioned dashboard + frame top-nav refactor

**Date:** 2026-07-07
**Sub-project:** #12.2 (Setup → sectioned dashboard), which in this pass
**re-opens #12.1 / #8** (the frame shell) to move the mode nav from a left
sidebar to a top app bar. Both ship together.

## Goal

Two coupled changes, one build:

1. **Frame:** replace the left `.ar-sidebar` mode nav with a **top app bar**
   (TWISTY-style), drop the sidebar entirely so content is full-width, and
   strip the cluttered circle icons (Refresh / Undo / Redo) from the bar.
2. **Setup:** re-house the one-scroll, 7-section Setup page as a **sectioned
   dashboard** — a horizontal section tab strip, one section visible at a
   time, each section an elevated `.card()`, with a persistent overview strip
   of `.stat_tile()`s and completeness tracked on each tab.

**Hard invariant:** this is a UI re-housing. `.SETUP_SPEC`, `.wire_all()`,
every collector (`.collect_pops/_arms/_foots/_conts/_band_rows`), every
observer, the mode-switch mechanism, and the export path have **zero logic
change**.

## Reversal note (must update the locks)

CLAUDE.md **#8** and **#12.1** currently lock the mode nav as a *left
sidebar* (built + verified last session). This pass supersedes that: mode
nav is now a *top app bar*. CLAUDE.md #8/#12.1 get rewritten in the same PR
so the record matches the code (no stale lock).

---

## Part A — Frame refactor (`mod_frame.R`, `arframe.css §02`, `arframe.js`)

### A1. Top app bar, no sidebar

Drop `.frame_sidebar()` and `.ar-sidebar`. The workspace becomes a single
column: **app bar** over the five mounted mode bodies.

App bar (left → right):
- `arframe` **brand** (mark + wordmark), left.
- **Mode tabs** — `Setup · Data · Report · Review · Logs`, peers, active =
  **accent underline** (the TWISTY "Home" treatment). `role="tablist"`.
- Flexible gap.
- **⌘K palette hint** (`.ar-bar-hint`, glyph filled client-side per
  `navigator.platform`, unchanged).
- **Package** button (`#export_btn`, unchanged).
- **Avatar** slot (right; presence avatars land here in the planned
  follow-up — empty for now, no new work).

**Mode switching mechanism is untouched:** items keep `data-ar-mode`; the
existing arframe.js delegated click still posts `input$mode`; the active
body is still the pure-CSS `.ar-mode-*` class on `.ar-workspace`. Only the
nav *markup* relocates (sidebar → app bar) and the *CSS* changes (vertical
rail → horizontal underline tabs). `mod_frame_server()`'s mode observer,
`toggle_rail`/`toggle_insp`, and the `ar-mode`/`ar-collapse` messages are
unchanged.

### A2. Remove the three circle icons

- **Refresh (`#refresh_btn`)** — **deleted outright.** It is already
  redundant: `.refresh_all(store)` fires on the window-focus event through
  the same call site (see the existing comment at the refresh observer), so
  on-disk / other-session edits still get picked up. The
  `observeEvent(input$refresh_btn, …)` server block is removed; the
  focus-event refresh path stays.
- **Undo / Redo (`#undo_btn` / `#redo_btn`)** — buttons **deleted**, feature
  **kept** via keyboard. Add a `keydown` handler in arframe.js:
  **⌘Z / Ctrl+Z → `Shiny.setInputValue` on the `undo_btn` id**;
  **⌘⇧Z / Ctrl+Shift+Z → the `redo_btn` id** (same ids the server already
  observes, so `observeEvent(input$undo_btn, undo(store))` /
  `redo` are unchanged). `undo()`/`redo()` already no-op when the stack is
  empty, so no guard is needed.
- The `ar-disable` mirroring observer that greyed out the undo/redo buttons
  is **removed** (nothing to disable now; the keyboard shortcut is a
  server-side no-op when `can_undo`/`can_redo` is false).

### A3. Save chip — kept

`#save_chip` stays in the app bar (text status, not a circle). Its
`ar-save-state` driver is unchanged.

### A4. Report name → page-header line (option A)

The click-to-edit report title leaves the app bar (the mode tabs claim that
space) and becomes a slim **page-header row** below the app bar, present in
all modes:

```
.ar-main
  .ar-topbar      app bar: brand · mode tabs · ⌘K · Package · avatar
  .ar-pagehead    report name (click-to-edit, left)   ← frame-level, all modes
  .ar-body        five CSS-toggled mode bodies
```

The report-name component (`.frame_title` → `input$name` in
`mod_frame_server`) is unchanged — it just renders in `.ar-pagehead` instead
of the bar. It stays **frame-level and rendered once** (not per-mode) so the
`input$name` wiring is not duplicated.

**Option A's "name + overview tiles on one line" is realised by placement,
not cross-module coupling:** the report name lives in the frame `.ar-pagehead`;
the Setup overview tiles are the first row of the *Setup body* (Part B).
CSS styles `.ar-pagehead` + the Setup overview strip as one contiguous
header band (name row above, tiles row below, tight spacing) so the
dashboard reads as one header. This keeps the frame/Setup module boundary
clean (frame owns the name; Setup owns its tiles). Literal single-line
name+tiles is a later CSS-only refinement if wanted.

---

## Part B — Setup sectioned dashboard (`mod_setup.R`, `arframe.css` setup block)

### B1. Section shell → cards

Each of the 7 sections (`Study · Paths · Treatment · Populations ·
Page & Style · Summaries · Team`) is wrapped in **`.card()`** (`.ar-panel`,
the Foundation atom) instead of the hairline-ruled `.ar-setup-section`. The
card header carries the section title; the section body is the existing
`.setup_group`s **unchanged**. `.setup_section()` is rewritten to emit a
`.card()`; `.setup_group`, `.flat_input`, `.seg_control`, `.select_input`,
and every section builder (`.setup_study`, `.setup_paths`, …) are otherwise
untouched.

### B2. Horizontal section tab strip

A tab strip at the top of the Setup body, below the overview strip:

```
Study ✓ · Paths ✓ · Treatment ② · Populations · Page · Summaries · Team ●
         (active tab = accent underline; overflow-x: auto if narrow)
```

- 7 tabs, each: **label + status indicator**. The indicator reuses
  `.section_status(theme, section)` (unchanged): `ok` → check, `partial` →
  missing-count, else a plain dot / nothing. This **replaces** the old
  `.ar-setup-glyph` chip and its CSS, which is on the **dead tokens**
  `--ar-ok` / `--ar-warn` / `--ar-chrome-2` (absent from the new
  `tokens.css`). The new indicator uses live status tokens
  (`--ar-ready` / `--ar-draft` etc.).
- Active tab = accent underline (matches the app-bar mode tabs and the
  reference).

### B3. One section visible at a time

All 7 section cards stay **rendered in the DOM** (so every `.wire_all`
observer keeps its inputs alive — the wiring depends on the inputs
existing). CSS shows only the active one via `display:none` on the rest,
keyed off a class on the Setup root: `ar-setup-tab-<active>` →
`.ar-setup-tab-study .ar-setup-section[data-ar-section="study"] { display:block }`,
others `display:none`.

### B4. Active tab is server-authoritative (the load-bearing detail)

`output$page` re-renders on **every commit** (it reads `store$rv$report` +
`catalog_nonce` to keep the tab status live). A pure client-CSS active tab
would therefore **reset to the first tab mid-edit** (edit Treatment → commit
→ re-render → bounced to Study). Fix:

- **Client (snappy):** tab-click JS immediately swaps the active class on
  the tabs + the root (`ar-setup-tab-<id>`) so the switch is instant, AND
  posts `Shiny.setInputValue(ns("setup_tab"), id, {priority:"event"})` —
  same shape as `.seg_control`'s inline handler.
- **Server (durable):** `active_tab <- reactiveVal("study")`;
  `observeEvent(input$setup_tab, active_tab(input$setup_tab))`; `renderUI`
  reads `active_tab()` and stamps `ar-setup-tab-<active>` on the root + marks
  the active tab. A mid-edit re-render re-applies the last active tab → no
  bounce.

Result: instant clicks, and the tab survives re-render. This is the one
piece of genuinely new state in the sub-project.

### B5. Overview strip (`.stat_tile()`s)

A persistent row of stat tiles at the top of the Setup body (the dashboard
header, contiguous with `.ar-pagehead` per A4). Tiles, in order:

- **Sections done** — `"<k>/7"`, `k` = count of sections whose
  `.section_status$state == "ok"`. Cheap; no new data.
- **Datasets** — `nrow(arpillar::catalog_grid(store$con))` (already probed
  in `.setup_paths`).
- **Records** — row count of the population dataset (from `catalog_grid`;
  Data mode already surfaces a ROWS column, so the count is available
  without a new query).
- **Subjects** — `COUNT(DISTINCT <subject_id>)` on the pop dataset via
  `store$con`. **Rendered only when** the subject-id column is resolved AND
  the count query succeeds; otherwise the tile is **omitted** (no fabricated
  / zero value — honours the `no-fabricated-render-content` rule). Wrapped in
  `tryCatch`.

Tiles read from the same catalog probe already done in `.setup_paths`; no
new heavy work per render beyond the one optional `DISTINCT` query.

---

## What does NOT change (explicit)

- `.SETUP_SPEC`, `.wire_all()`, `.theme_set()`, `.bind_theme_*`.
- Every collector: `.collect_pops`, `.collect_arms`, `.collect_foots`,
  `.collect_conts`, `.collect_band_rows`, `.band_to_rows`.
- Every structural observer (add/delete pop / arm / footnote / cont row /
  band row), the population-seed observer, the folder pickers
  (`.ar-setup-pickers` + `.picker_proxy` teleport pattern).
- Mode switching (`input$mode`, `.ar-mode-*`, `ar-mode` message), rail /
  inspector collapse, the export pipeline, the save-state chip driver.
- The Foundation atoms `.card` / `.stat_tile` / `.stamp` / `.avatar` and
  their CSS (used as-is; no signature change).

## Files touched

- `R/mod_frame.R` — app bar replaces sidebar; remove refresh/undo/redo
  buttons + the refresh & disable observers; report name → `.ar-pagehead`.
- `R/mod_setup.R` — `.setup_section()` → card; new tab strip + overview
  builders; `active_tab` reactiveVal + `input$setup_tab` observer; renderUI
  stamps the root class.
- `inst/www/arframe.css` — §02 rewrite (horizontal app bar + underline mode
  tabs, drop `.ar-sidebar`); Setup block (cards, tab strip, one-at-a-time
  visibility, overview strip); delete dead `.ar-setup-glyph-*`.
- `inst/www/arframe.js` — ⌘Z / ⌘⇧Z keydown → undo/redo input ids; Setup
  tab-click handler (or reuse the inline `.seg_control` pattern).
- `tests/testthat/test-mod_frame.R` — rewrite for the top-nav structure;
  assert no sidebar, mode tabs present, no refresh/undo/redo buttons,
  report name in pagehead; keep the e2e 5-mode screenshot walk.
- `tests/testthat/test-mod_setup_wire.R` — wiring assertions unchanged
  (must still pass, proving zero logic change); add the active-tab
  regression below.
- `CLAUDE.md` — rewrite #8 / #12.1 (top nav supersedes left sidebar);
  update #12.2 build state.

## Testing

- **Bug-fix-test-first for the active-tab reset** (the one real behavioural
  risk): a `testServer` test — set `input$setup_tab <- "treatment"`, force a
  commit (which re-renders `output$page`), assert the rendered root still
  carries `ar-setup-tab-treatment` (not `-study`). Must fail against a naive
  client-only implementation, pass with the reactiveVal.
- **Wiring intact:** existing `test-mod_setup_wire.R` assertions pass
  unchanged (they prove `.SETUP_SPEC` still writes through).
- **Frame structure:** `test-mod_frame.R` asserts the new DOM (mode tabs in
  app bar, no `.ar-sidebar`, no `#refresh_btn`/`#undo_btn`/`#redo_btn`
  buttons, report name under `.ar-pagehead`).
- **e2e:** the 5-mode screenshot walk still runs and shoots each mode to
  `.local/screens/0N-<mode>.png`.

## Verification gate

- `devtools::test()` FAIL 0.
- `devtools::check(args="--no-manual")` Status OK (0/0/0).
- **Screenshot eyeball (all 5 modes) on real CDISC data** — boot on the
  ADaM pilot, drive Chrome, shoot + eyeball each mode (the
  `feedback-screenshot-eyeball` rule; green tests are not sufficient).
  Specifically eyeball: top nav underline on the active mode, the three
  circles gone, report name in the pagehead, Setup tab-switch + overview
  tiles, tab survives a mid-edit commit.

## Deferrals

None. Per the `feedback-never-defer` rule, everything above lands in this
pass (subjects-tile omission is a correctness guard, not a deferral).
