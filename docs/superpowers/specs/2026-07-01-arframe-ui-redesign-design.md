# arframe / arpillar — UI redesign design spec

- **Status:** approved design (brainstorming complete); next step is `writing-plans`.
- **Date:** 2026-07-01
- **Supersedes:** the repo-root `design.md` and the datasetviewer/SAS-blue "north
  star" *as binding constraints*. This was a first-principles redesign; `design.md`
  was explicitly reopened by the user. The datasetviewer design system is retained
  as the **default light skin**, not as an unquestionable law.
- **Author:** Vignesh Thanikachalam.

---

## 1. What we are building

`arframe` is a local-first, R-native, open-source **submission-native clinical
report builder + data explorer**. It is modelled on SAS Viya's *Manage Data* +
*Explore & Visualize*, but re-centred on the clinical deliverable: a report is an
ordered set of **shell-conforming TLF outputs** (tables, listings, figures) exported
to RTF/PDF/HTML/DOCX and backed by reproducible R code.

Goal: beat SAS Viya, teal, blockr, and GSK rapido on **design and quality**. The
wedge is that none of them make the *deliverable itself* the primary object — they
assemble outputs and treat the report as an export afterthought. `arframe` makes the
report (the TLF package) the thing you edit.

## 2. Packages and naming

**The `AR` family.** `AR` = **Analysis Report / Analysis Result**. The packages carry
the `ar` prefix so they self-describe and read as one family (the `gt` / `sf`
pattern). This is a **deliberate departure** from the house "bare evocative single
word, no prefix" convention — chosen for self-describing discoverability across a
stats/clinical audience. Do not "correct" it back to an evocative single word; the
prefix is intentional. (It governs *package* names only; individual exported
functions still follow the house rule — bare verbs/nouns, no per-function prefix.)

`arframe` **reclaims the ecosystem name.** The earlier `arframe` in the roadmap
(*Herald / arframe / Mint / Loom*) was renamed to **`tabular`** (the render/export
layer), which frees the `arframe` name for this app. The global `~/.claude/CLAUDE.md`
roadmap line is stale on this point and should be updated separately.

| Package | Role |
|---|---|
| `arframe` | Thin bslib Shiny shell — the UI **frame**. `arframe()` opens/creates a report project. Section modules, `state.R` store, draft/commit helpers. |
| `arpillar` | Pure engine, no Shiny — the **pillar** the frame stands on. DuckDB catalog/engine, S7 document model, SQL compiler (pushdown), `render_plan()` seam, `render_ggplot`/`render_tabular`/`ard_cards`, JSON serialization + migrators. Unit-testable with no `session`. |

- Both names are CRAN-free (verified 2026-07-01). They shadow no base R (note: bare
  `ar()` is `stats::ar` — autoregression — but the prefixed names do not collide).
- Replaces the temporary `explorer`/`explorerengine` scaffolding and the `ex_`
  export prefix.
- The pairing `arframe` (UI) on `arpillar` (core) avoids an ugly `...engine` suffix.
- Not adopted: `arrender` (overlaps `tabular`, the existing render/export layer —
  composed, not reinvented) and `arkit` (an umbrella is redundant for a 2-package
  split — YAGNI until 3+ packages exist).

## 3. Paradigm — spec/shell-driven (submission-native)

The controlling decision. A report is an ordered list of **specified outputs**, each
mapped to an SAP-style **shell**. You do not build ad-hoc charts on a free canvas;
you **fill a shell**. The shell is visible from the first second (as ghosted
placeholder rows/cells) and fills in as data is assigned.

Rejected alternatives: BI free-form canvas (SAS Viya's own model — poor fit for
standardized TLFs, and competes with SAS on its home turf); task/catalog-only
(too close to the current Section B, less differentiated); explore-first notebook
(weak for reproducible standardized TLFs).

## 4. Information architecture

Two top-level **modes**, switched by a segmented toggle in the app bar:

- **Data** — the supporting explorer: dataset catalog, grid, variable profiles,
  import. Its job is to *feed* the report.
- **Report** — the deliverable: the report is a project holding **many outputs**,
  each independently editable. This is home.

The report is never a single output — the whole set of outputs travels together as
one project (see §9 persistence).

## 5. App shell — the SAS Viya dual-rail frame

The frame is SAS Viya's *Explore & Visualize* five-region shell, reskinned and
repurposed. Chosen over three invented alternatives (collapsible-outline+editor,
full-bleed focused, three-pane workbench) because it is the proven, familiar
structure for this audience and it holds everything already designed.

```
┌───────────────────────────────────────────────────────────────────────┐
│  app bar:  arframe · [Data | Report] · report name · undo/redo · Export │
├────┬──────────────┬───────────────────────────────┬──────────────┬─────┤
│ L  │ left pane    │  canvas: [output tabs] +       │ right pane   │  R  │
│ a  │ (contextual  │                                │ (contextual  │  a  │
│ c  │  to active   │   ┌─────────────────────────┐  │  to active   │  i  │
│ t  │  activity)   │   │  white paper shell      │  │  property)   │  l  │
│ i  │              │   │  preview (screen==paper)│  │              │     │
│ v  │              │   └─────────────────────────┘  │              │     │
│ «  │              │                                │              │  »  │
└────┴──────────────┴───────────────────────────────┴──────────────┴─────┘
```

| SAS Viya region | arframe mapping |
|---|---|
| Left rail: Data / Objects / Outline / Suggest / Review | **Data** (variables) · **Outputs** (clinical TLF templates) · **Outline** (the TLF deliverable list) · **Suggest** (recommend standard tables for the loaded ADaM) · **Review** (QC / shell-conformance / log) |
| Center canvas + page tabs | one **output per tab**; the white **paper shell preview** |
| Right rail: Options / Roles / Actions / Rules / Filters / Ranks | **Roles · Options · Filters · Ranks** for the selected output; **Actions/Rules** deferred (cross-output interactivity, later milestone) |
| Both rails collapse `«` `»` | same — collapse either rail to hand full width to a wide many-arm table |

The width cost of two rails + two panes is real; the mitigation is the same as SAS
Viya's — both rails and both panes collapse, giving a focused wide canvas on demand.

## 5.1 UI module architecture (binding)

Grounded in ThinkR's `signature.py` (2024 Shiny Contest winner) — a reference for
*clean* Shiny craft, not complexity. These are binding rules for how `arframe` is
built. They exist to prevent regressing into the old `explorer` app-layer debt the
codebase audit flagged (`setup_pane.R` = 7085-line monolith, ~30 `draft_*` reactive
vals, two conflicting "is configured" checks, and `suspendWhenHidden`-driven config
loss).

1. **Uniform module contract.** Every UI surface is a Shiny module — a
   `<surface>_ui(id)` + `<surface>_server(id, store, ...)` pair, one file per module
   (`mod_<surface>.R`). This applies to each left-rail activity, the canvas, each
   right-rail properties pane, and each Data-mode panel. No monoliths: if a module
   file grows large, it is doing too much and must be split. This is the direct
   antidote to the 942-line `do_commit`.

2. **One structured state store, injected — the ONLY inter-module channel.**
   `signature.py` threads a single `reactive.Value` into every module server; modules
   communicate solely through it, never with each other. `arframe` does the same,
   scaled up: **one structured store** (the S7 `report` document + a `selected`
   pointer + the catalog handle) is created once and injected into every module's
   server. The Outline reads `report`; the editor writes the `selected` object's
   roles/options; the canvas reads it. Modules never reach into one another.
   **All draft/edit state lives in this store, OUT of the DOM** — so a folded,
   lazy-mounted, or unmounted pane can never silently drop configuration (the audit's
   top data-loss risk). This replaces the `draft_*` sprawl and the two-sources-of-truth
   entirely; `output_status(object)` in `arpillar` is the single "configured?" oracle.

3. **Logic out of modules; render is a pure function of state.** `signature.py`
   renders its preview from a template filled with state (data ≠ presentation). In
   `arframe` **all business logic lives in `arpillar`**; modules only wire UI ↔ store,
   and the canvas renders the model through the single `render_plan` seam. A module
   never computes an ARD or shapes an output — it calls the engine.

4. **Theme by variable override, not inline CSS.** One `bslib::bs_theme()` variable
   layer + one SCSS file (the `signature.py` `$primary`/`$secondary` pattern). The
   light SAS-blue default and the dark modern skin are a **variable swap**, not a
   rewrite. No scattered inline styles or duplicated `addResourcePath` (an explorer
   wart).

5. **Test pyramid in CI from day one.** `testthat` for `arpillar` (engine, off-session,
   >=95% per file) + `shinytest2` for `arframe` UI flows + the golden gates (RTF
   byte-golden; figure round-trip), all wired to CI on every push — the `signature.py`
   unit + E2E discipline.

6. **Mockups are the module contract.** The committed mockups/spec define each
   module's UI; build modules against them (the `signature.py` "mockup-first to
   organize the code" lesson, which this redesign already followed).

**Scaling caveat (do not cargo-cult):** `signature.py` is tiny (3 flat modules, a flat
dict of 7 strings). `arframe` uses a **structured** store (the document model, not a
flat dict) and a **module hierarchy** (rail -> pane -> widget), not 3 flat modules.
Take the principles, scale the mechanics.

## 5.2 Reactivity & performance discipline (engineering-shiny, binding)

From *Engineering Production-Grade Shiny Apps* (Fay et al.).

- **Logic in `arpillar`, modules only wire.** No `DBI`/`cards`/`ggplot2`/`tabular`
  call ever runs inside an `arframe` `render*`/`observe`/`reactive`. Every heavy
  computation is a pure `arpillar` function the server assigns to `output$*`.
- **`observeEvent` with an explicit event list, never a monolithic `observe()`.**
  Each right-rail control fires one store mutation via its own `observeEvent`; a role
  commit is `observeEvent(input$roles_commit, ...)`, not an `observe()` that re-reads
  the whole properties panel on any change.
- **Gate heavy reactivity with explicit trigger/watch flags (gargoyle pattern).**
  `data_loaded` / `ard_ready` / `render_ready` flags are set by the `arpillar` call
  that finishes each stage; the shell-preview render and the export button *watch*
  `render_ready`, so a filter change `trigger()`s a recompute instead of cascading
  through implicit dependencies.
- **Update in place; do not `renderUI()` whole components.** The shell preview and the
  properties rail are stable DOM rendered once; cells/labels update via
  `update*Input` / `insertUI` / `removeUI` / a small JS handler. Assigning a role must
  not regenerate the shell (preserves the selection stripe, no flicker).
- **Guard every `arpillar` call with `req()`/`validate()`** (`req(active_output(),
  roles_complete())`) so a half-configured output never renders or exports.
- **File organization.** `arframe`: `mod_<region>.R` (one module per screen region,
  numbered by layout position, `ns()`-namespaced; one output tab = one parameterized
  `mod_output_*` instance) + `utils_ui_*`. `arpillar`: `fct_*.R` (SQL-compile / ARD /
  render / export) + `utils_*`. One logical unit per file, one mirrored test file.

## 5.3 Async execution model (mirai, binding)

The four heavy operations — DuckDB pushdown query, `ggplot2`+`tabular` render,
RTF/DOCX export, `cards` ARD — run OFF the Shiny main loop so the dual-rail UI (and
every other session) never blocks.

- **One `run_async(expr)` `arpillar` helper wrapping `mirai(...)`**, returning a
  promise; every heavy op routes through it.
- **Each output tab is backed by its own `shiny::ExtendedTask`.** Render/Export
  controls use `bind_task_button` (auto-disable mid-run); `task$status()`
  invoke->success drives the ghost-cells-fill animation.
- **DuckDB connections (external pointers) CANNOT cross into a daemon.** Never capture
  the app's live `con`. Send the compiled SQL string + the DuckDB file path into the
  `mirai` expression, which opens its OWN connection
  (`dbConnect(...); on.exit(dbDisconnect(...))`). The main-session read-only
  connection is for the interactive grid only.
- **Own an isolated, namespaced daemon pool.** `daemons(n, .compute = "arframe")` in
  `app()` startup (NEVER at package load), torn down via
  `onStop(function() daemons(0, .compute = "arframe"))`. Pre-warm with
  `everywhere({ library(arpillar); library(tabular); library(ggplot2); library(cards) })`
  + static reference data (shell templates, CT dictionaries). Never touch the default
  profile.
- **Batch progress.** "Render all / Export report" fans outputs with
  `mirai_map(..., .promise = cb)`; per-item resolution ticks a progress bar
  ("Rendered 7 of 22") and updates the Outline status dots.
- **Cancellable.** Hold each `mirai` reference; a right-rail Cancel affordance
  (visible while running) calls `stop_mirai()`.
- **Observability + CRAN-safety.** Opt-in OpenTelemetry spans per stage
  (query/render/export/ARD) locate the slow one before optimizing. Async
  tests/examples use `daemons(1, dispatcher = FALSE, .compute = "arframe")` + teardown
  + `Sys.sleep(1)` to stay within the 2-core CRAN ceiling. Default runtime pool modest
  (2-4; `app(daemons = )` for power users).

## 5.4 Accessible component foundation (GOV.UK, binding)

- **`fieldset` + `legend`** wraps every grouped control (a role's radios, a filter's
  operator radios, the Ranks toggle); the uppercase micro-label IS the legend, so
  screen readers announce the group's question.
- **Focus != selection.** Keyboard focus is a high-visibility two-tone indicator
  (yellow `#ffdd00` + a thick black border) meeting WCAG 2.2 non-text-contrast; the
  2px blue stripe is reserved for the *selected/active* state only.
- **Never colour alone.** Every colour signal pairs with text/icon/shape; all
  ink/paper pairs hold WCAG AA 4.5:1 (a contrast check lives in the token file). Ghost
  cells = dashed border + "unassigned" text (not just tint); type chips encode type by
  glyph, not just hue; errors carry red + a message + an icon.
- **Error summary.** On a configure/render failure a `role=alert` "There is a problem"
  summary renders at the top of the canvas, moves focus to itself, and jump-links to
  each offending control; inline messages are worded identically and ordered to match
  the controls. `arpillar` validation emits structured `{control_id, message,
  order_index}` objects so summary + inline come from ONE source (no duplicated
  strings).
- **Control by cardinality.** Radios for small mutually-exclusive sets (never
  pre-selected; always an explicit None/Any), checkboxes for multi, native select only
  when space is tight. Hints are one short sentence, no full stop, no links, via
  `aria-describedby`. Numeric inputs: content-width, `inputmode="numeric"/"decimal"`,
  `type="text"` (not `number`), visible `<label for>`.
- **One spacing scale** (e.g. 4/8/12/16/24/32 px) via tokens for all rail/panel/canvas
  padding. **Locked colour semantics** in `design.md`: blue = selection/primary, red =
  error, yellow = focus — reserved, never repurposed; uniform form-input border weights
  across Data and Report modes.

## 5.5 Dashboard UX rules (Jumping Rivers / Litmus, binding)

- **Never hang.** Every long op is non-blocking (§5.3) with a per-output inline
  skeleton/spinner scoped to the canvas paper (not a full-screen block); export
  completion toasts via a non-modal notification.
- **Top-down hierarchy.** The white paper is the visual hero (largest, highest-contrast
  zone); an empty canvas shows a one-line orientation + a single **Add output** CTA,
  never blank grey.
- **Tabs as lenses over one focus.** Data mode = Grid / Variable profile / Query tabs
  over the *one selected dataset* (not separate rail destinations); the Report canvas =
  output tabs over the one report.
- **Presets over the common case.** The Filters/Ranks pane leads with named intent
  presets (Safety population, Treated only, Completers, Full set) that write the config
  in one click; the manual builder serves the long tail.
- **Column visibility.** The Data grid and the shell preview offer visibility toggles
  that push a SELECT-column list into DuckDB (hidden columns are never fetched).
- **Expectations upfront.** Ghost zones name the next step ("Drag a treatment variable
  here"); deferred output types show disabled with a "coming" tag, not omitted.
- **Responsive to width extremes.** Below a breakpoint the left rail collapses to an
  icon strip and the right rail becomes an overlay drawer so the paper never squeezes
  to unreadable; verify reflow (not clip) at ultrawide and laptop widths via
  `shinytest2` viewport captures.
- **Persistent visible labels** on every input — never placeholder-as-label.

## 6. Left activity rail (what you add / navigate)

- **Data** — rich variable picker (type chip + name + label) scoped to the output's
  dataset(s). Also the drag source for role assignment.
- **Outputs** — the catalog of standard clinical TLF **templates** (the existing
  presets catalog; see the clinical table/graph catalogs). `+ Add output` picks from
  here. Each template declares the role slots it exposes (§8).
- **Outline** — the ordered TLF deliverable list, grouped Tables / Listings /
  Figures, numbered (14.2.1, 16.2.1, ...), each row carrying a live **status**
  (Ready / Draft / Needs data / Broken). Reorderable. This is the report's spine.
- **Suggest** — given the loaded ADaM datasets, recommend standard outputs
  (Demographics for ADSL, AE summary for ADAE, KM for ADTTE, ...).
- **Review** — QC / validation surface: shell-conformance per output, missing
  outputs, the run log.

## 7. Center canvas — the shell preview

- **Output tabs** across the top (one output per tab, `+` to add).
- **White paper shell preview.** Renders the actual output as it will appear on the
  page via the **same `render_plan → tabular`/`ggplot` seam that exports** — screen
  == paper, no dual-render divergence. A Fit-width ⇄ RTF-page toggle switches between
  screen fit and true paginated paper.
- **Ghost slots.** Unassigned roles render as ghosted placeholder rows/cells, so the
  mock shell is visible and fills in as data is assigned. An inline `+ Add variable`
  target sits in the stub.
- **Direct manipulation.** Clicking a part of the shell (banner, stub row, title,
  footnote) selects it and drives the right-rail property pane to that element's
  options. This is what beats teal's opaque wall of sidebar inputs: the preview *is*
  the encoding.

## 8. Right properties rail (how the selected output is configured)

Role slots are **derived from the output template** — Demographics exposes Treatment
arms / Summarize / Population; an AE table instead exposes a SOC▸PT hierarchy slot.
Each empty slot opens a picker showing **only eligible variables** (Treatment arms
won't offer a numeric).

- **Roles** — the template's role slots (the primary tab).
- **Options** — per-stat decimals, statistic recipes, labels, footnotes, format.
- **Filters** — population / subset filters.
- **Ranks** — top-N / incidence cutoff (e.g. AE tables).
- **Actions / Rules** — cross-output interactivity; deferred to a later milestone.

## 9. Persistence — JSON project + reproducible code

Two complementary saves; both matter and are not either/or.

- **JSON project (the reopenable build).** The S7 document model — as built in
  `arpillar` (slice 1a) — is `report → page → object → role → data_item` (the `object`
  class is one TLF output; **roles-only, no block duality**). Each `object` **binds its
  source `dataset`** so a saved report is self-contained and re-renders without
  threading a dataset id (the hardening fix). It serialises to **JSON with a
  `schema_version` (starts at `1L`)**; because this is greenfield there is **no legacy
  migration burden** — `.migrate_doc()` is an identity stub kept as the single seam for
  future non-additive schema changes. A shared named-vector value codec preserves the
  names of every `format`/`options`/`filters`/`theme` vector across the round-trip.
  Save, close, reopen later, keep working. Never `saveRDS` the live S7 tree. The whole
  set of outputs persists as one project.
- **Reproducible code (the deliverable).** Every output — and the whole report —
  emits **runnable R** (the `render_plan`/emit seam). Generated *from* the model so it
  can never drift from what is shown. This is the artifact for the submission and for
  QC / double-programming.

## 10. Data mode

- **Catalog** (left) — loaded/available datasets, lazy via DuckDB, row/col counts,
  in-memory vs lazy state indicator; `+ Import` (artoo ingest).
- **Grid** (center) — the datasetviewer DuckDB grid: lazy sample, sort, filter,
  type-chip column headers.
- **Variable profile** (right) — type, label, format, distinct values, distribution,
  missingness for the selected column.
- **The bridge** — a variable/dataset action **"Use in a new output →"** jumps to
  Report mode with that data pre-seeded, so exploration flows into the deliverable.

## 11. Visual language

- **Default skin: light SAS-blue (datasetviewer).** SAS blue `#0378cd`, 13px base,
  10.5px uppercase micro-label headers, 18×18 type chips (category=blue, measure=
  violet, date=amber), 3px radius, 2px blue left-stripe selection, no solid-blue
  selectize fill. Familiar to the SAS/clinical audience, honours the north star.
- **Dark modern skin: a later theme toggle.** Indigo-violet accent, tabular figures,
  keyboard-driven (⌘K palette, shortcut hints), output rendered as a white paper
  sheet on a dark canvas. The frame is skin-agnostic, so shipping this as a toggle is
  cheap once the light default lands. Not required for v1.
- **Recorded defaults** (flagged during design, changeable): light default + dark
  toggle; ship a light mode primarily; ⌘K palette, undo/redo, autosave are in;
  accent hex pending a proper token pass.

## 12. What changes vs the current app

- **Demotes the Section A / Section B peer structure.** Manage Data becomes the
  **Data** mode; the Report Builder becomes **Report** mode with the dual-rail frame.
- The current `task_catalog` / `setup_pane` / roles concepts survive but move into the
  rail structure: task catalog → **Outputs** rail; setup pane → **right properties
  rail**; the deliverable list → **Outline** rail.
- The `ex_` prefix and `explorer`/`explorerengine` names are retired in favour of
  bare exports and the `arframe` / `arpillar` packages.

## 13. Out of scope for this design / deferred

- Cross-output **Actions/Rules** interactivity (later milestone).
- The dark skin toggle (post-v1).
- A standalone template-**Library** management screen (templates are reached via the
  Outputs rail / Add-output; a full manage screen is YAGNI until needed).
- Connectors to heavy DB drivers (stay in `Suggests`).

## 14. Golden-gate invariants (preserved through the rebuild)

- Non-empty RTF export matching a byte/XML golden.
- Every graph round-trips ggplot → `figure()` → golden.

## 15. Next step

Hand to `writing-plans` to decompose this into a staged implementation (this is a
large rebuild: package rename to `arframe`/`arpillar`, shell/dual-rail frame, the
five left activities, the canvas shell-preview + direct manipulation, the right
properties rail, Data mode, JSON persistence + migrators, the light skin, and the
code-export seam).
