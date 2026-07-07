# Setup sectioned dashboard + frame top-nav — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Re-house the app frame with a top mode-nav bar (drop the left sidebar, strip the Refresh/Undo/Redo circles) and re-house the Setup page as a sectioned dashboard (section tab strip, one card per section, an overview stat strip), with zero change to Setup's wiring/logic.

**Architecture:** Pure UI re-housing over the existing store + mode-switch machinery. Frame: nav markup moves sidebar→top app bar; report title moves to a page-header row; undo/redo become ⌘Z/⌘⇧Z. Setup: `.setup_section()` emits a `.card()`; a horizontal tab strip shows one section at a time via a CSS class (`ar-setup-tab-<id>`) that is **server-authoritative** (a `reactiveVal` restamped on every `renderUI`) so a mid-edit commit never bounces the active tab. `.SETUP_SPEC`, all collectors, all observers, and export are untouched.

**Tech Stack:** R + Shiny modules, S7 report object (via `arpillar`), bslib theme, hand-written CSS (`inst/www/arframe.css`), esbuild JS bundle from `srcjs/bridge.js`, testthat + a Chrome e2e screenshot walk.

## Global Constraints

- **Zero logic change** to `.SETUP_SPEC`, `.wire_all()`, `.theme_set()`, every collector (`.collect_pops/_arms/_foots/_conts/_band_rows`, `.band_to_rows`), every structural observer, the mode-switch mechanism (`input$mode` / `ar-mode` message / `.ar-mode-*`), the export pipeline, and the save-state chip.
- **`arframe()` is the only export**; every new/edited helper is `@noRd`.
- **No `--` in user-facing text**; em-dash `—`. Message strings to `cli_*`/`stop`/`warning` stay ASCII (escape `\uXXXX` in string literals as the existing code does).
- **Native pipe `|>`**, `::`-qualify everything, no `library()` in `R/`, `vapply` not `sapply`, `seq_along`/`seq_len`.
- **Frame module namespace is `"frame"`** (ids are `frame-undo_btn`, `frame-export_btn`, …).
- **Dev-loop gate before any commit that touches R:** `devtools::document()` then `devtools::test()` then `devtools::check(args="--no-manual")` all 0/0/0; `air format R/ tests/`. When `srcjs/` changes: `Rscript tools/build.R`.
- **Verification is real-data:** boot on `/Users/vignesh/projects/data/cdisc-adam-pilot`; screenshot-eyeball every mode (green tests are not sufficient).

---

## File Structure

- `R/mod_frame.R` — UI: sidebar→top app bar, report title→pagehead, drop Refresh/Undo/Redo buttons. Server: drop the refresh observer + the undo/redo `ar-disable` mirroring; keep the `input$undo_btn`/`input$redo_btn` observers (now keyboard-fed).
- `R/mod_setup.R` — `.setup_section()`→`.card()`; new `.setup_tabstrip()` + `.setup_overview()` builders; `active_tab` reactiveVal + `input$setup_tab` observer; `renderUI` wraps output in `.ar-setup-dash ar-setup-tab-<active>`.
- `inst/www/arframe.css` — §02 frame block (horizontal app bar + underline mode tabs; drop `.ar-sidebar`; add `.ar-pagehead`); Setup block (cards, tab strip, one-at-a-time visibility, overview strip); delete dead `.ar-setup-glyph-*`.
- `srcjs/bridge.js` + `inst/www/arframe.bundle.js` — global ⌘Z/⌘⇧Z keydown → `frame-undo_btn`/`frame-redo_btn`.
- `tests/testthat/test-mod_frame.R` — rewrite structural assertions for the top nav.
- `tests/testthat/test-mod_setup_wire.R` — unchanged wiring assertions + new active-tab regression.
- `CLAUDE.md` — rewrite #8 / #12.1; update #12.2.

---

## Task 1: Frame UI + server → top app bar, buttons removed

**Files:**
- Modify: `R/mod_frame.R` (`.frame_sidebar`→remove; `.frame_topbar`, `.frame_nav`, `mod_frame_ui`, `mod_frame_server`)
- Test: `tests/testthat/test-mod_frame.R`

**Interfaces:**
- Consumes: `.icon()`, `.action_btn()`, `.nav_item()` (unchanged), `store` API (`undo/redo/open_project/export`).
- Produces: `mod_frame_ui(id, report_body, data_body, qc_body, logs_body, setup_body)` DOM = `.ar-workspace.ar-mode-setup > .ar-main > (.ar-topbar, .ar-pagehead, .ar-body)`; `.ar-topbar` contains `.ar-appbar-brand`, `.ar-nav[role=tablist]` of `.ar-nav-item[data-ar-mode]`, `.ex-appbar-actions` (⌘K hint + Package + hidden dl). `.ar-pagehead` contains the click-to-edit title. No `#frame-refresh_btn`/`#frame-undo_btn`/`#frame-redo_btn` buttons.

- [ ] **Step 1: Rewrite the structural test** (`test-mod_frame.R`, first `test_that` block) to assert the top-nav DOM. Replace the sidebar assertions:

```r
test_that("mod_frame_ui HTML is a top app bar (mode tabs), a pagehead title, and five mode bodies", {
  ui <- mod_frame_ui(
    "frame",
    report_body = shiny::div("report placeholder"),
    data_body = shiny::div("data placeholder"),
    qc_body = shiny::div("qc placeholder"),
    logs_body = shiny::div("logs placeholder"),
    setup_body = shiny::div("setup placeholder")
  )
  html <- as.character(ui)

  # Top app bar carries the brand, the mode tablist, and the actions cluster.
  expect_match(html, "ar-topbar", fixed = TRUE)
  expect_match(html, "ar-appbar-brand", fixed = TRUE)
  expect_match(html, "ar-nav-item", fixed = TRUE)
  expect_match(html, "ex-appbar-actions", fixed = TRUE)
  # The left sidebar is gone.
  expect_no_match(html, "ar-sidebar", fixed = TRUE)
  # The report title now lives in a page-header row, not the bar.
  expect_match(html, "ar-pagehead", fixed = TRUE)
  # One nav item per mode, each carrying data-ar-mode for bridge.js.
  for (m in c("setup", "data", "report", "qc", "logs")) {
    expect_match(html, sprintf('data-ar-mode="%s"', m), fixed = TRUE)
    expect_match(html, sprintf("ar-body-%s", m), fixed = TRUE)
  }
  # The three circle icons are gone.
  expect_no_match(html, 'id="frame-refresh_btn"', fixed = TRUE)
  expect_no_match(html, 'id="frame-undo_btn"', fixed = TRUE)
  expect_no_match(html, 'id="frame-redo_btn"', fixed = TRUE)
  # Async export button + hidden download link survive.
  expect_match(html, 'id="frame-export_btn"', fixed = TRUE)
  expect_match(html, 'id="frame-export_dl"', fixed = TRUE)
})
```

- [ ] **Step 2: Run it, expect FAIL**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-mod_frame.R")'`
Expected: FAIL (still emits `ar-sidebar`, no `ar-pagehead`, still has the removed buttons).

- [ ] **Step 3: Rewrite `mod_frame_ui`** — sidebar out, app bar + pagehead in:

```r
mod_frame_ui <- function(
  id,
  report_body,
  data_body,
  qc_body,
  logs_body,
  setup_body = NULL
) {
  ns <- shiny::NS(id)
  shiny::div(
    # Opens in Setup mode -- study configuration is the first stop.
    class = "ar-workspace ar-mode-setup",
    shiny::div(
      class = "ar-main",
      .frame_topbar(ns),
      .frame_pagehead(ns),
      shiny::div(
        class = "ar-body",
        shiny::div(class = "ar-body-setup", setup_body),
        shiny::div(class = "ar-body-data", data_body),
        shiny::div(class = "ar-body-report", report_body),
        shiny::div(class = "ar-body-qc", qc_body),
        shiny::div(class = "ar-body-logs", logs_body)
      )
    )
  )
}
```

- [ ] **Step 4: Rewrite `.frame_topbar`** — brand + mode tabs + actions (no Refresh/Undo/Redo; keep Open, save chip, ⌘K hint, Package, hidden dl). Delete `.frame_sidebar`:

```r
#' The top app bar: brand, the horizontal mode tablist, and the global
#' actions cluster (Open / save chip / palette hint / Package). Mode
#' switching is the delegated `[data-ar-mode]` click (bridge.js) -> the
#' pure-CSS `.ar-mode-*` class; unchanged from the sidebar era, only
#' relocated and restyled as underline tabs.
#' @noRd
.frame_topbar <- function(ns) {
  shiny::div(
    class = "ar-topbar",
    shiny::div(
      class = "ar-appbar-brand",
      shiny::span(class = "ar-appbar-mark", `aria-hidden` = "true"),
      shiny::span(class = "ar-appbar-word", "arframe")
    ),
    .frame_nav(),
    shiny::div(
      class = "ex-appbar-actions",
      shiny::div(
        class = "ar-picker",
        shinyFiles::shinyDirButton(
          ns("open_project"),
          label = "Open",
          title = "Open project folder",
          class = "ex-btn-sm btn btn-outline-secondary"
        )
      ),
      shiny::span(
        id = ns("save_chip"),
        class = "ar-save-chip",
        `data-state` = "idle",
        shiny::span(class = "ar-save-chip-lbl", "Saved")
      ),
      # Command palette hint (bridge.js fills the glyph per navigator.platform).
      shiny::span(class = "ar-bar-hint ar-mono"),
      shiny::span(class = "ex-tb-sep"),
      shiny::tags$button(
        id = ns("export_btn"),
        type = "button",
        class = "ar-btn-ink action-button",
        .icon("package", 13),
        shiny::span("Package"),
        shiny::span(
          class = "ar-btn-kbd ar-mono",
          shiny::HTML("&#8984;&#8679;E")
        )
      ),
      shiny::tagAppendAttributes(
        shiny::downloadLink(ns("export_dl"), label = NULL, class = "ar-hidden-dl"),
        `aria-hidden` = "true",
        tabindex = "-1"
      )
    )
  )
}
```

- [ ] **Step 5: Add `.frame_pagehead`** (report title relocated here). Keep `.frame_title`'s internals; just wrap in the pagehead row:

```r
#' The page-header row below the app bar: the click-to-edit report title,
#' left-aligned, present in every mode. Per-mode header content (e.g.
#' Setup's overview strip) is rendered inside that mode's body, styled to
#' sit contiguously beneath this row.
#' @noRd
.frame_pagehead <- function(ns) {
  shiny::div(class = "ar-pagehead", .frame_title(ns))
}
```

Leave `.frame_nav`, `.nav_item`, `.frame_title` bodies unchanged.

- [ ] **Step 6: Trim `mod_frame_server`** — remove the Refresh observer and the undo/redo `ar-disable` mirroring; keep undo/redo observers (now keyboard-fed). Delete this block:

```r
    shiny::observeEvent(input$refresh_btn, {
      tryCatch(
        .refresh_all(store),
        error = function(e) {
          log_line(store, sprintf("refresh failed: %s", conditionMessage(e)))
        }
      )
    })
```

and delete the trailing `shiny::observe({ store$rv$report; session$sendCustomMessage("ar-disable", ... undo_btn ...); ... redo_btn ... })` block (the buttons it disabled no longer exist). Keep `observeEvent(input$undo_btn, undo(store))` and `observeEvent(input$redo_btn, redo(store))` verbatim. Keep everything else (mode, collapse, name, open_project, save chip, export).

> Note: `.refresh_all` still fires via the window-focus path (bridge.js / tab-focus) — confirm that call site remains; the manual button was redundant. If the ONLY caller of `.refresh_all` was `input$refresh_btn`, keep the function and wire it to the existing focus handler instead of deleting the trigger. Grep before deleting: `grep -rn "refresh_all\|refresh_btn" R/ srcjs/`.

- [ ] **Step 7: Update the server test comment/first-mode expectation** if the startup-mode comment in `test-mod_frame.R` references the sidebar; the `mode`-switch `testServer` assertions themselves are unchanged (mechanism is identical). Run the full file:

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-mod_frame.R")'`
Expected: PASS.

- [ ] **Step 8: Gate + commit**

```bash
Rscript -e 'devtools::document()'
air format R/ tests/
Rscript -e 'devtools::test()'
git add R/mod_frame.R tests/testthat/test-mod_frame.R
git commit -m "feat(frame): top app bar mode nav; drop sidebar + refresh/undo/redo buttons"
```

---

## Task 2: Frame CSS — app bar, underline mode tabs, pagehead

**Files:**
- Modify: `inst/www/arframe.css` (§02 block: `.ar-workspace`, `.ar-sidebar*`→remove, `.ar-topbar`, `.ar-nav*`, add `.ar-appbar-*`, `.ar-pagehead`)

**Interfaces:**
- Consumes: the DOM from Task 1. Produces: no R interface (visual only; gated by the screenshot walk in Task 8).

- [ ] **Step 1: Change `.ar-workspace` from a two-column grid to a single column.** Replace the sidebar-based rule (near line 268) so the workspace stacks the app bar over content, full width. Example:

```css
.ar-workspace {
  display: flex;
  flex-direction: column;   /* was: grid with a sidebar column */
  height: 100vh;
  min-height: 0;
  background: var(--ar-desk);
}
.ar-main {
  display: flex;
  flex-direction: column;
  min-height: 0;
  flex: 1 1 auto;
}
```

- [ ] **Step 2: Delete the `.ar-sidebar*` rules** (`.ar-sidebar`, `-brand`, `-mark`, `-word`, `-foot`, and the `<1024px` fold rules that target `.ar-sidebar`). Keep `.ar-nav` / `.ar-nav-item` but restyle them as a **horizontal** tablist:

```css
.ar-topbar {
  display: flex;
  align-items: center;
  gap: var(--ar-space-5);
  height: 56px;
  padding: 0 var(--ar-space-5);
  border-bottom: 1px solid var(--ar-rule);
  background: var(--ar-chrome);
}
.ar-appbar-brand { display: flex; align-items: center; gap: var(--ar-space-2); }
.ar-appbar-word  { font-weight: var(--ar-fw-bold); letter-spacing: -0.01em; }
.ar-nav {
  display: flex;
  gap: var(--ar-space-4);
  height: 100%;
}
.ar-nav-item {
  display: inline-flex;
  align-items: center;
  gap: var(--ar-space-2);
  height: 100%;
  padding: 0 2px;
  border: none;
  border-bottom: 2px solid transparent;   /* underline slot */
  background: transparent;
  color: var(--ar-ink-3);
  font-size: var(--ex-fs-body);
  cursor: pointer;
}
.ar-nav-item:hover { color: var(--ar-ink); }
```

- [ ] **Step 3: Re-point the active-mode rule to the underline** (replace the block at ~line 344 that colours the sidebar item):

```css
.ar-mode-setup  .ar-nav-item[data-ar-mode="setup"],
.ar-mode-data   .ar-nav-item[data-ar-mode="data"],
.ar-mode-report .ar-nav-item[data-ar-mode="report"],
.ar-mode-qc     .ar-nav-item[data-ar-mode="qc"],
.ar-mode-logs   .ar-nav-item[data-ar-mode="logs"] {
  color: var(--ar-ink);
  border-bottom-color: var(--ar-accent);
  font-weight: var(--ar-fw-medium);
}
```

Push the actions cluster to the right: ensure `.ex-appbar-actions { margin-left: auto; display: flex; align-items: center; gap: var(--ar-space-3); }` (add `margin-left:auto` if not already present in the reused block near line 3369).

- [ ] **Step 4: Add the pagehead row:**

```css
.ar-pagehead {
  display: flex;
  align-items: center;
  gap: var(--ar-space-4);
  padding: var(--ar-space-4) var(--ar-space-5) 0;
}
.ar-pagehead .ar-title { font-size: var(--ar-fs-page); font-weight: var(--ar-fw-semibold); }
```

- [ ] **Step 5: Manual smoke** — launch and eyeball once (full gate is Task 8):

```bash
Rscript -e 'devtools::load_all(quiet=TRUE); options(shiny.port=7910); arframe::arframe("/Users/vignesh/projects/data/cdisc-adam-pilot")' &
```
Confirm in Chrome: horizontal mode tabs with the active one underlined, no left sidebar, report name under the bar, no circle icons. Kill the server.

- [ ] **Step 6: Commit**

```bash
git add inst/www/arframe.css
git commit -m "feat(frame): CSS for horizontal app bar, underline mode tabs, pagehead"
```

---

## Task 3: bridge.js — ⌘Z / ⌘⇧Z undo/redo, rebuild bundle

**Files:**
- Modify: `srcjs/bridge.js`
- Build: `inst/www/arframe.bundle.js` (via `tools/build.R`)

**Interfaces:**
- Consumes: Shiny global; the frame ns `"frame"`. Produces: keydown → `Shiny.setInputValue("frame-undo_btn"/"frame-redo_btn", Date.now(), {priority:"event"})`, which Task 1's server observers already handle.

- [ ] **Step 1: Append a global keydown handler** to `srcjs/bridge.js` (mirroring the existing Escape/Arrow global handlers). Guard against form fields so native undo in inputs is not hijacked:

```js
// Undo / redo via keyboard (the app-bar circle buttons were removed 2026-07-07).
// Cmd/Ctrl+Z -> frame-undo_btn ; Cmd/Ctrl+Shift+Z -> frame-redo_btn. Skipped
// while focus is in a text field so native input undo still works there.
$(document).on("keydown", function (e) {
  var key = (e.key || "").toLowerCase();
  if (key !== "z" || !(e.metaKey || e.ctrlKey)) return;
  if (
    e.target &&
    typeof e.target.closest === "function" &&
    e.target.closest("input, textarea, select, [contenteditable]")
  ) {
    return;
  }
  e.preventDefault();
  var id = e.shiftKey ? "frame-redo_btn" : "frame-undo_btn";
  Shiny.setInputValue(id, Date.now(), { priority: "event" });
});
```

- [ ] **Step 2: Rebuild the bundle**

Run: `Rscript tools/build.R`
Expected: `Wrote inst/www/arframe.bundle.js`.

- [ ] **Step 3: Verify the handler is in the built bundle**

Run: `grep -c "frame-undo_btn" inst/www/arframe.bundle.js`
Expected: `>= 1`.

- [ ] **Step 4: Manual verify** — reboot the app, make an edit, press ⌘Z, confirm the edit reverts (and ⌘⇧Z re-applies). Confirm ⌘Z inside a text input still does native text undo, not a report undo.

- [ ] **Step 5: Commit**

```bash
git add srcjs/bridge.js inst/www/arframe.bundle.js
git commit -m "feat(frame): undo/redo on Cmd/Ctrl+Z, replacing the removed buttons"
```

---

## Task 4: Setup section shell → card

**Files:**
- Modify: `R/mod_setup.R` (`.setup_section`)
- Test: `tests/testthat/test-mod_setup_wire.R` (add a structural assertion)

**Interfaces:**
- Consumes: `.card(..., title=, class=)` (Foundation atom, `.ar-panel`). Produces: `.setup_section(ns, id, title, glyph, body)` now emits `.card(class="ar-setup-section", ...)` carrying `data-ar-section=<id>`, title in the card head, `body` in the card body. The `glyph` argument is dropped (status now lives on the tab, Task 5) — update the call sites.

- [ ] **Step 1: Add a structural test** to `test-mod_setup_wire.R`:

```r
test_that(".setup_section renders an elevated card carrying its section id", {
  sec <- .setup_section(shiny::NS("s"), "study", "Study", shiny::div("body"))
  html <- as.character(sec)
  expect_match(html, "ar-panel", fixed = TRUE)
  expect_match(html, 'data-ar-section="study"', fixed = TRUE)
  expect_match(html, "Study", fixed = TRUE)
})
```

- [ ] **Step 2: Run, expect FAIL** (current `.setup_section` has a `glyph` arg and emits `.ar-setup-section` divs, not `.ar-panel`).

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-mod_setup_wire.R")'`

- [ ] **Step 3: Rewrite `.setup_section`** (drop `glyph`, emit a card):

```r
.setup_section <- function(ns, id, title, body) {
  .card(
    body,
    title = title,
    class = "ar-setup-section",
    `data-ar-section` = id
  )
}
```

Update `.card` to forward extra attributes: it currently ignores unknown args. Change its signature to `.card <- function(..., title = NULL, action = NULL, class = NULL)` — it already `...`-collects the body; the `data-ar-section` must reach the root div. Simplest: add an explicit `attribs = NULL` param and `shiny::tagAppendAttributes(root, !!!attribs)`. To keep `.card` generic, instead pass the attribute via `class`-sibling: set it in `.setup_section` by wrapping:

```r
.setup_section <- function(ns, id, title, body) {
  shiny::tagAppendAttributes(
    .card(body, title = title, class = "ar-setup-section"),
    `data-ar-section` = id
  )
}
```

(No `.card` signature change — keeps the atom untouched.)

- [ ] **Step 4: Update the seven `.setup_section(...)` call sites** in `output$page` to drop the glyph argument:

```r
.setup_section(ns, "study", "Study", .setup_study(ns, store)),
.setup_section(ns, "paths", "Paths", .setup_paths(ns, store)),
.setup_section(ns, "treatment", "Treatment", .setup_treatment(ns, store)),
.setup_section(ns, "populations", "Populations", .setup_populations(ns, store)),
.setup_section(ns, "page", "Page & Style", .setup_page_body(ns, store)),
.setup_section(ns, "summaries", "Summaries", .setup_summaries(ns, store)),
.setup_section(ns, "team", "Team", .setup_team(ns, store))
```

(These are re-wrapped by Task 5's tab strip; leave the `.setup_reviewed_banner(store)` call where it is for now.)

- [ ] **Step 5: Run, expect PASS**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-mod_setup_wire.R")'`

- [ ] **Step 6: Gate + commit**

```bash
Rscript -e 'devtools::document()'; air format R/ tests/
Rscript -e 'devtools::test()'
git add R/mod_setup.R tests/testthat/test-mod_setup_wire.R
git commit -m "feat(setup): each section renders as an elevated card"
```

---

## Task 5: Section tab strip + server-authoritative active tab (regression-tested)

**Files:**
- Modify: `R/mod_setup.R` (`output$page` renderUI; add `.setup_tabstrip`; add `active_tab` reactiveVal + `input$setup_tab` observer)
- Test: `tests/testthat/test-mod_setup_wire.R`

**Interfaces:**
- Consumes: `.section_status(theme, section)` (unchanged). Produces: `renderUI` output = `div.ar-setup-dash.ar-setup-tab-<active>` wrapping `.setup_overview(...)` (Task 6), `.setup_tabstrip(ns, store, active)`, then the seven `.setup_section(...)` cards. Client tab click sets the wrapper class + posts `input$setup_tab`; server holds `active_tab` (default `"study"`) and restamps the class every render.

- [ ] **Step 1: Write the regression test** (the load-bearing behaviour — a mid-edit commit must NOT reset the active tab):

```r
test_that("active Setup tab survives a re-render triggered by a commit", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  shiny::testServer(mod_setup_server, args = list(store = store), {
    # User switches to a non-default tab.
    session$setInputs(setup_tab = "summaries")
    expect_identical(active_tab(), "summaries")

    # An unrelated edit commits -> output$page re-renders.
    session$setInputs(study_sponsor = "Acme Pharma")

    # The active tab must still be "summaries" (not reset to "study"),
    # and the rendered page must stamp it on the wrapper.
    expect_identical(active_tab(), "summaries")
    expect_match(
      as.character(output$page),
      "ar-setup-tab-summaries",
      fixed = TRUE
    )
  })
})
```

- [ ] **Step 2: Run, expect FAIL** (`active_tab` not defined; `setup_tab` unhandled).

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-mod_setup_wire.R")'`

- [ ] **Step 3: Add the `active_tab` state + observer** near the top of `mod_setup_server` (after `ns <- session$ns`):

```r
    # The visible section. Server-authoritative so a commit-driven
    # re-render of `output$page` never bounces the user back to the first
    # tab: the client swaps the class instantly for feel, and posts here so
    # the next render restamps the same tab.
    active_tab <- shiny::reactiveVal("study")
    shiny::observeEvent(input$setup_tab, {
      if (is.character(input$setup_tab) && nzchar(input$setup_tab)) {
        active_tab(input$setup_tab)
      }
    })
```

- [ ] **Step 4: Wrap `output$page`'s `tagList` in the dashboard wrapper** and add the tab strip + overview. Replace the `shiny::tagList(.setup_reviewed_banner(store), .setup_section(...), ...)` with:

```r
      active <- active_tab()
      sections <- list(
        list(id = "study", title = "Study", body = .setup_study(ns, store)),
        list(id = "paths", title = "Paths", body = .setup_paths(ns, store)),
        list(id = "treatment", title = "Treatment", body = .setup_treatment(ns, store)),
        list(id = "populations", title = "Populations", body = .setup_populations(ns, store)),
        list(id = "page", title = "Page & Style", body = .setup_page_body(ns, store)),
        list(id = "summaries", title = "Summaries", body = .setup_summaries(ns, store)),
        list(id = "team", title = "Team", body = .setup_team(ns, store))
      )
      shiny::div(
        class = paste0("ar-setup-dash ar-setup-tab-", active),
        .setup_overview(store, sections),
        .setup_reviewed_banner(store),
        .setup_tabstrip(ns, store, sections, active),
        lapply(sections, function(s) {
          .setup_section(ns, s$id, s$title, s$body)
        })
      )
```

- [ ] **Step 5: Add `.setup_tabstrip`** (below `.setup_section`). Reuses `.section_status`; the tab click mirrors `.seg_control`'s inline pattern (instant class swap on `.ar-setup-dash` + `Shiny.setInputValue`), no bundle change:

```r
# Horizontal section tab strip. Each tab shows the section title + a status
# glyph from `.section_status` (check when complete, missing-count when
# partial). The inline click handler swaps the active class on the strip and
# on the `.ar-setup-dash` wrapper (instant), then posts `setup_tab` so the
# server keeps authoritative state across re-renders.
.setup_tabstrip <- function(ns, store, sections, active) {
  theme <- store$rv$report@theme
  input_id <- ns("setup_tab")
  shiny::div(
    class = "ar-setup-tabs",
    role = "tablist",
    lapply(sections, function(s) {
      st <- .section_status(theme, s$id)
      badge <- switch(
        st$state,
        ok = shiny::span(class = "ar-setup-tab-badge ar-setup-tab-ok", "✓"),
        partial = shiny::span(
          class = "ar-setup-tab-badge ar-setup-tab-partial",
          as.character(st$missing)
        ),
        NULL
      )
      click_js <- sprintf(
        "(function(btn){var dash=btn.closest('.ar-setup-dash');if(dash){dash.className=dash.className.replace(/\\bar-setup-tab-[a-z]+\\b/,'ar-setup-tab-%s');}var sibs=btn.parentElement.querySelectorAll('.ar-setup-tab');for(var i=0;i<sibs.length;i++)sibs[i].classList.remove('ar-setup-tab-active');btn.classList.add('ar-setup-tab-active');Shiny.setInputValue('%s','%s',{priority:'event'});})(this)",
        s$id,
        input_id,
        s$id
      )
      shiny::tags$button(
        type = "button",
        class = paste(
          "ar-setup-tab",
          if (identical(s$id, active)) "ar-setup-tab-active" else ""
        ),
        role = "tab",
        `data-ar-setup-tab` = s$id,
        onclick = click_js,
        shiny::span(class = "ar-setup-tab-lbl", s$title),
        badge
      )
    })
  )
}
```

- [ ] **Step 6: Run, expect PASS** (both the regression and the existing wiring tests):

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-mod_setup_wire.R")'`

> If `as.character(output$page)` errors in testServer (renderUI returns a list, not a string), fall back to asserting only `active_tab()` persistence in the regression test and cover the class stamp with a direct render call: `expect_match(as.character(shiny::isolate(...)), ...)`. Prefer the `active_tab()` assertion as the primary gate — it is the bug's crux.

- [ ] **Step 7: Gate + commit**

```bash
Rscript -e 'devtools::document()'; air format R/ tests/
Rscript -e 'devtools::test()'
git add R/mod_setup.R tests/testthat/test-mod_setup_wire.R
git commit -m "feat(setup): section tab strip; server-authoritative active tab"
```

---

## Task 6: Overview stat strip

**Files:**
- Modify: `R/mod_setup.R` (add `.setup_overview`)
- Test: `tests/testthat/test-mod_setup_wire.R`

**Interfaces:**
- Consumes: `.stat_tile(value, label, icon=)`, `.section_status`, `arpillar::catalog_grid(con)` (cols `name/rows/...`), `arpillar::distinct_values(con, dataset, col)`. Produces: `.setup_overview(store, sections)` = `div.ar-setup-overview` of stat tiles: sections-done (always), datasets (when a catalog is mounted), records (pop-dataset rows), subjects (only when the subject-id column resolves and the distinct query succeeds).

- [ ] **Step 1: Write the test** — sections-done tile always present; subjects tile omitted on an empty/naive catalog:

```r
test_that(".setup_overview shows sections-done and omits subjects when unresolved", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  sections <- list(list(id = "study"), list(id = "team"))
  html <- as.character(.setup_overview(store, sections))
  expect_match(html, "ar-setup-overview", fixed = TRUE)
  expect_match(html, "Sections", fixed = TRUE)   # the sections-done tile label
  # No subject-id bound in a bare demo store -> no fabricated subjects tile.
  expect_no_match(html, "Subjects", fixed = TRUE)
})
```

- [ ] **Step 2: Run, expect FAIL** (`.setup_overview` undefined).

- [ ] **Step 3: Implement `.setup_overview`** (defensive; no fabricated numbers):

```r
# The Setup dashboard header strip: a row of stat tiles carrying study-level
# machine facts. Every figure is real or omitted -- a tile whose data is not
# yet resolved does not render (never a fabricated 0). See the
# no-fabricated-render-content rule.
.setup_overview <- function(store, sections) {
  theme <- store$rv$report@theme
  # Sections done: count sections whose status is "ok".
  n_done <- sum(vapply(
    sections,
    function(s) identical(.section_status(theme, s$id)$state, "ok"),
    logical(1)
  ))
  tiles <- list(
    .stat_tile(
      value = sprintf("%d/%d", n_done, length(sections)),
      label = "Sections ready",
      icon = "check"
    )
  )

  cat <- tryCatch(arpillar::catalog_grid(store$con), error = function(e) NULL)
  if (!is.null(cat) && nrow(cat) > 0L) {
    tiles <- c(tiles, list(.stat_tile(
      value = format(nrow(cat), big.mark = ","),
      label = "Datasets",
      icon = "database"
    )))
    # Population dataset (same resolution as .setup_paths: prefer ADSL).
    d <- theme$data %||% list()
    pop <- d$pop_dataset %||%
      (if ("ADSL" %in% cat$name) "ADSL" else cat$name[[1L]])
    pop_row <- cat[cat$name == pop, , drop = FALSE]
    if (nrow(pop_row) == 1L) {
      tiles <- c(tiles, list(.stat_tile(
        value = format(pop_row$rows[[1L]], big.mark = ","),
        label = sprintf("Records (%s)", tolower(pop)),
        icon = "table"
      )))
    }
    # Subjects: distinct subject-id in the pop dataset. Only when resolved.
    subj_col <- d$subject_id %||% "USUBJID"
    subj_col <- trimws(strsplit(subj_col, ",", fixed = TRUE)[[1L]][[1L]])
    n_subj <- tryCatch(
      length(arpillar::distinct_values(store$con, pop, subj_col)),
      error = function(e) NA_integer_
    )
    if (!is.na(n_subj) && n_subj > 0L) {
      tiles <- c(tiles, list(.stat_tile(
        value = format(n_subj, big.mark = ","),
        label = "Subjects",
        icon = "database"
      )))
    }
  }
  shiny::div(class = "ar-setup-overview", tiles)
}
```

> `%||%` is rlang's (already imported); do not redefine. If the demo catalog's `ADSL` happens to resolve a `USUBJID`, the subjects tile WILL show — adjust the test's `expect_no_match` to use a store with no catalog, or assert the tile shows a real count instead. Verify the demo shape first: `Rscript -e 'devtools::load_all(quiet=TRUE); con<-.demo_catalog(); print(arpillar::catalog_grid(con)$name); print(tryCatch(arpillar::distinct_values(con,"ADSL","USUBJID"),error=function(e)e))'` and write the assertion to match reality (real count present, not fabricated).

- [ ] **Step 4: Run, expect PASS** (adjust the assertion per the note if the demo resolves subjects).

- [ ] **Step 5: Gate + commit**

```bash
Rscript -e 'devtools::document()'; air format R/ tests/
Rscript -e 'devtools::test()'
git add R/mod_setup.R tests/testthat/test-mod_setup_wire.R
git commit -m "feat(setup): overview stat strip (sections/datasets/records/subjects)"
```

---

## Task 7: Setup CSS — cards, tab strip, one-at-a-time, overview; delete dead glyph

**Files:**
- Modify: `inst/www/arframe.css` (Setup block ~lines 490-645)

**Interfaces:**
- Consumes: DOM from Tasks 4-6 (`.ar-setup-dash.ar-setup-tab-<id>`, `.ar-setup-tabs > .ar-setup-tab`, `.ar-setup-section[data-ar-section]`, `.ar-setup-overview`). Produces: visual only (gated in Task 8).

- [ ] **Step 1: Restyle the Setup shell** — the content column now holds a stacked dashboard (overview → tabs → the one visible card). Keep the centered column + container queries but drop the per-section hairline rule:

```css
.ar-setup > .shiny-html-output { padding: 0; }   /* the dash owns its own padding */
.ar-setup-dash {
  max-width: 1120px;
  margin: 0 auto;
  padding: var(--ar-space-5) var(--ar-space-7) 120px;
  container-type: inline-size;
}
.ar-setup-overview {
  display: flex;
  flex-wrap: wrap;
  gap: var(--ar-space-5);
  padding: var(--ar-space-4) var(--ar-space-5);
  margin-bottom: var(--ar-space-5);
  background: var(--ar-paper);
  border: 1px solid var(--ar-paper-edge);
  border-radius: var(--ar-radius-xl);
  box-shadow: var(--ar-shadow-card);
}
```

- [ ] **Step 2: Style the tab strip:**

```css
.ar-setup-tabs {
  display: flex;
  gap: var(--ar-space-4);
  margin-bottom: var(--ar-space-5);
  border-bottom: 1px solid var(--ar-rule);
  overflow-x: auto;
}
.ar-setup-tab {
  display: inline-flex;
  align-items: center;
  gap: var(--ar-space-2);
  padding: var(--ar-space-3) 2px;
  border: none;
  border-bottom: 2px solid transparent;
  background: transparent;
  color: var(--ar-ink-3);
  font-size: var(--ex-fs-body);
  white-space: nowrap;
  cursor: pointer;
}
.ar-setup-tab:hover { color: var(--ar-ink); }
.ar-setup-tab-active {
  color: var(--ar-ink);
  border-bottom-color: var(--ar-accent);
  font-weight: var(--ar-fw-medium);
}
.ar-setup-tab-badge {
  font-size: var(--ex-fs-micro);
  padding: 1px 6px;
  border-radius: var(--ar-radius-pill);
  font-family: "IBM Plex Mono", monospace;
}
.ar-setup-tab-ok { background: var(--ar-ready-bg); color: var(--ar-ready); }
.ar-setup-tab-partial { background: var(--ar-draft-bg); color: var(--ar-draft); }
```

- [ ] **Step 3: One section visible at a time** — driven by the wrapper class:

```css
.ar-setup-dash .ar-setup-section { display: none; }
.ar-setup-tab-study       .ar-setup-section[data-ar-section="study"],
.ar-setup-tab-paths       .ar-setup-section[data-ar-section="paths"],
.ar-setup-tab-treatment   .ar-setup-section[data-ar-section="treatment"],
.ar-setup-tab-populations .ar-setup-section[data-ar-section="populations"],
.ar-setup-tab-page        .ar-setup-section[data-ar-section="page"],
.ar-setup-tab-summaries   .ar-setup-section[data-ar-section="summaries"],
.ar-setup-tab-team        .ar-setup-section[data-ar-section="team"] {
  display: block;
}
```

- [ ] **Step 4: Delete the dead rules** — `.ar-setup-glyph`, `.ar-setup-glyph-ok`, `.ar-setup-glyph-partial`, `.ar-setup-glyph-none` (they styled the removed chip on non-existent tokens `--ar-ok`/`--ar-warn`/`--ar-chrome-2`), and the old `.ar-setup-section { border-top }` / `.ar-setup-sechead*` rules superseded by the card. Grep to be sure nothing else references them: `grep -n "ar-setup-glyph\|ar-setup-sechead" inst/www/arframe.css R/*.R`.

- [ ] **Step 5: Smoke** — reboot, click through all 7 tabs, confirm one card at a time, status badges on complete/partial tabs, overview tiles across the top. Kill the server.

- [ ] **Step 6: Commit**

```bash
git add inst/www/arframe.css
git commit -m "feat(setup): CSS for cards, section tab strip, overview; drop dead glyph"
```

---

## Task 8: Docs + full verification gate

**Files:**
- Modify: `CLAUDE.md` (#8, #12.1, #12.2)

- [ ] **Step 1: Rewrite CLAUDE.md #8 and #12.1** so the mode nav is documented as a **top app bar** (not a left sidebar), Refresh removed (auto on focus), Undo/Redo on ⌘Z/⌘⇧Z, report name in the pagehead. Mark #12.2 (Setup → sectioned dashboard) **DONE** with a one-line description (tab strip, per-section cards, overview strip, server-authoritative active tab).

- [ ] **Step 2: Full gate**

```bash
Rscript -e 'devtools::document()'
air format R/ tests/
Rscript -e 'devtools::test()'                      # FAIL 0
Rscript -e 'devtools::check(args="--no-manual")'   # Status: OK (0/0/0)
```

Expected: FAIL 0; Status OK 0/0/0. Fix anything that isn't before proceeding.

- [ ] **Step 3: Real-data screenshot eyeball (all 5 modes)** — per the `feedback-screenshot-eyeball` rule, green tests are not sufficient:

```bash
NOT_CRAN=true Rscript -e 'devtools::test(filter="mod_frame")'   # e2e mode-walk + screenshots
```

Then boot on the CDISC ADaM pilot and drive Chrome to shoot each mode; eyeball every one:

```bash
Rscript -e 'devtools::load_all(quiet=TRUE); options(shiny.port=7910); arframe::arframe("/Users/vignesh/projects/data/cdisc-adam-pilot")'
```

Eyeball checklist: (a) top mode tabs, active one underlined, no left sidebar; (b) the three circle icons gone; (c) report name in the pagehead; (d) Setup — overview tiles, tab strip with status badges, one card at a time; (e) switch to Treatment, edit a field (commit), confirm the tab does **not** jump back to Study; (f) ⌘Z reverts, ⌘⇧Z re-applies; (g) Data / Report / Review / Logs still render cleanly under the new frame.

- [ ] **Step 4: Commit docs**

```bash
git add CLAUDE.md
git commit -m "docs: top-nav frame + Setup sectioned dashboard (supersede #8/#12.1 sidebar lock)"
```

---

## Self-review (against the spec)

- **Spec Part A1 (top app bar, no sidebar)** → Tasks 1-2. **A2 (remove circles; undo/redo→keyboard; refresh redundant)** → Task 1 (buttons/observers) + Task 3 (keyboard). **A3 (save chip kept)** → Task 1 Step 4. **A4 (report name → pagehead)** → Task 1 Steps 5, Task 2 Step 4.
- **Spec Part B1 (sections→cards)** → Task 4. **B2 (tab strip + status)** → Task 5 (strip) + Task 7 (style). **B3 (one at a time)** → Task 7 Step 3. **B4 (server-authoritative active tab)** → Task 5 (reactiveVal + regression test). **B5 (overview tiles, subjects defensive)** → Task 6.
- **Dead `.ar-setup-glyph` removal** → Task 7 Step 4. **Docs/lock update** → Task 8 Step 1. **Zero-logic-change** preserved: no task edits `.SETUP_SPEC`/collectors/observers/export; existing `test-mod_setup_wire.R` wiring assertions must stay green (Tasks 4-6 gates).
- **Verification gate** (test/check 0/0/0 + 5-mode eyeball) → Task 8.
- **Type consistency:** `active_tab()` (reactiveVal) and `input$setup_tab` (character id) used identically in Tasks 5-6; `.setup_section(ns, id, title, body)` 4-arg signature used at all call sites (Task 4 Step 4, Task 5 Step 4); `.setup_overview(store, sections)` and `.setup_tabstrip(ns, store, sections, active)` signatures consistent between definition and call.
- **Open risk flagged inline:** testServer `as.character(output$page)` may return a list — Task 5 Step 6 note gives the `active_tab()`-only fallback (the crux assertion). Task 6 Step 3 note requires verifying the demo catalog's subject resolution and writing the assertion to reality.
