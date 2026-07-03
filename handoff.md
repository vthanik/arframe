# handoff — arframe

Submission-native clinical report builder: the **Galley** UI (thin bslib Shiny
shell) on the **arpillar** engine. `arframe()` is the only export; everything
else is `@noRd`. Binding design spec + plan live in `docs/superpowers/specs/`
and `docs/superpowers/plans/`.

## STATUS (master @ `5a2129c`, 2026-07-03)

Plan Stage-5 (Tasks 1–17) is DONE and merged. On top of it, a large run of UI
iterations this session (below). **`devtools::check` = 0 errors / 0 warnings**
(the lone NOTE, "unable to verify current time", is the spurious offline
time-server check — ignore it). Working tree clean. **Nothing is pushed** —
see Integration.

## Architecture (what a fresh session must know)

- **One injected store** (`new_store(con)` in `fct_store.R`): all draft/edit
  state in `store$rv` (reactiveValues); modules communicate ONLY through it,
  never the DOM. Plain-env side maps: `sources`/`kinds` (data provenance),
  `cache` (ARD memo). `commit()/undo()/redo()`; `update_object()` marks
  `rv$stale` on a heavy edit (the two-stage `.ard_key()` seam).
- **Logic lives in arpillar**; modules wire UI↔store. No DBI/tabular/ggplot2
  in an arframe `render*`/`observe`/`reactive`.
- **Modules** (`R/mod_*.R`): frame (bar/modes/undo/redo/async-export),
  contents (TOC), paper (the canvas), card (the inspector),
  card_roles/options/filters, data (Data mode), add_output (overlay), qc (the
  "Logs" sheet). Pure walkers in `utils_report.R`; atoms in `utils_atoms.R`
  (`.fa_names` icon map, `.stamp`, `.action_btn`); ghost shell `utils_ghost.R`;
  export `fct_export.R`; async `fct_async.R`.
- **Data mode = embedded `datasetviewer` widget** (in-browser DuckDB,
  virtualized, full filter/sort/find). `output$dv <-
  datasetviewer::renderDatasetViewer(dataset_viewer(arpillar::dataset_path(
  con, name)))`. arframe supplies only the breadcrumb + × close chrome; the
  SAMPLE-table grid and the whole `.dataset_meta`/`fct_meta` seam were REMOVED
  when the widget landed.
- **Report canvas = READ-ONLY tabular preview** (`mod_paper.R`): renders
  `arpillar::render_spec()` -> `htmltools::as.tags()`. The galley
  click-to-edit region system (decisions #7/#8) was REMOVED — no
  `data-ar-region`, no margin-marks, no ghost-slot clicking. Editing is
  entirely in the right rail. The error-summary jump links still post
  `input$region` (navigation to the rail) — that observer stays.
- **Inspector** (`mod_card.R`): a VERTICAL icon+label tab strip
  (Roles/Options/Filters/Ranks, glyphs table-list/sliders/filter/sort) on the
  rail's **FAR-RIGHT edge** (CSS `order:1`), role content to its left. A
  **resize handle** (`.ar-insp-resize`, arframe.js drag) sets the rail width
  (220–640px, session-persistent). Clicking the ACTIVE tab **collapses** the
  pane to the 63px icon strip (the strip stays = the show/hide toggle);
  clicking any tab re-opens. `rv$insp_collapsed` mirrored via the frame's
  `ar-collapse` message.

## This session's changes (all merged to master, newest first)

1. `5a2129c` inspector tab strip doubles as a show/hide toggle.
2. `7df8e6d` ONE compact XL button size app-wide — set Bootstrap 5's
   `--bs-btn-*` vars on `.btn` (default was ~16px/38px = the "XXXL"); softened
   the last two yellow focus rings (`.ar-add-card:focus`, `.ar-problem:focus`).
3. `b63b815` inspector tabs -> far-right icon+label strip (mockup piece "D").
4. `d3c2bd4` Report redesign: read-only canvas + vertical side tabs +
   draggable rail (reverses locked decisions #7/#8).
5. `8c9ee62` focus ring softened globally, inspector footer buttons trimmed,
   shinyFiles dialog -> IBM Plex, **QC renamed to Logs**, Sources rail chevron.
6. `b2cd72a`/`daca4e0`/`f6d445a` Data mode: embed datasetviewer (drop the
   sample grid), IBM Plex on the widget, grid fills height + X-close + hidden
   list toolbar.
7. Earlier: artoo labels/property/sort (SUPERSEDED by the widget); dropped
   nanoparquet, `artoo` -> Imports; Tasks 15/16/17 (Logs sheet, mirai async
   export, a11y/responsive).

## NEXT STEPS — the mockup redesign (user is steering here)

The user shared an IDE-style mockup ("Widget from visualize show_widget") and
wants arframe to move toward it. It **supersedes binding decision #1** (which
said NO canvas tabs, NO dual rails). Pieces:

- **DONE — D**: right-rail icon tabs on the far edge (`b63b815`).
- **PENDING — C**: richer OUTLINE (rename CONTENTS): group TABLES/FIGURES/
  LISTINGS, colored status **dot** + word (Ready/Draft/Needs data) + TLF
  number, card-highlighted active row.
- **PENDING — A**: left **activity bar** (Data / Outputs / Outline / Suggest /
  Review vertical icon rail). Ask which of the 5 are real vs placeholder.
- **PENDING — B**: **canvas per-output tabs** (Demographics / Adverse events /
  … / +). Biggest — adds multi-open-output state, reverses "no canvas tabs".

Suggested order if they say continue: C -> A -> B. Confirm scope before B.

## Integration / push — NOTHING PUSHED (decision pending)

- **No GitHub repos exist**: `gh repo view vthanik/arpillar` and
  `vthanik/arframe` both 404. `vthanik/tabular` DOES exist (public). arframe
  `DESCRIPTION` Remotes: `vthanik/arpillar`, `vthanik/artoo`,
  `vthanik/datasetviewer`, `vthanik/tabular@<sha>` — none but tabular resolve.
- arpillar `333b8ce` (`unregister_dataset()`) is LOCAL-ONLY on its
  `feat/ui-prereqs` branch (8 commits ahead of arpillar `master`, the default
  branch — NOT `main`), no git remote configured.
- Publishing arframe = a **first-time multi-repo publish** (create
  `vthanik/{arpillar,artoo,datasetviewer,arframe}` + push), NOT a simple push.
  User chose **HOLD**. Do NOT create repos or push without an explicit
  per-session go-ahead + visibility (public/private) + the public-surface
  hygiene denylist check.

## How to run it (eyeball, real CDISC data — decision #9)

```r
devtools::load_all("/Users/vignesh/projects/r/arframe")
arframe(folders = c(
  "/Users/vignesh/projects/data/cdisc-adam-pilot",
  "/Users/vignesh/projects/data/cdisc-sdtm-pilot"
))
```

Headless eyeball: `shiny::runApp(app, port = 7788, launch.browser = FALSE)` in
a background Rscript, drive with `chromote`. Screens in `.local/screens/`
(gitignored). Pass `daemons = 0` for test/headless launches (skips the mirai
pool).

## Gotchas / seams

- **`arframe()` builds ONE store** at construction, shared by every browser
  session of that process — repeated `chromote` connections ACCUMULATE
  outputs. Restart the app for clean eyeball state.
- **testServer can't replay `priority:event`** — repeated same-value inputs
  won't re-fire `observeEvent`; give the payload a nonce (keyboard nav + tab
  toggle tests do this).
- **`.dataset_meta`/`fct_meta` are GONE** — datasetviewer reads its own
  metadata. If Report-mode ever wants variable labels, re-add (~90 lines + a
  test).
- `datasetviewer` vendors the DuckDB-WASM bundle in its package (works
  offline). Its font vars (`--dv-sans/--dv-mono/--gdg-font-family`) are
  overridden to IBM Plex on `.ar-dx-dv .datasetviewer-root`.
- A few now-orphaned section-08 CSS selectors (`.ar-colpick`, `.ar-dx-data`,
  `.ar-dx-th`) are harmless dead rules from the removed sample grid — later
  CSS sweep. `.ar-insp-slim` is vestigial (superseded by the tab strip).

## Conventions in force

Worktree + staged + real-data eyeball per change; merged `--ff-only` to
master; worktree/branch removed after. Gate `devtools::check` 0/0/0.
`\uXXXX` escapes for non-ASCII in R strings (even HTML output). **No `--` in
user-facing text** — em-dash `—` (`"—"` in R, literal in JS/CSS); rule in
`CLAUDE.md`. `air format` (PostToolUse hook). No `Co-Authored-By: Claude` / AI
attribution. **Never push without explicit per-session approval.** Ponytail
mode + ultracode were active this session.
