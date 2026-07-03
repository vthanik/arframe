# handoff — arframe

## Latest: Data grid = embedded datasetviewer (2026-07-03)

`feat/data-dsv` (off `master` @ 3877bda), merged. **Reverses the "skip
DuckDB-WASM / hand-rolled sample grid" direction** below — the sample-table
approach was slow at high row counts AND could not find a specific subject (a
sample won't contain an arbitrary one; the "Filter datasets" box only filters
the dataset LIST). So Data mode's "View data" grid is now the **embedded
`datasetviewer` widget** (the user's own package): in-browser DuckDB,
virtualized rows, full typed sort / filter / column-select / property panel,
no sampling. arframe supplies only the breadcrumb chrome; `output$dv <-
datasetviewer::renderDatasetViewer(dataset_viewer(dataset_path(con, name)))`
feeds it the on-disk file (labels intact in the parquet).

- **Added** `datasetviewer` to Imports (+ `Remotes: vthanik/datasetviewer`);
  its DuckDB-WASM bundle is vendored in the package (works offline).
- **Removed (superseded):** the sample table, `.column_picker`,
  `.property_panel`/`.property_rows`, `.grid_preview`, `.type_badge`, the
  `.sample_size_select`/`grid_n` selector, the `.ar-colpick`/`.ar-dx-th` JS
  handlers, CSS section 13 data-meta, AND the whole `R/fct_meta.R` +
  `.dataset_meta` + `store$meta` (no consumer once the widget reads its own
  metadata). `artoo` stays Imports (still used by `.demo_register`).
- Gate 0/0/0 (lone NOTE = spurious offline time check); 815 tests. Real-data
  eyeball verified the widget mounts + loads: ADSL Total rows 254 / cols 48,
  pagination + FILTER + property panel all live (`.local/screens/data-dsv.png`).
- Note: `.data_grid`'s `datasetviewerOutput(ns("dv"))` lives inside the
  `output$explorer` renderUI (re-inits the widget per dataset-open -- fine).
  A few now-orphaned section-08 CSS selectors (`.ar-colpick`, `.ar-dx-data`,
  `.ar-dx-th`) are harmless dead rules left for a later CSS sweep.

## Superseded: Data-mode follow-ups (2026-07-03)

`feat/data-polish` (off `master` @ 9ac17cd), merged. Three tweaks on the
data-mode work below:
- **Dropped `nanoparquet`; `artoo` -> Imports.** artoo reads AND writes
  parquet, and nanoparquet was write-only in fixtures/demo -- so `.demo_register`
  + the test fixtures now use `artoo::write_parquet`. Because `utils_demo.R` is
  in `R/`, artoo can no longer be a conditional Suggests; it is Imports now
  (core to Data mode anyway), which also let `.read_dataset_meta()` drop its
  `requireNamespace` guard (the file-unreadable `tryCatch` fallback stays).
- **Banned `--` in user-facing text** -> em-dash `—` (`"—"` as `—` in R to
  keep source ASCII-clean; literal `—` in JS). Fixed 6 placeholders in
  `mod_data.R` + arframe.js. New hard rule in `CLAUDE.md` (Working
  conventions): no `--` in displayed strings; `--`-as-em-dash in `#`/`#'`
  comments is still fine.
- **Sample-size selector** in the grid breadcrumb (`Show [50/100/250/500/1000]
  rows`) -> `store$rv$grid_n` (default 100, persists across datasets); the grid
  re-samples on change. Server guards the value to the preset set.
- Gate 0/0/0 (lone NOTE = spurious offline time check); 841 tests. Real-data
  eyeball: `.local/screens/data-samplesize.png` (selector at 500 -> all 254
  ADSL rows; Format `—`).

## Most recent: Data-mode column metadata + sort (2026-07-03)

`feat/data-meta-sort` (worktree `../arframe-wt-datameta`, off `master` @
ae0c32a), merged to `master`. Data mode now shows the SAS-style variable
metadata artoo recovers from the on-disk file (the DuckDB catalog drops
labels/formats): the column list shows each variable's **label**; a
**Property/Value panel** (Label/Name/Type/Length/Format) tracks the active
column client-side; grid headers **sort** the 100-row sample typed
(numeric/lexical, asc/desc/original) with zero round-trip. DuckDB-WASM stays
out (it never shipped); the sample stays capped at 100.

- Decision: use artoo directly (the user's own package; Suggests + Remotes
  `vthanik/artoo`, guarded by `requireNamespace`) rather than routing through
  arpillar. `.dataset_meta()` reads `artoo::columns(read_dataset(path, n_max =
  0))` off `dataset_path()` (labels survive arpillar's xpt->parquet convert in
  the parquet metadata, even though `data_items()` returns them NA), memoized
  in `store$meta` (cleared on unmount).
- Files: `R/fct_meta.R` (+ test), `R/mod_data.R` (`.column_picker`/
  `.property_panel`/`.property_rows`/`.grid_preview` rewrite), `R/fct_store.R`
  (`meta` env), `inst/www/arframe.js` (property-panel fill + typed sort),
  `inst/www/arframe.css` (section 13; column list capped `55vh` so the panel
  stays on screen for a 48-column ADaM dataset), DESCRIPTION.
- `devtools::check` = 0 errors / 0 warnings; the lone NOTE ("unable to verify
  current time") is the spurious offline time-server check, not code. Real
  CDISC eyeball: 48/48 ADSL columns labelled, property panel Study Identifier
  / STUDYID / Character / 12, AGE sort asc=51 desc=88 (`.local/screens/
  data-meta.png`, `data-sorted.png`).
- Known ceiling (ponytail): the data grid still scrolls with the page rather
  than each pane scrolling independently (SAS-Studio fixed-pane model) -- the
  column list is capped so the panel shows, but a full fixed-pane refactor of
  `.ar-data-main` is deferred (it would ripple into the explorer-table view).

---

# handoff — arframe QC sheet + async export + a11y/responsive (Tasks 15/16/17)

**Branch state (2026-07-02):** `feat/qc-async-polish` (worktree
`../arframe-wt-qcpolish`, branched off `master` @ c3e1da1) carries the final
three plan tasks: the QC sheet (Task 15), async mirai export (Task 16), and
the a11y / responsive / polish sweep (Task 17). Three commits, staged
test-first, real-data eyeball sweep done. Do NOT push without explicit
approval.

## STATUS: Tasks 15 + 16 + 17 COMPLETE, gate green — the Stage-5 plan is DONE

- `devtools::check` = **0 errors / 0 warnings / 0 notes**; **815 tests pass**
  (0 skipped locally with `NOT_CRAN=true`: the daemon + shinytest2 browser
  tests all ran).
- Real-data sweep on the CDISC pilot mounts (decision #9): the QC sheet
  renders "Quality control — Untitled report / 1 of 1 output ready", the
  14.1.1 Demographics row + READY stamp, and the run log newest-first (the
  two real folder mounts). Keyboard nav verified live (ArrowDown/Up move the
  TOC selection: out001 -> out002 -> out001). No horizontal page scroll at
  1000 / 1440 / 1920 wide. The async Export button renders as a real
  `<button>` + a hidden download link. Screenshots: `.local/screens/15-qc.png`,
  `17-vp-1000.png` / `-1440` / `-1920`, `final/table.png`.

## What landed (3 commits on feat/qc-async-polish)

1. **QC sheet** (`R/mod_qc.R` + `test-mod_qc.R`; wired in `R/app.R`, CSS
   section 11): QC mode swaps the desk to a paper-styled proof-check sheet —
   running head, "N of M outputs ready" summary, one row per output (mono
   number + title + the SAME stamp the TOC shows via `.toc_rows()`), each
   not-ready output's `validate_output()` gaps as jump links, then the run
   log newest-first. A jump click selects the output, flips to Report mode
   (mirrors `ar-mode` to the client), and `open_card()`s the mapped region.
   `suspendWhenHidden = FALSE` (the mode body is a custom-class toggle, not a
   Shiny tabset — same trap the inspector panes hit).
2. **Async export** (`R/fct_async.R` + `test-fct_async.R`; `R/mod_frame.R`
   rewire, `R/app.R` daemon pool, `R/fct_export.R` `rendered=` param,
   DESCRIPTION `mirai`): `export_mirai()` renders every ready output on a
   mirai daemon and returns the written RTF paths; **a DuckDB connection
   NEVER crosses the daemon** — the daemon gets only the report JSON string +
   the dataset file paths (`arpillar::dataset_path()`) and opens its own
   engine, `arpillar::`-qualified (no `library()`, so R CMD check stays
   clean). `.build_export_package(rendered=)` is a backward-compatible split:
   `NULL` renders synchronously (the old path, all its tests unchanged), a
   named `id->path` map trusts the daemon's files and only assembles
   programs/report.json/manifest. The Export button is now a plain action
   button that `invoke()`s the ExtendedTask; the status observer assembles +
   zips and clicks a hidden download link (`ar-click`) — the render never
   blocks the request. Daemon pool set in `arframe(daemons = 2L)`, torn down
   `onStop`; the 5 shinytest2 fixtures pass `daemons = 0`.
3. **a11y / responsive / polish** (`R/mod_contents.R` keyboard observers,
   `R/mod_frame.R` aria-labels, `inst/www/arframe.js` keyday map, CSS section
   12, `test-a11y.R` + `test-mod_contents.R` keyboard tests): Up/Down move the
   TOC selection (payload carries a nonce so a repeated same-direction press
   is a fresh event under both `priority:event` and testServer), Enter opens
   the inspector on the selected output's first gap; the map is suppressed
   while focus is in a form field / button / link and guarded against a
   document target lacking `.closest`. aria-labels added to undo/redo; the
   hidden export download link is `aria-hidden`. Responsive: `min-width:0` on
   the desk column + a `<=1100px` / `<=860px` media query narrowing the side
   panels so a wide crosstab scrolls inside its own table-wrap and the page
   never scrolls horizontally.

## Plan deviations (deliberate, keep)

- **Export lives in `mod_frame.R`, not a `mod_project.R`** — Task 14 landed
  the export in the frame bar; Task 16 rewired it there. No `mod_project.R`.
- **No `mod_project.R` "all-outputs menu item"** to swap — the single Export
  button IS the all-outputs path; it became async in place.
- **Async Shiny glue is browser-only** — the ExtendedTask invoke/status +
  hidden-download click can't be driven in testServer without a daemon +
  reactive domain; the render + package assembly ARE unit-tested
  (`test-fct_async.R`: daemon byte-identical to the sync seam +
  `.build_export_package(rendered=)`), the button/hidden-link affordances in
  `test-mod_frame.R`, and the whole flow eyeball-verified.
- **Responsive is a graceful narrowing, not the plan's flyout/bottom-sheet** —
  the binding success criterion (no horizontal scroll at the three viewports)
  is met and verified; the slim-strip flyout + bottom-sheet card are deferred
  as a heavier UX pass (the rail/inspector already collapse via the chevrons).
- **Esc-to-close the card is N/A** in the v5 docked inspector (it is always
  docked, not summoned) — Esc keeps its Add-output-overlay meaning.

## Known ceilings (ponytail-noted, not bugs)

- `arframe()` builds ONE store at app construction (all browser sessions share
  it) — fine for the single-user dev tool; noted while replaying screenshots
  (repeated chromote connections accumulate outputs in the shared store).
- The daemon renders `<slug>.rtf` filenames from a main-process names map; two
  ready outputs with the same number+title would collide (same as the sync
  path — pre-existing).
- Keyboard nav clamps at the ends (ArrowDown on the last row is a no-op) — by
  design; needs >=2 outputs to observe movement.

## Next steps

1. **Merge `feat/qc-async-polish` -> `master` (ff-only), delete branch +
   worktree** (the standing pattern; may already be done — check `git log`).
2. **Integration (needs explicit approval):** push arpillar `333b8ce`
   (`unregister_dataset()`, LOCAL-ONLY on branch `feat/ui-prereqs`) — arframe
   CI cannot resolve `Remotes: vthanik/arpillar` until it lands — then push
   arframe. No push without per-session approval.
3. Deferred per spec (not scheduled): ⌘K command palette (v1.1), Actions /
   Rules, dark "Instrument" skin.

## Key seams (verified this session, additive to prior handoffs')

- `mirai 2.7.1`: `daemons(n, dispatcher=, .compute=)` and
  `mirai(.expr, ..., .compute=)` both take `.compute` (a named per-launch
  profile); `shiny::ExtendedTask` present. The daemon block must be
  `arpillar::`-qualified (never `library(arpillar)`) or R CMD check flags a
  WARNING (library-in-package-code) + a NOTE (no-visible-global) on the
  quoted expression.
- `arpillar::report_to_json(report)` (no path) returns a JSON **string**;
  `arpillar::dataset_path(con, name)` returns the registered source path.
- A DuckDB connection is a C handle bound to THIS process — serialise paths +
  JSON, never the connection.
- QC / TOC status share ONE oracle (`.toc_rows()` -> `output_status()`,
  folding `rv$broken`/`rv$stale`) — a QC stamp can never disagree with the
  Contents stamp.
- testServer can't replay `priority:event`, so a repeated same-value input
  won't re-fire `observeEvent`; give the payload a nonce (the keyboard nav
  input does) to make repeated presses testable.

## Conventions in force

Store-first state (never DOM), classed cli conditions, air format hook,
test-first per stage, `.demo_catalog()` fixtures for tests but REAL pilot
data for any visual claim (decision #9). Eyeball verification is binding.
`\uXXXX` escapes for non-ASCII in R strings (even HTML output — the em-dash
in the QC running head is `—`). No AI attribution in commits. No push
without explicit approval.
