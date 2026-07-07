# handoff -- 2026-07-07

## Goal

Two-repo epic: make **every Setup setting actually drive the rendered outputs**
(Setup = study defaults, per-output = the standing override) AND build the
flagship **Report -> List-of-Contents table** (drill into paper+inspector,
mirroring Data's list->grid). Source-of-truth plan (5 phases):
`~/.claude/plans/quiet-gliding-lerdorf.md`. Live task list: 5 tasks
(`TaskList`), Phase 1 in-progress.

Spans `~/projects/r/arframe` (UI) + `~/projects/r/arpillar` (engine). arframe
consumes arpillar as an installed `Remotes` dep.

## Current state

- **arframe** HEAD `b690259` (prior session). Working tree DIRTY (in-flight,
  nothing committed this session): `R/fct_async.R`, `R/fct_export.R`,
  `R/mod_toolbar.R`.
- **arpillar** HEAD `6db85f5`. Working tree DIRTY: `R/fct_render_table.R` (mod),
  `tests/testthat/test-resolve_theme.R` (NEW).
- Gates: **full arpillar test suite GREEN**; golden gate byte-identical
  (run with `NOT_CRAN=true`, else the byte-goldens skip_on_cran). arframe
  render-parity tests (`test-mod_toolbar`/`fct_export`/`fct_async`) GREEN.
- NOT yet run: `devtools::document()` (not needed -- all changes are `@noRd`
  internal, no `@export`/NAMESPACE change), `air format`, full arframe
  `devtools::check()`. Do before committing.

| Phase | State |
|---|---|
| 1 Resolver + thread theme | CORE DONE + verified; 2 sub-pieces remain (below) |
| 2 Population filter engine | NOT started (next) |
| 3 Dedup Options pane | blocked by Ph1 |
| 4 LoC table + drill (flagship) | NOT started |
| 5 Cleanup + gates + live verify | NOT started |

## What landed this session (all IN-FLIGHT, uncommitted)

- **arframe threading** -- `report@theme` now passed to every render seam, not
  just the screen: `fct_async.R` (daemon export), `mod_toolbar.R` (per-output
  `.rtf`), `fct_export.R` (`.export_render_one`). Fixes a silent screen<->export
  divergence on any theme-set option.
- **arpillar resolver extensions** (`R/fct_render_table.R`):
  - Header-N + `show_header_n` resolve from `theme$arm` (`.header_n_labels`).
  - Stats-selection resolves from `theme$summaries$continuous` (`.stats_opt` +
    `.rows_to_measure` + `.ATOM_TO_STAT` map n->N/q1->p25/q3->p75).
  - Granular decimals -- per-stat absolute + per-variable base (new `.stat_dp`
    replacing `.stat_decimals`; `.decimals_opt` now returns
    `list(base, per, by_var)`; `.derive_dp`, `.STAT_TO_DEC`; threaded through
    `.fmt_measure_cell`/`.fmt_count_cell`/`.category_block`/`.display_block`).
  - 12 new functional tests: `tests/testthat/test-resolve_theme.R`.

## Locked design decisions

- **Resolver is the single precedence mechanism.** arpillar's
  `resolve_option(object, theme, key, theme_path, engine_default)` already
  implements per-output-wins -> theme-default -> engine-default. Do NOT add
  ad-hoc theme->options merges. `theme` IS `report@theme` (what Setup writes).
- **Golden-safe rule (non-negotiable):** an unset value must fall through to the
  UNCHANGED engine default, so empty theme + empty options stays byte-identical
  (`test-golden_strip.R`). Every extension keeps the old derivation as the
  fallback -- that's why goldens held.
- **Decimals precedence: per-variable base > per-stat absolute > base
  derivation** (`.stat_dp`). This is a JUDGMENT CALL in ambiguous clinical
  territory -- CONFIRM with the user at the visual-verification pass (a
  per-variable `V|AGE` dp should win over a study-wide `decimals$sd`).
- **Population = one binding** (Population = Analysis Set), and it will be made
  **functional** (bind -> subset data) in Phase 2 -- user chose this explicitly.
- Free to **delete/rewrite old empty-state paths + their tests**; the ONLY
  preserved invariant is the golden gate.

## Files in scope

- arframe (mod): `R/fct_async.R`, `R/fct_export.R`, `R/mod_toolbar.R`.
- arpillar (mod): `R/fct_render_table.R`. (new test) `tests/testthat/test-resolve_theme.R`.
- Reference: `R/theme.R` (arpillar `.SPEC_THEME` -- the shared theme contract);
  `R/resolve.R` (`resolve_option`, `.theme_at`); `R/fct_render_ard.R`
  (`.summary_call` = where the ARD-vocab extension lands).
- arframe Setup shapes: `R/mod_setup.R` (`.CONT_SEEDS`/`.CONT_ATOM_DESC`
  vocabulary ~line 2554/2696; `.dec_encode` `V|`/`P|` ~2223).

## What was tried / learned (do not redo)

- **build_ard does NOT need a theme for Phase 1.** `.summary_call` computes the
  FULL default continuous set always; stats-selection/header-N/decimals are all
  DISPLAY-stage (`render_display`/`render_spec`, which already take `theme`). So
  Phase 1 never touched `build_ard` -- lower golden risk. `build_ard` gets a
  theme only in Phase 2 (population).
- **ARD-vocab gap (real, not deferred-by-choice):** arframe's continuous
  vocabulary (~26 atoms: se/cv/var/iqr/geomean/geosd/geose/percentiles/lclm/uclm)
  EXCEEDS cards' default 8 (N/mean/sd/median/p25/p75/min/max). Richer atoms
  render `NA` until `.summary_call` computes them via cards custom fns. Common
  set works today.
- Tom Select was rejected earlier (2026-07-07) -- selectize is the picker engine
  (unrelated to this work, but do not re-suggest).

## What to do next (in order)

1. **Install the updated arpillar** so arframe's runtime + any visual check use
   the new resolution: `R CMD INSTALL ~/projects/r/arpillar` (or
   `devtools::install("~/projects/r/arpillar")`). arframe threading works with
   the OLD installed arpillar, but the RESOLUTION changes need this reinstall.
2. **ARD vocabulary extension** (finish Phase 1 stats): extend
   `arpillar::.summary_call` (`R/fct_render_ard.R:317`) to compute arframe's full
   continuous vocabulary via cards custom fns; add the missing atom->stat_name
   entries to `.ATOM_TO_STAT`. KEEP the existing 8 stats byte-identical (goldens
   select only those) -- verify with `NOT_CRAN=true` golden run.
3. **Phase 2 -- population filter engine** (arpillar, ARD-mutating, golden-risk):
   string->predicate compiler (`'SAFFL=="Y"'` -> `list(col,op,val)` for
   `.filter_one`, `R/fct_dataitems.R:602`, classed error on unsupported op);
   `build_ard(con, object, theme)` resolves `options$population` ->
   `theme$populations[[id]]` -> `{label,dataset,filter}`, ANDs the filter into
   `.collect_filtered` main pull (`fct_render_ard.R:128/185`) + occurrence
   denominator (`:189`); derive occ denominator DATASET from the set's `dataset`
   (untangle the `"ADSL"`-vs-id overload); backward-compat legacy
   `population=<dataset-name>`. Migrate ~20 arframe presets `population="ADSL"`
   -> analysis-set id; add `options$population` to arframe `.ard_key`
   (`fct_store.R:337`) so it invalidates the memo.
4. **Consolidated visual verification** (user's chosen cadence): install
   arpillar -> launch arframe on the CDISC ADaM pilot -> change a Setup value
   (header-N / continuous rows / decimals / population) -> screenshot the Report
   paper honoring it. Then Phases 3-5.

## Operating constraints

- **No `Co-Authored-By: Claude`** anywhere (commits/PRs). Author = Vignesh
  Thanikachalam `<about.vignesh@gmail.com>`.
- **ASCII inside cli/stop/warning/message strings**; typography free in comments/
  roxygen. `air format` after edits (PostToolUse hook enforces).
- **Golden gate is sacred** -- re-run after every engine change with
  `NOT_CRAN=true`; if bytes drift, FIX the resolver, never adjust the golden.
- **No `--` in arframe user-facing text** (em-dash instead).
- Commit/push only when the user asks; branch off `main` per task near release.
- Real-data-only for visual claims: `/Users/vignesh/projects/data/cdisc-adam-pilot`.

## Quick orient (paste to confirm state in <60s)

```
cd ~/projects/r/arframe && git status --short
cd ~/projects/r/arpillar && git status --short
# golden gate + resolver tests (byte-goldens need NOT_CRAN):
cd ~/projects/r/arpillar && NOT_CRAN=true Rscript -e 'Sys.setenv(NOT_CRAN="true"); devtools::load_all(".",quiet=TRUE); testthat::test_file("tests/testthat/test-resolve_theme.R"); testthat::test_file("tests/testthat/test-golden_strip.R")'
cat ~/.claude/plans/quiet-gliding-lerdorf.md   # the 5-phase plan
```
