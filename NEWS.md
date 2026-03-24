# arframe 0.1.0.9000 (development version)

## New features

* `fr_wide_ard()` converts ARD (Analysis Results Data) from cards/cardx into wide summary data frames for `fr_table()`. Supports multi-row continuous output (named vector format specs), per-variable decimal precision, hierarchical SOC/PT structures, custom format functions (including `p.value < 0.001` threshold), and all 60+ cards/cardx stat types. Zero dependency on cards — duck-types on column names.

* `fr_rows()` refactored from 14 params down to 9. `group_label` moves into `group_by` list form: `group_by = list(cols = "variable", label = "stat_label")`. `page_by_visible` moves into `page_by` list form: `page_by = list(cols = "PARAM", visible = FALSE)`. `page_by_bold`, `page_by_align`, `group_bold` removed — style via `fr_styles()` instead.

## Breaking changes

* `fr_rows(group_by)` no longer auto-implies `blank_after`. To get blank rows between groups, set `blank_after` explicitly: `fr_rows(group_by = "col", blank_after = "col")`.

## Bug fixes

* Fixed decimal alignment across pages: page 2+ was showing page 1's values because geometry indices used per-page row numbers instead of global indices. All three backends (RTF, HTML, LaTeX) now correctly map page-local rows to global decimal geometry.

* Fixed decimal alignment for mixed integer + decimal columns: columns mixing pure integers (e.g., "143") with decimal stats (e.g., "75.7 (8.19)") now align correctly.

* Fixed `n_only` decimal alignment: widened integer zone for columns with wider n values (e.g., 3-digit counts).

* Added pharma percentage boundary rules for integer percentages (d=0): handles edge cases like "100%" and "0" alignment.

* Unified internal decimal parser: eliminated parallel implementations into a single stat-type detection + alignment engine.

* Injected group header rows and blank-after rows now inherit `page_by` column values, fixing incorrect page splits when `page_by` and `group_by`/`blank_after` are used together.

## Font system overhaul

* Default font changed from Courier New to **Times New Roman** (FDA recommended for submission documents).
* Font resolution order: FDA-recommended → `ARFRAME_FONT_DIR` → Adobe open-source (Source Serif 4, Source Sans 3, Source Code Pro) → CSS generic.
* Latin Modern and Liberation fonts removed from fallback chain.
* New `vignettes("fonts")` documents the full font resolution order and FDA guidance.

## HTML backend improvements

* HTML output now respects all `fr_spacing()` settings: `titles_after`, `footnotes_before`, `pagehead_after`, `pagefoot_before`, `page_by_after`.
* `col_gap` wired to HTML cell padding (matching RTF).
* `pagehead`/`pagefoot` `font_size` applied in HTML output.
* Knitr/pkgdown output uses web-optimized sans-serif font (Source Sans 3).
* Bootstrap table style reset prevents pkgdown from overriding arframe table borders.
* File output uses plain white background (matching R HTML widget preview).
* Fixed pkgdown "On this page" sidebar displacement by adding `overflow: hidden` to outer container.
* CSS font stack deduplication (no more repeated font names).

## New features

### HTML render backend

* New HTML output format: `fr_render(spec, "output.html")` produces a
  self-contained HTML document with paper simulation (orientation-aware page
  dimensions, margins, flexbox page footer push-down).

* `print(spec)` auto-previews in the RStudio/Positron Viewer panel via
  `htmltools::browsable()` — just type the spec to see it, like gt.

* `knit_print.fr_spec()` renders tables inline in R Markdown, Quarto, and
  pkgdown vignettes with scoped CSS (multiple tables on one page don't
  conflict).

* All vignettes now show live rendered tables instead of static PNG
  screenshots.

### Multi-page figures

* `fr_figure()` now accepts a list of plot objects and an optional `meta`
  data frame. Each column of `meta` becomes a `{token}` resolved in titles
  and footnotes dynamically per page, enabling figure programmes that loop
  over parameters (e.g. one page per biomarker or visit).

### Group label injection

* `fr_rows(group_by = list(cols = ..., label = ...))` auto-injects a header
  row at each group boundary using values from a specified column. This is the
  standard demographics pattern where the group variable label appears as a row
  above indented summary statistics, without requiring pre-formatted data.

### Multi-level indent_by

* `fr_rows(indent_by = )` now accepts a named list specifying `key`, `col`,
  and `levels` for SOC/HLT/PT three-level hierarchies. Each row is indented
  to its natural depth, replacing the previous single-column form that only
  handled two levels.

### Pagination controls

* `fr_rows(group_keep = FALSE)` disables RTF `\keepn` for visual-only
  grouping (blank rows and indentation without page-keep constraints).
  Useful when groups are short and the default keep behaviour wastes space.
* `fr_rows(page_by = list(cols = ..., visible = FALSE))` inserts page breaks
  at `page_by` boundaries while hiding the label row itself.
* `fr_page(orphan_min = , widow_min = )` sets minimum body-row counts at
  the bottom and top of a page respectively, preventing isolated rows at
  page breaks.

### Wrap parameter

* `fr_rows(wrap = )` controls whether long cell content wraps within its
  column or is truncated, complementing the existing `suppress` behaviour.

## Bug fixes

* `measure_text_width_twips()`: multi-byte UTF-8 characters (em dashes,
  arrows, etc.) now measured correctly via `utf8ToInt()` instead of
  misindexing the raw-byte lookup table.
* `resolve_borders()`: early return when the table body is empty prevents
  index-out-of-bounds crashes on zero-row tables.
* `escape_and_resolve()`: guard against malformed sentinel tokens with
  fewer than three capture groups (was a subscript-out-of-bounds crash).
* `fr_rows()`: error when `group_by$label` is set without `group_by$cols`.
* `fr_rows()`: warning when `page_by` and `group_by` share the same column.
* LaTeX hline dedup regex anchored with `^` to prevent row 10 matching row 100.
* Decimal alignment: `round()` replaces `as.integer()` for center-offset
  calculation, removing a systematic 1-twip leftward bias.
* `paginate_rows()`: all consecutive trailing blank rows captured, not just
  the first.

## Pipeline API

* `fr_table()`, `fr_listing()`, `fr_figure()` entry points for tables,
  patient listings, and embedded figures.
* 10 pipeline verbs: `fr_cols()`, `fr_titles()`, `fr_footnotes()`,
  `fr_header()`, `fr_rows()`, `fr_hlines()`, `fr_vlines()`, `fr_spans()`,
  `fr_styles()`, `fr_page()`.
* `fr_render()` produces RTF and PDF from the same spec.
* Verb order is irrelevant -- all resolution deferred to render time.

## Column system

* `fr_col()` constructor with width, alignment, visibility, and grouping.
* Width modes: fixed inches, percentages, `"auto"` (AFM font metrics),
  `"fit"`, and `"equal"`.
* Decimal alignment engine (15 stat-display types) for pharma summary tables.
* N-count injection via `.n` and `.n_format` in `fr_cols()`.
* Column splitting (`.split = TRUE`) for wide tables exceeding page width.

## Rendering

* Native RTF 1.9.1 backend with field codes for page numbering.
* LaTeX/tabularray backend compiled via XeLaTeX for PDF output.
* R-side pagination with group-aware page breaks.
* Token system (`{thepage}`, `{total_pages}`, `{program}`, `{datetime}`)
  for running headers and footers.
* Adobe open-source font fallback for PDF/RTF on Linux/Docker without Microsoft fonts.
* `ARFRAME_FONT_DIR` environment variable: point to a directory of
  `.ttf`/`.otf` files for project-local fonts without system-wide
  installation. Ideal for Docker/CI pipelines.

## Styling

* `fr_style()`, `fr_row_style()`, `fr_col_style()` for cell-level formatting.
* `fr_style_if()` for data-driven conditional styles.
* `fr_rows_matches()` for pattern-based row selectors.
* `fr_style_explain()` for debugging style cascade resolution.
* Inline markup: `fr_super()`, `fr_sub()`, `fr_bold()`, `fr_italic()`,
  `fr_dagger()`, `fr_emdash()`, and more.

## Configuration

* Four-tier defaults: package < `_arframe.yml` < `fr_theme()` < per-table verbs.
* `fr_recipe()` and `fr_apply()` for reusable pipeline fragments.
* `c.fr_recipe()` for composing recipes (company + study + table-type layers).

## Validation

* `fr_validate()` pre-render checks for columns, widths, styles, spans, and fonts.
* `fr_get_*()` accessors for programmatic QC of spec internals.

## Datasets

* Synthetic CDISC ADaM datasets (study TFRM-2024-001, 135 subjects):
  `adsl`, `adae`, `adtte`, `adcm`, `advs`.
* Pre-summarized TFL-ready tables: `tbl_demog`, `tbl_ae_soc`,
  `tbl_ae_summary`, `tbl_disp`, `tbl_tte`, `tbl_cm`, `tbl_vs`.
