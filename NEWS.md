# tlframe 0.1.0.9000 (development version)

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
* Latin Modern font fallback for PDF on Linux/Docker without Microsoft fonts
  (built into tinytex/texlive, no bundled fonts needed).
* `TLFRAME_FONT_DIR` environment variable: point to a directory of
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

* Four-tier defaults: package < `_tlframe.yml` < `fr_theme()` < per-table verbs.
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
