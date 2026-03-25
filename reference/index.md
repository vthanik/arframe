# Package index

## Data Conversion

[`fr_wide_ard()`](https://vthanik.github.io/arframe/reference/fr_wide_ard.md)
converts Analysis Results Data (ARD) from packages like cards and cardx
into wide summary data frames ready for
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).
Supports multi-row continuous output, per-variable decimals,
hierarchical SOC/PT structures, and all cards/cardx stat types. No
dependency on cards.

- [`fr_wide_ard()`](https://vthanik.github.io/arframe/reference/fr_wide_ard.md)
  : Convert ARD (Long) to Wide Summary Data Frame

## Pipeline Entry Points

Every arframe pipeline starts here.
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
creates a spec object from a data frame.
[`fr_listing()`](https://vthanik.github.io/arframe/reference/fr_listing.md)
and
[`fr_figure()`](https://vthanik.github.io/arframe/reference/fr_figure.md)
create specs for patient listings and embedded figures.
[`fr_figure()`](https://vthanik.github.io/arframe/reference/fr_figure.md)
accepts a single plot or a list of plots for multi-page figures, with
optional per-page metadata tokens. All three return an `fr_spec` that
flows through the same pipeline verbs.

- [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
  : Start a arframe Table Pipeline
- [`fr_listing()`](https://vthanik.github.io/arframe/reference/fr_listing.md)
  : Start a arframe Listing Pipeline
- [`fr_figure()`](https://vthanik.github.io/arframe/reference/fr_figure.md)
  : Start a arframe Figure Pipeline

## Column Configuration

[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md) is
the single source of truth for column structure: labels, widths,
alignment, visibility, N-counts, and column splitting.
[`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md)
constructs individual column specs. Width modes: fixed inches,
percentages, `"auto"` (content-measured), `"fit"` (scale to page width),
and `"equal"` (divide remaining space). Alignment modes include
`"decimal"` for stat-display alignment. N-counts accept named vectors,
data frames, or lists and auto-route to spanning headers or column
labels. `fr_col(group=)` creates inline spanning headers. Tidyselect
formula syntax applies shared specs to multiple columns at once.

- [`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
  : Configure Column Display
- [`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md) :
  Define a Column Specification

## Titles and Footnotes

[`fr_titles()`](https://vthanik.github.io/arframe/reference/fr_titles.md)
adds title lines above the table. Each argument is one line; plain text
or the list form for per-line `align`, `bold`, and `font_size`
overrides.
[`fr_footnotes()`](https://vthanik.github.io/arframe/reference/fr_footnotes.md)
adds footnote lines with `.placement` (`"every"` / `"last"`),
`.separator` (rule above block), and per-line overrides via the list
form. Both support inline `{fr_*()}` markup.

- [`fr_titles()`](https://vthanik.github.io/arframe/reference/fr_titles.md)
  : Set Table Titles
- [`fr_footnotes()`](https://vthanik.github.io/arframe/reference/fr_footnotes.md)
  : Set Table Footnotes

## Header Presentation

[`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md)
controls header presentation: bold, `align` (scalar or tidyselect named
list for per-column targeting), `valign` (`"bottom"` default aligns
short labels under multi-line ones), `background`/`color` colors,
`font_size`, and `repeat_on_page`. N-counts are set via `fr_cols(.n=)`.
[`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)
adds spanning headers at one or more levels (`.level`) with optional
underline (`.hline`) and gap columns (`.gap`). For simple grouping, use
`fr_col(group=)` inside
[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
instead.

- [`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md)
  : Configure Column Header Presentation
- [`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)
  : Add Spanning Column Headers

## Page Layout

Paper size, orientation, margins, fonts, and pagination controls.
[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md)
sets `orphan_min`/`widow_min` (default 3 rows), a `continuation` label
for multi-page tables, `col_gap` (inter-column padding), and `tokens`
(named list for custom `{token}` values).
[`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md)
and
[`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md)
define three-zone running chrome with token substitution: built-in
`{thepage}`, `{total_pages}`, `{program}`, `{datetime}`, plus any custom
tokens.
[`fr_spacing()`](https://vthanik.github.io/arframe/reference/fr_spacing.md)
controls blank lines at five table junctions.

- [`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md)
  : Set Page Layout Options
- [`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md)
  : Set Running Page Header
- [`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md)
  : Set Running Page Footer
- [`fr_spacing()`](https://vthanik.github.io/arframe/reference/fr_spacing.md)
  : Control Spacing Between Table Sections

## Row Organization

[`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md)
handles `group_by` (keep-together groups, with optional `label` for
header row injection via list form), `page_by` (page breaks with a group
label, with optional `visible` control via list form), `indent_by`
(single-level string or multi-level named list for SOC/HLT/PT
hierarchies), `blank_after`, `sort_by`, `suppress`, `wrap`, and
`group_keep`.
[`fr_rows_matches()`](https://vthanik.github.io/arframe/reference/fr_rows_matches.md)
creates data-driven row selectors (exact value or regex) for use in
styling verbs.

- [`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md)
  : Configure Row Grouping and Pagination
- [`fr_rows_matches()`](https://vthanik.github.io/arframe/reference/fr_rows_matches.md)
  : Select Rows by Column Value or Pattern

## Rules and Borders

Horizontal rules
([`fr_hlines()`](https://vthanik.github.io/arframe/reference/fr_hlines.md)),
vertical rules
([`fr_vlines()`](https://vthanik.github.io/arframe/reference/fr_vlines.md)),
and full grids
([`fr_grid()`](https://vthanik.github.io/arframe/reference/fr_grid.md)).
Named presets (`"header"`, `"booktabs"`, `"box"`, `"void"`, etc.) or
custom `width`, `color`, and `linestyle` for all rules in the set.

- [`fr_hlines()`](https://vthanik.github.io/arframe/reference/fr_hlines.md)
  : Apply Horizontal Rules
- [`fr_vlines()`](https://vthanik.github.io/arframe/reference/fr_vlines.md)
  : Apply Vertical Rules
- [`fr_grid()`](https://vthanik.github.io/arframe/reference/fr_grid.md)
  : Apply Horizontal and Vertical Rules Together

## Cell Styling

Cell, row, and column style overrides applied via
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md).
[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md)
targets cells by position, region, or tidyselect column selection;
supports `colspan`/`rowspan` for merging.
[`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md)
and
[`fr_col_style()`](https://vthanik.github.io/arframe/reference/fr_col_style.md)
apply uniformly across rows or columns.
[`fr_style_if()`](https://vthanik.github.io/arframe/reference/fr_style_if.md)
applies styles data-driven: a formula or function evaluates cell values
at render time. Narrower scope wins: cell \> row \> column; later styles
override earlier ones.
[`fr_style_explain()`](https://vthanik.github.io/arframe/reference/fr_style_explain.md)
audits the resolved styles.

- [`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md)
  : Define a Cell Style Override
- [`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md)
  : Define a Row Style Override
- [`fr_col_style()`](https://vthanik.github.io/arframe/reference/fr_col_style.md)
  : Define a Column Style Override
- [`fr_style_if()`](https://vthanik.github.io/arframe/reference/fr_style_if.md)
  : Create a Conditional Style Override
- [`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
  : Apply Style Overrides to a Table
- [`fr_style_explain()`](https://vthanik.github.io/arframe/reference/fr_style_explain.md)
  : Explain Style Resolution for a Cell

## Inline Markup

Rich text in any string field – titles, footnotes, column labels, cell
values. Includes
[`fr_super()`](https://vthanik.github.io/arframe/reference/fr_super.md)/[`fr_sub()`](https://vthanik.github.io/arframe/reference/fr_sub.md)
(superscript/subscript),
[`fr_bold()`](https://vthanik.github.io/arframe/reference/fr_bold.md)/[`fr_italic()`](https://vthanik.github.io/arframe/reference/fr_italic.md)/[`fr_underline()`](https://vthanik.github.io/arframe/reference/fr_underline.md)
(font style),
[`fr_dagger()`](https://vthanik.github.io/arframe/reference/fr_dagger.md)/[`fr_ddagger()`](https://vthanik.github.io/arframe/reference/fr_ddagger.md)
(regulatory symbols), em/en dashes,
[`fr_newline()`](https://vthanik.github.io/arframe/reference/fr_newline.md)
(line break within a cell), and
[`fr_unicode()`](https://vthanik.github.io/arframe/reference/fr_unicode.md)
for arbitrary Unicode characters. Sentinel tokens are resolved
per-backend at render time.

- [`fr_super()`](https://vthanik.github.io/arframe/reference/fr_super.md)
  : Superscript Markup
- [`fr_sub()`](https://vthanik.github.io/arframe/reference/fr_sub.md) :
  Subscript Markup
- [`fr_bold()`](https://vthanik.github.io/arframe/reference/fr_bold.md)
  : Bold Markup
- [`fr_italic()`](https://vthanik.github.io/arframe/reference/fr_italic.md)
  : Italic Markup
- [`fr_underline()`](https://vthanik.github.io/arframe/reference/fr_underline.md)
  : Underline Markup
- [`fr_newline()`](https://vthanik.github.io/arframe/reference/fr_newline.md)
  : Line Break Within a Text Element
- [`fr_dagger()`](https://vthanik.github.io/arframe/reference/fr_dagger.md)
  : Dagger Symbol
- [`fr_ddagger()`](https://vthanik.github.io/arframe/reference/fr_ddagger.md)
  : Double Dagger Symbol
- [`fr_emdash()`](https://vthanik.github.io/arframe/reference/fr_emdash.md)
  : Em Dash
- [`fr_endash()`](https://vthanik.github.io/arframe/reference/fr_endash.md)
  : En Dash
- [`fr_unicode()`](https://vthanik.github.io/arframe/reference/fr_unicode.md)
  : Unicode Character by Codepoint

## Theme and Configuration

Four-tier defaults: package defaults \< `_arframe.yml` (auto-discovered)
\<
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
(session-wide) \< per-table verbs.
[`fr_config()`](https://vthanik.github.io/arframe/reference/fr_config.md)
loads a YAML file covering page layout, fonts, column behavior, header
style, rules, spacing, tokens, and footnote defaults.
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
sets the same options programmatically.
[`fr_theme_get()`](https://vthanik.github.io/arframe/reference/fr_theme_get.md)
/
[`fr_theme_reset()`](https://vthanik.github.io/arframe/reference/fr_theme_reset.md)
inspect and clear the session theme.

- [`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
  [`fr_theme_set()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
  : Set or Update Study-Level Table Theme
- [`fr_theme_get()`](https://vthanik.github.io/arframe/reference/fr_theme_get.md)
  : Get the Current Study-Level Table Theme
- [`fr_theme_reset()`](https://vthanik.github.io/arframe/reference/fr_theme_reset.md)
  : Reset the Study-Level Table Theme
- [`fr_config()`](https://vthanik.github.io/arframe/reference/fr_config.md)
  : Load Configuration from a YAML File
- [`fr_config_get()`](https://vthanik.github.io/arframe/reference/fr_config_get.md)
  : Get the Current Configuration
- [`fr_config_reset()`](https://vthanik.github.io/arframe/reference/fr_config_reset.md)
  : Reset Configuration

## Recipes

Reusable pipeline fragments.
[`fr_recipe()`](https://vthanik.github.io/arframe/reference/fr_recipe.md)
captures a sequence of `fr_*()` verb calls as a language object
(survives [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)). Apply
with `fr_apply(spec, recipe)`. Compose multiple recipes with
[`c()`](https://rdrr.io/r/base/c.html): later verbs override earlier
replace-mode verbs and append to accumulate-mode verbs (`fr_spans`,
`fr_styles`).

- [`fr_recipe()`](https://vthanik.github.io/arframe/reference/fr_recipe.md)
  : Create a Reusable Table Recipe
- [`c(`*`<fr_recipe>`*`)`](https://vthanik.github.io/arframe/reference/c.fr_recipe.md)
  : Combine Recipes
- [`fr_apply()`](https://vthanik.github.io/arframe/reference/fr_apply.md)
  : Apply a Recipe to a Spec

## Rendering

`fr_render(spec, path)` writes RTF, HTML (`.html`/`.htm`), LaTeX source
(`.tex`), or PDF (via XeLaTeX). Format is detected from the file
extension. HTML output is self-contained with paper simulation
(orientation-aware page dimensions, margins, and page chrome).
`print(spec)` auto-previews in the RStudio/Positron Viewer panel.
[`knit_print.fr_spec()`](https://vthanik.github.io/arframe/reference/knit_print.fr_spec.md)
renders tables inline in Rmd/Quarto/pkgdown vignettes with scoped CSS.
PDF requires XeLaTeX or tinytex; call
[`fr_install_latex_deps()`](https://vthanik.github.io/arframe/reference/fr_install_latex_deps.md)
once to install required LaTeX packages.
[`fr_backends()`](https://vthanik.github.io/arframe/reference/fr_backends.md)
lists registered backends;
[`fr_register_backend()`](https://vthanik.github.io/arframe/reference/fr_register_backend.md)
/
[`fr_unregister_backend()`](https://vthanik.github.io/arframe/reference/fr_unregister_backend.md)
extend the system with custom output formats.

- [`fr_render()`](https://vthanik.github.io/arframe/reference/fr_render.md)
  : Render a Table to File
- [`format(`*`<fr_spec>`*`)`](https://vthanik.github.io/arframe/reference/format.fr_spec.md)
  : Format an fr_spec as a compact one-liner
- [`knit_print(`*`<fr_spec>`*`)`](https://vthanik.github.io/arframe/reference/knit_print.fr_spec.md)
  : Render fr_spec as inline HTML in knitr documents
- [`fr_backends()`](https://vthanik.github.io/arframe/reference/fr_backends.md)
  : List Registered Render Backends
- [`fr_register_backend()`](https://vthanik.github.io/arframe/reference/fr_register_backend.md)
  : Register a Custom Render Backend
- [`fr_unregister_backend()`](https://vthanik.github.io/arframe/reference/fr_unregister_backend.md)
  : Remove a Custom Render Backend
- [`fr_register_stat_type()`](https://vthanik.github.io/arframe/reference/fr_register_stat_type.md)
  : Register a Custom Decimal Alignment Stat Type
- [`fr_latex_deps()`](https://vthanik.github.io/arframe/reference/fr_latex_deps.md)
  : List Required LaTeX Packages
- [`fr_install_latex_deps()`](https://vthanik.github.io/arframe/reference/fr_install_latex_deps.md)
  : Install Required LaTeX Packages

## Validation and Inspection

[`fr_validate()`](https://vthanik.github.io/arframe/reference/fr_validate.md)
checks a spec before rendering: column names, span contiguity, width
budget, style index range, font recognition, and listing-specific
columns. Returns the spec invisibly (pipeline-safe). `fr_get_*()`
getters provide programmatic read access to every spec component for QC
scripts.
[`is.fr_spec()`](https://vthanik.github.io/arframe/reference/is.fr_spec.md)
/
[`is.fr_col()`](https://vthanik.github.io/arframe/reference/is.fr_col.md)
test object types.

- [`fr_validate()`](https://vthanik.github.io/arframe/reference/fr_validate.md)
  : Validate a Table Specification Before Rendering
- [`fr_get_data()`](https://vthanik.github.io/arframe/reference/fr_get_data.md)
  : Get the Data Frame from a Spec
- [`fr_get_columns()`](https://vthanik.github.io/arframe/reference/fr_get_columns.md)
  : Get Column Specifications from a Spec
- [`fr_get_col()`](https://vthanik.github.io/arframe/reference/fr_get_col.md)
  : Get a Single Column Specification
- [`fr_get_titles()`](https://vthanik.github.io/arframe/reference/fr_get_titles.md)
  : Get Titles from a Spec
- [`fr_get_footnotes()`](https://vthanik.github.io/arframe/reference/fr_get_footnotes.md)
  : Get Footnotes from a Spec
- [`fr_get_page()`](https://vthanik.github.io/arframe/reference/fr_get_page.md)
  : Get Page Configuration from a Spec
- [`fr_get_rules()`](https://vthanik.github.io/arframe/reference/fr_get_rules.md)
  : Get Rules from a Spec
- [`fr_get_styles()`](https://vthanik.github.io/arframe/reference/fr_get_styles.md)
  : Get Cell Styles from a Spec
- [`is.fr_spec()`](https://vthanik.github.io/arframe/reference/is.fr_spec.md)
  : Test if an Object is an fr_spec
- [`is.fr_col()`](https://vthanik.github.io/arframe/reference/is.fr_col.md)
  : Test if an Object is an fr_col

## Built-in Datasets

Synthetic CDISC ADaM datasets from study TFRM-2024-001 (135 subjects,
Zomerane vs placebo). Raw ADaM: `adsl`, `adae`, `adtte`, `adcm`, `advs`.
Pre-summarized TFL-ready: `tbl_demog`, `tbl_ae_soc`, `tbl_ae_summary`,
`tbl_disp`, `tbl_tte`, `tbl_cm`, `tbl_vs` – each ready for direct use
with
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- [`adsl`](https://vthanik.github.io/arframe/reference/adsl.md) :
  Subject Level Analysis Dataset (ADSL)
- [`adae`](https://vthanik.github.io/arframe/reference/adae.md) :
  Adverse Events Analysis Dataset (ADAE)
- [`adtte`](https://vthanik.github.io/arframe/reference/adtte.md) : Time
  to Event Analysis Dataset (ADTTE)
- [`adcm`](https://vthanik.github.io/arframe/reference/adcm.md) :
  Concomitant Medications Analysis Dataset (ADCM)
- [`advs`](https://vthanik.github.io/arframe/reference/advs.md) : Vital
  Signs Analysis Dataset (ADVS)
- [`tbl_demog`](https://vthanik.github.io/arframe/reference/tbl_demog.md)
  : Demographics and Baseline Characteristics Table (Table 14.1.1)
- [`tbl_ae_soc`](https://vthanik.github.io/arframe/reference/tbl_ae_soc.md)
  : Adverse Events by System Organ Class Table (Table 14.3.1.2)
- [`tbl_ae_summary`](https://vthanik.github.io/arframe/reference/tbl_ae_summary.md)
  : Overall Adverse Event Summary Table (Table 14.3.1.1)
- [`tbl_disp`](https://vthanik.github.io/arframe/reference/tbl_disp.md)
  : Subject Disposition Table (Table 14.1.3)
- [`tbl_tte`](https://vthanik.github.io/arframe/reference/tbl_tte.md) :
  Time-to-Event Summary Table (Table 14.2.1.1)
- [`tbl_cm`](https://vthanik.github.io/arframe/reference/tbl_cm.md) :
  Concomitant Medications Table (Table 14.4.1)
- [`tbl_vs`](https://vthanik.github.io/arframe/reference/tbl_vs.md) :
  Vital Signs Change from Baseline Table (Table 14.3.5.1)
