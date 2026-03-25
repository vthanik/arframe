# Apply Horizontal Rules

Sets horizontal rules (lines) for the table using a named preset or
custom rule properties. Calling `fr_hlines()` again **replaces** all
previously set horizontal rules.

## Usage

``` r
fr_hlines(
  spec,
  preset = "header",
  width = NULL,
  color = NULL,
  linestyle = NULL
)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- preset:

  Named rule preset. One of:

  - `"header"` — single rule below the column header only. **Default and
    most common in TFL outputs** (ICH E3). No top or bottom border;
    footnotes follow the last body row directly.

  - `"open"` — rule above header + rule below header; no bottom border.
    Use when the table bottom is immediately followed by footnotes.

  - `"hsides"` — rule at the top (above header) and bottom (below body)
    only.

  - `"above"` — single rule above the column header only.

  - `"below"` — single rule below the last body row only.

  - `"box"` — full outer border on all four sides.

  - `"booktabs"` — thick top (1 pt) + thin mid (0.5 pt) + thick bottom
    (1 pt). The standard for PDF and publication output.

  - `"void"` — no horizontal rules. Clears all previously set rules.

- width:

  Rule width. Named shorthand or numeric in points:

  - `"hairline"` = 0.25 pt

  - `"thin"` = 0.5 pt (default preset width)

  - `"medium"` = 1.0 pt

  - `"thick"` = 1.5 pt

  - Numeric, e.g. `width = 0.75` (positive, in pt)

  When supplied, overrides the preset's default widths for **all** rules
  created by this call. `NULL` uses each preset's own default widths.

- color:

  Rule colour: hex string (`"#003366"`) or CSS named colour (`"navy"`).
  `NULL` = black (`"#000000"`).

- linestyle:

  Rule line style. One of `"solid"` (default), `"dashed"`, `"dotted"`,
  `"dashdot"`, or `"double"`.

## Value

A modified `fr_spec`. Rules stored in `spec$rules`.

## Regulatory conventions

Most pharma TFL outputs use `"header"`: a single thin solid rule
separating column headers from body. No top or bottom border. Footnotes
are placed directly below the last body row with a short separator line
drawn by the footnote block itself (controlled by the `.separator`
argument of
[`fr_footnotes()`](https://vthanik.github.io/arframe/reference/fr_footnotes.md)).

`"booktabs"` is the standard for PDF/publication output and matches the
LaTeX `\toprule` / `\midrule` / `\bottomrule` pattern.

## Tips

- Combine `fr_hlines()` and
  [`fr_vlines()`](https://vthanik.github.io/arframe/reference/fr_vlines.md)
  independently — each manages its own set of rules.

- `fr_hlines("void")` clears **only** horizontal rules; vertical rules
  from
  [`fr_vlines()`](https://vthanik.github.io/arframe/reference/fr_vlines.md)
  are not affected.

- `width`, `color`, and `linestyle` apply uniformly to **all** rules
  created by this preset call. For mixed widths (e.g. booktabs
  thick/thin), omit `width` and let the preset define its own widths.

## Parameter Precedence

Settings resolve from four tiers (lowest to highest priority): package
defaults \< `_arframe.yml` \<
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
\< this function. Only parameters you explicitly supply override
previous tiers.

## See also

[`fr_vlines()`](https://vthanik.github.io/arframe/reference/fr_vlines.md)
for vertical rules,
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
for cell shading and font overrides,
[`fr_footnotes()`](https://vthanik.github.io/arframe/reference/fr_footnotes.md)
for the `.separator` control.

## Examples

``` r
## ── Common presets ────────────────────────────────────────────────────────

# Standard TFL output: single rule under column header
tbl_demog |> fr_table() |> fr_hlines("header")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)

# Open: header rules, no bottom border
tbl_demog |> fr_table() |> fr_hlines("open")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 2 hline(s)

# Booktabs: publication-quality thick/thin/thick
tbl_demog |> fr_table() |> fr_hlines("booktabs")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 3 hline(s)

# Top and bottom rules only
tbl_demog |> fr_table() |> fr_hlines("hsides")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 2 hline(s)

# Box: full outer border
tbl_demog |> fr_table() |> fr_hlines("box")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules:

# Remove all horizontal rules
tbl_demog |> fr_table() |> fr_hlines("void")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Custom width ─────────────────────────────────────────────────────────

# Hairline rules (0.25 pt)
tbl_demog |> fr_table() |> fr_hlines("header", width = "hairline")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)

# Thick rules (1.5 pt)
tbl_demog |> fr_table() |> fr_hlines("hsides", width = "thick")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 2 hline(s)

# Exact width in points
tbl_demog |> fr_table() |> fr_hlines("header", width = 0.75)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Custom colour (hex or CSS named colour) ────────────────────────────────

# Hex colour
tbl_demog |> fr_table() |> fr_hlines("booktabs", color = "#003366")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 3 hline(s)

# CSS named colour
tbl_demog |> fr_table() |> fr_hlines("header", color = "steelblue")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Custom linestyle ──────────────────────────────────────────────────────

# Dashed rule below header
tbl_demog |> fr_table() |> fr_hlines("header", linestyle = "dashed")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)

# Double rule below header
tbl_demog |> fr_table() |> fr_hlines("header", linestyle = "double")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)

# Dash-dot (SAS ODS equivalent)
tbl_demog |> fr_table() |> fr_hlines("header", linestyle = "dashdot")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Full pipeline ─────────────────────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1 Demographics", "Full Analysis Set") |>
  fr_footnotes("[a] Percentages based on N in column header.") |>
  fr_hlines("header") |>
  fr_vlines("box")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (2):
#> 1. [center] "Table 14.1.1 Demographics"
#> 2. [center] "Full Analysis Set"
#> Header: valign=bottom
#> Rules: 1 hline(s)
#> Footnotes (1):
#> 1. [left] "[a] Percentages based on N in column header."
```
