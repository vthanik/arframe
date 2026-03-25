# Apply Vertical Rules

Sets vertical rules (column separators) for the table. Calling
`fr_vlines()` again **replaces** all previously set vertical rules.
Horizontal rules set by
[`fr_hlines()`](https://vthanik.github.io/arframe/reference/fr_hlines.md)
are not affected.

## Usage

``` r
fr_vlines(
  spec,
  preset = "box",
  cols = NULL,
  width = NULL,
  color = NULL,
  linestyle = NULL,
  abovepos = NULL,
  belowpos = NULL
)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- preset:

  Named rule preset or `"void"` (no vertical rules):

  - `"box"` — rules on the leftmost and rightmost table edges only
    (outer vertical borders). Default.

  - `"all"` — rules between every column and on outer edges.

  - `"inner"` — rules between columns only; no outer edges.

  - `"void"` — no vertical rules. Clears all previously set vlines.

- cols:

  Integer vector of column **gap** positions to rule. Position `j` draws
  a rule to the right of column `j`. Use alongside `preset = "void"` to
  add individual column separators. `NULL` uses the preset.

- width:

  Rule width. Named shorthand (`"hairline"`, `"thin"`, `"medium"`,
  `"thick"`) or numeric in points. Default `NULL` (thin, 0.5 pt).

- color:

  Rule colour: hex string or CSS named colour. `NULL` = black.

- linestyle:

  One of `"solid"` (default), `"dashed"`, `"dotted"`, `"dashdot"`,
  `"double"`.

- abovepos, belowpos:

  Fractions (0–1) controlling partial vertical extent of the rule within
  a row. `NULL` = full height (default).

## Value

A modified `fr_spec`. Vertical rules stored in `spec$rules`.

## Preset comparison

|           |           |              |            |                                  |
|-----------|-----------|--------------|------------|----------------------------------|
| Preset    | Left edge | Between cols | Right edge | Use case                         |
| `"box"`   | Yes       | No           | Yes        | Clean outer border (most common) |
| `"all"`   | Yes       | Yes          | Yes        | Full grid with column separators |
| `"inner"` | No        | Yes          | No         | Column separators, open sides    |
| `"void"`  | No        | No           | No         | No vertical rules                |

## Regulatory conventions

Most pharma TFL outputs use **no vertical rules** (`"void"`) or just an
outer box border (`"box"`). Full grids (`"all"`) are uncommon in
regulatory submissions but may be used for dense listing tables or shift
tables where column boundaries aid readability.

## Tips

- `fr_vlines("box")` is the most common use — a clean outer border.

- Combine with `fr_hlines("header")` for the typical pharma style: box
  outer border + single rule under the column header.

- `cols = c(1L, 3L)` draws a rule to the right of columns 1 and 3 —
  useful for separating a stub column from data columns.

- `fr_vlines()` and
  [`fr_hlines()`](https://vthanik.github.io/arframe/reference/fr_hlines.md)
  are independent — each manages its own rules. To set both at once, use
  [`fr_grid()`](https://vthanik.github.io/arframe/reference/fr_grid.md).

## Parameter Precedence

Settings resolve from four tiers (lowest to highest priority): package
defaults \< `_arframe.yml` \<
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
\< this function. Only parameters you explicitly supply override
previous tiers.

## See also

[`fr_hlines()`](https://vthanik.github.io/arframe/reference/fr_hlines.md)
for horizontal rules,
[`fr_grid()`](https://vthanik.github.io/arframe/reference/fr_grid.md) to
set both in one call,
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
for cell shading and font styling.

## Examples

``` r
## ── Common presets ────────────────────────────────────────────────────────

# Outer box border only (most common)
tbl_demog |> fr_table() |> fr_vlines("box")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules:

# Rules between every column
tbl_demog |> fr_table() |> fr_vlines("all")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules:

# Inner column separators only (no outer edges)
tbl_demog |> fr_table() |> fr_vlines("inner")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules:

# Remove all vertical rules
tbl_demog |> fr_table() |> fr_vlines("void")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Custom column positions ───────────────────────────────────────────────

# Rule after the stub column (column 1) only
tbl_demog |>
  fr_table() |>
  fr_vlines("void") |>
  fr_vlines(cols = 1L)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules:

## ── Custom width, style, and colour ────────────────────────────────────────

# Thick outer box
tbl_demog |> fr_table() |> fr_vlines("box", width = "thick")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules:

# Dashed inner separators
tbl_demog |> fr_table() |> fr_vlines("all", linestyle = "dashed")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules:

# CSS named colour
tbl_demog |> fr_table() |> fr_vlines("box", color = "slategray")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules:

## ── Combined with fr_hlines ───────────────────────────────────────────────

# Typical regulatory pharma style
tbl_demog |>
  fr_table() |>
  fr_hlines("header") |>
  fr_vlines("box")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)

# Full grid: all borders
tbl_demog |>
  fr_table() |>
  fr_hlines("hsides") |>
  fr_vlines("all")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 2 hline(s)
```
