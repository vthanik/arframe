# Apply Horizontal and Vertical Rules Together

A convenience wrapper that calls
[`fr_hlines()`](https://vthanik.github.io/arframe/reference/fr_hlines.md)
and
[`fr_vlines()`](https://vthanik.github.io/arframe/reference/fr_vlines.md)
in one step. Use `fr_grid()` when you want to set both rule directions
at once with consistent styling (same width, colour, and linestyle).

Prefer `fr_grid()` over chaining `fr_hlines() |> fr_vlines()` when:

- You want uniform width/colour/linestyle for both directions.

- You use the slash shorthand for concise one-liners:
  `fr_grid("header/box")`.

Prefer separate
[`fr_hlines()`](https://vthanik.github.io/arframe/reference/fr_hlines.md) +
[`fr_vlines()`](https://vthanik.github.io/arframe/reference/fr_vlines.md)
calls when:

- Horizontal and vertical rules need different widths, colours, or
  styles.

- You need `fr_vlines(cols = ...)` for custom column positions.

## Usage

``` r
fr_grid(
  spec,
  hpreset = "header",
  vpreset = "box",
  width = NULL,
  color = NULL,
  linestyle = NULL
)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- hpreset:

  Horizontal rule preset passed to
  [`fr_hlines()`](https://vthanik.github.io/arframe/reference/fr_hlines.md).
  Default `"header"` (single rule below column header — standard TFL
  output).

  Alternatively, pass a **single slash-separated string** combining both
  presets: `"header/box"`, `"booktabs/all"`, etc. When a `/` is
  detected, the left side is used as `hpreset` and the right side as
  `vpreset`.

- vpreset:

  Vertical rule preset passed to
  [`fr_vlines()`](https://vthanik.github.io/arframe/reference/fr_vlines.md).
  Default `"box"` (outer border only). Ignored when `hpreset` contains a
  `/`.

- width:

  Rule width applied to **both** horizontal and vertical rules. Named
  shorthand (`"hairline"`, `"thin"`, `"medium"`, `"thick"`) or numeric
  in points. `NULL` uses each preset's own default.

- color:

  Rule colour for both directions. `NULL` = black.

- linestyle:

  Line style for both directions. `NULL` = `"solid"`.

## Value

A modified `fr_spec` with both horizontal and vertical rules set.

## Common combinations

|  |  |  |  |
|----|----|----|----|
| Call | Horizontal | Vertical | Use case |
| `fr_grid()` | header | box | Pharma standard (most common) |
| `fr_grid("booktabs/box")` | booktabs | box | Publication style |
| `fr_grid("hsides/all")` | top+bottom | all columns | Full grid |
| `fr_grid("open/box")` | above+below header | box | Header-framed |
| `fr_grid("void/void")` | none | none | Clear all rules |

## See also

[`fr_hlines()`](https://vthanik.github.io/arframe/reference/fr_hlines.md)
for horizontal rules only,
[`fr_vlines()`](https://vthanik.github.io/arframe/reference/fr_vlines.md)
for vertical rules only.

## Examples

``` r
## ── Standard pharma output (header + box) ─────────────────────────────

tbl_demog |> fr_table() |> fr_grid()
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Slash shorthand ───────────────────────────────────────────────────

# Equivalent to fr_grid("booktabs", "box")
tbl_demog |> fr_table() |> fr_grid("booktabs/box")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 3 hline(s)

## ── Full grid (top/bottom + all column separators) ────────────────────

tbl_demog |> fr_table() |> fr_grid("hsides/all")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 2 hline(s)

## ── Custom width and colour applied uniformly ─────────────────────────

tbl_demog |> fr_table() |> fr_grid("header/box", width = "medium")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)

# CSS named colour for both h and v rules
tbl_demog |> fr_table() |> fr_grid("header/box", color = "navy")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Full pipeline ─────────────────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1 Demographics") |>
  fr_grid("header/box") |>
  fr_page(orientation = "landscape")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (1):
#> 1. [center] "Table 14.1.1 Demographics"
#> Header: valign=bottom
#> Rules: 1 hline(s)
```
