# Get the Current Study-Level Table Theme

Returns the current study-level theme as a named list. Use this to
inspect what defaults have been set via
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md).

## Usage

``` r
fr_theme_get()
```

## Value

A named list of current theme settings, or an empty list if no theme has
been set. Possible keys (all optional):

- `orientation` — `"landscape"` or `"portrait"`

- `paper` — `"letter"`, `"a4"`, `"legal"`

- `font_family` — e.g. `"Times New Roman"`

- `font_size` — numeric (points)

- `margins` — numeric vector (inches)

- `tokens` — named list of token values

- `pagehead` — list with `left`, `center`, `right`, `font_size`, `bold`

- `pagefoot` — list with `left`, `center`, `right`, `font_size`, `bold`

- `hlines` — preset name (e.g. `"header"`)

- `vlines` — preset name (e.g. `"box"`)

- `spacing` — list with `titles_after`, `footnotes_before`,
  `pagehead_after`, `pagefoot_before`, `page_by_after` (integer blank
  lines)

- `col_gap` — integer (points)

- `split` — logical (`TRUE`/`FALSE`) column splitting

- `stub` — character vector (stub column names)

- `group_keep` — logical; whether group_by groups are kept together

- `group_style` — named list; style properties for group header rows

- `page_by_style` — named list; style properties for page_by labels

- `footnote_separator` — logical

## See also

[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
to set,
[`fr_theme_reset()`](https://vthanik.github.io/arframe/reference/fr_theme_reset.md)
to clear,
[`fr_config_get()`](https://vthanik.github.io/arframe/reference/fr_config_get.md)
for YAML config inspection.

## Examples

``` r
fr_theme(font_size = 9, hlines = "header", orientation = "landscape")
fr_theme_get()
#> $orientation
#> [1] "landscape"
#> 
#> $font_size
#> [1] 9
#> 
#> $hlines
#> [1] "header"
#> 
# $font_size
# [1] 9
# $hlines
# [1] "header"
# $orientation
# [1] "landscape"

# Programmatic access
theme <- fr_theme_get()
theme$font_size   # 9
#> [1] 9

## ── Empty state after reset ───────────────────────────────────────────────

fr_theme_reset()
fr_theme_get()     # list() — no theme settings active
#> list()
length(fr_theme_get()) == 0L   # TRUE
#> [1] TRUE
```
