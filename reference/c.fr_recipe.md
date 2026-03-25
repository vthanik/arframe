# Combine Recipes

Concatenates two or more `fr_recipe` objects into a single recipe. Later
recipes override earlier ones for verbs that replace (most verbs), and
append for verbs that accumulate
([`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md),
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)).

## Usage

``` r
# S3 method for class 'fr_recipe'
c(...)
```

## Arguments

- ...:

  `fr_recipe` objects to combine.

## Value

A combined `fr_recipe` object.

## See also

[`fr_recipe()`](https://vthanik.github.io/arframe/reference/fr_recipe.md),
[`fr_apply()`](https://vthanik.github.io/arframe/reference/fr_apply.md)

## Examples

``` r
# Compose two recipes
base_style <- fr_recipe(fr_page(orientation = "landscape"))
ae_style <- fr_recipe(fr_hlines("box"))
combined <- c(base_style, ae_style)
combined
#> 
#> ── fr_recipe (2 verbs) 
#> 1. fr_page(orientation="landscape")
#> 2. fr_hlines("box")

# Compose and apply in one pipeline
layout <- fr_recipe(
  fr_page(orientation = "landscape", font_size = 9),
  fr_pagehead(left = "{program}")
)
rules <- fr_recipe(
  fr_hlines("header"),
  fr_header(bold = TRUE)
)
full_style <- c(layout, rules)
full_style   # 4 verbs
#> 
#> ── fr_recipe (4 verbs) 
#> 1. fr_page(orientation="landscape", font_size=9)
#> 2. fr_pagehead(left="{program}")
#> 3. fr_hlines("header")
#> 4. fr_header(bold=TRUE)

out <- file.path(tempdir(), "composed.rtf")
tbl_tte |>
  fr_table() |>
  fr_apply(full_style) |>
  fr_titles("Table 14.2.1", "Time-to-Event Analysis") |>
  fr_render(out)
unlink(out)
```
