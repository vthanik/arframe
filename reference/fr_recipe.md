# Create a Reusable Table Recipe

Captures a sequence of `fr_*()` verb calls as an `fr_recipe` object that
can be applied to any `fr_spec`. Recipes enable team-wide
standardisation: define your company style once, reuse it across
hundreds of tables.

Apply a recipe to a spec with
[`fr_apply()`](https://vthanik.github.io/arframe/reference/fr_apply.md).
Compose multiple recipes with [`c()`](https://rdrr.io/r/base/c.html):
`c(recipe_a, recipe_b)` creates a combined recipe where `recipe_b`
overrides `recipe_a` for verbs that replace (most verbs), and appends
for verbs that accumulate (`fr_spans`, `fr_styles`).

## Usage

``` r
fr_recipe(...)
```

## Arguments

- ...:

  Unevaluated `fr_*()` verb calls. Each call must be a pipeline verb
  that takes an `fr_spec` as its first argument. The `spec` argument is
  omitted — it is injected when the recipe is applied.

## Value

An `fr_recipe` object (list of captured call expressions).

## Application

- **[`fr_apply()`](https://vthanik.github.io/arframe/reference/fr_apply.md)**:
  `spec |> fr_apply(recipe)` — pipe-friendly

- **[`c()`](https://rdrr.io/r/base/c.html)**: `c(recipe1, recipe2)` —
  compose two recipes

## Serialisation

Recipes store R language objects (calls), not closures, so they survive
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) /
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) roundtrips:

    saveRDS(company_style, "company_style.rds")
    company_style <- readRDS("company_style.rds")

## See also

[`fr_apply()`](https://vthanik.github.io/arframe/reference/fr_apply.md)
for pipe-friendly application,
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
for session-wide defaults.

## Examples

``` r
## ── Recipe for page layout ────────────────────────────────────────────────

page_recipe <- fr_recipe(
  fr_page(orientation = "landscape", font_size = 9),
  fr_pagehead(left = "{program}", right = "{datetime}"),
  fr_pagefoot(center = "Page {thepage} of {total_pages}")
)
page_recipe
#> 
#> ── fr_recipe (3 verbs) 
#> 1. fr_page(orientation="landscape", font_size=9)
#> 2. fr_pagehead(left="{program}", right="{datetime}")
#> 3. fr_pagefoot(center="Page {thepage} of {total_pages}")

## ── Recipe for styling ────────────────────────────────────────────────────

style_recipe <- fr_recipe(
  fr_hlines("header"),
  fr_header(bold = TRUE, align = "center")
)
style_recipe
#> 
#> ── fr_recipe (2 verbs) 
#> 1. fr_hlines("header")
#> 2. fr_header(bold=TRUE, align="center")

## ── Apply a recipe then add per-table customisation ───────────────────────

company_style <- c(page_recipe, style_recipe)

spec <- tbl_demog |>
  fr_table() |>
  fr_apply(company_style) |>
  fr_titles("Table 14.1.1", "Demographics")
spec
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (2):
#> 1. [center] "Table 14.1.1"
#> 2. [center] "Demographics"
#> Header: bold, valign=bottom, align=center
#> Rules: 1 hline(s)

## ── Compose: base + override for AE tables ────────────────────────────────

ae_style <- c(company_style, fr_recipe(
  fr_hlines("box"),
  fr_page(font_size = 8)
))
ae_spec <- tbl_ae_soc |>
  fr_table() |>
  fr_apply(ae_style) |>
  fr_titles("Table 14.3.1", "AE Summary by SOC")
```
