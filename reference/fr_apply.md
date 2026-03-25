# Apply a Recipe to a Spec

Applies an `fr_recipe` to an `fr_spec`, evaluating each captured verb
call in sequence with the spec as the first argument. This is the
primary way to use recipes in a pipeline.

## Usage

``` r
fr_apply(spec, recipe)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- recipe:

  An `fr_recipe` object from
  [`fr_recipe()`](https://vthanik.github.io/arframe/reference/fr_recipe.md).

## Value

A modified `fr_spec`.

## See also

[`fr_recipe()`](https://vthanik.github.io/arframe/reference/fr_recipe.md)

## Examples

``` r
# Basic application
style <- fr_recipe(fr_hlines("header"), fr_header(bold = TRUE))
spec <- tbl_demog |> fr_table() |> fr_apply(style)

# Apply a recipe and then override one parameter with a direct verb call
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_apply(style) |>
  fr_header(bold = FALSE)   # override the recipe's bold = TRUE

# Apply to different datasets with the same recipe
demog_spec <- tbl_demog |> fr_table() |> fr_apply(style)
disp_spec  <- tbl_disp  |> fr_table() |> fr_apply(style)
```
