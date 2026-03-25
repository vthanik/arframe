# Render fr_spec as inline HTML in knitr documents

Enables automatic rendering of `fr_spec` objects in R Markdown, Quarto,
and pkgdown vignettes — just like gt tables render inline.

## Usage

``` r
# S3 method for class 'fr_spec'
knit_print(x, ...)
```

## Arguments

- x:

  An `fr_spec` object.

- ...:

  Ignored.

## Value

An htmltools tag object, rendered by knitr natively.
