# Remove a Custom Render Backend

Removes a previously registered backend by format name. Built-in
backends (rtf, latex, pdf) can also be removed if needed.

## Usage

``` r
fr_unregister_backend(format)
```

## Arguments

- format:

  Character scalar. The format name to remove (e.g., `"csv"`).

## Value

`NULL` (invisible). Called for side effect.

## See also

[`fr_register_backend()`](https://vthanik.github.io/arframe/reference/fr_register_backend.md),
[`fr_backends()`](https://vthanik.github.io/arframe/reference/fr_backends.md)

## Examples

``` r
fr_register_backend("csv", "csv",
  render_fn = function(spec, page_groups, col_panels, path) {
    utils::write.csv(spec$data, path, row.names = FALSE)
  })
"csv" %in% fr_backends()$format
#> [1] TRUE
fr_unregister_backend("csv")
"csv" %in% fr_backends()$format
#> [1] FALSE
```
