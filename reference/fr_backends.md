# List Registered Render Backends

Returns a data frame listing all registered render backends, including
built-in (RTF, LaTeX, PDF) and any custom backends added via
[`fr_register_backend()`](https://vthanik.github.io/arframe/reference/fr_register_backend.md).

## Usage

``` r
fr_backends()
```

## Value

A data frame with columns `format`, `extensions`, and `description`.

## See also

[`fr_register_backend()`](https://vthanik.github.io/arframe/reference/fr_register_backend.md),
[`fr_render()`](https://vthanik.github.io/arframe/reference/fr_render.md)

## Examples

``` r
# List built-in backends
fr_backends()
#>   format extensions                   description
#> 1    rtf   rtf, doc              Rich Text Format
#> 2  latex        tex     LaTeX source (tabularray)
#> 3    pdf        pdf               PDF via XeLaTeX
#> 4   html  html, htm HTML preview (self-contained)

# Register a custom backend, then view updated list
fr_register_backend(
  format = "csv",
  extensions = "csv",
  render_fn = function(spec, page_groups, col_panels, path) {
    utils::write.csv(spec$data, path, row.names = FALSE)
  },
  description = "CSV export (data only)"
)
fr_backends()
#>   format extensions                   description
#> 1    rtf   rtf, doc              Rich Text Format
#> 2  latex        tex     LaTeX source (tabularray)
#> 3    pdf        pdf               PDF via XeLaTeX
#> 4   html  html, htm HTML preview (self-contained)
#> 5    csv        csv        CSV export (data only)
fr_unregister_backend("csv")
```
