# Register a Custom Render Backend

Registers a new output format backend so that
[`fr_render()`](https://vthanik.github.io/arframe/reference/fr_render.md)
can produce output in that format. Third-party packages can use this to
add HTML, DOCX, or other formats without modifying arframe source.

## Usage

``` r
fr_register_backend(format, extensions, render_fn, description = "")
```

## Arguments

- format:

  Character scalar. Format identifier (e.g., `"html"`).

- extensions:

  Character vector. File extensions that map to this format (e.g.,
  `c("html", "htm")`).

- render_fn:

  Function. The render function with signature
  `function(spec, page_groups, col_panels, path)`.

- description:

  Character scalar. Human-readable description.

## Value

Invisibly returns `NULL`.

## See also

[`fr_backends()`](https://vthanik.github.io/arframe/reference/fr_backends.md)
to list registered backends,
[`fr_render()`](https://vthanik.github.io/arframe/reference/fr_render.md).

## Examples

``` r
# Register a custom CSV backend
fr_register_backend(
  format = "csv",
  extensions = "csv",
  render_fn = function(spec, page_groups, col_panels, path) {
    utils::write.csv(spec$data, path, row.names = FALSE)
  },
  description = "CSV export (data only)"
)

# Verify it appears in the backend list
fr_backends()
#>   format extensions                   description
#> 1    rtf   rtf, doc              Rich Text Format
#> 2  latex        tex     LaTeX source (tabularray)
#> 3    pdf        pdf               PDF via XeLaTeX
#> 4   html  html, htm HTML preview (self-contained)
#> 5    csv        csv        CSV export (data only)

# Use the custom backend
out <- file.path(tempdir(), "demo.csv")
tbl_demog |> fr_table() |> fr_render(out)
readLines(out, n = 3)
#> [1] "\"characteristic\",\"placebo\",\"zom_50mg\",\"zom_100mg\",\"total\",\"group\""
#> [2] "\"Subjects, n\",\"45\",\"45\",\"45\",\"135\",\"n\""                           
#> [3] "\"Age (years)\",\"\",\"\",\"\",\"\",\"age_cont\""                             
unlink(out)
fr_unregister_backend("csv")
```
