# Reset the Study-Level Table Theme

Clears all study-level defaults previously set by
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md).
After calling `fr_theme_reset()`,
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
returns to its built-in defaults (or YAML config defaults if
[`fr_config()`](https://vthanik.github.io/arframe/reference/fr_config.md)
was loaded).

Call this between test runs, at the top of scripts, or when switching
between studies in the same R session.

## Usage

``` r
fr_theme_reset()
```

## Value

Invisibly `NULL`.

## See also

[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
to set,
[`fr_theme_get()`](https://vthanik.github.io/arframe/reference/fr_theme_get.md)
to inspect,
[`fr_config_reset()`](https://vthanik.github.io/arframe/reference/fr_config_reset.md)
to also clear YAML config.

## Examples

``` r
## ── Before / after effect ─────────────────────────────────────────────────

fr_theme(font_size = 9, hlines = "header", orientation = "landscape")
fr_theme_get()$font_size        # 9
#> [1] 9
fr_theme_get()$hlines           # "header"
#> [1] "header"

fr_theme_reset()                # clear everything
fr_theme_get()                  # list() — empty
#> list()
length(fr_theme_get()) == 0L    # TRUE
#> [1] TRUE

## ── Reset does not affect YAML config ─────────────────────────────────────

# fr_theme_reset() only clears the session theme.
# To also clear YAML config, call fr_config_reset() separately.
fr_theme(font_size = 8)
fr_theme_reset()
fr_theme_get()   # list()
#> list()
```
