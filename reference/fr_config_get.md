# Get the Current Configuration

Returns the current config as a named list. If no config has been loaded
yet, auto-discovers and loads the nearest `_arframe.yml`.

## Usage

``` r
fr_config_get()
```

## Value

A named list of config settings. Top-level keys mirror the YAML
structure: `page`, `header`, `pagehead`, `pagefoot`, `rules`,
`footnotes`, `spacing`, `tokens`. Returns an empty list if no config is
loaded and no `_arframe.yml` is found.

## See also

[`fr_config()`](https://vthanik.github.io/arframe/reference/fr_config.md)
to load,
[`fr_config_reset()`](https://vthanik.github.io/arframe/reference/fr_config_reset.md)
to clear,
[`fr_theme_get()`](https://vthanik.github.io/arframe/reference/fr_theme_get.md)
for session-level theme inspection.

## Examples

``` r
# Start clean — no config loaded
fr_config_reset()

# Load config and inspect top-level keys
default_cfg <- system.file("defaults/_arframe.yml", package = "arframe")
fr_config(default_cfg)
cfg <- fr_config_get()
names(cfg)                    # page, header, pagehead, etc.
#>  [1] "page"      "columns"   "header"    "pagehead"  "pagefoot"  "rules"    
#>  [7] "footnotes" "spacing"   "titles"    "tokens"   

# Access nested keys
cfg$page$font_size            # font size from config
#> [1] 9
cfg$page$orientation          # page orientation
#> [1] "landscape"
cfg$header$bold               # header bold setting
#> [1] FALSE

# After reset, fr_config_get() auto-discovers defaults
fr_config_reset()

# Load a custom config with specific tokens
yml <- file.path(tempdir(), "_arframe.yml")
writeLines(c(
  "tokens:",
  "  study_id: 'DEMO-001'"
), yml)
fr_config(yml)
fr_config_get()$tokens$study_id   # "DEMO-001"
#> [1] "DEMO-001"

# Clean up
fr_config_reset()
unlink(yml)
```
