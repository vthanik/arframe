# Reset Configuration

Clears the loaded YAML config. After calling `fr_config_reset()`,
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
uses built-in defaults (or
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
if set). Call
[`fr_config()`](https://vthanik.github.io/arframe/reference/fr_config.md)
again to reload.

## Usage

``` r
fr_config_reset()
```

## Value

Invisibly `NULL`.

## See also

[`fr_config()`](https://vthanik.github.io/arframe/reference/fr_config.md)
to load,
[`fr_config_get()`](https://vthanik.github.io/arframe/reference/fr_config_get.md)
to inspect.

## Examples

``` r
# Load a config
default_cfg <- system.file("defaults/_arframe.yml", package = "arframe")
fr_config(default_cfg)
fr_config_get()$page$orientation   # has a value
#> [1] "landscape"

# Reset clears everything
fr_config_reset()

# After reset and re-load, config is back to defaults
fr_config(default_cfg)
fr_config_get()$page$orientation
#> [1] "landscape"
fr_config_reset()                  # clean up
```
