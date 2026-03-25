# Register a Custom Decimal Alignment Stat Type

Adds a custom statistical format type to the decimal alignment engine.
Use this when your data contains formatted statistics that don't match
any of the 18 built-in types (see `fr_env$stat_type_registry` for the
full list).

## Usage

``` r
fr_register_stat_type(name, pattern, family = "custom", richness = 3L)
```

## Arguments

- name:

  Character scalar. Unique name for the type (e.g., `"ratio_ci"`). Must
  not conflict with built-in type names.

- pattern:

  Character scalar. Perl-compatible regex that matches the formatted
  string. Must match the **entire** string (anchored with `^...$`).

- family:

  Character scalar. Alignment family: `"compound"`, `"estimate"`,
  `"range"`, `"count"`, `"float"`, or `"custom"`. Determines how the
  type aligns relative to other types in the same column. Default
  `"custom"`.

- richness:

  Integer scalar. Component count (higher = more complex). Used for
  tie-breaking when multiple types match. Default `3L`.

## Value

Invisible `NULL`. Called for its side effect of registering the type.

## See also

[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
with `align = "decimal"` for decimal alignment.

## Examples

``` r
# Register a custom "ratio (lower, upper)" format
fr_register_stat_type(
  name = "ratio_ci",
  pattern = "^-?\\d+\\.?\\d*\\s*\\(-?\\d+\\.?\\d*,\\s*-?\\d+\\.?\\d*\\)$",
  family = "compound",
  richness = 4L
)
```
