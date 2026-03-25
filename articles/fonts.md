# Fonts

arframe uses fonts that align with FDA and EMA submission guidance. This
vignette covers the default font, the resolution order when a font isn’t
available, and how to override it.

## Default font

The package default is **Times New Roman** at **9 pt** — the
FDA-recommended serif font for submission documents. This applies to all
backends: RTF, PDF, and HTML.

``` yaml
# inst/defaults/_arframe.yml (shipped with arframe)
page:
  font_family: "Times New Roman"
  font_size: 9
```

## Font resolution order

When arframe needs a font, it checks availability in this order:

| Priority | Serif (roman) | Sans-serif (swiss) | Monospace (modern) |
|:---|:---|:---|:---|
| 1\. FDA-recommended | Times New Roman | Calibri / Arial | Courier New |
| 2\. `ARFRAME_FONT_DIR` | (user-supplied) | (user-supplied) | (user-supplied) |
| 3\. Adobe open-source | Source Serif 4 | Source Sans 3 | Source Code Pro |
| 4\. CSS generic | `serif` | `sans-serif` | `monospace` |

**Priority 1** covers Windows and macOS out of the box. On bare
Linux/Docker, these Microsoft fonts are typically absent, so arframe
moves to the next tier.

**Priority 2** checks the directory pointed to by the `ARFRAME_FONT_DIR`
environment variable. Place `.ttf` or `.otf` files there and arframe
will find them — no system-wide installation needed. This is the
recommended approach for Docker, CI, and reproducible environments:

``` bash
export ARFRAME_FONT_DIR="/opt/fonts"
# Place SourceSerif4-Regular.ttf etc. in /opt/fonts/
```

**Priority 3** uses the Adobe Source font family (SIL Open Font License
1.1). These are free, high-quality, and designed to pair together.
Install them from:

- [Source Serif 4](https://github.com/adobe-fonts/source-serif) — serif
- [Source Sans 3](https://github.com/adobe-fonts/source-sans) —
  sans-serif
- [Source Code Pro](https://github.com/adobe-fonts/source-code-pro) —
  monospace
- Or via [Google Fonts](https://fonts.google.com/?query=source)

**Priority 4** (HTML only) is the CSS generic family keyword. RTF/PDF
viewers will substitute the closest available system font.

## Three font families

arframe classifies every font into one of three families. Each family
maps to the correct AFM metrics (for auto-width columns), RTF family
keyword, and LaTeX font command:

| Family | Recognised fonts | AFM metrics |
|:---|:---|:---|
| roman (serif) | Times New Roman, Source Serif 4, Georgia, Palatino, Cambria, Noto Serif | Times-Roman |
| swiss (sans-serif) | Calibri, Arial, Helvetica, Source Sans 3, Verdana, Tahoma, Segoe UI, Noto Sans | Helvetica |
| modern (monospace) | Courier New, Source Code Pro, Consolas | Courier |

Unknown fonts default to the monospace family.

## Changing the font

### Per-table

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_page(font_family = "Calibri", font_size = 10)
```

### Session-wide (theme)

``` r
fr_theme(font_family = "Arial", font_size = 10)
# All subsequent fr_table() calls use Arial 10pt
```

### Project-wide (config file)

Place an `_arframe.yml` at your project root:

``` yaml
page:
  font_family: "Source Sans 3"
  font_size: 10
```

### Precedence

Package defaults \< `_arframe.yml` \<
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
\<
[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md).

## Monospace for listings

Courier New remains the standard for code listings where column
alignment matters. Use `fr_page(font_family = "Courier New")` for
listing outputs:

``` r
spec <- listing_data |>
  fr_table() |>
  fr_page(font_family = "Courier New", font_size = 8)
```

## FDA guidance summary

| Element        | Recommendation                    |
|:---------------|:----------------------------------|
| Narrative text | Times New Roman or Calibri, 12 pt |
| Table body     | 9–10 pt (arframe default: 9 pt)   |
| Footnotes      | 9–10 pt                           |
| Font colour    | Black only; blue for hyperlinks   |

Source: [FDA PDF
Specifications](https://www.fda.gov/regulatory-information/search-fda-guidance-documents/portable-document-format-specifications),
ICH E3 Structure and Content of Clinical Study Reports.
