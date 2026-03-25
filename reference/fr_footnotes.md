# Set Table Footnotes

Sets one or more footnote lines displayed below the table body. Each
argument in `...` produces one footnote line. Calling `fr_footnotes()`
again **replaces** all previously set footnotes.

Footnotes support inline `{fr_*()}` markup, including
[`fr_super()`](https://vthanik.github.io/arframe/reference/fr_super.md)
for superscript footnote labels.

## Usage

``` r
fr_footnotes(
  spec,
  ...,
  .align = "left",
  .placement = "every",
  .font_size = NULL,
  .separator = FALSE
)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- ...:

  One or more footnotes. Each may be:

  - A **character scalar** — the footnote text.

  - A **list**. The first element is the footnote text (can be unnamed).
    Optionally include `align`, `placement`, `font_size` to override
    formatting.

- .align:

  Default alignment. `"left"` (default) is standard for regulatory
  submissions. Override per line via the list form.

- .placement:

  When to print footnotes. `"every"` (default) repeats footnotes on
  every page; `"last"` prints them only on the final page. Override per
  footnote via the list form.

- .font_size:

  Default font size in points. `NULL` inherits from page.

- .separator:

  Logical. Whether to draw a horizontal separator rule above the
  footnote block. Default `FALSE`.

## Value

A modified `fr_spec`. Footnotes are stored in `spec$meta$footnotes`.

## Regulatory conventions

ICH E3 and common pharma submission standards specify:

**Labelling:**

- Footnote labels use lower-case letters in square brackets: `[a]`,
  `[b]`, `[c]`. Do **not** use numeric labels — `[1]`, `[2]` are
  reserved for literature references.

- Do **not** begin the footnote section with the word "Note" or
  "Notes:".

**Layout:**

- Each footnote on its own line, left-aligned, same or smaller font than
  the table body.

- RTF/SAS submissions: limit to **8 footnotes** per table for full
  compatibility with standard TLG macro footnote slots.

**Ordering:**

- `[a]`, `[b]`, … content footnotes first (in the order symbols appear
  reading left-to-right, top-to-bottom through the table).

- Abbreviation expansion line last (e.g.
  `"AE = Adverse Event; FAS = Full Analysis Set; SD = Standard Deviation."`).

- Source / data-cut note on the **final page only**
  (`placement = "last"`).

**Common regulatory footnote patterns:**

- *Percentage denominator*:
  `"[a] Percentages are based on N in the column header."` — required
  whenever `n (%)` columns appear.

- *MedDRA coding*: `"[a] Coded using MedDRA Version XX.X."` — for AE,
  medical history, and prior/concomitant medication tables.

- *Subject counting*:
  `"[a] A subject is counted only once for multiple events within the same System Organ Class or Preferred Term."`
  — AE tables where events \> subjects is possible.

- *Dagger symbol*: use `{fr_dagger()}` and `{fr_ddagger()}` markup for
  treatment-related or serious AE markers.

## Spacing

By default, **one blank line** is inserted between the last body row and
the first footnote. Control this with
[`fr_spacing()`](https://vthanik.github.io/arframe/reference/fr_spacing.md):

    spec |> fr_spacing(footnotes_before = 0L)   # no gap
    spec |> fr_spacing(footnotes_before = 2L)   # two blank lines

This can also be set in `_arframe.yml`:

    spacing:
      footnotes_before: 1

## Tips

- Use
  [`fr_super()`](https://vthanik.github.io/arframe/reference/fr_super.md)
  for superscript footnote markers inside cell values:
  `"{fr_super('a')} Value with footnote"`. The matching footnote line
  should begin with `"[a] ..."`.

- Set `.separator = FALSE` when the table ends with a bottom rule that
  already visually separates the footnotes.

- `placement = "last"` for the source note prevents it repeating on
  every continuation page. **Note:** this only works in PDF output. RTF
  has no mechanism for last-page-only footer content (`{\footer}`
  repeats on every page). In RTF, all footnotes appear on every page
  regardless of placement. A warning is emitted when rendering to RTF
  with `placement = "last"` footnotes.

## Parameter Precedence

Settings resolve from four tiers (lowest to highest priority): package
defaults \< `_arframe.yml` \<
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
\< this function. Only parameters you explicitly supply override
previous tiers.

## See also

[`fr_titles()`](https://vthanik.github.io/arframe/reference/fr_titles.md)
for titles above the table,
[`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md)
for running page footers,
[`fr_spacing()`](https://vthanik.github.io/arframe/reference/fr_spacing.md)
for gap control,
[`fr_dagger()`](https://vthanik.github.io/arframe/reference/fr_dagger.md),
[`fr_ddagger()`](https://vthanik.github.io/arframe/reference/fr_ddagger.md),
[`fr_super()`](https://vthanik.github.io/arframe/reference/fr_super.md)
for inline markup symbols.

## Examples

``` r
## ── Demographics table: standard footnote set ─────────────────────────────

# Correct order: [a][b] content footnotes, abbreviations last, source final page
tbl_demog |>
  fr_table() |>
  fr_footnotes(
    "[a] Percentages are based on N in the column header.",
    "[b] P-value from two-sample t-test (continuous) or chi-squared (categorical).",
    "FAS = Full Analysis Set; SD = Standard Deviation.",
    list("Source: ADSL; data cut 31DEC2024.", placement = "last")
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (4):
#> 1. [left] "[a] Percentages are based on N in the column header."
#> 2. [left] "[b] P-value from two-sample t-test (continuous) or chi-sq..."
#> 3. [left] "FAS = Full Analysis Set; SD = Standard Deviation."
#> 4. [left [last]] "Source: ADSL; data cut 31DEC2024."

## ── AE table: MedDRA + subject-counting + source footnotes ───────────────

tbl_ae_soc |>
  fr_table() |>
  fr_footnotes(
    "[a] Coded using MedDRA Version 27.1.",
    paste0("[b] A subject is counted only once for multiple events within ",
           "the same System Organ Class or Preferred Term."),
    "[c] Percentages are based on N in the column header.",
    paste0("AE = Adverse Event; PT = Preferred Term; ",
           "SOC = System Organ Class; TEAE = Treatment-Emergent AE."),
    list("Source: ADAE; data cut 31DEC2024.", placement = "last")
  )
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (5):
#> 1. [left] "[a] Coded using MedDRA Version 27.1."
#> 2. [left] "[b] A subject is counted only once for multiple events wi..."
#> 3. [left] "[c] Percentages are based on N in the column header."
#> 4. [left] "AE = Adverse Event; PT = Preferred Term; SOC = System Org..."
#> 5. [left [last]] "Source: ADAE; data cut 31DEC2024."

## ── Source note on last page only ────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_footnotes(
    "[a] Percentages based on N in column header.",
    list("Source: ADSL; data cut 31DEC2024.", placement = "last")
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (2):
#> 1. [left] "[a] Percentages based on N in column header."
#> 2. [left [last]] "Source: ADSL; data cut 31DEC2024."

## ── Inline markup: dagger and double-dagger symbols ──────────────────────

tbl_ae_soc |>
  fr_table() |>
  fr_footnotes(
    "{fr_dagger()} Treatment-related adverse events.",
    "{fr_ddagger()} Serious adverse events."
  )
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (2):
#> 1. [left] "† Treatment-related adverse events."
#> 2. [left] "‡ Serious adverse events."

## ── Per-line font size override ───────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_footnotes(
    "[a] Percentages based on N in column header.",
    list("Source: ADSL.", font_size = 7)
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (2):
#> 1. [left] "[a] Percentages based on N in column header."
#> 2. [left] "Source: ADSL."

## ── No separator rule ────────────────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_hlines("hsides") |>
  fr_footnotes(
    "[a] Percentages based on N in column header.",
    .separator = FALSE
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 2 hline(s)
#> Footnotes (1):
#> 1. [left] "[a] Percentages based on N in column header."

## ── Right-aligned footnotes ───────────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_footnotes(
    "[a] Percentages based on N in column header.",
    .align = "right"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [right] "[a] Percentages based on N in column header."

## ── Clear all footnotes ───────────────────────────────────────────────────

spec <- tbl_demog |> fr_table() |> fr_footnotes("[a] Old footnote.")
spec <- spec |> fr_footnotes()  # removes all footnotes
```
