# ============================================================================
# arframe Team Pitch — Features GSK Cannot Do Today
# ============================================================================
#
# This script showcases arframe capabilities that the current
# tfrmt + gt + docorator stack CANNOT produce:
#
#   1. Wide vital signs table with COLUMN SPLITTING (auto-paneled)
#   2. Decimal alignment in PDF (not possible with gt)
#   3. page_by — one table spec, separate pages per parameter
#   4. Spanning headers with per-page N-counts
#   5. One spec → PDF + RTF + HTML (three outputs, zero extra code)
#   6. Study-wide theme (set once, every table inherits)
#
# All examples use arframe's built-in ADaM datasets.

library(arframe)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  Study-wide theme — set ONCE, all tables below inherit                 ║
# ╚══════════════════════════════════════════════════════════════════════════╝
# In the pharmaverse stack, each table needs its own docorator wrapping
# with fancyhead/fancyfoot/geometry. In arframe, set it once:

fr_theme_reset()
fr_theme(
  font_size   = 9,
  font_family = "Courier New",
  orientation = "landscape",
  hlines      = "header",
  header      = list(bold = TRUE, align = "center"),
  n_format    = "{label}\n(N={n})",
  footnote_separator = FALSE,
  pagehead = list(left = "Protocol: TFRM-2024-001", right = "CONFIDENTIAL"),
  pagefoot = list(left = "{program}", right = "Page {thepage} of {total_pages}")
)

# Done. Every fr_render() call below inherits these settings.
# No tfl_format() wrapper needed. No gsk_styling() function needed.


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  1. COLUMN SPLITTING — Wide table auto-paneled across pages            ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# Vital signs tables have 9+ data columns (3 treatments × 3 timepoints).
# When they don't fit on one page, you need COLUMN PANELS — the stub
# column repeats on each panel with a subset of data columns.
#
# gt/tfrmt/docorator CANNOT do this. You'd manually split the data
# and create separate gt objects. arframe does it with .split = TRUE.

cat("═══════════════════════════════════════════════════════════\n")
cat("  1. COLUMN SPLITTING — wide tables auto-paneled\n")
cat("═══════════════════════════════════════════════════════════\n\n")

vs_week24 <- arframe::tbl_vs |>
  filter(timepoint == "Week 24", param == "Systolic BP (mmHg)")

wide_spec <- vs_week24 |>
  fr_table() |>
  fr_titles(
    "Table 14.3.6.1",
    "Systolic Blood Pressure — Week 24 (Column Split Demo)",
    "Safety Population"
  ) |>
  fr_cols(
    param     = fr_col(visible = FALSE),
    timepoint = fr_col(visible = FALSE),
    # Stub column repeats on every panel
    statistic      = fr_col("Statistic", width = 1.5, stub = TRUE),
    # 9 data columns — too wide for one page
    placebo_base   = fr_col("Placebo\nBaseline",  width = 1.0, align = "decimal"),
    placebo_value  = fr_col("Placebo\nWeek 24",   width = 1.0, align = "decimal"),
    placebo_chg    = fr_col("Placebo\nCFB",       width = 1.0, align = "decimal"),
    zom_50mg_base  = fr_col("Zom 50mg\nBaseline", width = 1.0, align = "decimal"),
    zom_50mg_value = fr_col("Zom 50mg\nWeek 24",  width = 1.0, align = "decimal"),
    zom_50mg_chg   = fr_col("Zom 50mg\nCFB",      width = 1.0, align = "decimal"),
    zom_100mg_base  = fr_col("Zom 100mg\nBaseline", width = 1.0, align = "decimal"),
    zom_100mg_value = fr_col("Zom 100mg\nWeek 24",  width = 1.0, align = "decimal"),
    zom_100mg_chg   = fr_col("Zom 100mg\nCFB",      width = 1.0, align = "decimal"),
    .split = TRUE
  ) |>
  fr_footnotes("CFB = Change from Baseline.",
               "Column panels created automatically — stub repeats on each panel.")

wide_spec |> fr_render(file.path(tempdir(), "column_split.pdf"))
wide_spec  # HTML preview

cat("PDF with auto column panels written to:", file.path(tempdir(), "column_split.pdf"), "\n")
cat("Try this with gt/tfrmt — you can't. You'd manually split into 2 tables.\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  2. PAGE_BY — One spec, separate pages per parameter                   ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# Vital signs table with ALL parameters — each gets its own page.
# N-counts update per page automatically from a data frame.
# gt/tfrmt have no concept of page_by.

cat("═══════════════════════════════════════════════════════════\n")
cat("  2. PAGE_BY — one spec, separate pages per parameter\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Per-parameter N-counts (from raw ADVS)
vs_n <- arframe::advs |>
  filter(AVISIT == "Baseline") |>
  distinct(USUBJID, PARAM, TRTA) |>
  count(PARAM, TRTA, name = "USUBJID")

page_by_spec <- arframe::tbl_vs |>
  filter(timepoint == "Week 24") |>
  fr_table() |>
  fr_titles(
    "Table 14.3.6",
    "Vital Signs — Week 24 Summary by Parameter",
    "Safety Population"
  ) |>
  fr_cols(
    param     = fr_col(visible = FALSE),
    timepoint = fr_col(visible = FALSE),
    statistic         = fr_col("Statistic", width = 1.2),
    placebo_base      = fr_col("Baseline",  align = "decimal"),
    placebo_value     = fr_col("Value",     align = "decimal"),
    placebo_chg       = fr_col("CFB",       align = "decimal"),
    zom_50mg_base     = fr_col("Baseline",  align = "decimal"),
    zom_50mg_value    = fr_col("Value",     align = "decimal"),
    zom_50mg_chg      = fr_col("CFB",       align = "decimal"),
    zom_100mg_base    = fr_col("Baseline",  align = "decimal"),
    zom_100mg_value   = fr_col("Value",     align = "decimal"),
    zom_100mg_chg     = fr_col("CFB",       align = "decimal"),
    .n = vs_n
  ) |>
  fr_rows(page_by = "param", page_by_bold = TRUE) |>
  fr_spans(
    "Placebo"        = c("placebo_base", "placebo_value", "placebo_chg"),
    "Zomerane 50mg"  = c("zom_50mg_base", "zom_50mg_value", "zom_50mg_chg"),
    "Zomerane 100mg" = c("zom_100mg_base", "zom_100mg_value", "zom_100mg_chg")
  ) |>
  fr_footnotes("CFB = Change from Baseline.",
               "Each parameter on a separate page with its own N-counts.")

page_by_spec |> fr_render(file.path(tempdir(), "page_by_vital_signs.pdf"))
page_by_spec  # HTML preview

cat("PDF with per-parameter pages written to:", file.path(tempdir(), "page_by_vital_signs.pdf"), "\n")
cat("5 parameters = 5 pages, each with correct N-counts. One spec.\n")
cat("gt/tfrmt: you'd write 5 separate gt objects + 5 docorator calls.\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  3. DECIMAL ALIGNMENT — the #1 missing feature in gt                   ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# gt aligns text left or center. Numbers like "56.5 (12.15)" and "3 (6.7)"
# visually jump around when center-aligned. Decimal alignment keeps the
# decimal point (or parenthesis) in a fixed column position.

cat("═══════════════════════════════════════════════════════════\n")
cat("  3. DECIMAL ALIGNMENT — not possible in gt\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Use built-in tbl_demog for a quick demo
decimal_spec <- arframe::tbl_demog |>
  fr_table() |>
  fr_titles("Decimal Alignment Demo",
            "Notice how decimals and parentheses line up vertically") |>
  fr_cols(
    characteristic = fr_col("", width = 2.5),
    placebo   = fr_col("Placebo",        align = "decimal"),
    zom_50mg  = fr_col("Zomerane 50mg",  align = "decimal"),
    zom_100mg = fr_col("Zomerane 100mg", align = "decimal"),
    total     = fr_col("Total",          align = "decimal"),
    group     = fr_col(visible = FALSE),
    .n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)
  ) |>
  fr_rows(group_by = "group", blank_after = "group") |>
  fr_footnotes("align = 'decimal' keeps numbers visually aligned.",
               "gt/tfrmt can only do left/center/right alignment.")

decimal_spec |> fr_render(file.path(tempdir(), "decimal_alignment.pdf"))
decimal_spec  # HTML preview

cat("PDF written to:", file.path(tempdir(), "decimal_alignment.pdf"), "\n")
cat("Compare with gt's center alignment — decimals jump around.\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  4. ONE SPEC → THREE FORMATS                                          ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# In the pharmaverse stack:
#   - PDF: gt → docorator → render_pdf()
#   - RTF: gt → gtsave("file.rtf") — single-page, no pagination
#   - HTML: gt directly (different from PDF layout)
#
# In arframe: same spec, same formatting, all three outputs.

cat("═══════════════════════════════════════════════════════════\n")
cat("  4. ONE SPEC → PDF + RTF + HTML\n")
cat("═══════════════════════════════════════════════════════════\n\n")

ae_spec <- arframe::tbl_ae_soc |>
  fr_table() |>
  fr_titles("Table 14.3.1",
            list("TEAEs by SOC and Preferred Term", bold = TRUE),
            "Safety Population") |>
  fr_page(continuation = "(continued)") |>
  fr_cols(
    soc       = fr_col(visible = FALSE),
    pt        = fr_col("System Organ Class\n  Preferred Term", width = 3.5),
    row_type  = fr_col(visible = FALSE),
    placebo   = fr_col("Placebo",        align = "decimal"),
    zom_50mg  = fr_col("Zomerane\n50mg", align = "decimal"),
    zom_100mg = fr_col("Zomerane\n100mg", align = "decimal"),
    total     = fr_col("Total",          align = "decimal"),
    .n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)
  ) |>
  fr_rows(group_by = "soc", indent_by = "pt") |>
  fr_styles(
    fr_row_style(rows = fr_rows_matches("row_type", value = "total"), bold = TRUE),
    fr_row_style(rows = fr_rows_matches("row_type", value = "soc"),   bold = TRUE)
  ) |>
  fr_footnotes("MedDRA version 26.0.",
               "Subjects counted once per SOC and PT.")

# Same spec, three outputs — zero extra code
ae_spec |> fr_render(file.path(tempdir(), "ae_soc.pdf"))
ae_spec |> fr_render(file.path(tempdir(), "ae_soc.rtf"))
ae_spec  # HTML preview in viewer

cat("Same spec rendered to:\n")
cat("  PDF:", file.path(tempdir(), "ae_soc.pdf"), "\n")
cat("  RTF:", file.path(tempdir(), "ae_soc.rtf"), "\n")
cat("  HTML: viewer preview\n")
cat("pharmaverse needs different pipelines for each format.\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  5. BATCH RENDER — 7 tables in a loop                                 ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("═══════════════════════════════════════════════════════════\n")
cat("  5. BATCH RENDER — entire CSR in a loop\n")
cat("═══════════════════════════════════════════════════════════\n\n")

n_itt    <- c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)
n_safety <- n_itt

tables <- list(
  "Table_14_1_1" = arframe::tbl_demog |>
    fr_table() |>
    fr_titles("Table 14.1.1", "Demographics", "ITT Population") |>
    fr_cols(
      characteristic = fr_col("", width = 2.5),
      placebo = fr_col("Placebo", align = "decimal"),
      zom_50mg = fr_col("Zomerane 50mg", align = "decimal"),
      zom_100mg = fr_col("Zomerane 100mg", align = "decimal"),
      total = fr_col("Total", align = "decimal"),
      group = fr_col(visible = FALSE), .n = n_itt
    ) |>
    fr_rows(group_by = "group", blank_after = "group"),

  "Table_14_1_4" = arframe::tbl_disp |>
    fr_table() |>
    fr_titles("Table 14.1.4", "Subject Disposition") |>
    fr_cols(
      category = fr_col("", width = 2.5),
      placebo = fr_col("Placebo", align = "decimal"),
      zom_50mg = fr_col("Zomerane 50mg", align = "decimal"),
      zom_100mg = fr_col("Zomerane 100mg", align = "decimal"),
      total = fr_col("Total", align = "decimal"), .n = n_itt
    ),

  "Table_14_3_1" = ae_spec,

  "Table_14_3_1_1" = arframe::tbl_ae_summary |>
    fr_table() |>
    fr_titles("Table 14.3.1.1", "Overall AE Summary", "Safety Population") |>
    fr_cols(
      category = fr_col("", width = 3.5),
      zom_50mg = fr_col("Zomerane\n50mg", align = "decimal"),
      zom_100mg = fr_col("Zomerane\n100mg", align = "decimal"),
      placebo = fr_col("Placebo", align = "decimal"),
      total = fr_col("Total", align = "decimal"), .n = n_safety
    )
)

outdir <- file.path(tempdir(), "csr_batch")
dir.create(outdir, showWarnings = FALSE)

for (nm in names(tables)) {
  tables[[nm]] |> fr_render(file.path(outdir, paste0(nm, ".pdf")))
  tables[[nm]] |> fr_render(file.path(outdir, paste0(nm, ".rtf")))
}

cat("4 tables × 2 formats = 8 files written to:\n", outdir, "\n")
cat("With pharmaverse: 4 × tfl_format() calls, each ~20 lines = 80 lines extra.\n")
cat("With arframe: a 3-line for loop.\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  Summary for your team                                                 ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("═══════════════════════════════════════════════════════════\n")
cat("  WHY ARFRAME FOR GSK\n")
cat("═══════════════════════════════════════════════════════════\n\n")
cat("1. COLUMN SPLITTING   — Wide tables auto-panel. No manual split.\n")
cat("2. PAGE_BY             — Per-parameter pages with auto N-counts.\n")
cat("3. DECIMAL ALIGNMENT   — Real alignment, not center-faking-it.\n")
cat("4. ONE SPEC → 3 FMTS  — PDF + RTF + HTML from same pipeline.\n")
cat("5. STUDY-WIDE THEME    — Set once. No tfl_format()/gsk_styling().\n")
cat("6. ~25 LINES PER TABLE — vs ~300+ in pharmaverse stack.\n")
cat("7. 1 PACKAGE           — vs 5+ (cards/tfrmt/gt/docorator/tidyr).\n")
cat("8. NO TIDYVERSE DEP    — No dplyr/purrr/tidyr in arframe itself.\n")
cat("9. CONTINUATION TEXT   — '(continued)' on page 2+. docorator can't.\n")
cat("10. REPEATING HEADERS  — Column headers on every page. docorator can't.\n")

fr_theme_reset()
