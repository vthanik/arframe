#' arframe: Clinical Tables, Figures, and Listings for Regulatory Submission
#'
#' @description
#' Produces regulatory-grade clinical trial tables, listings, and figures
#' (TLFs) in RTF and PDF from a single specification. Designed for
#' pharmaceutical industry workflows with support for paginated output,
#' spanning headers, decimal alignment, programmatic page headers/footers,
#' and company-specific rule presets. Built on disciplined S3 classes with
#' a clean, composable pipeline API. No dplyr, tidyr, or purrr dependency
#' — only lightweight infrastructure packages (rlang, cli, vctrs).
#'
#' ## Core Pipeline
#'
#' ```r
#' # Study setup (once per program, or use _arframe.yml)
#' fr_theme(
#'   font_size = 9, orientation = "landscape",
#'   hlines = "header", header = list(bold = TRUE),
#'   n_format = "{label}\n(N={n})",
#'   pagehead = list(left = "TFRM-2024-001",
#'                   right = "Page {thepage} of {total_pages}")
#' )
#' n_itt <- c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)
#'
#' # Per-table: only what's unique
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles("Table 14.1.1", "Demographics and Baseline Characteristics") |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     placebo        = fr_col("Placebo", width = 1.5, align = "right"),
#'     zom_50mg       = fr_col("Zomerane 50mg", width = 1.5, align = "right"),
#'     zom_100mg      = fr_col("Zomerane 100mg", width = 1.5, align = "right"),
#'     total          = fr_col("Total", width = 1.5, align = "right"),
#'     .n = n_itt
#'   ) |>
#'   fr_footnotes("Source: ADSL") |>
#'   fr_render("output/t_14_1_1.rtf")
#' ```
#'
#' ## Design Principles
#'
#' - **`fr_` prefix** avoids all base R clashes (col, text, sub, page)
#' - **Glue-string markup** in titles, footnotes, column labels, and cell data:
#'   `"{fr_super(1)} chi-square test"`
#' - **Titles/footnotes always in body**: page header/footer via `fr_pagehead()`
#'   / `fr_pagefoot()`
#' - **`{token}` placeholders** in page headers/footers: `{thepage}`,
#'   `{total_pages}`, `{program}`, `{datetime}`, plus custom tokens.
#'   Literal brace: `{{` produces `{` in output.
#' - **No trimming** of leading whitespace — pharma indentation preserved
#' - **Immutable pipeline**: every verb returns a modified spec, side effects
#'   only at `fr_render()`
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import cli
#' @importFrom glue glue
#' @importFrom stringi stri_replace_all_fixed stri_detect_regex stri_count_fixed stri_detect_fixed stri_locate_first_fixed stri_sub stri_wrap
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom vctrs vec_cast
#' @importFrom yaml read_yaml
## usethis namespace: end
NULL


# ── Package load hook: initialise backend registry ───────────────────────────
.onLoad <- function(libname, pkgname) {
  fr_env$backends <- list(
    rtf = list(
      render = render_rtf,
      extensions = c("rtf", "doc"),
      description = "Rich Text Format"
    ),
    latex = list(
      render = render_latex,
      extensions = c("tex"),
      description = "LaTeX source (tabularray)"
    ),
    pdf = list(
      render = render_pdf,
      extensions = c("pdf"),
      description = "PDF via XeLaTeX"
    ),
    html = list(
      render = render_html,
      extensions = c("html", "htm"),
      description = "HTML preview (self-contained)"
    )
  )
}
