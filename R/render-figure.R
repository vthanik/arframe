# ──────────────────────────────────────────────────────────────────────────────
# render-figure.R — Figure rendering for PDF and RTF
#
# Renders fr_spec objects with type = "figure" by saving the plot to a
# temporary file and wrapping it with titles, footnotes, and page chrome.
# ──────────────────────────────────────────────────────────────────────────────

#' Render a figure spec to PDF
#'
#' Saves the plot as a temporary PDF, then includes it in a LaTeX document
#' with titles and footnotes via the same chrome infrastructure as tables.
#'
#' @param spec Finalized fr_spec with type = "figure".
#' @param path Output file path (.pdf).
#' @noRd
render_figure_pdf <- function(spec, path) {
  requireNamespace("grDevices", quietly = TRUE)

  page <- spec$page
  printable <- printable_area_inches(page)
  plot_w <- spec$figure_width %||% printable[["width"]]
  plot_h <- spec$figure_height %||% (printable[["height"]] * 0.7)

  # Save plot to temporary PDF
  tmp_plot <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp_plot), add = TRUE)

  if (inherits(spec$plot, "ggplot")) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      cli_abort(c(
        "Package {.pkg ggplot2} is required to render ggplot figures.",
        "i" = "Install via {.code install.packages(\"ggplot2\")}."
      ))
    }
    ggplot2::ggsave(
      tmp_plot,
      spec$plot,
      width = plot_w,
      height = plot_h,
      device = "pdf"
    )
  } else if (inherits(spec$plot, "recordedplot")) {
    grDevices::pdf(tmp_plot, width = plot_w, height = plot_h)
    grDevices::replayPlot(spec$plot)
    grDevices::dev.off()
  }

  # Build LaTeX document
  font_cmd <- latex_setmainfont(page$font_family)

  lines <- character(0)
  lines <- c(lines, "\\documentclass{article}")
  lines <- c(lines, "\\usepackage{fontspec}")
  lines <- c(lines, "\\usepackage{graphicx}")
  lines <- c(lines, "\\usepackage{geometry}")
  lines <- c(lines, "\\usepackage{fancyhdr}")

  # Page geometry
  orient <- if (page$orientation == "landscape") ",landscape" else ""
  paper <- if (page$paper == "a4") {
    "a4paper"
  } else if (page$paper == "legal") {
    "legalpaper"
  } else {
    "letterpaper"
  }
  lines <- c(
    lines,
    sprintf(
      "\\geometry{%s%s,top=%.2fin,bottom=%.2fin,left=%.2fin,right=%.2fin}",
      paper,
      orient,
      page$margins$top,
      page$margins$bottom,
      page$margins$left,
      page$margins$right
    )
  )

  lines <- c(lines, font_cmd)
  lines <- c(lines, sprintf("\\setlength{\\parindent}{0pt}"))
  lines <- c(lines, sprintf("\\setlength{\\parskip}{0pt}"))

  # Pagehead / pagefoot via fancyhdr
  token_map <- build_token_map(
    page_num = "\\thepage{}",
    total_pages = "\\pageref{LastPage}",
    spec = spec
  )
  lines <- c(lines, "\\usepackage{lastpage}")
  lines <- c(lines, "\\pagestyle{fancy}")
  lines <- c(lines, "\\fancyhf{}")
  lines <- c(lines, "\\renewcommand{\\headrulewidth}{0pt}")
  lines <- c(lines, "\\renewcommand{\\footrulewidth}{0pt}")

  if (!is.null(spec$pagehead)) {
    ph <- spec$pagehead
    fs <- ph$font_size %||% (page$font_size - 1)
    leading <- round(fs * fr_env$latex_leading_factor, 1)
    if (!is.null(ph$left)) {
      txt <- resolve_tokens(
        latex_escape_chrome(ph$left),
        token_map,
        "page header"
      )
      lines <- c(
        lines,
        sprintf("\\lhead{\\fontsize{%s}{%s}\\selectfont %s}", fs, leading, txt)
      )
    }
    if (!is.null(ph$center)) {
      txt <- resolve_tokens(
        latex_escape_chrome(ph$center),
        token_map,
        "page header"
      )
      lines <- c(
        lines,
        sprintf("\\chead{\\fontsize{%s}{%s}\\selectfont %s}", fs, leading, txt)
      )
    }
    if (!is.null(ph$right)) {
      txt <- resolve_tokens(
        latex_escape_chrome(ph$right),
        token_map,
        "page header"
      )
      lines <- c(
        lines,
        sprintf("\\rhead{\\fontsize{%s}{%s}\\selectfont %s}", fs, leading, txt)
      )
    }
  }
  if (!is.null(spec$pagefoot)) {
    pf <- spec$pagefoot
    fs <- pf$font_size %||% (page$font_size - 1)
    leading <- round(fs * fr_env$latex_leading_factor, 1)
    if (!is.null(pf$left)) {
      txt <- resolve_tokens(
        latex_escape_chrome(pf$left),
        token_map,
        "page footer"
      )
      lines <- c(
        lines,
        sprintf("\\lfoot{\\fontsize{%s}{%s}\\selectfont %s}", fs, leading, txt)
      )
    }
    if (!is.null(pf$center)) {
      txt <- resolve_tokens(
        latex_escape_chrome(pf$center),
        token_map,
        "page footer"
      )
      lines <- c(
        lines,
        sprintf("\\cfoot{\\fontsize{%s}{%s}\\selectfont %s}", fs, leading, txt)
      )
    }
    if (!is.null(pf$right)) {
      txt <- resolve_tokens(
        latex_escape_chrome(pf$right),
        token_map,
        "page footer"
      )
      lines <- c(
        lines,
        sprintf("\\rfoot{\\fontsize{%s}{%s}\\selectfont %s}", fs, leading, txt)
      )
    }
  }

  lines <- c(lines, "\\begin{document}")

  # Titles
  titles <- spec$meta$titles %||% list()
  if (length(titles) > 0L) {
    fs <- page$font_size
    for (t in titles) {
      t_fs <- t$font_size %||% fs
      align <- switch(
        t$align %||% "center",
        left = "\\raggedright",
        center = "\\centering",
        right = "\\raggedleft"
      )
      bold_on <- if (isTRUE(t$bold)) "\\bfseries " else ""
      content <- latex_escape(label_to_plain(t$content))
      lines <- c(
        lines,
        sprintf(
          "{%s\\fontsize{%s}{%s}\\selectfont %s%s\\par}",
          align,
          t_fs,
          round(t_fs * 1.2),
          bold_on,
          content
        )
      )
    }
    lines <- c(lines, "\\vspace{6pt}")
  }

  # Plot
  lines <- c(
    lines,
    sprintf(
      "\\begin{center}\\includegraphics[width=%.2fin,height=%.2fin]{%s}\\end{center}",
      plot_w,
      plot_h,
      gsub("\\\\", "/", tmp_plot)
    )
  )

  # Footnotes
  footnotes <- spec$meta$footnotes %||% list()
  if (length(footnotes) > 0L) {
    lines <- c(lines, "\\vspace{6pt}")
    fs <- page$font_size
    for (fn in footnotes) {
      fn_fs <- fn$font_size %||% fs
      align <- switch(
        fn$align %||% "left",
        left = "\\raggedright",
        center = "\\centering",
        right = "\\raggedleft"
      )
      content <- latex_escape(label_to_plain(fn$content))
      lines <- c(
        lines,
        sprintf(
          "{%s\\fontsize{%s}{%s}\\selectfont %s\\par}",
          align,
          fn_fs,
          round(fn_fs * 1.2),
          content
        )
      )
    }
  }

  lines <- c(lines, "\\end{document}")

  # Write .tex and compile
  tex_path <- sub("\\.pdf$", ".tex", path)
  writeLines(lines, tex_path)

  compile_xelatex_doc(tex_path)

  # Move compiled PDF if needed
  compiled <- sub("\\.tex$", ".pdf", tex_path)
  if (compiled != path && file.exists(compiled)) {
    file.copy(compiled, path, overwrite = TRUE)
    unlink(compiled)
  }

  invisible(path)
}


#' Render a figure spec to RTF
#'
#' Saves the plot as a temporary PNG and embeds it in RTF with titles
#' and footnotes as paragraphs.
#'
#' @param spec Finalized fr_spec with type = "figure".
#' @param path Output file path (.rtf).
#' @noRd
render_figure_rtf <- function(spec, path) {
  requireNamespace("grDevices", quietly = TRUE)

  page <- spec$page
  printable <- printable_area_inches(page)
  plot_w <- spec$figure_width %||% printable[["width"]]
  plot_h <- spec$figure_height %||% (printable[["height"]] * 0.7)

  # Save plot as PNG
  tmp_plot <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_plot), add = TRUE)

  dpi <- 300L
  px_w <- as.integer(plot_w * dpi)
  px_h <- as.integer(plot_h * dpi)

  if (inherits(spec$plot, "ggplot")) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      cli_abort(c(
        "Package {.pkg ggplot2} is required to render ggplot figures.",
        "i" = "Install via {.code install.packages(\"ggplot2\")}."
      ))
    }
    ggplot2::ggsave(
      tmp_plot,
      spec$plot,
      width = plot_w,
      height = plot_h,
      dpi = dpi,
      device = "png"
    )
  } else if (inherits(spec$plot, "recordedplot")) {
    grDevices::png(tmp_plot, width = px_w, height = px_h, res = dpi)
    grDevices::replayPlot(spec$plot)
    grDevices::dev.off()
  }

  # Read PNG binary data
  png_data <- readBin(tmp_plot, "raw", file.info(tmp_plot)$size)
  hex_data <- paste0(as.character(png_data), collapse = "")

  # RTF dimensions (in twips)
  pic_w <- inches_to_twips(plot_w)
  pic_h <- inches_to_twips(plot_h)

  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)

  font_name <- resolve_rtf_font(page$font_family)
  rtf_fam <- get_rtf_font_family(font_name)
  prq <- get_rtf_font_prq(font_name)
  fonttbl <- paste0(
    "{\\fonttbl{\\f0\\",
    rtf_fam,
    "\\fprq",
    prq,
    " ",
    font_name,
    ";}}"
  )

  rtf_write(con, paste0("{\\rtf1\\ansi\\ansicpg1252\\deff0\n", fonttbl, "\n"))

  # Section definition
  dims <- paper_dims_twips(page$paper, page$orientation)
  orient <- if (page$orientation == "landscape") "\\lndscpsxn" else ""
  rtf_write(
    con,
    paste0(
      "\\sectd\\sbkpage",
      orient,
      "\\pgwsxn",
      dims[["width"]],
      "\\pghsxn",
      dims[["height"]],
      "\\margl",
      inches_to_twips(page$margins$left),
      "\\margr",
      inches_to_twips(page$margins$right),
      "\\margt",
      inches_to_twips(page$margins$top),
      "\\margb",
      inches_to_twips(page$margins$bottom),
      "\n"
    )
  )

  fs <- pt_to_half_pt(page$font_size)

  # Titles
  titles <- spec$meta$titles %||% list()
  for (t in titles) {
    t_fs <- pt_to_half_pt(t$font_size %||% page$font_size)
    align_rtf <- fr_env$align_to_rtf[[t$align %||% "center"]]
    bold_on <- if (isTRUE(t$bold)) "\\b " else ""
    bold_off <- if (isTRUE(t$bold)) "\\b0" else ""
    content <- rtf_escape(label_to_plain(t$content))
    rtf_write(
      con,
      paste0(
        "\\pard\\plain",
        align_rtf,
        "\\fs",
        t_fs,
        " ",
        bold_on,
        content,
        bold_off,
        "\\par\n"
      )
    )
  }

  if (length(titles) > 0L) {
    rtf_write(con, paste0("\\pard\\plain\\fs", fs, "\\par\n"))
  }

  # Embedded PNG picture
  rtf_write(
    con,
    paste0(
      "\\pard\\qc{\\pict\\pngblip",
      "\\picw",
      px_w,
      "\\pich",
      px_h,
      "\\picwgoal",
      pic_w,
      "\\pichgoal",
      pic_h,
      "\n",
      hex_data,
      "}\\par\n"
    )
  )

  # Footnotes
  footnotes <- spec$meta$footnotes %||% list()
  if (length(footnotes) > 0L) {
    rtf_write(con, paste0("\\pard\\plain\\fs", fs, "\\par\n"))
    for (fn in footnotes) {
      fn_fs <- pt_to_half_pt(fn$font_size %||% page$font_size)
      align_rtf <- fr_env$align_to_rtf[[fn$align %||% "left"]]
      content <- rtf_escape(label_to_plain(fn$content))
      rtf_write(
        con,
        paste0(
          "\\pard\\plain",
          align_rtf,
          "\\fs",
          fn_fs,
          " ",
          content,
          "\\par\n"
        )
      )
    }
  }

  rtf_write(con, "}")
}
