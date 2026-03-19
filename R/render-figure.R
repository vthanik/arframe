# ──────────────────────────────────────────────────────────────────────────────
# render-figure.R — Figure rendering for PDF and RTF
#
# Renders fr_spec objects with type = "figure" by saving the plot to a
# temporary file and wrapping it with titles, footnotes, and page chrome.
#
# Multi-page figures: when spec$plots is a list with > 1 element, each plot
# gets its own page. Per-page metadata (spec$figure_meta) columns are merged
# into the token map and resolved in titles/footnotes via {column_name}.
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# Shared helpers
# ══════════════════════════════════════════════════════════════════════════════

#' Get the list of plots to render (single or multi-page)
#' @noRd
figure_plot_list <- function(spec) {
  if (!is.null(spec$plots) && length(spec$plots) > 0L) {
    spec$plots
  } else {
    list(spec$plot)
  }
}


#' Extract metadata row as a named list of character values
#' @noRd
meta_row_tokens <- function(spec, page_idx) {
  if (is.null(spec$figure_meta)) {
    return(NULL)
  }
  row <- spec$figure_meta[page_idx, , drop = FALSE]
  lapply(as.list(row), as.character)
}


#' Build per-page token map from a base map plus per-page metadata
#' @noRd
figure_page_token_map <- function(
  base_token_map,
  page_idx,
  n_pages,
  meta_tokens = NULL
) {
  token_map <- base_token_map
  token_map[["thepage"]] <- as.character(page_idx)
  token_map[["total_pages"]] <- as.character(n_pages)

  if (!is.null(meta_tokens)) {
    token_map <- c(token_map, meta_tokens)
  }

  token_map
}


#' Save a single plot to a file
#' @noRd
save_plot_to_file <- function(
  plot,
  path,
  width,
  height,
  device = "png",
  dpi = 300L
) {
  if (inherits(plot, "ggplot")) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      cli_abort(c(
        "Package {.pkg ggplot2} is required to render ggplot figures.",
        "i" = "Install via {.code install.packages(\"ggplot2\")}."
      ))
    }
    args <- list(
      filename = path,
      plot = plot,
      width = width,
      height = height,
      device = device
    )
    if (device == "png") {
      args$dpi <- dpi
    }
    do.call(ggplot2::ggsave, args)
  } else if (inherits(plot, "recordedplot")) {
    if (device == "pdf") {
      grDevices::pdf(path, width = width, height = height)
    } else {
      px_w <- as.integer(width * dpi)
      px_h <- as.integer(height * dpi)
      grDevices::png(path, width = px_w, height = px_h, res = dpi)
    }
    grDevices::replayPlot(plot)
    grDevices::dev.off()
  }

  invisible(path)
}


# ══════════════════════════════════════════════════════════════════════════════
# PDF rendering
# ══════════════════════════════════════════════════════════════════════════════

#' Render a figure spec to PDF
#'
#' Saves each plot as a temporary PDF, then includes it in a LaTeX document
#' with titles and footnotes. Multi-page figures get one LaTeX page per plot.
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

  plot_list <- figure_plot_list(spec)
  n_pages <- length(plot_list)

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

  # Pagehead / pagefoot via fancyhdr (uses LaTeX native page counters)
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

  # ── Pre-process titles/footnotes (hoist label_to_plain out of loop) ──────
  titles <- spec$meta$titles %||% list()
  footnotes <- spec$meta$footnotes %||% list()
  plain_titles <- lapply(titles, function(t) label_to_plain(t$content))
  plain_footnotes <- lapply(footnotes, function(fn) label_to_plain(fn$content))

  # Build base token map once (hoists get_source_path/get_timestamp)
  base_token_map <- build_token_map(
    page_num = 1L,
    total_pages = n_pages,
    spec = spec
  )

  # ── Per-page content ─────────────────────────────────────────────────────
  tmp_plots <- character(0)
  on.exit(unlink(tmp_plots), add = TRUE)

  for (pg in seq_len(n_pages)) {
    if (pg > 1L) {
      lines <- c(lines, "\\newpage")
    }

    # Save plot to temp file (one at a time)
    tmp <- tempfile(fileext = ".pdf")
    save_plot_to_file(plot_list[[pg]], tmp, plot_w, plot_h, device = "pdf")
    tmp_plots <- c(tmp_plots, tmp)

    # Build per-page token map (only page number + meta differ)
    meta_toks <- meta_row_tokens(spec, pg)
    page_token_map <- figure_page_token_map(
      base_token_map,
      pg,
      n_pages,
      meta_toks
    )

    # Titles (with per-page token resolution)
    if (length(titles) > 0L) {
      fs <- page$font_size
      for (i in seq_along(titles)) {
        t <- titles[[i]]
        t_fs <- t$font_size %||% fs
        align <- switch(
          t$align %||% "center",
          left = "\\raggedright",
          center = "\\centering",
          right = "\\raggedleft"
        )
        bold_on <- if (isTRUE(t$bold)) "\\bfseries " else ""
        content <- resolve_tokens(
          plain_titles[[i]],
          page_token_map,
          "figure title"
        )
        content <- latex_escape(content)
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
        gsub("\\\\", "/", tmp)
      )
    )

    # Footnotes (with per-page token resolution)
    if (length(footnotes) > 0L) {
      lines <- c(lines, "\\vspace{6pt}")
      fs <- page$font_size
      for (i in seq_along(footnotes)) {
        fn <- footnotes[[i]]
        fn_fs <- fn$font_size %||% fs
        align <- switch(
          fn$align %||% "left",
          left = "\\raggedright",
          center = "\\centering",
          right = "\\raggedleft"
        )
        content <- resolve_tokens(
          plain_footnotes[[i]],
          page_token_map,
          "figure footnote"
        )
        content <- latex_escape(content)
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


# ══════════════════════════════════════════════════════════════════════════════
# RTF rendering
# ══════════════════════════════════════════════════════════════════════════════

#' Render a figure spec to RTF
#'
#' Saves each plot as a temporary PNG and embeds it in RTF with titles
#' and footnotes as paragraphs. Multi-page figures emit \\sect between pages.
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

  plot_list <- figure_plot_list(spec)
  n_pages <- length(plot_list)
  dpi <- 300L
  px_w <- as.integer(plot_w * dpi)
  px_h <- as.integer(plot_h * dpi)

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
  sect_def <- paste0(
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

  fs <- pt_to_half_pt(page$font_size)

  # ── Pre-process titles/footnotes (hoist label_to_plain out of loop) ──────
  titles <- spec$meta$titles %||% list()
  footnotes <- spec$meta$footnotes %||% list()
  plain_titles <- lapply(titles, function(t) label_to_plain(t$content))
  plain_footnotes <- lapply(footnotes, function(fn) label_to_plain(fn$content))

  # Build base token map once (hoists get_source_path/get_timestamp)
  base_token_map <- build_token_map(
    page_num = 1L,
    total_pages = n_pages,
    spec = spec
  )

  # ── Per-page content ─────────────────────────────────────────────────────
  for (pg in seq_len(n_pages)) {
    if (pg == 1L) {
      rtf_write(con, sect_def)
    } else {
      rtf_write(con, paste0("\\sect\n", sect_def))
    }

    # Save plot to temp file, embed, then clean up
    tmp_plot <- tempfile(fileext = ".png")
    save_plot_to_file(plot_list[[pg]], tmp_plot, plot_w, plot_h, dpi = dpi)

    # Build per-page token map (only page number + meta differ)
    meta_toks <- meta_row_tokens(spec, pg)
    page_token_map <- figure_page_token_map(
      base_token_map,
      pg,
      n_pages,
      meta_toks
    )

    # Titles (with per-page token resolution)
    for (i in seq_along(titles)) {
      t <- titles[[i]]
      t_fs <- pt_to_half_pt(t$font_size %||% page$font_size)
      align_rtf <- fr_env$align_to_rtf[[t$align %||% "center"]]
      bold_on <- if (isTRUE(t$bold)) "\\b " else ""
      bold_off <- if (isTRUE(t$bold)) "\\b0" else ""
      content <- resolve_tokens(
        plain_titles[[i]],
        page_token_map,
        "figure title"
      )
      content <- rtf_escape(content)
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

    # Read PNG binary data for this page
    png_data <- readBin(tmp_plot, "raw", file.info(tmp_plot)$size)
    hex_data <- paste0(as.character(png_data), collapse = "")

    # Clean up temp file immediately
    unlink(tmp_plot)

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

    # Footnotes (with per-page token resolution)
    if (length(footnotes) > 0L) {
      rtf_write(con, paste0("\\pard\\plain\\fs", fs, "\\par\n"))
      for (i in seq_along(footnotes)) {
        fn <- footnotes[[i]]
        fn_fs <- pt_to_half_pt(fn$font_size %||% page$font_size)
        align_rtf <- fr_env$align_to_rtf[[fn$align %||% "left"]]
        content <- resolve_tokens(
          plain_footnotes[[i]],
          page_token_map,
          "figure footnote"
        )
        content <- rtf_escape(content)
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
  }

  rtf_write(con, "}")
}


# ══════════════════════════════════════════════════════════════════════════════
# HTML rendering
# ══════════════════════════════════════════════════════════════════════════════

#' Render a figure spec to HTML
#'
#' Saves each plot as a temporary PNG, base64-encodes it, and embeds it as a
#' `data:image/png;base64,...` URI in a self-contained HTML document with
#' titles, footnotes, and page chrome. Multi-page figures produce multiple
#' `<section>` elements.
#'
#' @param spec Finalized fr_spec with type = "figure".
#' @param path Output file path (.html/.htm).
#' @noRd
render_figure_html <- function(spec, path) {
  requireNamespace("grDevices", quietly = TRUE)

  page <- spec$page
  printable <- printable_area_inches(page)
  plot_w <- spec$figure_width %||% printable[["width"]]
  plot_h <- spec$figure_height %||% (printable[["height"]] * 0.7)
  dpi <- 300L

  plot_list <- figure_plot_list(spec)
  n_pages <- length(plot_list)

  titles <- spec$meta$titles %||% list()
  footnotes <- spec$meta$footnotes %||% list()

  base_token_map <- build_token_map(
    page_num = 1L,
    total_pages = n_pages,
    spec = spec
  )

  sections <- vector("list", n_pages)
  for (pg in seq_len(n_pages)) {
    parts <- character(0)

    # Per-page token map
    meta_toks <- meta_row_tokens(spec, pg)
    page_token_map <- figure_page_token_map(
      base_token_map,
      pg,
      n_pages,
      meta_toks
    )

    # Pagehead
    ph <- html_figure_chrome(spec$pagehead, spec, page_token_map, "page header")
    if (nzchar(ph)) {
      parts <- c(parts, ph)
    }

    # Titles
    if (length(titles) > 0L) {
      title_lines <- vapply(
        titles,
        function(t) {
          fs <- t$font_size %||% page$font_size
          align <- t$align %||% "center"
          bold <- isTRUE(t$bold)
          content <- resolve_tokens(
            label_to_plain(t$content),
            page_token_map,
            "figure title"
          )
          content <- html_escape(content)

          style_parts <- paste0("text-align:", align)
          if (fs != page$font_size) {
            style_parts <- c(style_parts, paste0("font-size:", fs, "pt"))
          }
          if (bold) {
            style_parts <- c(style_parts, "font-weight:bold")
          }
          paste0(
            "<div class=\"ar-title\" style=\"",
            paste0(style_parts, collapse = ";"),
            "\">",
            content,
            "</div>"
          )
        },
        character(1)
      )
      parts <- c(
        parts,
        paste0(
          "<div class=\"ar-titles\">\n",
          paste0(title_lines, collapse = "\n"),
          "\n</div>"
        )
      )
    }

    # Save plot to temp PNG, base64-encode, embed
    tmp_plot <- tempfile(fileext = ".png")
    save_plot_to_file(plot_list[[pg]], tmp_plot, plot_w, plot_h, dpi = dpi)
    png_raw <- readBin(tmp_plot, "raw", file.info(tmp_plot)$size)
    unlink(tmp_plot)
    b64 <- base64_encode(png_raw)

    parts <- c(
      parts,
      paste0(
        "<div style=\"text-align:center;margin:0.5em 0\">\n",
        "<img src=\"data:image/png;base64,",
        b64,
        "\" style=\"max-width:100%;width:",
        plot_w,
        "in\" alt=\"figure\">\n",
        "</div>"
      )
    )

    # Footnotes
    if (length(footnotes) > 0L) {
      fn_lines <- vapply(
        footnotes,
        function(fn) {
          fs <- fn$font_size %||% page$font_size
          align <- fn$align %||% "left"
          content <- resolve_tokens(
            label_to_plain(fn$content),
            page_token_map,
            "figure footnote"
          )
          content <- html_escape(content)

          style_parts <- character(0)
          if (align != "left") {
            style_parts <- c(style_parts, paste0("text-align:", align))
          }
          if (fs != page$font_size) {
            style_parts <- c(style_parts, paste0("font-size:", fs, "pt"))
          }
          style_str <- if (length(style_parts) > 0L) {
            paste0(" style=\"", paste0(style_parts, collapse = ";"), "\"")
          } else {
            ""
          }
          paste0(
            "<div class=\"ar-footnote\"",
            style_str,
            ">",
            content,
            "</div>"
          )
        },
        character(1)
      )

      parts <- c(
        parts,
        paste0(
          "<div class=\"ar-footnotes\">\n",
          paste0(fn_lines, collapse = "\n"),
          "\n</div>"
        )
      )
    }

    # Pagefoot (wrapped in ar-chrome-foot for bottom push)
    pf <- html_figure_chrome(
      spec$pagefoot,
      spec,
      page_token_map,
      "page footer"
    )
    if (nzchar(pf)) {
      parts <- c(
        parts,
        paste0("<div class=\"ar-chrome-foot\">\n", pf, "\n</div>")
      )
    }

    sections[[pg]] <- paste0(
      "<section class=\"ar-section\">\n",
      paste0(parts, collapse = "\n"),
      "\n</section>"
    )
  }

  body <- paste0(sections, collapse = "\n")
  doc <- html_document(body, spec, viewer = isTRUE(spec$.viewer))
  writeLines(doc, path, useBytes = TRUE)
}


#' Build page chrome div for figure HTML
#' @noRd
html_figure_chrome <- function(chrome, spec, token_map, context) {
  if (is.null(chrome)) {
    return("")
  }

  has_left <- !is.null(chrome$left)
  has_center <- !is.null(chrome$center)
  has_right <- !is.null(chrome$right)
  if (!has_left && !has_center && !has_right) {
    return("")
  }

  chrome_escape <- function(txt) {
    txt <- resolve_tokens(txt, token_map, context)
    html_escape(txt)
  }

  bold_style <- if (isTRUE(chrome$bold)) " style=\"font-weight:bold\"" else ""

  spans <- character(0)
  if (has_left) {
    spans <- c(
      spans,
      paste0(
        "<span class=\"ar-chrome-left\"",
        bold_style,
        ">",
        chrome_escape(chrome$left),
        "</span>"
      )
    )
  }
  if (has_center) {
    spans <- c(
      spans,
      paste0(
        "<span class=\"ar-chrome-center\"",
        bold_style,
        ">",
        chrome_escape(chrome$center),
        "</span>"
      )
    )
  }
  if (has_right) {
    spans <- c(
      spans,
      paste0(
        "<span class=\"ar-chrome-right\"",
        bold_style,
        ">",
        chrome_escape(chrome$right),
        "</span>"
      )
    )
  }

  paste0(
    "<div class=\"ar-chrome\">\n",
    paste0(spans, collapse = "\n"),
    "\n</div>"
  )
}


#' Base64-encode raw bytes (pure R, no external dependency)
#'
#' @param raw_bytes Raw vector to encode.
#' @return Character scalar — base64-encoded string.
#' @noRd
base64_encode <- function(raw_bytes) {
  b64_chars <- c(
    LETTERS,
    letters,
    as.character(0:9),
    "+",
    "/"
  )
  n <- length(raw_bytes)
  if (n == 0L) {
    return("")
  }

  int_vals <- as.integer(raw_bytes)

  # Pad to multiple of 3
  pad <- (3L - n %% 3L) %% 3L
  if (pad > 0L) {
    int_vals <- c(int_vals, rep(0L, pad))
  }
  n_padded <- length(int_vals)

  # Process 3 bytes at a time → 4 base64 characters
  n_groups <- n_padded %/% 3L
  out <- character(n_groups * 4L)
  oi <- 0L
  for (i in seq(1L, n_padded, by = 3L)) {
    a <- int_vals[i]
    b <- int_vals[i + 1L]
    cc <- int_vals[i + 2L]

    oi <- oi + 1L
    out[oi] <- b64_chars[bitwShiftR(a, 2L) + 1L]
    oi <- oi + 1L
    out[oi] <- b64_chars[
      bitwOr(bitwShiftL(bitwAnd(a, 3L), 4L), bitwShiftR(b, 4L)) + 1L
    ]
    oi <- oi + 1L
    out[oi] <- b64_chars[
      bitwOr(bitwShiftL(bitwAnd(b, 15L), 2L), bitwShiftR(cc, 6L)) + 1L
    ]
    oi <- oi + 1L
    out[oi] <- b64_chars[bitwAnd(cc, 63L) + 1L]
  }

  # Replace padding positions with '='
  if (pad >= 1L) {
    out[length(out)] <- "="
  }
  if (pad >= 2L) {
    out[length(out) - 1L] <- "="
  }

  paste0(out, collapse = "")
}
