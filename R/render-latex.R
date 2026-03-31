# ──────────────────────────────────────────────────────────────────────────────
# render-latex.R — LaTeX backend for fr_render()
#
# Emits a complete LaTeX document using tabularray (longtblr) with:
#   - XeLaTeX preamble (fontspec, geometry, fancyhdr, tabularray, xcolor)
#   - Page headers/footers via fancyhdr
#   - Titles via DeclareTblrTemplate (firsthead/middlehead/lasthead) — repeat
#     on every page automatically
#   - Column headers via tabularray rowhead
#   - Spanning headers via \SetCell[c=N]{c}
#   - Body rows with per-cell styling and borders
#   - Footnotes via DeclareTblrTemplate (firstfoot/middlefoot/lastfoot)
#   - \clearpage for page_by groups and column split panels
# ──────────────────────────────────────────────────────────────────────────────

#' Render an fr_spec to LaTeX (.tex)
#'
#' @param spec Finalized fr_spec object.
#' @param page_groups List of page group lists (data + group_label).
#' @param col_panels List of column name vectors (one per panel).
#' @param path Output file path (.tex).
#' @noRd
render_latex <- function(spec, page_groups, col_panels, path) {
  # Pre-allocate list of chunks to avoid O(n^2) string growth
  chunks <- vector("list", 4L + 5L * length(col_panels) * length(page_groups))
  ci <- 0L
  add_chunk <- function(x) {
    ci <<- ci + 1L
    chunks[[ci]] <<- x
  }

  add_chunk(latex_preamble(spec))
  add_chunk("\\begin{document}")
  add_chunk(latex_fancyhdr_setup(spec))

  total_sections <- length(col_panels) * length(page_groups)
  section_idx <- 0L

  for (group_idx in seq_along(page_groups)) {
    group <- page_groups[[group_idx]]

    for (panel_idx in seq_along(col_panels)) {
      panel_cols <- col_panels[[panel_idx]]
      vis_columns <- spec$columns[intersect(panel_cols, names(spec$columns))]
      section_idx <- section_idx + 1L

      if (section_idx > 1L) {
        add_chunk("\\clearpage")
      }

      is_last <- (group_idx == length(page_groups) &&
        panel_idx == length(col_panels))

      # Redefine head template per section: handles group labels and
      # continuation text (panel_idx > 1 gets continuation on all pages)
      add_chunk(latex_head_template(
        spec,
        group_label = group$group_label,
        panel_idx = panel_idx
      ))

      # Redefine foot template per section: "last" footnotes only in final section
      add_chunk(latex_foot_template(
        spec,
        is_last = is_last,
        vis_columns = vis_columns
      ))

      # Resolve borders
      nrow_header <- 1L + n_spanner_levels(spec$header$spans)
      borders <- resolve_borders(
        spec$rules,
        vctrs::vec_size(group$data),
        length(vis_columns),
        nrow_header
      )

      # Per-group header label overrides (pre-computed in prepare_pages,
      # or computed here for single-group specs without page_by)
      if (!is.null(group$label_overrides) || !is.null(group$span_overrides)) {
        label_overrides <- group$label_overrides
        span_overrides <- group$span_overrides
      } else {
        resolved <- resolve_group_labels(spec, group$data, group$group_label)
        if (is.list(resolved)) {
          label_overrides <- resolved$columns
          span_overrides <- resolved$spans
        } else {
          label_overrides <- resolved
          span_overrides <- NULL
        }
      }

      # Build cell grid
      cell_grid <- build_cell_grid(
        group$data,
        vis_columns,
        spec$cell_styles,
        spec$page
      )

      # Table (titles/footnotes are in longtblr head/foot templates)
      add_chunk(latex_table(
        spec,
        group$data,
        vis_columns,
        cell_grid,
        borders,
        label_overrides = label_overrides,
        span_overrides = span_overrides,
        orig_rows = group$orig_rows
      ))
    }
  }

  add_chunk("\\end{document}")

  # Flatten all chunks and write
  lines <- unlist(chunks[seq_len(ci)], use.names = FALSE)
  writeLines(lines, path, useBytes = FALSE)
}


# ══════════════════════════════════════════════════════════════════════════════
# LaTeX Document Components
# ══════════════════════════════════════════════════════════════════════════════

#' Build \\setmainfont command, with open-source fallback for missing fonts
#' @noRd
latex_setmainfont <- function(font_name) {
  resolved <- resolve_latex_font(font_name)
  paste0("\\setmainfont{", resolved, "}")
}


#' LaTeX preamble: documentclass, packages, geometry, fonts, templates
#' @noRd
latex_preamble <- function(spec) {
  page <- spec$page
  font_name <- page$font_family
  lf <- fr_env$latex_leading_factor

  # Paper and orientation
  paper_map <- c(letter = "letterpaper", a4 = "a4paper", legal = "legalpaper")
  paper_opt <- unname(paper_map[page$paper]) %||% "letterpaper"
  orient_opt <- if (page$orientation == "landscape") "landscape" else ""

  # Margins
  ml <- page$margins$left
  mr <- page$margins$right
  mt <- page$margins$top
  mb <- page$margins$bottom

  # Pagehead/pagefoot positioning — match RTF behavior:
  # The pagehead's LAST line sits at the bottom of the top margin (1in mark).
  # Content flows upward into the margin. A thin headsep gap separates the
  # pagehead from titles. fancyhdr baseline-aligns content at headheight
  # distance from the body top, so headheight + headsep must fit within
  # the top margin. Body (titles via longtblr head template) starts at
  # top + headsep below the original margin.
  headsep_pt <- 0

  if (!is.null(spec$pagehead)) {
    ph_fs <- spec$pagehead$font_size %||% (page$font_size - 1)
    ph_line_pt <- round(ph_fs * lf, 1)
    ph_max_lines <- max_chrome_lines(spec$pagehead)
    headheight_pt <- round(ph_max_lines * ph_line_pt, 1)
    # Hairline gap between pagehead bottom and titles (2pt)
    headsep_pt <- 2
    headheight_str <- paste0(",headheight=", headheight_pt, "pt")
    headsep_str <- paste0(",headsep=", headsep_pt, "pt")
  } else {
    headheight_str <- ",headheight=0pt"
    headsep_str <- ",headsep=0pt"
  }

  # fancyhdr requires footskip > content height (strict inequality).
  # LaTeX default is 30pt. We compute based on pagefoot content with a
  # 1pt buffer to avoid "footskip is too small" warnings from fancyhdr.
  min_footskip_pt <- round(page$font_size * lf + 1, 1)
  if (!is.null(spec$pagefoot)) {
    pf_fs <- spec$pagefoot$font_size %||% (page$font_size - 1)
    pf_line_pt <- round(pf_fs * lf, 1)
    pf_max_lines <- max_chrome_lines(spec$pagefoot)
    footskip_val <- max(
      round(pf_max_lines * pf_line_pt + 1, 1),
      min_footskip_pt
    )
    footskip_str <- paste0(",footskip=", footskip_val, "pt")
  } else {
    footskip_str <- paste0(",footskip=", min_footskip_pt, "pt")
  }

  # Expand top margin by headsep so the body (titles) starts just below
  # the original margin line, matching RTF where pagehead bottom = margin.
  # Expand bottom margin by footskip so the pagefoot sits at the margin edge.
  top_in <- mt + headsep_pt / fr_env$points_per_inch
  bot_in <- if (!is.null(spec$pagefoot)) {
    mb + footskip_val / fr_env$points_per_inch
  } else {
    mb + min_footskip_pt / fr_env$points_per_inch
  }

  geom_opts <- paste0(
    paper_opt,
    if (nzchar(orient_opt)) paste0(",", orient_opt) else "",
    ",left=",
    ml,
    "in",
    ",right=",
    mr,
    "in",
    ",top=",
    round(top_in, 4),
    "in",
    ",bottom=",
    round(bot_in, 4),
    "in",
    headheight_str,
    headsep_str,
    footskip_str
  )

  lines <- c(
    "\\documentclass[]{article}",
    "",
    "% Encoding and fonts",
    "\\usepackage{fontspec}",
    latex_setmainfont(font_name),
    "",
    "% Page layout",
    paste0("\\usepackage[", geom_opts, "]{geometry}"),
    "\\setlength{\\parskip}{0pt}",
    "\\tolerance=200",
    "\\emergencystretch=1em",
    "",
    "% Tables",
    "\\usepackage{tabularray}",
    "\\UseTblrLibrary{booktabs}",
    "\\DefTblrTemplate{caption}{default}{}",
    "\\DefTblrTemplate{capcont}{default}{}"
  )

  # Title templates (repeat on every page via longtblr head templates)
  # Foot templates are declared per-section in the render loop (is_last-aware)
  lines <- c(lines, latex_head_template(spec))

  lines <- c(lines, "", "% Colors", "\\usepackage[table]{xcolor}")

  # Collect ALL colors used: borders, header styling, cell styles
  all_colors <- collect_colors(spec)
  for (hex in all_colors) {
    cname <- hex_to_tblr_color(hex)
    lines <- c(
      lines,
      paste0(
        "\\definecolor{",
        cname,
        "}{HTML}{",
        toupper(sub("^#", "", hex)),
        "}"
      )
    )
  }

  # lastpage package (needed when {total_pages} token is used in pagehead/pagefoot)
  needs_lastpage <- FALSE
  for (chrome in list(spec$pagehead, spec$pagefoot)) {
    if (!is.null(chrome)) {
      txt <- paste0(c(chrome$left, chrome$center, chrome$right), collapse = "")
      if (grepl("total_pages", txt, fixed = TRUE)) needs_lastpage <- TRUE
    }
  }

  lines <- c(
    lines,
    "",
    "% Headers and footers",
    "\\usepackage{fancyhdr}",
    if (needs_lastpage) "\\usepackage{lastpage}",
    "\\pagestyle{fancy}",
    "",
    "% Font size",
    paste0(
      "\\renewcommand{\\normalsize}{\\fontsize{",
      page$font_size,
      "}{",
      round(page$font_size * lf, 1),
      "}\\selectfont}"
    ),
    "\\normalsize",
    ""
  )

  lines
}


#' Setup fancyhdr page headers and footers
#' @noRd
latex_fancyhdr_setup <- function(spec) {
  lines <- character(0)

  # Clear defaults
  lines <- c(lines, "\\fancyhf{}")
  lines <- c(lines, "\\renewcommand{\\headrulewidth}{0pt}")
  lines <- c(lines, "\\renewcommand{\\footrulewidth}{0pt}")

  token_map <- build_token_map(
    page_num = "\\thepage{}",
    total_pages = "\\pageref{LastPage}",
    spec = spec
  )

  # Escape plain-text token values (program, datetime) for LaTeX.
  # Readonly tokens (thepage, total_pages) are LaTeX commands and must NOT
  # be escaped. User custom tokens are also escaped for safety.
  latex_safe_keys <- setdiff(names(token_map), c("thepage", "total_pages"))
  for (k in latex_safe_keys) {
    val <- token_map[[k]]
    if (!is.na(val) && nzchar(val)) {
      # Normalize Windows backslashes to forward slashes (safe in LaTeX text,
      # avoids \textbackslash{} double-escape issue with latex_escape)
      val <- gsub("\\\\", "/", val)
      token_map[[k]] <- latex_escape(val)
    }
  }

  # Escape literal text first, then resolve tokens (pre-escaped above)
  # and sentinels (produce LaTeX commands). This ensures underscores and
  # other LaTeX specials in user text are safe, while token values and
  # sentinel output pass through unescaped.
  latex_chrome_text <- function(
    raw_text,
    token_map,
    context,
    align_key = "l",
    max_lines = 1L
  ) {
    # Escape literal text, preserving {token} placeholders and \n
    escaped <- latex_escape_chrome(raw_text)
    txt <- resolve_tokens(escaped, token_map, context)
    # Resolve inline markup sentinels only (not full escape)
    if (has_sentinel(txt)) {
      pattern <- fr_env$sentinel_pattern
      m <- gregexpr(pattern, txt, perl = TRUE)
      sentinels <- regmatches(txt, m)[[1L]]
      for (s in sentinels) {
        tok_parts <- regmatches(s, regexec(pattern, s, perl = TRUE))[[1L]]
        resolved <- latex_sentinel_resolver(tok_parts[[2L]], tok_parts[[3L]])
        txt <- sub(s, resolved, txt, fixed = TRUE)
      }
    }
    # When ANY zone in the chrome has multiple lines, ALL zones must use
    # \shortstack padded to the same height. fancyhdr baseline-aligns content
    # at the bottom of headheight; padding ensures single-line zones align
    # with the first line of multi-line zones (top-alignment).
    parts <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
    if (max_lines > 1L) {
      # Pad with phantom lines so all zones have equal height
      n_pad <- max_lines - length(parts)
      if (n_pad > 0L) {
        parts <- c(parts, rep("\\phantom{Xg}", n_pad))
      }
      txt <- paste0(
        "\\shortstack[",
        align_key,
        "]{",
        paste0(parts, collapse = "\\\\"),
        "}"
      )
    }
    txt
  }

  # Page header
  if (!is.null(spec$pagehead)) {
    chrome <- spec$pagehead
    fs <- chrome$font_size %||% (spec$page$font_size - 1)
    bold_on <- if (isTRUE(chrome$bold)) "\\textbf{" else ""
    bold_off <- if (isTRUE(chrome$bold)) "}" else ""
    ph_ml <- max_chrome_lines(chrome)

    if (!is.null(chrome$left)) {
      txt <- latex_chrome_text(
        chrome$left,
        token_map,
        "page header",
        align_key = "l",
        max_lines = ph_ml
      )
      lines <- c(
        lines,
        paste0(
          "\\fancyhead[L]{\\fontsize{",
          fs,
          "}{",
          round(fs * fr_env$latex_leading_factor, 1),
          "}\\selectfont ",
          bold_on,
          txt,
          bold_off,
          "}"
        )
      )
    }
    if (!is.null(chrome$center)) {
      txt <- latex_chrome_text(
        chrome$center,
        token_map,
        "page header",
        align_key = "c",
        max_lines = ph_ml
      )
      lines <- c(
        lines,
        paste0(
          "\\fancyhead[C]{\\fontsize{",
          fs,
          "}{",
          round(fs * fr_env$latex_leading_factor, 1),
          "}\\selectfont ",
          bold_on,
          txt,
          bold_off,
          "}"
        )
      )
    }
    if (!is.null(chrome$right)) {
      txt <- latex_chrome_text(
        chrome$right,
        token_map,
        "page header",
        align_key = "r",
        max_lines = ph_ml
      )
      lines <- c(
        lines,
        paste0(
          "\\fancyhead[R]{\\fontsize{",
          fs,
          "}{",
          round(fs * fr_env$latex_leading_factor, 1),
          "}\\selectfont ",
          bold_on,
          txt,
          bold_off,
          "}"
        )
      )
    }
  }

  # Page footer: only pagefoot goes to \fancyfoot (footnotes are body-area
  # content via tabularray foot templates)
  if (!is.null(spec$pagefoot)) {
    chrome <- spec$pagefoot
    fs <- chrome$font_size %||% (spec$page$font_size - 1)
    bold_on <- if (isTRUE(chrome$bold)) "\\textbf{" else ""
    bold_off <- if (isTRUE(chrome$bold)) "}" else ""
    pf_ml <- max_chrome_lines(chrome)

    if (!is.null(chrome$left)) {
      txt <- latex_chrome_text(
        chrome$left,
        token_map,
        "page footer",
        align_key = "l",
        max_lines = pf_ml
      )
      lines <- c(
        lines,
        paste0(
          "\\fancyfoot[L]{\\fontsize{",
          fs,
          "}{",
          round(fs * fr_env$latex_leading_factor, 1),
          "}\\selectfont ",
          bold_on,
          txt,
          bold_off,
          "}"
        )
      )
    }
    if (!is.null(chrome$center)) {
      txt <- latex_chrome_text(
        chrome$center,
        token_map,
        "page footer",
        align_key = "c",
        max_lines = pf_ml
      )
      lines <- c(
        lines,
        paste0(
          "\\fancyfoot[C]{\\fontsize{",
          fs,
          "}{",
          round(fs * fr_env$latex_leading_factor, 1),
          "}\\selectfont ",
          bold_on,
          txt,
          bold_off,
          "}"
        )
      )
    }
    if (!is.null(chrome$right)) {
      txt <- latex_chrome_text(
        chrome$right,
        token_map,
        "page footer",
        align_key = "r",
        max_lines = pf_ml
      )
      lines <- c(
        lines,
        paste0(
          "\\fancyfoot[R]{\\fontsize{",
          fs,
          "}{",
          round(fs * fr_env$latex_leading_factor, 1),
          "}\\selectfont ",
          bold_on,
          txt,
          bold_off,
          "}"
        )
      )
    }
  }

  c(lines, "")
}


#' Build title content lines for embedding in tabularray head template
#'
#' @param spec Finalized fr_spec.
#' @param continuation Logical. If TRUE, append continuation text to first title.
#' @noRd
latex_title_content <- function(spec, continuation = FALSE) {
  titles <- spec$meta$titles
  if (length(titles) == 0L) {
    return(character(0))
  }
  lf <- fr_env$latex_leading_factor
  cont_text <- spec$page$continuation

  lines <- character(0)
  for (idx in seq_along(titles)) {
    entry <- titles[[idx]]
    fs <- entry$font_size %||%
      spec$meta$title_font_size %||%
      spec$page$font_size
    align <- entry$align %||% spec$meta$title_align %||% "center"
    entry_bold <- entry$bold %||% spec$meta$title_bold
    bold_on <- if (isTRUE(entry_bold)) "\\textbf{" else ""
    bold_off <- if (isTRUE(entry_bold)) "}" else ""
    content <- latex_escape_and_resolve(entry$content)

    # Append continuation text to first title when requested
    if (idx == 1L && isTRUE(continuation) && !is.null(cont_text)) {
      content <- paste0(content, " ", latex_escape(cont_text))
    }

    tex_align <- switch(
      align,
      center = "\\centering",
      right = "\\raggedleft",
      ""
    )

    if (nzchar(tex_align)) {
      lines <- c(
        lines,
        paste0(
          "{",
          tex_align,
          "\\fontsize{",
          fs,
          "}{",
          round(fs * lf, 1),
          "}\\selectfont ",
          bold_on,
          content,
          bold_off,
          "\\par}"
        )
      )
    } else {
      lines <- c(
        lines,
        paste0(
          "{\\fontsize{",
          fs,
          "}{",
          round(fs * lf, 1),
          "}\\selectfont ",
          bold_on,
          content,
          bold_off,
          "\\par}"
        )
      )
    }
  }

  # Spacing after titles
  n_after <- spec$spacing$titles_after %||% 1L
  if (n_after > 0L) {
    vsp <- round(n_after * spec$page$font_size * 0.5, 1)
    lines <- c(lines, paste0("\\vspace{", vsp, "pt}"))
  }

  lines
}


#' Build \\DeclareTblrTemplate for title heads (firsthead/middlehead/lasthead)
#'
#' Supports continuation text across panels and within multi-page panels:
#' - **Panel 1**: firsthead = no continuation; middlehead/lasthead = with
#'   continuation (for within-panel page breaks).
#' - **Panel 2+**: ALL head templates = with continuation.
#'
#' @param spec Finalized fr_spec.
#' @param group_label Page-by group label or NULL.
#' @param panel_idx Panel index (1-based). Default 1L.
#' @noRd
latex_head_template <- function(spec, group_label = NULL, panel_idx = 1L) {
  # Build title content without and with continuation
  base_content <- latex_title_content(spec, continuation = FALSE)
  cont_content <- if (!is.null(spec$page$continuation)) {
    latex_title_content(spec, continuation = TRUE)
  } else {
    base_content
  }

  # Append page_by group label to both variants
  gl_line <- NULL
  if (
    !is.null(group_label) &&
      nzchar(group_label) &&
      isTRUE(spec$body$page_by_visible %||% TRUE)
  ) {
    lf <- fr_env$latex_leading_factor
    pb_style <- resolve_page_by_style(spec$page_by_styles %||% list())
    fs <- pb_style$font_size %||% spec$page$font_size
    pb_bold_on <- if (isTRUE(pb_style$bold)) "\\textbf{" else ""
    pb_bold_off <- if (isTRUE(pb_style$bold)) "}" else ""
    pb_italic_on <- if (isTRUE(pb_style$italic)) "\\textit{" else ""
    pb_italic_off <- if (isTRUE(pb_style$italic)) "}" else ""
    pb_align <- pb_style$align %||% "left"
    if (pb_align == "left") {
      gl_line <- paste0(
        "\\noindent{\\fontsize{",
        fs,
        "}{",
        round(fs * lf, 1),
        "}\\selectfont ",
        pb_bold_on,
        pb_italic_on,
        latex_escape(group_label),
        pb_italic_off,
        pb_bold_off,
        "}\\par"
      )
    } else {
      tex_align <- if (pb_align == "center") "\\centering" else "\\raggedleft"
      gl_line <- paste0(
        "{",
        tex_align,
        "\\fontsize{",
        fs,
        "}{",
        round(fs * lf, 1),
        "}\\selectfont ",
        pb_bold_on,
        pb_italic_on,
        latex_escape(group_label),
        pb_italic_off,
        pb_bold_off,
        "\\par}"
      )
    }
    n_gl <- spec$spacing$page_by_after %||% 1L
    if (n_gl > 0L) {
      vsp <- round(n_gl * fs * 0.5, 1)
      gl_line <- paste0(gl_line, "\n\\vspace{", vsp, "pt}")
    }
    base_content <- c(base_content, gl_line)
    cont_content <- c(cont_content, gl_line)
  }

  if (length(base_content) == 0L && length(cont_content) == 0L) {
    return(c(
      "\\DefTblrTemplate{conthead}{default}{}",
      "\\DefTblrTemplate{contfoot}{default}{}"
    ))
  }

  # Decide which content goes to firsthead vs middlehead/lasthead
  rest_body <- paste0(cont_content, collapse = "\n")
  first_body <- if (panel_idx == 1L) {
    paste0(base_content, collapse = "\n")
  } else {
    rest_body
  }

  c(
    "\\DeclareTblrTemplate{firsthead}{default}{",
    first_body,
    "}",
    "\\DeclareTblrTemplate{middlehead, lasthead}{default}{",
    rest_body,
    "}",
    "\\DefTblrTemplate{conthead}{default}{}",
    "\\DefTblrTemplate{contfoot}{default}{}"
  )
}


#' Build \DeclareTblrTemplate for footnote feet
#'
#' All footnotes are rendered as body-area content via tabularray foot templates:
#' - `placement = "every"` → firstfoot, middlefoot, lastfoot (repeats every page)
#' - `placement = "last"` → lastfoot only (appended after "every" footnotes)
#'
#' @param spec Finalized fr_spec object.
#' @param is_last Logical. If FALSE, "last" footnotes are suppressed so they
#'   don't appear in non-final sections of multi-section tables.
#' @noRd
latex_foot_template <- function(spec, is_last = TRUE, vis_columns = NULL) {
  footnotes <- spec$meta$footnotes %||% list()
  fn_split <- split_footnotes(footnotes)
  every_fns <- fn_split$every
  last_fns <- if (is_last) fn_split$last else list()

  has_every <- length(every_fns) > 0L
  has_last <- length(last_fns) > 0L

  lines <- character(0)

  if (has_every) {
    # "every" footnotes repeat on all pages (firstfoot, middlefoot, lastfoot)
    every_content <- paste0(
      build_fn_latex_content(every_fns, spec, vis_columns = vis_columns),
      collapse = "\n"
    )

    # firstfoot and middlefoot get "every" footnotes only
    lines <- c(
      lines,
      paste0("\\DeclareTblrTemplate{firstfoot, middlefoot}{default}{"),
      every_content,
      "}"
    )

    if (has_last) {
      # lastfoot gets "every" + "last" footnotes combined
      last_content <- paste0(
        build_fn_latex_content(
          last_fns,
          spec,
          skip_spacing = TRUE,
          vis_columns = vis_columns
        ),
        collapse = "\n"
      )
      lines <- c(
        lines,
        paste0("\\DeclareTblrTemplate{lastfoot}{default}{"),
        every_content,
        last_content,
        "}"
      )
    } else {
      # lastfoot same as firstfoot/middlefoot
      lines <- c(
        lines,
        paste0("\\DeclareTblrTemplate{lastfoot}{default}{"),
        every_content,
        "}"
      )
    }
  } else if (has_last) {
    # Only "last" footnotes — suppress firstfoot/middlefoot
    lines <- c(
      lines,
      "\\DefTblrTemplate{firstfoot}{default}{}",
      "\\DefTblrTemplate{middlefoot}{default}{}"
    )
    last_content <- paste0(
      build_fn_latex_content(last_fns, spec, vis_columns = vis_columns),
      collapse = "\n"
    )
    lines <- c(
      lines,
      paste0("\\DeclareTblrTemplate{lastfoot}{default}{"),
      last_content,
      "}"
    )
  } else {
    # No footnotes — suppress all foot templates
    lines <- c(
      lines,
      "\\DefTblrTemplate{firstfoot}{default}{}",
      "\\DefTblrTemplate{middlefoot}{default}{}",
      "\\DefTblrTemplate{lastfoot}{default}{}"
    )
  }

  lines
}

#' Build LaTeX footnote content lines for tabularray foot templates
#'
#' Wraps all footnote content (separator + text) in a minipage matching the
#' rendered table width, so the separator line and text align with the table
#' columns rather than extending to `\\linewidth` (the full page text width).
#'
#' @param entries List of footnote entries.
#' @param spec The fr_spec.
#' @param skip_spacing If TRUE, skip the \\vspace and separator line (used when
#'   appending "last" footnotes after "every" in the same template).
#' @param vis_columns Named list of visible panel columns (used to calculate
#'   the actual rendered table width). Falls back to all `spec$columns` if NULL.
#' @noRd
build_fn_latex_content <- function(
  entries,
  spec,
  skip_spacing = FALSE,
  vis_columns = NULL
) {
  if (length(entries) == 0L) {
    return(character(0))
  }
  lf <- fr_env$latex_leading_factor

  # Table width from visible panel columns (matches actual rendered table)
  cols <- vis_columns %||% spec$columns
  tbl_w <- round(sum(vapply(cols, function(c) c$width, numeric(1))), 4)

  fn_lines <- character(0)

  if (!skip_spacing) {
    # Spacing before footnotes (blank lines) — outside minipage
    n_before <- spec$spacing$footnotes_before %||% 1L
    if (n_before > 0L) {
      vsp <- round(n_before * spec$page$font_size * 0.5, 1)
      fn_lines <- c(fn_lines, paste0("\\vspace{", vsp, "pt}"))
    }
  }

  # Open minipage constrained to table width
  fn_lines <- c(fn_lines, paste0("\\begin{minipage}{", tbl_w, "in}"))

  if (!skip_spacing) {
    # Separator line matching table width
    if (isTRUE(spec$meta$footnote_separator)) {
      fn_lines <- c(
        fn_lines,
        paste0(
          "\\noindent\\rule{\\linewidth}{",
          fr_env$latex_fn_sep_width_pt,
          "pt}"
        )
      )
    }
  }

  for (fn in entries) {
    fs <- fn$font_size %||% spec$page$font_size
    align <- fn$align %||% "left"
    content <- latex_escape_and_resolve(fn$content)

    tex_align <- switch(
      align,
      center = "\\centering",
      right = "\\raggedleft",
      ""
    )

    if (nzchar(tex_align)) {
      fn_lines <- c(
        fn_lines,
        paste0(
          "{",
          tex_align,
          "\\fontsize{",
          fs,
          "}{",
          round(fs * lf, 1),
          "}\\selectfont ",
          content,
          "\\par}"
        )
      )
    } else {
      fn_lines <- c(
        fn_lines,
        paste0(
          "\\noindent{\\fontsize{",
          fs,
          "}{",
          round(fs * lf, 1),
          "}\\selectfont ",
          content,
          "}\\par"
        )
      )
    }
  }

  # Close minipage
  fn_lines <- c(fn_lines, "\\end{minipage}")

  fn_lines
}


#' Build the complete tabularray table
#' @noRd
latex_table <- function(
  spec,
  data,
  columns,
  cell_grid,
  borders,
  label_overrides = NULL,
  span_overrides = NULL,
  orig_rows = NULL
) {
  col_names <- names(columns)
  nc <- length(col_names)
  nr <- vctrs::vec_size(data)

  # Build column spec for tabularray
  # Subtract leftsep + rightsep per column so total rendered width matches
  # the intended table width (tabularray adds colsep outside wd=)
  colsep_pt <- spec$page$col_gap / 2
  colsep_in <- 2 * colsep_pt / fr_env$points_per_inch
  gap_col_indices <- integer(0)
  col_spec_parts <- vapply(
    seq_along(col_names),
    function(j) {
      col <- columns[[col_names[j]]]
      align <- fr_env$align_to_latex[col$align %||% "left"]
      if (isTRUE(col$is_gap)) {
        # Gap columns use exact width, no colsep subtraction (sep zeroed below)
        gap_col_indices[length(gap_col_indices) + 1L] <<- j
        width_in <- col$width
      } else {
        width_in <- max(0.1, col$width - colsep_in)
      }
      paste0("Q[", tolower(align), ",wd=", round(width_in, 4), "in]")
    },
    character(1)
  )
  colspec_str <- paste0(col_spec_parts, collapse = "")

  # Number of header rows (spanners + column header)
  nrow_header <- 1L + n_spanner_levels(spec$header$spans)

  # Collect cell/row/column style specs for tabularray inner keys
  inner_keys <- character(0)

  # rowhead: how many header rows repeat on each page
  inner_keys <- c(inner_keys, paste0("rowhead = ", nrow_header))

  # Zero out colsep for gap columns so only the narrow width remains
  for (gi in gap_col_indices) {
    inner_keys <- c(
      inner_keys,
      paste0("column{", gi, "}={leftsep=0pt,rightsep=0pt}")
    )
  }

  # Column widths already in colspec; add row-level styles
  row_heights <- build_row_heights(nr, spec$cell_styles)

  # Hlines from borders
  inner_keys <- c(inner_keys, latex_border_specs(borders, nr, nc, nrow_header))

  # Cell-level styles (bold, italic, background, color) from cell_grid
  inner_keys <- c(
    inner_keys,
    latex_cell_style_specs(cell_grid, nr, nc, nrow_header)
  )

  # Header cell styles
  header_valign <- spec$header$valign %||% "bottom"
  hgrid <- build_header_cell_grid(
    columns,
    spec$cell_styles,
    spec$page,
    nrow_header,
    default_valign = header_valign,
    header_cfg = spec$header
  )
  inner_keys <- c(
    inner_keys,
    latex_header_style_specs(
      hgrid,
      nrow_header,
      nc,
      columns = columns,
      header_default_align = spec$header$align
    )
  )

  # Build table option string
  inner_str <- paste0(inner_keys, collapse = ",\n  ")

  lines <- character(0)
  # Header row vertical alignment (bottom = match RTF multiline header style)
  header_valign_key <- paste0(
    "row{1-",
    nrow_header,
    "}={valign=b}"
  )

  # Spanner rows (compute early to get hline keys for inner spec)
  spanner_result <- latex_spanner_rows(
    spec,
    columns,
    span_overrides = span_overrides
  )
  if (length(spanner_result$hlines) > 0L) {
    inner_keys <- c(inner_keys, spanner_result$hlines)
    inner_str <- paste0(inner_keys, collapse = ",\n  ")
  }

  lines <- c(
    lines,
    paste0(
      "\\begin{longtblr}[presep=0pt, postsep=0pt]{",
      "\n  colspec={",
      colspec_str,
      "},",
      "\n  row{1-Z}={abovesep=",
      fr_env$latex_rowsep,
      ",belowsep=",
      fr_env$latex_rowsep,
      "},",
      "\n  column{1-Z}={leftsep=",
      colsep_pt,
      "pt,rightsep=",
      colsep_pt,
      "pt},",
      "\n  ",
      header_valign_key,
      ",",
      "\n  ",
      inner_str,
      "\n}"
    )
  )

  # Spanner rows
  lines <- c(lines, spanner_result$rows)

  # Column header row
  lines <- c(
    lines,
    latex_col_header_row(spec, columns, label_overrides = label_overrides)
  )

  # Keep-together mask for orphan/widow control
  # Groups that fit on one page are kept entirely together
  # Disabled when group_keep = FALSE (visual-only grouping).
  if (isTRUE(spec$body$group_keep %||% TRUE)) {
    one_row <- row_height_twips(spec$page$font_size)
    page_budget <- compute_page_budget(spec)
    page_rows <- as.integer(page_budget / one_row)
    keep_mask <- build_keep_mask(
      data,
      spec$body$group_by,
      orphan_min = spec$page$orphan_min %||% fr_env$default_orphan_min,
      widow_min = spec$page$widow_min %||% fr_env$default_widow_min,
      page_rows = page_rows
    )
  } else {
    keep_mask <- rep(FALSE, vctrs::vec_size(data))
  }

  # Body rows (uses pre-computed decimal geometry from spec)
  lines <- c(
    lines,
    latex_body_rows(
      data,
      columns,
      cell_grid,
      dec_geom = spec$decimal_geometry,
      keep_mask = keep_mask,
      orig_rows = orig_rows
    )
  )

  lines <- c(lines, "\\end{longtblr}")

  lines
}


#' Convert hex color to tabularray color name (matching definecolor in preamble)
#' @noRd
hex_to_tblr_color <- function(hex) {
  paste0("tblr", toupper(sub("^#", "", hex)))
}


#' Generate tabularray hline/vline specs from border matrices
#' @noRd
latex_border_specs <- function(borders, nr, nc, nrow_header) {
  specs <- character(0)

  # Process horizontal borders from header
  h <- borders$header
  for (i in seq_len(nrow_header)) {
    for (j in seq_len(nc)) {
      bs <- h$top[i, j][[1L]]
      if (!is.null(bs) && i == 1L && j == 1L) {
        ls <- fr_env$linestyle_latex[bs$linestyle] %||% "solid"
        cname <- hex_to_tblr_color(bs$fg)
        wd <- if (!is.null(bs$width)) paste0("wd=", bs$width, "pt, ") else ""
        specs <- c(
          specs,
          paste0(
            "hline{1} = {",
            wd,
            "dash=",
            ls,
            ", fg=",
            cname,
            "}"
          )
        )
      }
      bs <- h$bottom[i, j][[1L]]
      if (!is.null(bs) && i == nrow_header && j == 1L) {
        ls <- fr_env$linestyle_latex[bs$linestyle] %||% "solid"
        cname <- hex_to_tblr_color(bs$fg)
        wd <- if (!is.null(bs$width)) paste0("wd=", bs$width, "pt, ") else ""
        specs <- c(
          specs,
          paste0(
            "hline{",
            nrow_header + 1L,
            "} = {",
            wd,
            "dash=",
            ls,
            ", fg=",
            cname,
            "}"
          )
        )
      }
    }
  }

  # Process horizontal borders from body
  b <- borders$body
  if (nr > 0L) {
    for (i in seq_len(nr)) {
      # Check first column for bottom border (representative)
      bs <- b$bottom[i, 1L][[1L]]
      if (!is.null(bs)) {
        ls <- fr_env$linestyle_latex[bs$linestyle] %||% "solid"
        cname <- hex_to_tblr_color(bs$fg)
        wd <- if (!is.null(bs$width)) paste0("wd=", bs$width, "pt, ") else ""
        tblr_row <- nrow_header + i + 1L
        specs <- c(
          specs,
          paste0(
            "hline{",
            tblr_row,
            "} = {",
            wd,
            "dash=",
            ls,
            ", fg=",
            cname,
            "}"
          )
        )
      }
      # Top border on first body row
      if (i == 1L) {
        bs_top <- b$top[1L, 1L][[1L]]
        if (!is.null(bs_top)) {
          ls <- fr_env$linestyle_latex[bs_top$linestyle] %||% "solid"
          cname <- hex_to_tblr_color(bs_top$fg)
          wd <- if (!is.null(bs_top$width)) {
            paste0("wd=", bs_top$width, "pt, ")
          } else {
            ""
          }
          tblr_row <- nrow_header + 1L
          # Only add if not already covered by header bottom
          if (!any(grepl(paste0("^hline\\{", tblr_row, "\\}"), specs))) {
            specs <- c(
              specs,
              paste0(
                "hline{",
                tblr_row,
                "} = {",
                wd,
                "dash=",
                ls,
                ", fg=",
                cname,
                "}"
              )
            )
          }
        }
      }
    }
  }

  # Vertical borders — check left edge, right edge, and inner gaps
  for (j in seq_len(nc)) {
    # Left border from header (representative: row 1)
    if (j == 1L) {
      bs <- h$left[1L, 1L][[1L]]
      if (!is.null(bs)) {
        ls <- fr_env$linestyle_latex[bs$linestyle] %||% "solid"
        specs <- c(specs, paste0("vline{1} = {dash=", ls, "}"))
      }
    }
    # Right border
    bs <- h$right[1L, j][[1L]]
    if (!is.null(bs)) {
      ls <- fr_env$linestyle_latex[bs$linestyle] %||% "solid"
      specs <- c(specs, paste0("vline{", j + 1L, "} = {dash=", ls, "}"))
    }
  }

  unique(specs)
}


#' Generate tabularray cell style specs from cell_grid
#' @noRd
latex_cell_style_specs <- function(cell_grid, nr, nc, nrow_header) {
  if (vctrs::vec_size(cell_grid) == 0L) {
    return(character(0))
  }
  n_cells <- vctrs::vec_size(cell_grid)
  spec_list <- vector("list", n_cells)
  idx <- 0L

  for (k in seq_len(n_cells)) {
    g <- vctrs::vec_slice(cell_grid, k)
    parts <- character(0)

    if (isTRUE(g$bold)) {
      parts <- c(parts, "font=\\bfseries")
    }
    if (isTRUE(g$italic)) {
      parts <- c(parts, "font=\\itshape")
    }
    if (!is.na(g$background) && nzchar(g$background)) {
      parts <- c(parts, paste0("bg=", hex_to_tblr_color(g$background)))
    }
    if (!is.na(g$color) && g$color != "#000000") {
      parts <- c(parts, paste0("fg=", hex_to_tblr_color(g$color)))
    }
    # Note: indent is handled in latex_body_rows() via \hspace in content,
    # not via tabularray preto= (which is unreliable with Q[l,wd=X] columns)

    if (length(parts) > 0L) {
      # tabularray row index: header rows + body row index
      tblr_row <- nrow_header + g$row_idx
      idx <- idx + 1L
      spec_list[[idx]] <- paste0(
        "cell{",
        tblr_row,
        "}{",
        g$col_idx,
        "} = {",
        paste0(parts, collapse = ", "),
        "}"
      )
    }
  }

  if (idx == 0L) character(0) else unlist(spec_list[seq_len(idx)])
}


#' Generate tabularray cell style specs for header grid
#' @noRd
latex_header_style_specs <- function(
  hgrid,
  header_row_idx,
  nc,
  columns = NULL,
  header_default_align = NULL
) {
  spec_list <- vector("list", nc)
  idx <- 0L

  for (j in seq_len(nc)) {
    g <- vctrs::vec_slice(hgrid, j)
    parts <- character(0)

    if (isTRUE(g$bold)) {
      parts <- c(parts, "font=\\bfseries")
    }
    if (isTRUE(g$italic)) {
      parts <- c(parts, "font=\\itshape")
    }
    if (!is.na(g$background) && nzchar(g$background)) {
      parts <- c(parts, paste0("bg=", hex_to_tblr_color(g$background)))
    }
    if (!is.na(g$color) && g$color != "#000000") {
      parts <- c(parts, paste0("fg=", hex_to_tblr_color(g$color)))
    }

    # Header alignment override: per-column header_align or global header align
    col <- if (!is.null(columns)) columns[[j]] else NULL
    header_align <- col$header_align %||% header_default_align
    body_align <- col$align %||% "left"
    if (!is.null(header_align) && header_align != body_align) {
      latex_a <- tolower(fr_env$align_to_latex[header_align])
      parts <- c(parts, paste0("halign=", latex_a))
    }

    if (length(parts) > 0L) {
      idx <- idx + 1L
      spec_list[[idx]] <- paste0(
        "cell{",
        header_row_idx,
        "}{",
        j,
        "} = {",
        paste0(parts, collapse = ", "),
        "}"
      )
    }
  }

  if (idx == 0L) character(0) else unlist(spec_list[seq_len(idx)])
}


#' LaTeX spanner (spanning header) rows
#'
#' Renders spanning column header rows (fr_span objects) as tabularray
#' `\SetCell[c=N]{c}` cells. Newlines (`\n`) in spanner labels are converted
#' to `\shortstack{...}` so multi-line spanner labels render correctly.
#' @noRd
latex_spanner_rows <- function(spec, columns, span_overrides = NULL) {
  spans <- spec$header$spans
  if (length(spans) == 0L) {
    return(list(rows = character(0), hlines = character(0)))
  }

  col_names <- names(columns)
  nc <- length(col_names)

  levels <- sort(unique(vapply(spans, function(s) s$level, integer(1))))

  lines <- character(0)
  span_hlines <- character(0)
  lvl_row <- 0L
  for (lvl in rev(levels)) {
    lvl_row <- lvl_row + 1L
    lvl_spans <- Filter(function(s) s$level == lvl, spans)

    cells <- character(0)
    j <- 1L

    while (j <= nc) {
      matching_span <- NULL
      for (sp in lvl_spans) {
        sp_cols <- intersect(sp$columns, col_names)
        if (length(sp_cols) > 0L && col_names[j] == sp_cols[1L]) {
          matching_span <- sp
          break
        }
      }

      if (!is.null(matching_span)) {
        sp_cols <- intersect(matching_span$columns, col_names)
        span_width <- length(sp_cols)
        span_label <- matching_span$label
        if (!is.null(span_overrides)) {
          ov <- span_overrides[matching_span$label]
          if (!is.na(ov)) span_label <- ov
        }
        content <- latex_escape_and_resolve(span_label)
        if (grepl("\n", content, fixed = TRUE)) {
          inner <- newline_to_latex_break(content)
          content <- paste0("\\shortstack{", inner, "}")
        }
        cells <- c(
          cells,
          paste0(
            "\\SetCell[c=",
            span_width,
            "]{c} ",
            content
          )
        )
        # Empty cells for the rest of the span
        if (span_width > 1L) {
          cells <- c(cells, rep("", span_width - 1L))
        }
        # Track hline for this span (gap columns provide visual separation)
        if (isTRUE(matching_span$hline)) {
          col_start <- j
          col_end <- j + span_width - 1L
          span_hlines <- c(
            span_hlines,
            paste0(
              "hline{",
              lvl_row + 1L,
              "}={",
              col_start,
              "-",
              col_end,
              "}{wd=0.5pt, dash=solid, fg=tblr000000}"
            )
          )
        }
        j <- j + span_width
      } else {
        cells <- c(cells, "")
        j <- j + 1L
      }
    }

    lines <- c(lines, paste0(paste0(cells, collapse = " & "), " \\\\"))
  }

  list(rows = lines, hlines = span_hlines)
}


#' LaTeX column header row
#' @noRd
latex_col_header_row <- function(spec, columns, label_overrides = NULL) {
  col_names <- names(columns)
  nc <- length(col_names)
  header_default_align <- spec$header$align

  cells <- character(nc)
  for (j in seq_len(nc)) {
    # Use per-group override if available, else fall back to column label
    label <- label_overrides[col_names[j]]
    if (is.na(label) || is.null(label)) {
      label <- columns[[j]]$label
      if (is.null(label) && !isTRUE(columns[[j]]$is_gap)) {
        label <- col_names[j]
      }
    }
    content <- latex_escape_and_resolve(label %||% "")

    # Header alignment: tabularray's Q[l,wd=X] wraps cell content in a
    # parbox with \raggedright, so \newline inside that parbox does not
    # respect the column's declared alignment for all lines.
    # Fix: wrap ALL multiline content in a nested \parbox with the
    # effective alignment, using \\ for line breaks (safe inside \parbox).
    header_align <- columns[[j]]$header_align %||% header_default_align
    body_align <- columns[[j]]$align %||% "left"
    effective_align <- header_align %||% body_align
    has_newline <- grepl("\n", content, fixed = TRUE)

    if (has_newline) {
      tex_align <- switch(
        effective_align,
        center = "\\centering",
        right = "\\raggedleft",
        left = "\\raggedright",
        "\\raggedright"
      )
      inner <- newline_to_latex_break(content)
      content <- paste0("\\parbox[b]{\\hsize}{", tex_align, " ", inner, "}")
    }

    cells[j] <- if (nzchar(content)) {
      paste0("\\hspace{0pt}", content)
    } else {
      content
    }
  }

  paste0(paste0(cells, collapse = " & "), " \\\\")
}


#' LaTeX body rows
#'
#' @param keep_mask Logical vector of length nrow(data). TRUE = keep with next
#'   row (emit `\\\\*` instead of `\\\\`).
#' @noRd
latex_body_rows <- function(
  data,
  columns,
  cell_grid,
  dec_geom = NULL,
  keep_mask = NULL,
  orig_rows = NULL
) {
  nr <- vctrs::vec_size(data)
  if (nr == 0L) {
    return(character(0))
  }

  col_names <- names(columns)
  nc <- length(col_names)

  # Pre-compute which columns are decimal-aligned
  is_decimal <- col_names %in% names(dec_geom %||% list())
  row_idx <- orig_rows %||% seq_len(nr)

  # Pre-extract cell_grid columns as vectors for O(1) indexed access.
  # Grid is column-major (build_cell_grid): cell (i, j) → index (j-1)*nr + i.
  # LaTeX per-cell bold/italic is applied via tabularray cell specs (see
  # latex_tblr_spec_str), not inline here — only content, indent, and align
  # are needed.
  cg_content <- cell_grid$content
  cg_indent <- cell_grid$indent
  cg_align <- cell_grid$align

  # Pre-extract column-level properties as vectors (same pattern as build_cell_grid)
  col_spaces_vec <- vapply(
    col_names,
    function(nm) {
      columns[[nm]]$space_mode %||% "indent"
    },
    character(1)
  )

  lines <- character(nr)
  for (i in seq_len(nr)) {
    cells <- character(nc)
    for (j in seq_len(nc)) {
      # O(1) cell lookup
      idx <- (j - 1L) * nr + i
      content <- cg_content[idx]

      # Apply indent from cell_grid (font-metric-based, set by apply_indent_by)
      cell_indent <- cg_indent[idx]

      if (is_decimal[j]) {
        # Decimal alignment via pre-formatted string with centering offset
        geom <- dec_geom[[col_names[j]]]
        formatted <- geom$formatted[row_idx[i]]
        if (!nzchar(trimws(formatted))) {
          cells[j] <- ""
          next
        }
        formatted_esc <- latex_escape_and_resolve(formatted)
        # Replace spaces with ~ (non-breaking) for LaTeX alignment preservation
        formatted_esc <- gsub(" ", "~", formatted_esc, fixed = TRUE)
        offset_pt <- round(geom$center_offset[row_idx[i]] / 20, 1)
        cells[j] <- paste0("\\hspace{", offset_pt, "pt}", formatted_esc)
      } else {
        # Standard cell handling
        if (col_spaces_vec[j] == "preserve") {
          # Preserve mode: convert leading spaces to \hspace (first-line visual spacing)
          n_lead <- nchar(content) - nchar(sub("^ +", "", content))
          if (n_lead > 0L) {
            content <- sub("^ +", "", content)
          }
          content <- latex_escape_and_resolve(content)
          if (n_lead > 0L) {
            content <- paste0(
              "\\hspace{",
              round(n_lead * fr_env$latex_space_width_em, 2),
              "em}",
              content
            )
          }
        } else {
          # Indent mode: spaces already stripped upstream, \leftskip handles indent
          content <- latex_escape_and_resolve(content)
        }
        # Apply paragraph-level indent (all lines, including wrapped)
        if (cell_indent > 0) {
          content <- paste0(
            "\\leftskip=",
            round(cell_indent, 4),
            "in\\relax ",
            content
          )
        }
        # Handle multiline content: wrap in \parbox with per-cell alignment
        if (grepl("\n", content, fixed = TRUE)) {
          tex_align <- switch(
            cg_align[idx],
            center = "\\centering",
            right = "\\raggedleft",
            left = "\\raggedright",
            "\\raggedright"
          )
          inner <- newline_to_latex_break(content)
          content <- paste0("\\parbox[t]{\\hsize}{", tex_align, " ", inner, "}")
        }
        cells[j] <- content
      }
    }
    # Prefix non-empty cells with \hspace{0pt} for word-break hint
    mask <- nzchar(cells)
    cells[mask] <- paste0("\\hspace{0pt}", cells[mask])

    # Row terminator: \\* = keep with next row (no page break after)
    row_end <- if (!is.null(keep_mask) && isTRUE(keep_mask[i])) {
      " \\\\*"
    } else {
      " \\\\"
    }
    lines[i] <- paste0(paste0(cells, collapse = " & "), row_end)
  }

  lines
}


# ══════════════════════════════════════════════════════════════════════════════
# Helpers
# ══════════════════════════════════════════════════════════════════════════════

#' Count maximum number of lines across pagehead/pagefoot zones
#'
#' Multi-line content arises from `c("Line 1", "Line 2")` in fr_pagehead(),
#' which is collapsed to "Line 1\nLine 2" by collapse_chrome_text().
#' @param chrome A pagehead or pagefoot list with $left, $center, $right.
#' @return Integer: maximum line count across all zones (minimum 1).
#' @noRd
max_chrome_lines <- function(chrome) {
  if (is.null(chrome)) {
    return(1L)
  }
  zones <- c(chrome$left %||% "", chrome$center %||% "", chrome$right %||% "")
  max(vapply(
    zones,
    function(z) {
      length(strsplit(z, "\n", fixed = TRUE)[[1L]])
    },
    integer(1)
  ))
}


#' Escape LaTeX specials in chrome text, preserving {token} placeholders
#'
#' Finds `{...}` token placeholders via regex, escapes everything else,
#' and reassembles. Newlines (`\n`) are preserved for later shortstack wrapping.
#' @param text Character scalar with possible `{token}` placeholders.
#' @return LaTeX-safe string with tokens intact.
#' @noRd
latex_escape_chrome <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(text)
  }
  # Temporarily protect \n from escaping
  text <- gsub("\n", "\x01NL\x02", text, fixed = TRUE)
  # Find all {token} placeholders
  m <- gregexpr("\\{[^}]+\\}", text, perl = TRUE)
  tokens <- regmatches(text, m)[[1L]]
  if (length(tokens) == 0L) {
    # No tokens — escape everything and restore \n
    return(gsub("\x01NL\x02", "\n", latex_escape(text), fixed = TRUE))
  }
  # Replace tokens with unique sentinels, escape, restore
  for (i in seq_along(tokens)) {
    text <- sub(tokens[i], paste0("\x01TK", i, "\x02"), text, fixed = TRUE)
  }
  text <- latex_escape(text)
  for (i in seq_along(tokens)) {
    text <- sub(paste0("\x01TK", i, "\x02"), tokens[i], text, fixed = TRUE)
  }
  gsub("\x01NL\x02", "\n", text, fixed = TRUE)
}
