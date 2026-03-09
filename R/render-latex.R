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
  lines <- character(0)

  lines <- c(lines, latex_preamble(spec))
  lines <- c(lines, "\\begin{document}")
  lines <- c(lines, latex_fancyhdr_setup(spec))

  total_sections <- length(col_panels) * length(page_groups)
  section_idx <- 0L

  for (group_idx in seq_along(page_groups)) {
    group <- page_groups[[group_idx]]

    for (panel_idx in seq_along(col_panels)) {
      panel_cols <- col_panels[[panel_idx]]
      vis_columns <- spec$columns[intersect(panel_cols, names(spec$columns))]
      section_idx <- section_idx + 1L

      if (section_idx > 1L) lines <- c(lines, "\\clearpage")

      is_last <- (group_idx == length(page_groups) &&
                    panel_idx == length(col_panels))

      # Redefine head template per section: handles group labels and
      # continuation text (panel_idx > 1 gets continuation on all pages)
      lines <- c(lines, latex_head_template(spec, group_label = group$group_label,
                                             panel_idx = panel_idx))

      # Redefine foot template per section: "last" footnotes only in final section
      lines <- c(lines, latex_foot_template(spec, is_last = is_last,
                                             vis_columns = vis_columns))

      # Resolve borders
      nrow_header <- 1L + n_spanner_levels(spec$header$spans)
      borders <- resolve_borders(spec$rules, nrow(group$data),
                                 length(vis_columns), nrow_header)

      # Per-group header label overrides (per-group / function / auto N-counts)
      label_overrides <- resolve_group_labels(
        spec, group$data, group$group_label
      )

      # Build cell grid
      cell_grid <- build_cell_grid(group$data, vis_columns,
                                   spec$cell_styles, spec$page)

      # Table (titles/footnotes are in longtblr head/foot templates)
      lines <- c(lines, latex_table(spec, group$data, vis_columns,
                                     cell_grid, borders,
                                     label_overrides = label_overrides))
    }
  }

  lines <- c(lines, "\\end{document}")

  writeLines(lines, path, useBytes = FALSE)
}


# ══════════════════════════════════════════════════════════════════════════════
# LaTeX Document Components
# ══════════════════════════════════════════════════════════════════════════════

#' Build \\setmainfont command, with Liberation fallback for missing fonts
#' @noRd
latex_setmainfont <- function(font_name) {
  resolved <- resolve_latex_font(font_name)

  if (is.null(resolved$path)) {
    # System font available — simple reference
    return(paste0("\\setmainfont{", resolved$name, "}"))
  }

  # Bundled Liberation font — specify file paths for fontspec

  prefix <- fr_env$liberation_file_prefix[[resolved$name]]
  font_path <- gsub("\\\\", "/", resolved$path)
  if (!endsWith(font_path, "/")) font_path <- paste0(font_path, "/")

  paste0(
    "\\setmainfont{", resolved$name, "}[",
    "Path=", font_path, ",",
    "Extension=.ttf,",
    "UprightFont=", prefix, "-Regular,",
    "BoldFont=", prefix, "-Bold,",
    "ItalicFont=", prefix, "-Italic,",
    "BoldItalicFont=", prefix, "-BoldItalic",
    "]"
  )
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
  # - Pagehead flows from start of body area upward (headsep=0pt)
  # - Pagefoot flows from end of body area downward (footskip=line height)
  # When absent, zero them out to avoid dead space.
  if (!is.null(spec$pagehead)) {
    ph_fs <- spec$pagehead$font_size %||% (page$font_size - 1)
    ph_line_pt <- round(ph_fs * lf, 1)
    headheight_str <- paste0(",headheight=", ph_line_pt, "pt")
    headsep_str <- ",headsep=0pt"
  } else {
    headheight_str <- ",headheight=0pt"
    headsep_str <- ",headsep=0pt"
  }

  if (!is.null(spec$pagefoot)) {
    pf_fs <- spec$pagefoot$font_size %||% (page$font_size - 1)
    pf_line_pt <- round(pf_fs * lf, 1)
    footskip_str <- paste0(",footskip=", pf_line_pt, "pt")
  } else {
    footskip_str <- ",footskip=0pt"
  }

  geom_opts <- paste0(
    paper_opt,
    if (nzchar(orient_opt)) paste0(",", orient_opt) else "",
    ",left=", ml, "in",
    ",right=", mr, "in",
    ",top=", mt, "in",
    ",bottom=", mb, "in",
    headheight_str, headsep_str, footskip_str
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

  lines <- c(lines,
    "",
    "% Colors",
    "\\usepackage[table]{xcolor}"
  )

  # Collect ALL colors used: borders, header styling, cell styles
  all_colors <- collect_colors(spec)
  for (hex in all_colors) {
    cname <- hex_to_tblr_color(hex)
    lines <- c(lines, paste0(
      "\\definecolor{", cname, "}{HTML}{", toupper(sub("^#", "", hex)), "}"
    ))
  }

  # lastpage package (needed when {total_pages} token is used in pagehead/pagefoot)
  needs_lastpage <- FALSE
  for (chrome in list(spec$pagehead, spec$pagefoot)) {
    if (!is.null(chrome)) {
      txt <- paste0(chrome$left, chrome$center, chrome$right)
      if (grepl("total_pages", txt, fixed = TRUE)) needs_lastpage <- TRUE
    }
  }

  lines <- c(lines,
    "",
    "% Headers and footers",
    "\\usepackage{fancyhdr}",
    if (needs_lastpage) "\\usepackage{lastpage}",
    "\\pagestyle{fancy}",
    "",
    "% Font size",
    paste0("\\renewcommand{\\normalsize}{\\fontsize{",
           page$font_size, "}{",
           round(page$font_size * lf, 1),
           "}\\selectfont}"),
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

  # Helper: resolve tokens first (replacing {thepage} with \thepage etc.),
  # then resolve sentinels but do NOT escape LaTeX specials — token values
  # are pre-escaped above, and LaTeX commands (\thepage) must survive.
  # Fancyhdr content is trusted (user-authored), same as RTF page chrome.
  latex_chrome_text <- function(raw_text, token_map, context) {
    txt <- resolve_tokens(raw_text, token_map, context)
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
    # Convert \n to LaTeX line break for multi-line chrome text
    gsub("\n", "\\\\\\\\", txt)
  }

  # Page header
  if (!is.null(spec$pagehead)) {
    chrome <- spec$pagehead
    fs <- chrome$font_size %||% (spec$page$font_size - 1)
    bold_on  <- if (isTRUE(chrome$bold)) "\\textbf{" else ""
    bold_off <- if (isTRUE(chrome$bold)) "}" else ""

    if (!is.null(chrome$left)) {
      txt <- latex_chrome_text(chrome$left, token_map, "page header")
      lines <- c(lines, paste0("\\fancyhead[L]{\\fontsize{", fs, "}{",
                                 round(fs * fr_env$latex_leading_factor, 1), "}\\selectfont ",
                                 bold_on, txt, bold_off, "}"))
    }
    if (!is.null(chrome$center)) {
      txt <- latex_chrome_text(chrome$center, token_map, "page header")
      lines <- c(lines, paste0("\\fancyhead[C]{\\fontsize{", fs, "}{",
                                 round(fs * fr_env$latex_leading_factor, 1), "}\\selectfont ",
                                 bold_on, txt, bold_off, "}"))
    }
    if (!is.null(chrome$right)) {
      txt <- latex_chrome_text(chrome$right, token_map, "page header")
      lines <- c(lines, paste0("\\fancyhead[R]{\\fontsize{", fs, "}{",
                                 round(fs * fr_env$latex_leading_factor, 1), "}\\selectfont ",
                                 bold_on, txt, bold_off, "}"))
    }
  }

  # Page footer: only pagefoot goes to \fancyfoot (footnotes are body-area
  # content via tabularray foot templates)
  if (!is.null(spec$pagefoot)) {
    chrome <- spec$pagefoot
    fs <- chrome$font_size %||% (spec$page$font_size - 1)
    bold_on  <- if (isTRUE(chrome$bold)) "\\textbf{" else ""
    bold_off <- if (isTRUE(chrome$bold)) "}" else ""

    if (!is.null(chrome$left)) {
      txt <- latex_chrome_text(chrome$left, token_map, "page footer")
      lines <- c(lines, paste0("\\fancyfoot[L]{\\fontsize{", fs, "}{",
                                 round(fs * fr_env$latex_leading_factor, 1), "}\\selectfont ",
                                 bold_on, txt, bold_off, "}"))
    }
    if (!is.null(chrome$center)) {
      txt <- latex_chrome_text(chrome$center, token_map, "page footer")
      lines <- c(lines, paste0("\\fancyfoot[C]{\\fontsize{", fs, "}{",
                                 round(fs * fr_env$latex_leading_factor, 1), "}\\selectfont ",
                                 bold_on, txt, bold_off, "}"))
    }
    if (!is.null(chrome$right)) {
      txt <- latex_chrome_text(chrome$right, token_map, "page footer")
      lines <- c(lines, paste0("\\fancyfoot[R]{\\fontsize{", fs, "}{",
                                 round(fs * fr_env$latex_leading_factor, 1), "}\\selectfont ",
                                 bold_on, txt, bold_off, "}"))
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
  if (length(titles) == 0L) return(character(0))
  lf <- fr_env$latex_leading_factor
  cont_text <- spec$page$continuation

  lines <- character(0)
  for (idx in seq_along(titles)) {
    entry <- titles[[idx]]
    fs <- entry$font_size %||% spec$page$font_size
    align <- entry$align %||% "center"
    bold_on  <- if (isTRUE(entry$bold)) "\\textbf{" else ""
    bold_off <- if (isTRUE(entry$bold)) "}" else ""
    content <- latex_escape_and_resolve(entry$content)

    # Append continuation text to first title when requested
    if (idx == 1L && isTRUE(continuation) && !is.null(cont_text)) {
      content <- paste0(content, " ", latex_escape(cont_text))
    }

    tex_align <- switch(align,
      center = "\\centering",
      right  = "\\raggedleft",
      ""
    )

    if (nzchar(tex_align)) {
      lines <- c(lines, paste0(
        "{", tex_align, "\\fontsize{", fs, "}{",
        round(fs * lf, 1), "}\\selectfont ",
        bold_on, content, bold_off, "\\par}"
      ))
    } else {
      lines <- c(lines, paste0(
        "{\\fontsize{", fs, "}{", round(fs * lf, 1), "}\\selectfont ",
        bold_on, content, bold_off, "\\par}"
      ))
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
  if (!is.null(group_label) && nzchar(group_label)) {
    lf <- fr_env$latex_leading_factor
    fs <- spec$page$font_size
    pb_bold_on  <- if (isTRUE(spec$body$page_by_bold)) "\\textbf{" else ""
    pb_bold_off <- if (isTRUE(spec$body$page_by_bold)) "}" else ""
    pb_align <- spec$body$page_by_align %||% "left"
    if (pb_align == "left") {
      gl_line <- paste0(
        "\\noindent{\\fontsize{", fs, "}{", round(fs * lf, 1),
        "}\\selectfont ", pb_bold_on,
        latex_escape(group_label), pb_bold_off, "}\\par"
      )
    } else {
      tex_align <- if (pb_align == "center") "\\centering" else "\\raggedleft"
      gl_line <- paste0(
        "{", tex_align, "\\fontsize{", fs, "}{", round(fs * lf, 1),
        "}\\selectfont ", pb_bold_on,
        latex_escape(group_label), pb_bold_off, "\\par}"
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
  rest_body  <- paste0(cont_content, collapse = "\n")
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
  last_fns  <- if (is_last) fn_split$last else list()

  has_every <- length(every_fns) > 0L
  has_last  <- length(last_fns)  > 0L

  lines <- character(0)

  if (has_every) {
    # "every" footnotes repeat on all pages (firstfoot, middlefoot, lastfoot)
    every_content <- paste0(
      build_fn_latex_content(every_fns, spec, vis_columns = vis_columns),
      collapse = "\n")

    # firstfoot and middlefoot get "every" footnotes only
    lines <- c(lines,
      paste0("\\DeclareTblrTemplate{firstfoot, middlefoot}{default}{"),
      every_content,
      "}"
    )

    if (has_last) {
      # lastfoot gets "every" + "last" footnotes combined
      last_content <- paste0(
        build_fn_latex_content(last_fns, spec, skip_spacing = TRUE,
                               vis_columns = vis_columns),
        collapse = "\n"
      )
      lines <- c(lines,
        paste0("\\DeclareTblrTemplate{lastfoot}{default}{"),
        every_content,
        last_content,
        "}"
      )
    } else {
      # lastfoot same as firstfoot/middlefoot
      lines <- c(lines,
        paste0("\\DeclareTblrTemplate{lastfoot}{default}{"),
        every_content,
        "}"
      )
    }
  } else if (has_last) {
    # Only "last" footnotes — suppress firstfoot/middlefoot
    lines <- c(lines,
      "\\DefTblrTemplate{firstfoot}{default}{}",
      "\\DefTblrTemplate{middlefoot}{default}{}"
    )
    last_content <- paste0(
      build_fn_latex_content(last_fns, spec, vis_columns = vis_columns),
      collapse = "\n")
    lines <- c(lines,
      paste0("\\DeclareTblrTemplate{lastfoot}{default}{"),
      last_content,
      "}"
    )
  } else {
    # No footnotes — suppress all foot templates
    lines <- c(lines,
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
build_fn_latex_content <- function(entries, spec, skip_spacing = FALSE,
                                   vis_columns = NULL) {
  if (length(entries) == 0L) return(character(0))
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
      fn_lines <- c(fn_lines, paste0(
        "\\noindent\\rule{\\linewidth}{", fr_env$latex_fn_sep_width_pt, "pt}"
      ))
    }
  }

  for (fn in entries) {
    fs <- fn$font_size %||% spec$page$font_size
    align <- fn$align %||% "left"
    content <- latex_escape_and_resolve(fn$content)

    tex_align <- switch(align,
      center = "\\centering",
      right  = "\\raggedleft",
      ""
    )

    if (nzchar(tex_align)) {
      fn_lines <- c(fn_lines, paste0(
        "{", tex_align, "\\fontsize{", fs, "}{",
        round(fs * lf, 1), "}\\selectfont ",
        content, "\\par}"
      ))
    } else {
      fn_lines <- c(fn_lines, paste0(
        "\\noindent{\\fontsize{", fs, "}{", round(fs * lf, 1),
        "}\\selectfont ", content, "}\\par"
      ))
    }
  }

  # Close minipage
  fn_lines <- c(fn_lines, "\\end{minipage}")

  fn_lines
}


#' Build the complete tabularray table
#' @noRd
latex_table <- function(spec, data, columns, cell_grid, borders,
                         label_overrides = NULL) {
  col_names <- names(columns)
  nc <- length(col_names)
  nr <- nrow(data)

  # Build column spec for tabularray
  # Subtract leftsep + rightsep per column so total rendered width matches
  # the intended table width (tabularray adds colsep outside wd=)
  colsep_pt <- spec$page$col_gap / 2
  colsep_in <- 2 * colsep_pt / fr_env$points_per_inch
  gap_col_indices <- integer(0)
  col_spec_parts <- vapply(seq_along(col_names), function(j) {
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
  }, character(1))
  colspec_str <- paste0(col_spec_parts, collapse = "")

  # Number of header rows (spanners + column header)
  nrow_header <- 1L + n_spanner_levels(spec$header$spans)

  # Collect cell/row/column style specs for tabularray inner keys
  inner_keys <- character(0)

  # rowhead: how many header rows repeat on each page
  inner_keys <- c(inner_keys, paste0("rowhead = ", nrow_header))

  # Zero out colsep for gap columns so only the narrow width remains
  for (gi in gap_col_indices) {
    inner_keys <- c(inner_keys,
      paste0("column{", gi, "}={leftsep=0pt,rightsep=0pt}")
    )
  }


  # Column widths already in colspec; add row-level styles
  row_heights <- build_row_heights(nr, spec$cell_styles)

  # Hlines from borders
  inner_keys <- c(inner_keys, latex_border_specs(borders, nr, nc, nrow_header))

  # Cell-level styles (bold, italic, bg, fg) from cell_grid
  inner_keys <- c(inner_keys, latex_cell_style_specs(cell_grid, nr, nc,
                                                      nrow_header))

  # Header cell styles
  header_valign <- spec$header$valign %||% "bottom"
  hgrid <- build_header_cell_grid(columns, spec$cell_styles, spec$page,
                                   nrow_header,
                                   default_valign = header_valign,
                                   header_cfg = spec$header)
  inner_keys <- c(inner_keys, latex_header_style_specs(hgrid, nrow_header, nc,
                                                       columns = columns,
                                                       header_default_align = spec$header$align))

  # Build table option string
  inner_str <- paste0(inner_keys, collapse = ",\n  ")

  lines <- character(0)
  # Header row vertical alignment (bottom = match RTF multiline header style)
  header_valign_key <- paste0(
    "row{1-", nrow_header, "}={valign=b}"
  )

  # Spanner rows (compute early to get hline keys for inner spec)
  spanner_result <- latex_spanner_rows(spec, columns)
  if (length(spanner_result$hlines) > 0L) {
    inner_keys <- c(inner_keys, spanner_result$hlines)
    inner_str <- paste0(inner_keys, collapse = ",\n  ")
  }

  lines <- c(lines, paste0(
    "\\begin{longtblr}[presep=0pt, postsep=0pt]{",
    "\n  colspec={", colspec_str, "},",
    "\n  row{1-Z}={abovesep=", fr_env$latex_rowsep, ",belowsep=", fr_env$latex_rowsep, "},",
    "\n  column{1-Z}={leftsep=", colsep_pt, "pt,rightsep=", colsep_pt, "pt},",
    "\n  ", header_valign_key, ",",
    "\n  ", inner_str,
    "\n}"
  ))

  # Spanner rows
  lines <- c(lines, spanner_result$rows)

  # Column header row
  lines <- c(lines, latex_col_header_row(spec, columns,
                                          label_overrides = label_overrides))

  # Decimal alignment widths for makebox positioning
  decimal_widths_pt <- compute_decimal_before_pt(
    data, columns, cell_grid, spec$page$font_family, spec$page$font_size
  )

  # Keep-together mask for orphan/widow control
  keep_mask <- build_keep_mask(
    data, spec$body$group_by,
    orphan_min = spec$page$orphan_min %||% fr_env$default_orphan_min,
    widow_min  = spec$page$widow_min  %||% fr_env$default_widow_min
  )

  # Body rows
  lines <- c(lines, latex_body_rows(data, columns, cell_grid, decimal_widths_pt,
                                     keep_mask = keep_mask))

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
        specs <- c(specs, paste0(
          "hline{1} = {", wd, "dash=", ls, ", fg=", cname, "}"
        ))
      }
      bs <- h$bottom[i, j][[1L]]
      if (!is.null(bs) && i == nrow_header && j == 1L) {
        ls <- fr_env$linestyle_latex[bs$linestyle] %||% "solid"
        cname <- hex_to_tblr_color(bs$fg)
        wd <- if (!is.null(bs$width)) paste0("wd=", bs$width, "pt, ") else ""
        specs <- c(specs, paste0(
          "hline{", nrow_header + 1L, "} = {", wd, "dash=", ls, ", fg=", cname, "}"
        ))
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
        specs <- c(specs, paste0(
          "hline{", tblr_row, "} = {", wd, "dash=", ls, ", fg=", cname, "}"
        ))
      }
      # Top border on first body row
      if (i == 1L) {
        bs_top <- b$top[1L, 1L][[1L]]
        if (!is.null(bs_top)) {
          ls <- fr_env$linestyle_latex[bs_top$linestyle] %||% "solid"
          cname <- hex_to_tblr_color(bs_top$fg)
          wd <- if (!is.null(bs_top$width)) paste0("wd=", bs_top$width, "pt, ") else ""
          tblr_row <- nrow_header + 1L
          # Only add if not already covered by header bottom
          if (!any(grepl(paste0("hline\\{", tblr_row, "\\}"), specs)))
            specs <- c(specs, paste0(
              "hline{", tblr_row, "} = {", wd, "dash=", ls, ", fg=", cname, "}"
            ))
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
  if (nrow(cell_grid) == 0L) return(character(0))
  specs <- character(0)

  for (idx in seq_len(nrow(cell_grid))) {
    g <- cell_grid[idx, ]
    parts <- character(0)

    if (isTRUE(g$bold)) parts <- c(parts, "font=\\bfseries")
    if (isTRUE(g$italic)) parts <- c(parts, "font=\\itshape")
    if (!is.na(g$bg) && nzchar(g$bg)) {
      parts <- c(parts, paste0("bg=", hex_to_tblr_color(g$bg)))
    }
    if (!is.na(g$fg) && g$fg != "#000000") {
      parts <- c(parts, paste0("fg=", hex_to_tblr_color(g$fg)))
    }
    # Note: indent is handled in latex_body_rows() via \hspace in content,
    # not via tabularray preto= (which is unreliable with Q[l,wd=X] columns)

    if (length(parts) > 0L) {
      # tabularray row index: header rows + body row index
      tblr_row <- nrow_header + g$row_idx
      specs <- c(specs, paste0(
        "cell{", tblr_row, "}{", g$col_idx, "} = {",
        paste0(parts, collapse = ", "), "}"
      ))
    }
  }

  specs
}


#' Generate tabularray cell style specs for header grid
#' @noRd
latex_header_style_specs <- function(hgrid, header_row_idx, nc,
                                     columns = NULL,
                                     header_default_align = NULL) {
  specs <- character(0)

  for (j in seq_len(nc)) {
    g <- hgrid[j, ]
    parts <- character(0)

    if (isTRUE(g$bold)) parts <- c(parts, "font=\\bfseries")
    if (isTRUE(g$italic)) parts <- c(parts, "font=\\itshape")
    if (!is.na(g$bg) && nzchar(g$bg)) {
      parts <- c(parts, paste0("bg=", hex_to_tblr_color(g$bg)))
    }
    if (!is.na(g$fg) && g$fg != "#000000") {
      parts <- c(parts, paste0("fg=", hex_to_tblr_color(g$fg)))
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
      specs <- c(specs, paste0(
        "cell{", header_row_idx, "}{", j, "} = {",
        paste0(parts, collapse = ", "), "}"
      ))
    }
  }

  specs
}


#' LaTeX spanner (spanning header) rows
#' @noRd
latex_spanner_rows <- function(spec, columns) {
  spans <- spec$header$spans
  if (length(spans) == 0L) return(list(rows = character(0), hlines = character(0)))

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
        content <- latex_escape_and_resolve(matching_span$label)
        cells <- c(cells, paste0(
          "\\SetCell[c=", span_width, "]{c} ", content
        ))
        # Empty cells for the rest of the span
        if (span_width > 1L) {
          cells <- c(cells, rep("", span_width - 1L))
        }
        # Track hline for this span (gap columns provide visual separation)
        if (isTRUE(matching_span$hline)) {
          col_start <- j
          col_end <- j + span_width - 1L
          span_hlines <- c(span_hlines, paste0(
            "hline{", lvl_row + 1L, "}={",
            col_start, "-", col_end,
            "}{wd=0.5pt, dash=solid, fg=tblr000000}"
          ))
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
      if ((is.null(label) || !nzchar(label)) && !isTRUE(columns[[j]]$is_gap)) {
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
      tex_align <- switch(effective_align,
        center = "\\centering",
        right  = "\\raggedleft",
        left   = "\\raggedright",
        "\\raggedright"
      )
      inner <- gsub("\n", " \\\\ ", content, fixed = TRUE)
      content <- paste0("\\parbox[b]{\\hsize}{", tex_align, " ", inner, "}")
    }

    cells[j] <- if (nzchar(content)) paste0("\\hspace{0pt}", content) else content
  }

  paste0(paste0(cells, collapse = " & "), " \\\\")
}


#' LaTeX body rows
#'
#' @param keep_mask Logical vector of length nrow(data). TRUE = keep with next
#'   row (emit `\\\\*` instead of `\\\\`).
#' @noRd
latex_body_rows <- function(data, columns, cell_grid,
                            decimal_widths_pt = NULL,
                            keep_mask = NULL) {
  nr <- nrow(data)
  if (nr == 0L) return(character(0))

  col_names <- names(columns)
  nc <- length(col_names)

  # Pre-compute which columns are decimal-aligned
  if (is.null(decimal_widths_pt)) {
    is_decimal <- rep(FALSE, nc)
  } else {
    is_decimal <- !is.na(decimal_widths_pt)
  }

  lines <- character(nr)
  for (i in seq_len(nr)) {
    cells <- character(nc)
    for (j in seq_len(nc)) {
      grid_row <- which(cell_grid$row_idx == i & cell_grid$col_idx == j)
      if (length(grid_row) == 0L) {
        cells[j] <- ""
        next
      }
      content <- cell_grid$content[grid_row]

      # Apply indent from cell_grid (font-metric-based, set by apply_indent_by)
      cell_indent <- cell_grid$indent[grid_row]

      if (is_decimal[j]) {
        # Decimal alignment via \makebox
        trimmed <- trimws(content)
        if (!nzchar(trimmed)) {
          cells[j] <- ""
          next
        }
        parts <- split_at_decimal(trimmed)
        # Escape each part separately (split before escape)
        before_esc <- latex_escape_and_resolve(parts$before)
        after_esc  <- latex_escape_and_resolve(parts$after)
        w <- round(decimal_widths_pt[j], 1)
        cells[j] <- paste0("\\makebox[", w, "pt][r]{", before_esc, "}", after_esc)
      } else {
        # Standard cell handling
        # Preserve leading whitespace: convert leading spaces to \hspace
        n_lead <- nchar(content) - nchar(sub("^ +", "", content))
        if (n_lead > 0L) content <- sub("^ +", "", content)
        content <- latex_escape_and_resolve(content)
        if (n_lead > 0L) {
          # ~0.55em per space in monospace font
          content <- paste0("\\hspace{", round(n_lead * fr_env$latex_space_width_em, 2), "em}", content)
        }
        # Apply cell indent (from fr_rows(indent_by=))
        if (cell_indent > 0) {
          content <- paste0("\\hspace{", round(cell_indent, 4), "in}", content)
        }
        # Handle multiline content: wrap in \parbox with column alignment
        if (grepl("\n", content, fixed = TRUE)) {
          col_align <- columns[[col_names[j]]]$align %||% "left"
          tex_align <- switch(col_align,
            center = "\\centering",
            right  = "\\raggedleft",
            left   = "\\raggedright",
            "\\raggedright"
          )
          inner <- gsub("\n", " \\\\ ", content, fixed = TRUE)
          content <- paste0("\\parbox[t]{\\hsize}{", tex_align, " ", inner, "}")
        }
        cells[j] <- content
      }
    }
    # Prefix non-empty cells with \hspace{0pt} for word-break hint
    mask <- nzchar(cells)
    cells[mask] <- paste0("\\hspace{0pt}", cells[mask])

    # Row terminator: \\* = keep with next row (no page break after)
    row_end <- if (!is.null(keep_mask) && isTRUE(keep_mask[i])) " \\\\*" else " \\\\"
    lines[i] <- paste0(paste0(cells, collapse = " & "), row_end)
  }

  lines
}


