# ──────────────────────────────────────────────────────────────────────────────
# render-rtf.R — RTF backend for fr_render()
#
# Emits a complete RTF document with:
#   - Font table and color table
#   - Section definitions (page size, margins, orientation)
#   - Page header (pagehead) via RTF {\header} group
#   - Footnotes + page footer (pagefoot) combined in RTF {\footer} group
#     (repeats on every page automatically)
#   - Titles as \trhdr rows (repeat on every page)
#   - Column headers with \trhdr (repeat on every page)
#   - Spanning headers with \clmgf / \clmrg
#   - Body rows with per-cell styling and borders
#   - Section breaks (\sect) for page_by groups and column split panels
# ──────────────────────────────────────────────────────────────────────────────


#' Zero top/bottom cell padding (eliminates Word's default ~29twips each side)
#' @noRd
rtf_zero_cell_padding <- "\\trpaddt0\\trpaddft3\\trpaddb0\\trpaddfb3"


#' Row-level padding string: at-least height + zero top/bottom cell padding
#'
#' Produces RTF control words for row height:
#' `\trrhN` (at-least height — grows for multi-line cells),
#' `\trpaddt0\trpaddft3` (zero top padding),
#' `\trpaddb0\trpaddfb3` (zero bottom padding).
#'
#' @param font_size_pt Numeric. Font size in points.
#' @return Character scalar. RTF control word string.
#' @noRd
rtf_row_height_str <- function(font_size_pt) {
  rh <- row_height_twips(font_size_pt)
  paste0("\\trrh", rh, rtf_zero_cell_padding)
}


#' Paragraph-level spacing string: zero space before/after
#'
#' Produces `\sb0\sa0` to eliminate paragraph spacing. Line spacing is
#' left at the Word default (single-line) so the viewer controls row
#' height naturally, with `\trrh` providing the minimum.
#'
#' @param font_size_pt Numeric. Font size in points.
#' @return Character scalar. RTF paragraph spacing string.
#' @noRd
rtf_cell_spacing_str <- function(font_size_pt) {
  "\\sb0\\sa0"
}


#' Build RTF cell padding string from col_gap (points)
#'
#' Converts `spec$page$col_gap` (total gap in points) to symmetric left/right
#' cell padding in twips. Returns an empty string when col_gap is 0.
#' @noRd
rtf_col_gap_str <- function(spec) {
  gap_pt <- spec$page$col_gap
  if (gap_pt <= 0L) return("")
  # Half on each side, convert pt → twips (1 pt = 20 twips)
  pad_twips <- as.integer(round(gap_pt / 2 * 20))
  paste0("\\clpadt", pad_twips, "\\clpadr", pad_twips, "\\clpadft3\\clpadfr3")
}


#' Render an fr_spec to RTF
#'
#' @param spec Finalized fr_spec object.
#' @param page_groups List of page group lists (data + group_label).
#' @param col_panels List of column name vectors (one per panel).
#' @param path Output file path.
#' @noRd
render_rtf <- function(spec, page_groups, col_panels, path) {
  con <- file(path, open = "wb")
  on.exit(close(con))

  # Separate "last" footnotes — they render as body rows, not in {\footer}
  footnotes <- spec$meta$footnotes %||% list()
  fn_split <- split_footnotes(footnotes)
  last_footnotes <- fn_split$last
  every_footnotes <- fn_split$every

  colors <- collect_colors(spec)
  color_info <- build_rtf_colortbl(colors)

  rtf_write(con, rtf_preamble(spec, color_info))

  total_sections <- length(col_panels) * length(page_groups)
  section_idx <- 0L

  for (group_idx in seq_along(page_groups)) {
    group <- page_groups[[group_idx]]

    for (panel_idx in seq_along(col_panels)) {
      panel_cols <- col_panels[[panel_idx]]
      # Filter columns to this panel's visible columns
      vis_columns <- spec$columns[intersect(panel_cols, names(spec$columns))]
      section_idx <- section_idx + 1L

      if (section_idx > 1L) {
        # End previous table with \pard before \sect to exit table context;
        # without this, Word treats consecutive \trowd rows across \sect as
        # one continuous table and ignores the page break.
        rtf_write(con, "\\pard\\par\n\\sect\n")
      }

      # Determine is_last before section def (affects footer height)
      is_last <- (group_idx == length(page_groups) &&
                    panel_idx == length(col_panels))

      # Single-page estimation: when content fits on one page, render
      # footnotes as body rows instead of in {\footer} group
      is_single_page <- estimate_single_page(spec, group$data)

      rtf_write(con, rtf_section_def(spec, is_last,
                                      body_footnotes = is_single_page))

      # Page header/footer (RTF header/footer groups)
      # Use RTF field codes for dynamic page numbering.
      # Placeholders survive rtf_escape_and_resolve(), then get replaced
      # with actual field codes by rtf_resolve_page_fields().
      token_map <- build_token_map(
        page_num = "\x01RTFPAGE\x02",
        total_pages = "\x01RTFNUMPAGES\x02",
        spec = spec
      )
      rtf_write(con, rtf_resolve_page_fields(
        rtf_page_header(spec, token_map)))
      rtf_write(con, rtf_resolve_page_fields(
        rtf_footer_group(spec, token_map, is_last, vis_columns,
                         skip_footnotes = is_single_page)))

      # Blank lines after page header (spacing$pagehead_after)
      if (!is.null(spec$pagehead)) {
        n_ph <- spec$spacing$pagehead_after %||% 1L
        if (n_ph > 0L) {
          ph_fs <- pt_to_half_pt(spec$page$font_size)
          rtf_write(con, strrep(
            paste0("\\pard\\plain\\fs", ph_fs, "\\par\n"), n_ph
          ))
        }
      }

      # Resolve borders for this section
      nrow_header <- 1L + n_spanner_levels(spec$header$spans)
      borders <- resolve_borders(spec$rules, nrow(group$data),
                                 length(vis_columns), nrow_header)

      # Suppress body bottom border when another element will render it:
      # - Multi-page: {\footer} group renders the bottom rule
      # - Single-page: body footnote rows render it as cell top border
      bottom_rule <- find_bottom_rule(spec)
      if (!is.null(bottom_rule)) {
        suppress_bottom <- if (is_single_page) {
          length(spec$meta$footnotes %||% list()) > 0L
        } else {
          has_every_fn <- any(vapply(spec$meta$footnotes %||% list(),
                       function(fn) fn$placement == "every", logical(1)))
          has_last_body_fn <- is_last && length(last_footnotes) > 0L
          has_every_fn || has_last_body_fn || !is.null(spec$pagefoot)
        }
        if (suppress_bottom) {
          nr <- nrow(group$data)
          if (nr > 0L) {
            for (j in seq_along(vis_columns)) {
              borders$body$bottom[nr, j] <- list(NULL)
            }
          }
        }
      }

      # Title rows as \trhdr (always repeat on every page)
      rtf_write(con, rtf_title_rows(spec, vis_columns, color_info,
                                     panel_idx = panel_idx))

      # Group label (page_by value) — \trhdr row to stay within the table
      if (!is.null(group$group_label) && nzchar(group$group_label)) {
        rtf_write(con, rtf_page_by_rows(spec, vis_columns, group$group_label))
      }

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

      # Spanning header rows
      rtf_write(con, rtf_spanner_rows(spec, vis_columns, borders, color_info,
                                        span_overrides = span_overrides))

      # Column header row
      rtf_write(con, rtf_col_header_row(spec, vis_columns, borders, color_info,
                                         label_overrides = label_overrides))

      # Body rows — conditional R-side pagination
      has_group_by <- length(spec$body$group_by) > 0L

      if (has_group_by && nrow(group$data) > 0L) {
        # R-side pagination: split body into sub-pages
        heights <- calculate_row_heights(group$data, vis_columns, spec$page)
        budget  <- calculate_page_budget(spec)
        page_assignments <- paginate_rows(
          heights, budget, group$data, spec$body$group_by,
          orphan_min = spec$page$orphan_min %||% fr_env$default_orphan_min,
          widow_min  = spec$page$widow_min  %||% fr_env$default_widow_min
        )

        n_subpages <- max(page_assignments)
        for (sub_page in seq_len(n_subpages)) {
          sub_rows <- which(page_assignments == sub_page)
          if (length(sub_rows) == 0L) next

          if (sub_page > 1L) {
            # New section for each R-computed page after the first
            rtf_write(con, "\\pard\\par\n\\sect\n")
            section_idx <- section_idx + 1L

            is_last <- (group_idx == length(page_groups) &&
                          panel_idx == length(col_panels) &&
                          sub_page == n_subpages)

            rtf_write(con, rtf_section_def(spec, is_last,
                                            body_footnotes = FALSE))
            rtf_write(con, rtf_resolve_page_fields(
              rtf_page_header(spec, token_map)))
            rtf_write(con, rtf_resolve_page_fields(
              rtf_footer_group(spec, token_map, is_last, vis_columns,
                               skip_footnotes = FALSE)))

            if (!is.null(spec$pagehead)) {
              n_ph <- spec$spacing$pagehead_after %||% 1L
              if (n_ph > 0L) {
                ph_fs <- pt_to_half_pt(spec$page$font_size)
                rtf_write(con, strrep(
                  paste0("\\pard\\plain\\fs", ph_fs, "\\par\n"), n_ph
                ))
              }
            }

            # Titles, spanners, column header repeat on each sub-page
            rtf_write(con, rtf_title_rows(spec, vis_columns, color_info,
                                           panel_idx = panel_idx))
            if (!is.null(group$group_label) && nzchar(group$group_label)) {
              rtf_write(con, rtf_page_by_rows(spec, vis_columns,
                                               group$group_label))
            }
            rtf_write(con, rtf_spanner_rows(spec, vis_columns, borders,
                                             color_info,
                                             span_overrides = span_overrides))
            rtf_write(con, rtf_col_header_row(spec, vis_columns, borders,
                                               color_info,
                                               label_overrides = label_overrides))
          }

          # Emit body rows for this sub-page
          sub_data <- group$data[sub_rows, , drop = FALSE]
          sub_styles <- remap_styles_for_subpage(spec$cell_styles, sub_rows)
          sub_grid <- build_cell_grid(sub_data, vis_columns,
                                       sub_styles, spec$page)
          # Resolve borders for sub-page row count
          sub_borders <- resolve_borders(spec$rules, nrow(sub_data),
                                          length(vis_columns), nrow_header)

          # Suppress bottom border on non-final sub-pages (table continues)
          if (sub_page < n_subpages) {
            sub_nr <- nrow(sub_data)
            if (sub_nr > 0L) {
              for (j in seq_along(vis_columns)) {
                sub_borders$body$bottom[sub_nr, j] <- list(NULL)
              }
            }
          }

          # Slice decimal geometry for sub-page rows
          sub_spec <- spec
          if (!is.null(spec$decimal_geometry)) {
            sub_dec <- list()
            for (nm in names(spec$decimal_geometry)) {
              geom <- spec$decimal_geometry[[nm]]
              sub_dec[[nm]] <- list(
                formatted     = geom$formatted[sub_rows],
                center_offset = geom$center_offset[sub_rows],
                max_width     = geom$max_width
              )
            }
            sub_spec$decimal_geometry <- sub_dec
          }

          rtf_write(con, rtf_body_rows(sub_spec, sub_data, vis_columns,
                                       sub_grid, sub_borders, color_info))
        }

        # Body footnotes after the last sub-page
        if (is_single_page) {
          rtf_write(con, rtf_body_footnotes(spec, vis_columns, is_last,
                                             borders, color_info))
        } else if (is_last && length(last_footnotes) > 0L) {
          rtf_write(con, rtf_body_footnotes_last(
            spec, vis_columns, last_footnotes, borders, color_info
          ))
        }

      } else {
        # No group_by: emit all body rows, let RTF viewer paginate
        cell_grid <- build_cell_grid(group$data, vis_columns,
                                     spec$cell_styles, spec$page)
        rtf_write(con, rtf_body_rows(spec, group$data, vis_columns,
                                     cell_grid, borders, color_info))

        # Body footnotes: single-page mode OR "last" placement footnotes
        if (is_single_page) {
          rtf_write(con, rtf_body_footnotes(spec, vis_columns, is_last,
                                             borders, color_info))
        } else if (is_last && length(last_footnotes) > 0L) {
          rtf_write(con, rtf_body_footnotes_last(
            spec, vis_columns, last_footnotes, borders, color_info
          ))
        }
      }

    }
  }

  rtf_write(con, "}")
}


#' Replace page number placeholders with RTF field codes
#'
#' Token map uses sentinel placeholders for {thepage} and {total_pages}
#' to survive rtf_escape_and_resolve(). This function replaces them with
#' actual RTF field codes that Word evaluates dynamically.
#' @noRd
rtf_resolve_page_fields <- function(text) {
  text <- gsub("\x01RTFPAGE\x02",
               "{\\field{\\*\\fldinst PAGE}}", text, fixed = TRUE)
  text <- gsub("\x01RTFNUMPAGES\x02",
               "{\\field{\\*\\fldinst NUMPAGES}}", text, fixed = TRUE)
  text
}


# ══════════════════════════════════════════════════════════════════════════════
# RTF Document Components
# ══════════════════════════════════════════════════════════════════════════════

#' RTF preamble: header, font table, color table
#' @noRd
rtf_preamble <- function(spec, color_info) {
  font_name <- spec$page$font_family
  rtf_fam <- get_rtf_font_family(font_name)
  prq <- get_rtf_font_prq(font_name)

  fonttbl <- paste0("{\\fonttbl{\\f0\\", rtf_fam, "\\fprq", prq,
                    " ", font_name, ";}}")
  paste0("{\\rtf1\\ansi\\ansicpg1252\\deff0\n",
         fonttbl, "\n",
         color_info$rtf, "\n")
}


#' RTF section definition: page size, margins, orientation, header/footer position
#' @param spec Finalized fr_spec object.
#' @param is_last Logical. TRUE for the final section (affects footer space).
#' @param body_footnotes Logical. When TRUE, footnotes are rendered as body
#'   rows instead of in the footer, so footer height does not account for them.
#' @noRd
rtf_section_def <- function(spec, is_last = FALSE, body_footnotes = FALSE) {
  page <- spec$page
  dims <- paper_dims_twips(page$paper, page$orientation)
  ml <- inches_to_twips(page$margins$left)
  mr <- inches_to_twips(page$margins$right)
  mt <- inches_to_twips(page$margins$top)
  mb <- inches_to_twips(page$margins$bottom)

  orient <- if (page$orientation == "landscape") "\\lndscpsxn" else ""

  # \headery: only emit when pagehead exists, to avoid reserving dead space
  headery_str <- ""
  if (!is.null(spec$pagehead)) {
    chrome_fs <- spec$pagehead$font_size %||% (page$font_size - 1)
    line_twips <- as.integer(round(chrome_fs * 20 * fr_env$rtf_leading_factor))
    headery_str <- paste0("\\headery", max(as.integer(mt - line_twips), fr_env$rtf_min_headery))
  }

  # \footery = bottom margin. Word auto-expands the footer area upward
  # if content exceeds it, shrinking body area to fit.
  # Only "every" footnotes go in {\footer}; "last" are body rows.
  # When body_footnotes = TRUE, all footnotes are in body rows, not footer.
  has_footer_fn <- if (body_footnotes) {
    FALSE
  } else {
    any(vapply(spec$meta$footnotes %||% list(),
               function(fn) fn$placement == "every", logical(1)))
  }
  footery_str <- if (!is.null(spec$pagefoot) || has_footer_fn) {
    paste0("\\footery", mb)
  } else ""

  paste0("\\sectd\\sbkpage", orient,
         "\\pgwsxn", dims[["width"]],
         "\\pghsxn", dims[["height"]],
         "\\margl", ml,
         "\\margr", mr,
         "\\margt", mt,
         "\\margb", mb,
         headery_str,
         footery_str,
         "\n")
}


#' RTF page header (running header)
#'
#' Uses a single paragraph with tab stops for left/center/right layout:
#' - Left text at position 0 (default paragraph alignment \ql)
#' - Center text at a \tqc tab stop at the page midpoint
#' - Right text at a \tqr tab stop at the right margin
#' @noRd
rtf_page_header <- function(spec, token_map) {
  if (is.null(spec$pagehead)) return("")
  rtf_pagechrome_paragraph(spec$pagehead, spec, token_map, "header", "page header")
}

#' Find the bottom rule for the table body
#'
#' Searches spec$rules for a horizontal rule with region="body", side="below",
#' rows=NULL (i.e., targets the last row = table bottom border).
#' Returns the rule object or NULL if none found.
#' @noRd
find_bottom_rule <- function(spec) {
  rules <- spec$rules %||% list()
  for (rule in rules) {
    if (inherits(rule, "fr_rule_hline") &&
        rule$region == "body" &&
        rule$side == "below" &&
        is.null(rule$rows)) {
      return(rule)
    }
  }
  NULL
}


#' RTF footer group — footnotes + pagefoot combined in {\footer}
#'
#' Builds a single `{\footer ...}` group containing:
#'   1. Bottom rule (table bottom border as paragraph top border, repeats every page)
#'   2. Blank spacing lines (footnotes_before — gap after bottom rule)
#'   3. Footnote separator line + footnote paragraphs (table content width)
#'   4. 1/4 baselineskip gap
#'   5. Pagefoot paragraph (L/C/R tab-stop layout, full printable width)
#'
#' Only `placement = "every"` footnotes are included (they repeat on every
#' page). `placement = "last"` footnotes are rendered as body rows via
#' `rtf_body_footnotes_last()` so they appear only once at the end.
#'
#' @param spec Finalized fr_spec object.
#' @param token_map Token map for page chrome resolution.
#' @param is_last Logical. TRUE for the final section.
#' @param vis_columns Named list of fr_col objects for the current panel.
#'   Used to calculate table content width for footnote paragraph width.
#' @noRd
rtf_footer_group <- function(spec, token_map, is_last, vis_columns,
                             skip_footnotes = FALSE) {
  footnotes <- spec$meta$footnotes %||% list()
  has_pagefoot <- !is.null(spec$pagefoot)

  # When skip_footnotes = TRUE (single-page mode), footnotes are rendered
  # as body rows — the footer only contains pagefoot (if any).
  # "last" placement footnotes are NEVER in {\footer} — they're body rows.
  entries <- if (skip_footnotes) {
    list()
  } else {
    split_footnotes(footnotes)$every
  }

  has_footnotes <- length(entries) > 0L

  if (!has_footnotes && !has_pagefoot) return("")

  parts <- character(0)
  fn_fs <- pt_to_half_pt(spec$page$font_size)

  # Calculate left/right indent so footnotes are centered under the table
  # Table width = sum of visible column widths
  # Split the gap equally into \li and \ri for horizontal centering
  printable_twips <- printable_area_twips(spec$page)[["width"]]
  table_twips <- sum(vapply(vis_columns, function(c) inches_to_twips(c$width),
                            integer(1), USE.NAMES = FALSE))
  fn_gap_twips <- max(0L, as.integer(printable_twips - table_twips))
  fn_indent_each <- as.integer(fn_gap_twips / 2L)
  fn_indent_str <- if (fn_indent_each > 0L) {
    paste0("\\li", fn_indent_each, "\\ri", fn_indent_each)
  } else ""

  # Bottom rule — render table bottom border as paragraph top border in footer
  # so it repeats on every page (mirrors body-below hline from spec$rules).
  # Skip when footnotes are in body rows (they handle the bottom rule).
  if (!skip_footnotes) {
    bottom_rule <- find_bottom_rule(spec)
    if (!is.null(bottom_rule)) {
      bw <- as.integer(round(bottom_rule$width * 20))  # pt to twips
      parts <- c(parts, paste0(
        "\\pard\\plain\\sb0", fn_indent_str,
        "\\brdrt\\brdrs\\brdrw", bw,
        "\\fs2\\par\n"
      ))
    }
  }

  # Footnote paragraphs (width constrained to table content width)
  # Layout matches PDF: [bottom border] → [spacing] → [separator] → [footnotes]
  if (has_footnotes) {
    # Blank lines before footnotes (spacing$footnotes_before, default 1)
    n_before <- spec$spacing$footnotes_before %||% 1L
    if (n_before > 0L) {
      parts <- c(parts, strrep(
        paste0("\\pard\\plain\\sb0", fn_indent_str, "\\fs", fn_fs, "\\par\n"),
        n_before
      ))
    }

    # Separator as standalone paragraph (after spacing, before footnote text)
    if (isTRUE(spec$meta$footnote_separator)) {
      parts <- c(parts, paste0(
        "\\pard\\plain\\sb0", fn_indent_str,
        "\\brdrt\\brdrs\\brdrw5",
        "\\fs2\\par\n"
      ))
    }

    for (idx in seq_along(entries)) {
      fn <- entries[[idx]]
      fs <- pt_to_half_pt(fn$font_size %||% spec$page$font_size)
      align_rtf <- fr_env$align_to_rtf[[fn$align %||% "left"]]
      content <- rtf_escape_and_resolve(fn$content)

      parts <- c(parts, paste0(
        "\\pard\\plain\\sb0", fn_indent_str, align_rtf,
        "\\fs", fs, " ", content, "\\par\n"
      ))
    }
  }

  # Pagefoot (invisible table row layout, full printable width)
  if (has_pagefoot) {
    # 1/4 baselineskip gap between footnotes and pagefoot
    gap_twips <- if (has_footnotes) {
      chrome_fs <- spec$pagefoot$font_size %||% (spec$page$font_size - 1)
      as.integer(round(chrome_fs * 20 * fr_env$rtf_leading_factor / 4))
    } else {
      0L
    }
    pf_content <- rtf_chrome_content(spec$pagefoot, spec, token_map,
                                     "page footer", sb_twips = gap_twips)
    if (nzchar(pf_content)) {
      parts <- c(parts, pf_content)
    }
  }

  paste0("{\\footer\n", paste0(parts, collapse = ""), "}\n")
}



#' Build RTF chrome content as invisible table row
#'
#' Returns an invisible single-row RTF table with 1–3 cells for L/C/R layout.
#' Each cell has its own paragraph alignment, so multi-line text stays aligned.
#' Used by rtf_pagechrome_paragraph() and rtf_footer_group().
#' @param chrome Chrome spec (left/center/right/bold/font_size).
#' @param spec Finalized fr_spec object.
#' @param token_map Token map for token resolution.
#' @param context Context string for error messages.
#' @param sb_twips Space before in twips. Emits a spacer \\par before the row.
#' @noRd
rtf_chrome_content <- function(chrome, spec, token_map, context,
                               sb_twips = 0L) {
  has_left   <- !is.null(chrome$left)
  has_center <- !is.null(chrome$center)
  has_right  <- !is.null(chrome$right)
  if (!has_left && !has_center && !has_right) return("")

  fs <- pt_to_half_pt(chrome$font_size %||% (spec$page$font_size - 1))
  printable_twips <- printable_area_twips(spec$page)[["width"]]

  # Helper: resolve tokens + escape + \n → \line
  chrome_escape <- function(txt) {
    txt <- resolve_tokens(txt, token_map, context)
    txt <- rtf_escape_and_resolve(txt)
    gsub("\n", "\\\\line ", txt)
  }

  bold_on  <- if (isTRUE(chrome$bold)) "\\b " else ""
  bold_off <- if (isTRUE(chrome$bold)) "\\b0" else ""

  # Build zones: list of (alignment, content) pairs
  zones <- list()
  if (has_left)   zones <- c(zones, list(list(align = "\\ql", text = chrome_escape(chrome$left))))
  if (has_center) zones <- c(zones, list(list(align = "\\qc", text = chrome_escape(chrome$center))))
  if (has_right)  zones <- c(zones, list(list(align = "\\qr", text = chrome_escape(chrome$right))))

  n_zones <- length(zones)

  # Cell widths: equal split (cumulative \cellx positions)
  zone_widths <- as.integer(round(seq_len(n_zones) / n_zones * printable_twips))

  # Row definition (no borders, auto height)
  cell_defs <- paste0("\\cellx", zone_widths, collapse = "")
  row_def <- paste0("\\trowd\\trrh0\\trqc", cell_defs, "\n")

  # Cell contents
  cell_contents <- vapply(zones, function(z) {
    paste0("\\pard\\plain\\intbl", z$align, "\\fs", fs, " ",
           bold_on, z$text, bold_off, "\\cell")
  }, character(1))

  # Assemble: optional space-before + row
  sb_part <- ""
  if (sb_twips > 0L) {
    sb_part <- paste0("\\pard\\plain\\sb", as.integer(sb_twips), "\\fs2\\par\n")
  }

  paste0(sb_part, row_def, paste0(cell_contents, collapse = ""), "\\row\\pard\n")
}


#' Build RTF header or footer paragraph with group wrapper
#'
#' Wraps rtf_chrome_content() in a {\header ...} or {\footer ...} group.
#' Used for pagehead ({\header}). Footer path uses rtf_footer_group().
#' @noRd
rtf_pagechrome_paragraph <- function(chrome, spec, token_map, group, context) {
  content <- rtf_chrome_content(chrome, spec, token_map, context)
  if (!nzchar(content)) return("")
  paste0("{\\", group, "\n", content, "}\n")
}


#' RTF title rows (repeating \trhdr rows — always used)
#'
#' Renders titles as full-width merged table rows with \trhdr so they repeat
#' on every page. The first title gets an IF PAGE > 1 field code to append
#' the continuation text (e.g., "(continued)") on pages 2+.
#'
#' @noRd
rtf_title_rows <- function(spec, columns, color_info, panel_idx = 1L) {
  titles <- spec$meta$titles
  if (length(titles) == 0L) return("")

  continuation <- spec$page$continuation
  col_names <- names(columns)
  ncol <- length(col_names)

  # Total table width in twips (single merged cell spanning all columns)
  total_twips <- sum(vapply(columns, function(c) inches_to_twips(c$width),
                            integer(1), USE.NAMES = FALSE))

  lines <- character(length(titles))
  for (idx in seq_along(titles)) {
    entry <- titles[[idx]]
    fs <- pt_to_half_pt(entry$font_size %||% spec$page$font_size)
    align_rtf <- fr_env$align_to_rtf[[entry$align %||% "center"]]
    bold_on  <- if (isTRUE(entry$bold)) "\\b " else ""
    bold_off <- if (isTRUE(entry$bold)) "\\b0" else ""
    content <- rtf_escape_and_resolve(entry$content)

    # Append continuation text to first title on panel 2+ only.
    # Panel 1 is the first page of the table — no continuation needed.
    # RTF field codes don't work in table cells, so we use plain text.
    cont_field <- ""
    if (idx == 1L && !is.null(continuation) && panel_idx > 1L) {
      cont_field <- paste0(" ", rtf_escape(continuation))
    }

    # Row: \trhdr with a single merged cell spanning full width
    title_fs_pt <- entry$font_size %||% spec$page$font_size
    rh_str <- rtf_row_height_str(title_fs_pt)
    sp_str <- rtf_cell_spacing_str(title_fs_pt)
    row_def <- paste0("\\trowd\\trhdr\\trqc", rh_str)

    # Merge cells: first cell = \clmgf, rest = \clmrg
    cum_widths <- cumsum(vapply(columns, function(c) inches_to_twips(c$width),
                                integer(1), USE.NAMES = FALSE))
    cell_defs <- paste0("\\clmgf\\cellx", cum_widths[1L])
    if (ncol > 1L) {
      cell_defs <- paste0(cell_defs,
                          paste0("\\clmrg\\cellx", cum_widths[-1L], collapse = ""))
    }

    # Cell content (only first cell has content, rest are empty merge targets)
    cell_content <- paste0(
      "\\pard\\intbl", align_rtf, sp_str, "\\fs", fs, " ",
      bold_on, content, cont_field, bold_off, "\\cell"
    )
    if (ncol > 1L) {
      cell_content <- paste0(cell_content,
                             paste0(rep("\\pard\\intbl\\cell", ncol - 1L),
                                    collapse = ""))
    }

    lines[idx] <- paste0(row_def, cell_defs, "\n", cell_content, "\\row\n")
  }

  # Blank \trhdr rows for titles_after spacing
  n_after <- spec$spacing$titles_after %||% 1L
  if (n_after > 0L) {
    blank_fs <- pt_to_half_pt(spec$page$font_size)
    blank_rh <- rtf_row_height_str(spec$page$font_size)
    blank_sp <- rtf_cell_spacing_str(spec$page$font_size)
    blank_cell <- paste0("\\pard\\intbl", blank_sp, "\\fs", blank_fs, " \\cell")
    blank_empty <- if (ncol > 1L) {
      paste0(rep("\\pard\\intbl\\cell", ncol - 1L), collapse = "")
    } else {
      ""
    }
    blank_row <- paste0("\\trowd\\trhdr\\trqc", blank_rh, cell_defs, "\n",
                         blank_cell, blank_empty, "\\row\n")
    lines <- c(lines, rep(blank_row, n_after))
  }

  paste0(lines, collapse = "")
}


#' RTF page-by group label as \trhdr rows
#'
#' Emits the page_by group label as a merged \trhdr row (same pattern as
#' title rows) so it stays within the table, has table content width,
#' and repeats on continuation pages.
#' @noRd
rtf_page_by_rows <- function(spec, columns, group_label) {
  col_names <- names(columns)
  ncol <- length(col_names)
  fs <- pt_to_half_pt(spec$page$font_size)
  pb_align <- fr_env$align_to_rtf[[spec$body$page_by_align %||% "left"]]
  pb_bold_on  <- if (isTRUE(spec$body$page_by_bold)) "\\b " else ""
  pb_bold_off <- if (isTRUE(spec$body$page_by_bold)) "\\b0" else ""
  content <- rtf_escape_and_resolve(group_label)

  # Merged cell defs spanning all columns
  cum_widths <- cumsum(vapply(columns, function(c) inches_to_twips(c$width),
                              integer(1), USE.NAMES = FALSE))
  cell_defs <- paste0("\\clmgf\\cellx", cum_widths[1L])
  if (ncol > 1L) {
    cell_defs <- paste0(cell_defs,
                        paste0("\\clmrg\\cellx", cum_widths[-1L], collapse = ""))
  }

  pb_fs_pt <- spec$page$font_size
  rh_str <- rtf_row_height_str(pb_fs_pt)
  sp_str <- rtf_cell_spacing_str(pb_fs_pt)
  row_def <- paste0("\\trowd\\trhdr\\trqc", rh_str)
  cell_content <- paste0(
    "\\pard\\intbl", pb_align, sp_str, "\\fs", fs, " ",
    pb_bold_on, content, pb_bold_off, "\\cell"
  )
  if (ncol > 1L) {
    cell_content <- paste0(cell_content,
                           paste0(rep("\\pard\\intbl\\cell", ncol - 1L),
                                  collapse = ""))
  }

  lines <- paste0(row_def, cell_defs, "\n", cell_content, "\\row\n")

  # Blank \trhdr rows for page_by_after spacing
  n_after <- spec$spacing$page_by_after %||% 1L
  if (n_after > 0L) {
    blank_cell <- paste0("\\pard\\intbl", sp_str, "\\fs", fs, " \\cell")
    blank_empty <- if (ncol > 1L) {
      paste0(rep("\\pard\\intbl\\cell", ncol - 1L), collapse = "")
    } else ""
    blank_row <- paste0(row_def, cell_defs, "\n",
                        blank_cell, blank_empty, "\\row\n")
    lines <- c(lines, rep(blank_row, n_after))
  }

  paste0(lines, collapse = "")
}


#' RTF spanner (spanning header) rows
#' @noRd
rtf_spanner_rows <- function(spec, columns, borders, color_info,
                              span_overrides = NULL) {
  spans <- spec$header$spans
  if (length(spans) == 0L) return("")

  col_names <- names(columns)
  ncol <- length(col_names)
  fs <- pt_to_half_pt(spec$page$font_size)
  pad_str <- rtf_col_gap_str(spec)

  # Build cumulative column positions in twips
  col_widths_twips <- vapply(columns, function(c) inches_to_twips(c$width),
                             integer(1), USE.NAMES = FALSE)
  cum_widths <- cumsum(col_widths_twips)

  # Group spans by level
  levels <- sort(unique(vapply(spans, function(s) s$level, integer(1))))

  rh_str <- rtf_row_height_str(spec$page$font_size)
  sp_str <- rtf_cell_spacing_str(spec$page$font_size)

  lines <- character(0)
  for (lvl in rev(levels)) {
    lvl_spans <- Filter(function(s) s$level == lvl, spans)

    # Row definition: \trowd\trhdr
    row_def <- paste0("\\trowd\\trhdr\\trqc", rh_str)

    # Build cell definitions
    cell_defs <- character(0)
    cell_contents <- character(0)
    j <- 1L

    while (j <= ncol) {
      # Check if this column starts a span
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
        sp_end <- match(sp_cols[length(sp_cols)], col_names)

        # First cell: \clmgf, rest: \clmrg
        # Bottom border on spanned cells when span$hline is TRUE
        span_border <- ""
        if (isTRUE(matching_span$hline)) {
          span_border <- paste0("\\clbrdrb\\brdrs\\brdrw", fr_env$rtf_spanner_brdrw, "\\brdrcf1")
        }
        border_str <- rtf_cell_border_string(borders$header, 1L, j, color_info)
        cell_defs <- c(cell_defs, paste0(border_str, span_border, pad_str, "\\clmgf\\cellx", cum_widths[j]))
        for (k in (j + 1L):min(sp_end, ncol)) {
          border_str <- rtf_cell_border_string(borders$header, 1L, k, color_info)
          cell_defs <- c(cell_defs, paste0(border_str, span_border, pad_str, "\\clmrg\\cellx", cum_widths[k]))
        }
        span_label <- matching_span$label
        if (!is.null(span_overrides)) {
          ov <- span_overrides[matching_span$label]
          if (!is.na(ov)) span_label <- ov
        }
        content <- rtf_escape_and_resolve(span_label)
        content <- gsub("\n", "\\\\line ", content)
        cell_contents <- c(cell_contents,
                           paste0("\\pard\\intbl\\qc", sp_str, "\\fs", fs, " ",
                                  content, "\\cell"))
        for (k in (j + 1L):min(sp_end, ncol)) {
          cell_contents <- c(cell_contents, "\\pard\\intbl\\cell")
        }
        j <- sp_end + 1L
      } else {
        # Empty cell (no span covers this column)
        border_str <- rtf_cell_border_string(borders$header, 1L, j, color_info)
        cell_defs <- c(cell_defs, paste0(border_str, pad_str, "\\cellx", cum_widths[j]))
        cell_contents <- c(cell_contents, "\\pard\\intbl\\cell")
        j <- j + 1L
      }
    }

    lines <- c(lines, paste0(row_def,
                              paste0(cell_defs, collapse = ""),
                              "\n",
                              paste0(cell_contents, collapse = ""),
                              "\\row\n"))
  }

  paste0(lines, collapse = "")
}


#' RTF column header row
#' @noRd
rtf_col_header_row <- function(spec, columns, borders, color_info,
                                label_overrides = NULL) {
  col_names <- names(columns)
  ncol <- length(col_names)

  col_widths_twips <- vapply(columns, function(c) inches_to_twips(c$width),
                             integer(1), USE.NAMES = FALSE)
  cum_widths <- cumsum(col_widths_twips)

  # Header row index (after spanners)
  h_row <- 1L + n_spanner_levels(spec$header$spans)

  # Build header cell grid with style overrides
  header_valign <- spec$header$valign %||% "bottom"
  hgrid <- build_header_cell_grid(columns, spec$cell_styles, spec$page, h_row,
                                   default_valign = header_valign,
                                   header_cfg = spec$header)

  # \trowd\trhdr — marks row as header (repeats on each page)
  rh_str <- rtf_row_height_str(spec$page$font_size)
  sp_str <- rtf_cell_spacing_str(spec$page$font_size)
  row_def <- paste0("\\trowd\\trhdr\\trqc", rh_str)

  pad_str <- rtf_col_gap_str(spec)
  cell_defs <- character(ncol)
  cell_contents <- character(ncol)

  for (j in seq_len(ncol)) {
    g <- hgrid[j, ]

    # Background color
    bg_str <- ""
    if (!is.na(g$bg) && nzchar(g$bg)) {
      ci <- color_info$index[[g$bg]]
      if (!is.null(ci)) bg_str <- paste0("\\clcbpat", ci)
    }

    # Vertical alignment (per-cell, from grid — respects style overrides)
    va_str <- fr_env$valign_to_rtf[g$valign]

    border_str <- rtf_cell_border_string(borders$header, h_row, j, color_info)
    cell_defs[j] <- paste0(border_str, bg_str, va_str, pad_str, "\\cellx", cum_widths[j])

    # Use per-group override if available, else fall back to column label
    label <- label_overrides[col_names[j]]
    if (is.na(label) || is.null(label)) {
      label <- columns[[j]]$label
      if ((is.null(label) || !nzchar(label)) && !isTRUE(columns[[j]]$is_gap)) {
        label <- col_names[j]
      }
    }
    # Escape/resolve first, THEN replace \n with \line
    # (rtf_escape would double-escape the backslash in \line)
    content <- rtf_escape_and_resolve(label %||% "")
    content <- gsub("\n", "\\\\line ", content)

    fs <- pt_to_half_pt(g$font_size)
    align_rtf <- fr_env$align_to_rtf[[g$align]]

    # Inline formatting from style overrides
    fmt_on <- ""
    fmt_off <- ""
    if (isTRUE(g$bold)) { fmt_on <- paste0(fmt_on, "\\b "); fmt_off <- paste0("\\b0", fmt_off) }
    if (isTRUE(g$italic)) { fmt_on <- paste0(fmt_on, "\\i "); fmt_off <- paste0("\\i0", fmt_off) }
    if (isTRUE(g$underline)) { fmt_on <- paste0(fmt_on, "\\ul "); fmt_off <- paste0("\\ulnone", fmt_off) }

    # Foreground color
    fg_str <- ""
    if (!is.na(g$fg) && g$fg != "#000000") {
      ci <- color_info$index[[g$fg]]
      if (!is.null(ci)) fg_str <- paste0("\\cf", ci, " ")
    }

    cell_contents[j] <- paste0("\\pard\\plain\\intbl", align_rtf, sp_str,
                                "\\fs", fs, " ",
                                fg_str, fmt_on, content, fmt_off, "\\cell")
  }

  paste0(row_def,
         paste0(cell_defs, collapse = ""),
         "\n",
         paste0(cell_contents, collapse = ""),
         "\\row\n")
}


#' RTF body rows
#' @noRd
rtf_body_rows <- function(spec, data, columns, cell_grid, borders, color_info) {
  if (nrow(data) == 0L) return("")

  col_names <- names(columns)
  ncol <- length(col_names)
  nr <- nrow(data)
  pad_str <- rtf_col_gap_str(spec)

  col_widths_twips <- vapply(columns, function(c) inches_to_twips(c$width),
                             integer(1), USE.NAMES = FALSE)
  cum_widths <- cumsum(col_widths_twips)

  # Use pre-computed decimal geometry from finalize_spec()
  dec_geom <- spec$decimal_geometry
  is_decimal_col <- col_names %in% names(dec_geom %||% list())

  empty_cell <- "\\pard\\intbl\\cell"

  # Precompute keep-together mask from group_by with orphan/widow awareness
  keep_mask <- build_keep_mask(
    data, spec$body$group_by,
    orphan_min = spec$page$orphan_min %||% fr_env$default_orphan_min,
    widow_min  = spec$page$widow_min  %||% fr_env$default_widow_min
  )

  # Precompute row heights from fr_row_style objects
  row_heights <- build_row_heights(nr, spec$cell_styles)

  # Deterministic row height/spacing for body rows
  rh_str <- rtf_row_height_str(spec$page$font_size)
  sp_str <- rtf_cell_spacing_str(spec$page$font_size)

  lines <- vector("list", nr)
  for (i in seq_len(nr)) {
    # Row properties
    keep_str <- if (isTRUE(keep_mask[i])) "\\trkeep" else ""
    if (!is.na(row_heights[i])) {
      # User-set explicit height via fr_row_style overrides deterministic height
      height_str <- paste0("\\trrh", inches_to_twips(row_heights[i]),
                           rtf_zero_cell_padding)
    } else {
      height_str <- rh_str
    }
    row_def <- paste0("\\trowd\\trqc", keep_str, height_str)

    # Cell definitions with borders
    # Decimal columns emit TWO sub-cells, so collect into a list
    cell_defs <- vector("list", ncol)
    for (j in seq_len(ncol)) {
      # Background color
      grid_row <- cell_grid$row_idx == i & cell_grid$col_idx == j
      bg <- cell_grid$bg[grid_row]
      bg_str <- ""
      if (!is.na(bg) && nzchar(bg)) {
        ci <- color_info$index[[bg]]
        if (!is.null(ci)) bg_str <- paste0("\\clcbpat", ci)
      }

      # Vertical alignment
      va <- cell_grid$valign[grid_row]
      va_str <- fr_env$valign_to_rtf[va]

      border_str <- rtf_cell_border_string(borders$body, i, j, color_info)
      cell_defs[[j]] <- paste0(border_str, bg_str, va_str, pad_str, "\\cellx", cum_widths[j])
    }

    # Cell contents
    cell_contents <- vector("list", ncol)
    for (j in seq_len(ncol)) {
      grid_row <- which(cell_grid$row_idx == i & cell_grid$col_idx == j)
      if (length(grid_row) == 0L) {
        cell_contents[[j]] <- empty_cell
        next
      }
      g <- cell_grid[grid_row, ]

      fs <- pt_to_half_pt(g$font_size)

      # Inline formatting
      fmt_on <- ""
      fmt_off <- ""
      if (isTRUE(g$bold)) { fmt_on <- paste0(fmt_on, "\\b "); fmt_off <- paste0("\\b0", fmt_off) }
      if (isTRUE(g$italic)) { fmt_on <- paste0(fmt_on, "\\i "); fmt_off <- paste0("\\i0", fmt_off) }
      if (isTRUE(g$underline)) { fmt_on <- paste0(fmt_on, "\\ul "); fmt_off <- paste0("\\ulnone", fmt_off) }

      # Foreground color
      fg_str <- ""
      if (!is.na(g$fg) && g$fg != "#000000") {
        ci <- color_info$index[[g$fg]]
        if (!is.null(ci)) fg_str <- paste0("\\cf", ci, " ")
      }

      # Indentation
      indent_str <- ""
      if (g$indent > 0) {
        indent_twips <- inches_to_twips(g$indent)
        indent_str <- paste0("\\li", indent_twips)
      }

      # Decimal alignment: single cell with left indent for centering
      if (identical(g$align, "decimal") && is_decimal_col[j]) {
        geom <- dec_geom[[col_names[j]]]
        formatted <- geom$formatted[i]
        if (nzchar(trimws(formatted))) {
          formatted_esc <- rtf_escape_and_resolve(formatted)
          formatted_esc <- gsub("\n", "\\\\line ", formatted_esc)
          dec_indent <- paste0("\\li", geom$center_offset[i])
          cell_contents[[j]] <- paste0(
            "\\pard\\plain\\intbl\\ql", sp_str, dec_indent,
            "\\fs", fs, " ",
            fg_str, fmt_on, formatted_esc, fmt_off, "\\cell"
          )
        } else {
          cell_contents[[j]] <- empty_cell
        }
      } else {
        align_rtf <- fr_env$align_to_rtf[[g$align]]
        content <- rtf_escape_and_resolve(g$content)
        content <- gsub("\n", "\\\\line ", content)
        cell_contents[[j]] <- paste0(
          "\\pard\\plain\\intbl", align_rtf, sp_str, indent_str,
          "\\fs", fs, " ",
          fg_str, fmt_on, content, fmt_off, "\\cell"
        )
      }
    }

    lines[[i]] <- paste0(row_def,
                        paste0(unlist(cell_defs), collapse = ""),
                        "\n",
                        paste0(unlist(cell_contents), collapse = ""),
                        "\\row\n")
  }

  paste0(lines, collapse = "")
}





# ══════════════════════════════════════════════════════════════════════════════
# Single-Page Estimation + Body Footnotes
# ══════════════════════════════════════════════════════════════════════════════

#' Estimate whether a section fits on one page
#'
#' Counts row equivalents for all table components (pagehead, titles, spacing,
#' page_by, spanners, header, body, footnotes) and compares to the printable
#' page height. Used to decide whether footnotes should be rendered as body
#' rows (single-page) or in the `{\footer}` group (multi-page).
#'
#' @param spec Finalized fr_spec object.
#' @param group_data Data frame for the current page_by group.
#' @return Logical scalar. TRUE if content fits on one page.
#' @noRd
estimate_single_page <- function(spec, group_data) {
  row_twips <- row_height_twips(spec$page$font_size)
  printable_h <- printable_area_twips(spec$page)[["height"]]

  # Count rows: pagehead + titles + spacing + page_by + spanners + header + body
  n_rows <- 0L
  if (!is.null(spec$pagehead)) {
    n_rows <- n_rows + 1L + (spec$spacing$pagehead_after %||% 1L)
  }
  n_rows <- n_rows + length(spec$meta$titles %||% list())
  n_rows <- n_rows + (spec$spacing$titles_after %||% 1L)
  # page_by label + spacing (if present, estimated at 2 rows)
  if (!is.null(spec$body$page_by)) n_rows <- n_rows + 2L
  n_rows <- n_rows + n_spanner_levels(spec$header$spans) + 1L  # spanners + col header
  n_rows <- n_rows + nrow(group_data)

  # Also count footnote rows if they'd go in body
  n_fn <- length(spec$meta$footnotes %||% list())
  if (n_fn > 0L) {
    n_rows <- n_rows + n_fn + (spec$spacing$footnotes_before %||% 1L)
  }

  n_rows <= floor(printable_h / row_twips)
}


#' Render footnotes as body table rows (non-repeating)
#'
#' For single-page output, footnotes are rendered as merged table rows in the
#' body (without `\trhdr`) so they appear at the bottom of the table content
#' rather than in the page footer margin. Follows the same merged-cell pattern
#' as `rtf_title_rows()`.
#'
#' @param spec Finalized fr_spec object.
#' @param columns Named list of fr_col objects (visible columns).
#' @param is_last Logical. TRUE for the final section.
#' @param borders Border structures from resolve_borders().
#' @param color_info Color info from build_rtf_colortbl().
#' @return Character string of RTF table rows.
#' @noRd
rtf_body_footnotes <- function(spec, columns, is_last, borders, color_info) {
  footnotes <- spec$meta$footnotes %||% list()

  entries <- if (is_last) {
    footnotes
  } else {
    split_footnotes(footnotes)$every
  }

  rtf_footnote_rows(spec, columns, entries, color_info)
}


# ══════════════════════════════════════════════════════════════════════════════
# RTF Cell Border Helpers
#' Render "last" placement footnotes as body table rows
#'
#' For multi-page output, "last" footnotes are rendered as non-repeating
#' body rows at the end of the final section's data. They appear only once.
#'
#' @param spec Finalized fr_spec object.
#' @param columns Named list of fr_col objects (visible columns).
#' @param last_entries List of footnote entries with placement = "last".
#' @param borders Border structures from resolve_borders().
#' @param color_info Color info from build_rtf_colortbl().
#' @return Character string of RTF table rows.
#' @noRd
rtf_body_footnotes_last <- function(spec, columns, last_entries,
                                     borders, color_info) {
  rtf_footnote_rows(spec, columns, last_entries, color_info)
}

#' Shared helper: render footnote entries as RTF body table rows
#'
#' Generates merged-cell table rows for footnote content, with an optional
#' bottom rule row and spacing rows above the footnotes.
#'
#' @param spec Finalized fr_spec object.
#' @param columns Named list of fr_col objects (visible columns).
#' @param entries List of footnote entries to render.
#' @param color_info Color info from build_rtf_colortbl().
#' @return Character string of RTF table rows.
#' @noRd
rtf_footnote_rows <- function(spec, columns, entries, color_info) {
  if (length(entries) == 0L) return("")

  ncol <- length(columns)
  rh_str <- rtf_row_height_str(spec$page$font_size)
  sp_str <- rtf_cell_spacing_str(spec$page$font_size)

  # Cumulative column widths for merged cell defs
  cum_widths <- cumsum(vapply(columns, function(c) inches_to_twips(c$width),
                              integer(1), USE.NAMES = FALSE))
  cell_defs <- paste0("\\clmgf\\cellx", cum_widths[1L])
  if (ncol > 1L) {
    cell_defs <- paste0(cell_defs,
                        paste0("\\clmrg\\cellx", cum_widths[-1L], collapse = ""))
  }

  # Bottom rule and footnote separator — rendered as separate rows to match

  # PDF layout: [bottom border] → [spacing] → [separator] → [footnotes]
  bottom_rule <- find_bottom_rule(spec)
  br_str <- ""
  if (!is.null(bottom_rule)) {
    bw <- pt_to_twips(bottom_rule$width)
    ci <- color_info$index[[bottom_rule$fg]] %||% 1L
    br_str <- paste0("\\clbrdrt\\brdrs\\brdrw", bw, "\\brdrcf", ci)
  }
  has_sep <- isTRUE(spec$meta$footnote_separator)
  sep_str <- if (has_sep) "\\clbrdrt\\brdrs\\brdrw5\\brdrcf1" else ""

  # Pre-allocate lines list: rule row + blank spacing rows + sep row + fn rows
  n_before <- spec$spacing$footnotes_before %||% 1L
  has_rule <- nzchar(br_str)
  lines <- vector("list",
                  as.integer(has_rule) + n_before +
                    as.integer(has_sep) + length(entries))
  li <- 0L

  # 1) Bottom rule row — table bottom border only
  if (has_rule) {
    rule_cell_defs <- paste0(br_str, "\\clmgf\\cellx", cum_widths[1L])
    if (ncol > 1L) {
      rule_cell_defs <- paste0(rule_cell_defs,
                               paste0(br_str, "\\clmrg\\cellx",
                                      cum_widths[-1L], collapse = ""))
    }
    rule_fs <- pt_to_half_pt(2L)
    rule_cell <- paste0("\\pard\\intbl", sp_str, "\\fs", rule_fs, " \\cell")
    rule_empty <- if (ncol > 1L) {
      paste0(rep("\\pard\\intbl\\cell", ncol - 1L), collapse = "")
    } else ""
    li <- li + 1L
    lines[[li]] <- paste0("\\trowd\\trqc", rh_str, rule_cell_defs, "\n",
                           rule_cell, rule_empty, "\\row\n")
  }

  # 2) Spacing blank rows (footnotes_before)
  if (n_before > 0L) {
    blank_fs <- pt_to_half_pt(spec$page$font_size)
    blank_cell <- paste0("\\pard\\intbl", sp_str, "\\fs", blank_fs, " \\cell")
    blank_empty <- if (ncol > 1L) {
      paste0(rep("\\pard\\intbl\\cell", ncol - 1L), collapse = "")
    } else ""
    blank_row <- paste0("\\trowd\\trqc", rh_str, cell_defs, "\n",
                        blank_cell, blank_empty, "\\row\n")
    for (k in seq_len(n_before)) {
      li <- li + 1L
      lines[[li]] <- blank_row
    }
  }

  # 3) Separator row — separate from bottom border, after spacing
  if (has_sep) {
    sep_cell_defs <- paste0(sep_str, "\\clmgf\\cellx", cum_widths[1L])
    if (ncol > 1L) {
      sep_cell_defs <- paste0(sep_cell_defs,
                              paste0(sep_str, "\\clmrg\\cellx",
                                     cum_widths[-1L], collapse = ""))
    }
    sep_fs <- pt_to_half_pt(2L)
    sep_cell <- paste0("\\pard\\intbl", sp_str, "\\fs", sep_fs, " \\cell")
    sep_empty <- if (ncol > 1L) {
      paste0(rep("\\pard\\intbl\\cell", ncol - 1L), collapse = "")
    } else ""
    li <- li + 1L
    lines[[li]] <- paste0("\\trowd\\trqc", rh_str, sep_cell_defs, "\n",
                           sep_cell, sep_empty, "\\row\n")
  }

  # 4) Footnote content rows
  for (idx in seq_along(entries)) {
    fn <- entries[[idx]]
    fn_fs_pt <- fn$font_size %||% spec$page$font_size
    fs <- pt_to_half_pt(fn_fs_pt)
    fn_entry_rh <- rtf_row_height_str(fn_fs_pt)
    fn_entry_sp <- rtf_cell_spacing_str(fn_fs_pt)
    align_rtf <- fr_env$align_to_rtf[[fn$align %||% "left"]]
    content <- rtf_escape_and_resolve(fn$content)

    cell_content <- paste0(
      "\\pard\\intbl", align_rtf, fn_entry_sp, "\\fs", fs, " ", content, "\\cell"
    )
    if (ncol > 1L) {
      cell_content <- paste0(cell_content,
                             paste0(rep("\\pard\\intbl\\cell", ncol - 1L),
                                    collapse = ""))
    }

    li <- li + 1L
    lines[[li]] <- paste0("\\trowd\\trqc", fn_entry_rh, cell_defs, "\n",
                           cell_content, "\\row\n")
  }

  paste0(lines, collapse = "")
}


# ══════════════════════════════════════════════════════════════════════════════

#' Build RTF border control words for a single cell
#'
#' @param border_matrices List with top, bottom, left, right matrices.
#' @param i Row index.
#' @param j Column index.
#' @param color_info Color info from build_rtf_colortbl().
#' @return Character string of RTF border control words.
#' @noRd
rtf_cell_border_string <- function(border_matrices, i, j, color_info,
                                   sides = c("top", "bottom", "left", "right")) {
  rtf_side_keywords <- c(
    top    = "\\clbrdrt",
    bottom = "\\clbrdrb",
    left   = "\\clbrdrl",
    right  = "\\clbrdrr"
  )

  parts <- character(0)
  for (side in sides) {
    mat <- border_matrices[[side]]
    if (i > nrow(mat) || j > ncol(mat)) next
    bs <- mat[i, j][[1L]]
    if (is.null(bs)) next

    # RTF border: \clbrdr{side}\brdrs\brdrwN\brdrcfN
    linestyle_rtf <- fr_env$linestyle_rtf[[bs$linestyle]] %||% "\\brdrs"
    width_twips <- pt_to_twips(bs$width)
    color_idx <- color_info$index[[bs$fg]] %||% 1L

    parts <- c(parts, paste0(
      rtf_side_keywords[[side]],
      linestyle_rtf,
      "\\brdrw", width_twips,
      "\\brdrcf", color_idx
    ))
  }

  paste0(parts, collapse = "")
}


# ══════════════════════════════════════════════════════════════════════════════
# RTF Write Helper
# ══════════════════════════════════════════════════════════════════════════════

#' Write text to an RTF connection as raw bytes
#' @noRd
rtf_write <- function(con, text) {
  if (!nzchar(text)) return(invisible(NULL))
  writeBin(charToRaw(text), con)
  invisible(NULL)
}
