# ──────────────────────────────────────────────────────────────────────────────
# render-html.R — HTML backend for fr_render()
#
# Emits a self-contained HTML document with premium embedded CSS:
#   - Paper simulation (centered white card on light gray background)
#   - Typography optimised for clinical data readability
#   - Pharma-professional color palette
#   - Ultra-refined hairline borders
#   - Page chrome (pagehead/pagefoot) as subtle muted flex divs
#   - @media print produces clean regulatory-grade output
#   - Section breaks for page_by groups and column split panels
# ──────────────────────────────────────────────────────────────────────────────

#' Render an fr_spec to HTML
#'
#' @param spec Finalized fr_spec object.
#' @param page_groups List of page group lists (data + group_label).
#' @param col_panels List of column name vectors (one per panel).
#' @param path Output file path (.html/.htm).
#' @noRd
render_html <- function(spec, page_groups, col_panels, path) {
  viewer <- isTRUE(spec$.viewer)
  total_sections <- length(col_panels) * length(page_groups)
  section_idx <- 0L

  sections <- vector("list", total_sections)

  for (group_idx in seq_along(page_groups)) {
    group <- page_groups[[group_idx]]

    for (panel_idx in seq_along(col_panels)) {
      panel_cols <- col_panels[[panel_idx]]
      vis_columns <- spec$columns[intersect(panel_cols, names(spec$columns))]
      section_idx <- section_idx + 1L

      is_last <- (group_idx == length(page_groups) &&
        panel_idx == length(col_panels))

      # Resolve borders
      nrow_header <- 1L + n_spanner_levels(spec$header$spans)
      borders <- resolve_borders(
        spec$rules,
        nrow(group$data),
        length(vis_columns),
        nrow_header
      )

      # Per-group header label overrides
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

      # Token map for pagehead/pagefoot
      token_map <- build_token_map(
        page_num = section_idx,
        total_pages = total_sections,
        spec = spec
      )

      sections[[section_idx]] <- html_section(
        spec,
        group,
        vis_columns,
        cell_grid,
        borders,
        label_overrides,
        span_overrides,
        token_map,
        panel_idx,
        is_last
      )
    }
  }

  body <- paste0(sections, collapse = "\n")
  doc <- html_document(body, spec, viewer = viewer)
  writeLines(doc, path, useBytes = TRUE)
}


# ══════════════════════════════════════════════════════════════════════════════
# HTML Document Wrapper
# ══════════════════════════════════════════════════════════════════════════════

#' Build complete HTML document with embedded CSS
#' @noRd
html_document <- function(body, spec, viewer = FALSE) {
  css <- html_embedded_css(spec, viewer = viewer)
  paste0(
    "<!DOCTYPE html>\n",
    "<html lang=\"en\">\n",
    "<head>\n",
    "<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<meta name=\"generator\" content=\"arframe\">\n",
    "<title>arframe output</title>\n",
    "<style>\n",
    css,
    "</style>\n",
    "</head>\n",
    "<body>\n",
    "<div class=\"ar-page\">\n",
    body,
    "</div>\n",
    "</body>\n",
    "</html>\n"
  )
}


#' Build an HTML fragment for knitr embedding (no doctype/html/body)
#'
#' All CSS selectors are scoped under a unique container ID so multiple
#' arframe outputs on the same page don't collide.
#' @noRd
html_fragment <- function(body, spec) {
  css <- html_embedded_css(spec, viewer = TRUE)

  # Generate a unique container ID
  uid <- paste0(
    "arframe-",
    format(as.integer(Sys.time()) * 1000 + sample(999, 1), scientific = FALSE)
  )

  # Scope all CSS rules under the container ID
  scoped_css <- scope_css(css, uid)

  paste0(
    "<div id=\"",
    uid,
    "\">\n",
    "<style>\n",
    scoped_css,
    "</style>\n",
    "<div class=\"ar-page\">\n",
    body,
    "</div>\n",
    "</div>\n"
  )
}


#' Scope CSS selectors under a container ID
#'
#' Prefixes each CSS rule selector with `#uid` so styles don't leak
#' into the surrounding document. Handles `body`, `.ar-*`, and
#' `@media` blocks.
#' @noRd
scope_css <- function(css, uid) {
  prefix <- paste0("#", uid)
  lines <- strsplit(css, "\n", fixed = TRUE)[[1L]]
  result <- character(length(lines))

  in_media <- FALSE
  for (i in seq_along(lines)) {
    line <- lines[i]

    if (grepl("^@media", line)) {
      in_media <- TRUE
      result[i] <- line
    } else if (in_media && grepl("^\\}", line)) {
      in_media <- FALSE
      result[i] <- line
    } else if (grepl("^\\*\\s*\\{", line)) {
      # * { ... } → #uid * { ... }
      result[i] <- sub("^\\*", paste0(prefix, " *"), line)
    } else if (grepl("^body\\s*\\{", line)) {
      # body { ... } → #uid { ... }
      result[i] <- sub("^body", prefix, line)
    } else if (grepl("^\\.ar-", line)) {
      # .ar-foo { ... } → #uid .ar-foo { ... }
      result[i] <- paste0(prefix, " ", line)
    } else if (in_media && grepl("^\\s+\\.ar-", line)) {
      # indented .ar- in @media
      indent <- sub("^(\\s+).*", "\\1", line)
      rest <- sub("^\\s+", "", line)
      result[i] <- paste0(indent, prefix, " ", rest)
    } else if (in_media && grepl("^\\s+body\\s*\\{", line)) {
      indent <- sub("^(\\s+).*", "\\1", line)
      result[i] <- paste0(indent, prefix, " {")
    } else {
      result[i] <- line
    }
  }

  paste0(result, collapse = "\n")
}


# ══════════════════════════════════════════════════════════════════════════════
# Premium Embedded CSS
# ══════════════════════════════════════════════════════════════════════════════

#' Build premium CSS string from spec page settings
#' @noRd
html_embedded_css <- function(spec, viewer = FALSE) {
  page <- spec$page
  font_family <- page$font_family
  font_size <- page$font_size

  # Build a system font stack based on the configured font
  font_stack <- html_font_stack(font_family)

  # Full page dimensions (for the white card to simulate a real page)
  dims <- paper_dims_twips(page$paper, page$orientation)
  full_page_w <- twips_to_inches(dims[["width"]])
  full_page_h <- twips_to_inches(dims[["height"]])

  ml <- page$margins$left
  mr <- page$margins$right
  mt <- page$margins$top
  mb <- page$margins$bottom

  # Line height calculation: slightly tighter for monospace data
  lh <- "1.35"

  # Printable content width (used for viewer/knitr fixed-width layout)
  printable <- printable_area_inches(page)
  pw <- printable[["width"]]

  # Viewer mode: clean layout for IDE panels (no page simulation)
  if (viewer) {
    body_css <- paste0(
      "body {\n",
      "  background: white;\n",
      "  margin: 0;\n",
      "  padding: 12px 16px;\n",
      "  -webkit-font-smoothing: antialiased;\n",
      "  -moz-osx-font-smoothing: grayscale;\n",
      "}\n"
    )
    page_css <- paste0(
      ".ar-page {\n",
      "  width: ",
      pw,
      "in;\n",
      "  margin: 0 auto;\n",
      "  padding: 0;\n",
      "  display: flex;\n",
      "  flex-direction: column;\n"
    )
  } else {
    body_css <- paste0(
      "body {\n",
      "  background: #edf2f7;\n",
      "  margin: 0;\n",
      "  padding: 40px 20px;\n",
      "  -webkit-font-smoothing: antialiased;\n",
      "  -moz-osx-font-smoothing: grayscale;\n",
      "}\n"
    )
    page_css <- paste0(
      ".ar-page {\n",
      "  width: ",
      full_page_w,
      "in;\n",
      "  min-height: ",
      full_page_h,
      "in;\n",
      "  margin: 0 auto;\n",
      "  padding: ",
      mt,
      "in ",
      mr,
      "in ",
      mb,
      "in ",
      ml,
      "in;\n",
      "  background: #ffffff;\n",
      "  box-shadow: 0 1px 3px rgba(0,0,0,0.08), 0 4px 12px rgba(0,0,0,0.04);\n",
      "  border-radius: 2px;\n",
      "  display: flex;\n",
      "  flex-direction: column;\n"
    )
  }

  paste0(
    # ── Page Canvas ──
    "* { margin: 0; padding: 0; box-sizing: border-box; }\n",
    body_css,
    page_css,
    "  font-family: ",
    font_stack,
    ";\n",
    "  font-size: ",
    font_size,
    "pt;\n",
    "  color: #1a202c;\n",
    "  line-height: ",
    lh,
    ";\n",
    "}\n",

    # ── Page Chrome (header/footer) ──
    ".ar-chrome {\n",
    "  display: flex;\n",
    "  justify-content: space-between;\n",
    "  align-items: flex-end;\n",
    "  color: #718096;\n",
    "  font-size: ",
    max(font_size - 1, 6),
    "pt;\n",
    "  padding: 0 0 4px 0;\n",
    "}\n",
    ".ar-chrome-left { text-align: left; }\n",
    ".ar-chrome-center { text-align: center; flex: 1; }\n",
    ".ar-chrome-right { text-align: right; }\n",

    # ── Titles ──
    ".ar-titles {\n",
    "  margin-bottom: 0.4em;\n",
    "}\n",
    ".ar-title {\n",
    "  margin: 0;\n",
    "  padding: 1px 0;\n",
    "  line-height: 1.4;\n",
    "}\n",

    # ── Table ──
    ".ar-table {\n",
    "  border-collapse: collapse;\n",
    "  table-layout: fixed;\n",
    "  width: 100%;\n",
    "  font-variant-numeric: tabular-nums;\n",
    "}\n",
    ".ar-table thead th {\n",
    "  font-weight: 600;\n",
    "  vertical-align: bottom;\n",
    "  padding: 3px 4px;\n",
    "  white-space: pre-wrap;\n",
    "  word-wrap: break-word;\n",
    "}\n",
    ".ar-table tbody td {\n",
    "  vertical-align: top;\n",
    "  padding: 1.5px 4px;\n",
    "  white-space: pre-wrap;\n",
    "  word-wrap: break-word;\n",
    "}\n",
    ".ar-table .ar-blank td {\n",
    "  padding: 0;\n",
    "  height: 0.8em;\n",
    "  border: none !important;\n",
    "}\n",

    # ── Page-by Labels ──
    ".ar-page-by {\n",
    "  font-weight: 600;\n",
    "  padding: 4px 0;\n",
    "  margin-bottom: 0.3em;\n",
    "}\n",

    # ── Footnotes ──
    ".ar-footnotes {\n",
    "  margin-top: 0.4em;\n",
    "}\n",
    ".ar-fn-sep {\n",
    "  border: none;\n",
    "  border-top: 0.5pt solid #a0aec0;\n",
    "  margin: 4px 0;\n",
    "}\n",
    ".ar-footnote {\n",
    "  margin: 0;\n",
    "  padding: 1px 0;\n",
    "  color: #4a5568;\n",
    "}\n",

    # ── Section Layout ──
    ".ar-section {\n",
    "  display: flex;\n",
    "  flex-direction: column;\n",
    "  flex: 1;\n",
    "}\n",
    ".ar-chrome-foot {\n",
    "  margin-top: auto;\n",
    "  padding-top: 0.5in;\n",
    "}\n",

    # ── Section Breaks ──
    ".ar-section + .ar-section {\n",
    "  margin-top: 32px;\n",
    "  padding-top: 24px;\n",
    "  border-top: 1.5px dashed #cbd5e0;\n",
    "}\n",

    # ── Print ──
    "@media print {\n",
    "  body { background: white; padding: 0; }\n",
    "  .ar-page { box-shadow: none; border-radius: 0; padding: 0; margin: 0; width: auto; min-height: auto; }\n",
    "  .ar-section + .ar-section {\n",
    "    page-break-before: always;\n",
    "    border-top: none;\n",
    "    margin-top: 0;\n",
    "    padding-top: 0;\n",
    "  }\n",
    "  @page {\n",
    "    size: ",
    page$paper,
    " ",
    page$orientation,
    ";\n",
    "    margin: ",
    mt,
    "in ",
    mr,
    "in ",
    mb,
    "in ",
    ml,
    "in;\n",
    "  }\n",
    "}\n"
  )
}


#' Build a CSS font stack from the configured font family
#' @noRd
html_font_stack <- function(font_family) {
  # Detect font classification
  fam_info <- classify_font_family(font_family)

  if (fam_info == "modern") {
    # Monospace stack
    paste0(
      "\"",
      font_family,
      "\", ",
      "\"Courier New\", Consolas, \"Liberation Mono\", monospace"
    )
  } else if (fam_info == "swiss") {
    paste0(
      "\"",
      font_family,
      "\", ",
      "Arial, Helvetica, \"Liberation Sans\", sans-serif"
    )
  } else {
    paste0(
      "\"",
      font_family,
      "\", ",
      "\"Times New Roman\", Times, \"Liberation Serif\", serif"
    )
  }
}


#' Classify a font family name into modern/swiss/roman
#' @noRd
classify_font_family <- function(font_family) {
  for (fam_name in names(fr_env$fonts)) {
    if (font_family %in% fr_env$fonts[[fam_name]]$names) {
      return(fam_name)
    }
  }
  "modern" # default to monospace for pharma
}


# ══════════════════════════════════════════════════════════════════════════════
# Section Builder
# ══════════════════════════════════════════════════════════════════════════════

#' Build one HTML section (page_group x panel)
#' @noRd
html_section <- function(
  spec,
  group,
  vis_columns,
  cell_grid,
  borders,
  label_overrides,
  span_overrides,
  token_map,
  panel_idx,
  is_last
) {
  parts <- character(0)

  # Pagehead
  ph <- html_pagehead(spec, token_map)
  if (nzchar(ph)) {
    parts <- c(parts, ph)
  }

  # Titles
  titles <- html_titles(spec, panel_idx)
  if (nzchar(titles)) {
    parts <- c(parts, titles)
  }

  # Page-by label
  if (
    !is.null(group$group_label) &&
      nzchar(group$group_label) &&
      isTRUE(spec$body$page_by_visible %||% TRUE)
  ) {
    content <- html_escape_and_resolve(group$group_label)
    pb_bold <- if (isTRUE(spec$body$page_by_bold)) {
      " style=\"font-weight:bold\""
    } else {
      ""
    }
    parts <- c(
      parts,
      paste0("<div class=\"ar-page-by\"", pb_bold, ">", content, "</div>")
    )
  }

  # Table
  tbl <- html_table(
    spec,
    group$data,
    vis_columns,
    cell_grid,
    borders,
    label_overrides,
    span_overrides
  )
  parts <- c(parts, tbl)

  # Footnotes
  fn <- html_footnotes(spec, is_last)
  if (nzchar(fn)) {
    parts <- c(parts, fn)
  }

  # Pagefoot
  pf <- html_pagefoot(spec, token_map)
  if (nzchar(pf)) {
    parts <- c(parts, pf)
  }

  paste0(
    "<section class=\"ar-section\">\n",
    paste0(parts, collapse = "\n"),
    "\n</section>"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# Titles
# ══════════════════════════════════════════════════════════════════════════════

#' Build title divs
#' @noRd
html_titles <- function(spec, panel_idx = 1L) {
  titles <- spec$meta$titles
  if (length(titles) == 0L) {
    return("")
  }

  continuation <- spec$page$continuation

  lines <- vapply(
    seq_along(titles),
    function(idx) {
      entry <- titles[[idx]]
      fs <- entry$font_size %||%
        spec$meta$title_font_size %||%
        spec$page$font_size
      align <- entry$align %||% spec$meta$title_align %||% "center"
      entry_bold <- entry$bold %||% spec$meta$title_bold

      content <- html_escape_and_resolve(entry$content)
      # Newlines in title content
      content <- gsub("\n", "<br>", content, fixed = TRUE)

      # Continuation on panel 2+
      if (idx == 1L && !is.null(continuation) && panel_idx > 1L) {
        content <- paste0(content, " ", html_escape(continuation))
      }

      style_parts <- character(0)
      style_parts <- c(style_parts, paste0("text-align:", align))
      if (fs != spec$page$font_size) {
        style_parts <- c(style_parts, paste0("font-size:", fs, "pt"))
      }
      if (isTRUE(entry_bold)) {
        style_parts <- c(style_parts, "font-weight:bold")
      }

      style_str <- if (length(style_parts) > 0L) {
        paste0(" style=\"", paste0(style_parts, collapse = ";"), "\"")
      } else {
        ""
      }

      paste0("<div class=\"ar-title\"", style_str, ">", content, "</div>")
    },
    character(1)
  )

  paste0(
    "<div class=\"ar-titles\">\n",
    paste0(lines, collapse = "\n"),
    "\n</div>"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# Table
# ══════════════════════════════════════════════════════════════════════════════

#' Build the full HTML table element
#' @noRd
html_table <- function(
  spec,
  data,
  vis_columns,
  cell_grid,
  borders,
  label_overrides,
  span_overrides
) {
  col_names <- names(vis_columns)
  ncol <- length(col_names)

  # Colgroup
  colgroup <- paste0(
    "<colgroup>\n",
    paste0(
      vapply(
        vis_columns,
        function(c) {
          paste0("<col style=\"width:", round(c$width, 4), "in\">")
        },
        character(1)
      ),
      collapse = "\n"
    ),
    "\n</colgroup>"
  )

  # Header
  thead_rows <- character(0)

  # Spanning header rows
  span_rows <- html_spanner_rows(spec, vis_columns, borders, span_overrides)
  if (length(span_rows) > 0L) {
    thead_rows <- c(thead_rows, span_rows)
  }

  # Column header row
  col_hdr <- html_col_header_row(spec, vis_columns, borders, label_overrides)
  thead_rows <- c(thead_rows, col_hdr)

  thead <- paste0(
    "<thead>\n",
    paste0(thead_rows, collapse = "\n"),
    "\n</thead>"
  )

  # Body
  tbody <- html_body_rows(data, vis_columns, cell_grid, borders, spec)

  paste0(
    "<table class=\"ar-table\">\n",
    colgroup,
    "\n",
    thead,
    "\n",
    tbody,
    "\n</table>"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# Spanning Header Rows
# ══════════════════════════════════════════════════════════════════════════════

#' Build spanner row(s) as HTML <tr> elements
#' @noRd
html_spanner_rows <- function(spec, vis_columns, borders, span_overrides) {
  spans <- spec$header$spans
  if (length(spans) == 0L) {
    return(character(0))
  }

  col_names <- names(vis_columns)
  ncol <- length(col_names)
  levels <- sort(unique(vapply(spans, function(s) s$level, integer(1))))

  rows <- character(0)
  for (lvl in rev(levels)) {
    lvl_spans <- Filter(function(s) s$level == lvl, spans)

    cells <- character(0)
    j <- 1L
    while (j <= ncol) {
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
        colspan <- length(sp_cols)
        sp_end <- match(sp_cols[length(sp_cols)], col_names)

        span_label <- matching_span$label
        if (!is.null(span_overrides)) {
          ov <- span_overrides[matching_span$label]
          if (!is.na(ov)) span_label <- ov
        }
        content <- html_escape_and_resolve(span_label)
        content <- gsub("\n", "<br>", content, fixed = TRUE)

        # Border on spanning header
        border_css <- html_span_border_css(
          matching_span,
          borders$header,
          j,
          sp_end
        )

        style_str <- paste0(
          "text-align:center",
          if (nzchar(border_css)) paste0(";", border_css) else ""
        )

        cells <- c(
          cells,
          paste0(
            "<th colspan=\"",
            colspan,
            "\" style=\"",
            style_str,
            "\">",
            content,
            "</th>"
          )
        )
        j <- sp_end + 1L
      } else {
        # Empty cell (no span)
        cells <- c(cells, "<th></th>")
        j <- j + 1L
      }
    }

    rows <- c(rows, paste0("<tr>", paste0(cells, collapse = ""), "</tr>"))
  }

  rows
}


#' Build border CSS for a spanning header cell
#' @noRd
html_span_border_css <- function(span, header_borders, j_start, j_end) {
  parts <- character(0)

  # Bottom border from span hline

  if (isTRUE(span$hline)) {
    parts <- c(parts, "border-bottom:0.75pt solid #000000")
  }

  # Resolved borders from rules (top of first header row for the span range)
  for (jj in j_start:j_end) {
    bs <- header_borders$top[1L, jj][[1L]]
    if (!is.null(bs) && jj == j_start) {
      parts <- c(parts, paste0("border-top:", html_border_value(bs)))
    }
  }

  paste0(parts, collapse = ";")
}


# ══════════════════════════════════════════════════════════════════════════════
# Column Header Row
# ══════════════════════════════════════════════════════════════════════════════

#' Build the column header <tr>
#' @noRd
html_col_header_row <- function(spec, vis_columns, borders, label_overrides) {
  col_names <- names(vis_columns)
  ncol <- length(col_names)
  h_row <- 1L + n_spanner_levels(spec$header$spans)

  header_valign <- spec$header$valign %||% "bottom"
  hgrid <- build_header_cell_grid(
    vis_columns,
    spec$cell_styles,
    spec$page,
    h_row,
    default_valign = header_valign,
    header_cfg = spec$header
  )

  cells <- vapply(
    seq_len(ncol),
    function(j) {
      g <- hgrid[j, ]

      label <- label_overrides[col_names[j]]
      if (is.na(label) || is.null(label)) {
        label <- vis_columns[[j]]$label
        if (
          (is.null(label) || !nzchar(label)) && !isTRUE(vis_columns[[j]]$is_gap)
        ) {
          label <- col_names[j]
        }
      }
      content <- html_escape_and_resolve(label %||% "")
      content <- gsub("\n", "<br>", content, fixed = TRUE)

      style_parts <- character(0)
      style_parts <- c(style_parts, paste0("text-align:", g$align))
      style_parts <- c(style_parts, paste0("vertical-align:", g$valign))

      if (isTRUE(g$bold)) {
        style_parts <- c(style_parts, "font-weight:bold")
      }
      if (isTRUE(g$italic)) {
        style_parts <- c(style_parts, "font-style:italic")
      }
      if (isTRUE(g$underline)) {
        style_parts <- c(style_parts, "text-decoration:underline")
      }
      if (!is.na(g$fg) && g$fg != "#000000") {
        style_parts <- c(style_parts, paste0("color:", g$fg))
      }
      if (!is.na(g$bg) && nzchar(g$bg)) {
        style_parts <- c(style_parts, paste0("background-color:", g$bg))
      }
      if (g$font_size != spec$page$font_size) {
        style_parts <- c(style_parts, paste0("font-size:", g$font_size, "pt"))
      }

      # Borders
      border_css <- html_cell_border_css(borders$header, h_row, j)
      if (nzchar(border_css)) {
        style_parts <- c(style_parts, border_css)
      }

      paste0(
        "<th style=\"",
        paste0(style_parts, collapse = ";"),
        "\">",
        content,
        "</th>"
      )
    },
    character(1)
  )

  paste0("<tr>", paste0(cells, collapse = ""), "</tr>")
}


# ══════════════════════════════════════════════════════════════════════════════
# Body Rows
# ══════════════════════════════════════════════════════════════════════════════

#' Build HTML body rows
#' @noRd
html_body_rows <- function(data, vis_columns, cell_grid, borders, spec) {
  nr <- nrow(data)
  if (nr == 0L) {
    return("<tbody></tbody>")
  }

  col_names <- names(vis_columns)
  ncol <- length(col_names)

  # Detect blank rows
  is_blank <- detect_blank_rows(data)

  # Pre-extract cell_grid columns
  cg_align <- cell_grid$align
  cg_content <- cell_grid$content
  cg_bold <- cell_grid$bold
  cg_italic <- cell_grid$italic
  cg_underline <- cell_grid$underline
  cg_fg <- cell_grid$fg
  cg_bg <- cell_grid$bg
  cg_indent <- cell_grid$indent
  cg_font_size <- cell_grid$font_size
  cg_valign <- cell_grid$valign

  # Decimal geometry
  dec_geom <- spec$decimal_geometry
  is_decimal_col <- col_names %in% names(dec_geom %||% list())

  rows <- vector("list", nr)
  for (i in seq_len(nr)) {
    if (is_blank[i]) {
      rows[[i]] <- paste0(
        "<tr class=\"ar-blank\">",
        paste0(rep("<td></td>", ncol), collapse = ""),
        "</tr>"
      )
      next
    }

    cells <- vector("list", ncol)
    for (j in seq_len(ncol)) {
      idx <- (j - 1L) * nr + i

      style_parts <- character(0)
      style_parts <- c(style_parts, paste0("text-align:", cg_align[idx]))

      if (cg_valign[idx] != "top") {
        style_parts <- c(style_parts, paste0("vertical-align:", cg_valign[idx]))
      }
      if (isTRUE(cg_bold[idx])) {
        style_parts <- c(style_parts, "font-weight:bold")
      }
      if (isTRUE(cg_italic[idx])) {
        style_parts <- c(style_parts, "font-style:italic")
      }
      if (isTRUE(cg_underline[idx])) {
        style_parts <- c(style_parts, "text-decoration:underline")
      }
      if (!is.na(cg_fg[idx]) && cg_fg[idx] != "#000000") {
        style_parts <- c(style_parts, paste0("color:", cg_fg[idx]))
      }
      if (!is.na(cg_bg[idx]) && nzchar(cg_bg[idx])) {
        style_parts <- c(style_parts, paste0("background-color:", cg_bg[idx]))
      }
      if (cg_indent[idx] > 0) {
        style_parts <- c(
          style_parts,
          paste0("padding-left:", cg_indent[idx], "in")
        )
      }
      if (cg_font_size[idx] != spec$page$font_size) {
        style_parts <- c(
          style_parts,
          paste0("font-size:", cg_font_size[idx], "pt")
        )
      }

      # Borders
      border_css <- html_cell_border_css(borders$body, i, j)
      if (nzchar(border_css)) {
        style_parts <- c(style_parts, border_css)
      }

      # Content: decimal or regular
      if (identical(cg_align[idx], "decimal") && is_decimal_col[j]) {
        geom <- dec_geom[[col_names[j]]]
        formatted <- geom$formatted[i]
        if (nzchar(trimws(formatted))) {
          content <- html_escape_and_resolve(formatted)
          content <- gsub("\n", "<br>", content, fixed = TRUE)
          # Decimal-aligned cells use pre-formatted whitespace
          style_parts <- c(style_parts, "white-space:pre")
        } else {
          content <- ""
        }
      } else {
        content <- html_escape_and_resolve(cg_content[idx])
        content <- gsub("\n", "<br>", content, fixed = TRUE)
      }

      cells[[j]] <- paste0(
        "<td style=\"",
        paste0(style_parts, collapse = ";"),
        "\">",
        content,
        "</td>"
      )
    }

    rows[[i]] <- paste0("<tr>", paste0(unlist(cells), collapse = ""), "</tr>")
  }

  paste0("<tbody>\n", paste0(rows, collapse = "\n"), "\n</tbody>")
}


# ══════════════════════════════════════════════════════════════════════════════
# Footnotes
# ══════════════════════════════════════════════════════════════════════════════

#' Build footnote divs
#' @noRd
html_footnotes <- function(spec, is_last) {
  footnotes <- spec$meta$footnotes %||% list()
  if (length(footnotes) == 0L) {
    return("")
  }

  # On final section, show all; otherwise only "every"
  entries <- if (is_last) {
    footnotes
  } else {
    split_footnotes(footnotes)$every
  }

  if (length(entries) == 0L) {
    return("")
  }

  lines <- character(0)

  # Separator
  if (isTRUE(spec$meta$footnote_separator)) {
    lines <- c(lines, "<hr class=\"ar-fn-sep\">")
  }

  for (fn in entries) {
    fs <- fn$font_size %||% spec$page$font_size
    align <- fn$align %||% "left"
    content <- html_escape_and_resolve(fn$content)
    content <- gsub("\n", "<br>", content, fixed = TRUE)

    style_parts <- character(0)
    if (align != "left") {
      style_parts <- c(style_parts, paste0("text-align:", align))
    }
    if (fs != spec$page$font_size) {
      style_parts <- c(style_parts, paste0("font-size:", fs, "pt"))
    }

    style_str <- if (length(style_parts) > 0L) {
      paste0(" style=\"", paste0(style_parts, collapse = ";"), "\"")
    } else {
      ""
    }

    lines <- c(
      lines,
      paste0("<div class=\"ar-footnote\"", style_str, ">", content, "</div>")
    )
  }

  paste0(
    "<div class=\"ar-footnotes\">\n",
    paste0(lines, collapse = "\n"),
    "\n</div>"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# Page Chrome (Pagehead / Pagefoot)
# ══════════════════════════════════════════════════════════════════════════════

#' Build pagehead flex div
#' @noRd
html_pagehead <- function(spec, token_map) {
  html_chrome_div(spec$pagehead, spec, token_map, "page header")
}

#' Build pagefoot flex div (wrapped in ar-chrome-foot for bottom push)
#' @noRd
html_pagefoot <- function(spec, token_map) {
  inner <- html_chrome_div(spec$pagefoot, spec, token_map, "page footer")
  if (!nzchar(inner)) {
    return("")
  }
  paste0("<div class=\"ar-chrome-foot\">\n", inner, "\n</div>")
}

#' Build an L/C/R chrome div
#' @noRd
html_chrome_div <- function(chrome, spec, token_map, context) {
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
    txt <- html_escape_and_resolve(txt)
    gsub("\n", "<br>", txt, fixed = TRUE)
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


# ══════════════════════════════════════════════════════════════════════════════
# Border Helpers
# ══════════════════════════════════════════════════════════════════════════════

#' Build inline CSS border declarations for a cell
#' @noRd
html_cell_border_css <- function(border_matrices, i, j) {
  sides <- c("top", "bottom", "left", "right")
  parts <- character(0)

  for (side in sides) {
    mat <- border_matrices[[side]]
    if (i > nrow(mat) || j > ncol(mat)) {
      next
    }
    bs <- mat[i, j][[1L]]
    if (is.null(bs)) {
      next
    }
    parts <- c(parts, paste0("border-", side, ":", html_border_value(bs)))
  }

  paste0(parts, collapse = ";")
}


#' Convert a border spec to a CSS value string
#' @noRd
html_border_value <- function(bs) {
  style <- html_linestyle(bs$linestyle)
  paste0(bs$width, "pt ", style, " ", bs$fg)
}


#' Map arframe linestyle to CSS border-style
#' @noRd
html_linestyle <- function(ls) {
  switch(
    ls %||% "solid",
    solid = "solid",
    dashed = "dashed",
    dotted = "dotted",
    double = "double",
    dashdot = "dashed",
    "solid"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# Text Escaping & Sentinel Resolution
# ══════════════════════════════════════════════════════════════════════════════

#' Escape text for HTML output
#'
#' Escapes HTML special characters: &, <, >, "
#'
#' @param text Character vector.
#' @return Character vector with HTML-safe text.
#' @noRd
html_escape <- function(text) {
  if (length(text) == 0L) {
    return(character(0))
  }
  text <- stringi::stri_replace_all_fixed(text, "&", "&amp;")
  text <- stringi::stri_replace_all_fixed(text, "<", "&lt;")
  text <- stringi::stri_replace_all_fixed(text, ">", "&gt;")
  text <- stringi::stri_replace_all_fixed(text, "\"", "&quot;")
  text
}


#' Resolve a sentinel token to HTML markup
#'
#' Formatting types recursively escape their content via
#' `html_escape_and_resolve()` so that nested sentinels and HTML specials
#' are properly handled.
#' @noRd
html_sentinel_resolver <- function(type, content) {
  switch(
    toupper(type),
    "SUPER" = paste0("<sup>", html_escape_and_resolve(content), "</sup>"),
    "SUB" = paste0("<sub>", html_escape_and_resolve(content), "</sub>"),
    "BOLD" = paste0("<b>", html_escape_and_resolve(content), "</b>"),
    "ITALIC" = paste0("<em>", html_escape_and_resolve(content), "</em>"),
    "UNDERLINE" = paste0("<u>", html_escape_and_resolve(content), "</u>"),
    "NEWLINE" = "<br>",
    "UNICODE" = content, # HTML is UTF-8 — pass through
    content
  )
}


#' Escape AND resolve sentinels for HTML
#'
#' Splits text around sentinel markers, escapes non-sentinel parts,
#' then resolves sentinels to HTML tags.
#'
#' @param text Character scalar.
#' @return Character scalar with HTML-safe text and resolved sentinels.
#' @noRd
html_escape_and_resolve <- function(text) {
  escape_and_resolve(text, html_escape, html_sentinel_resolver)
}
