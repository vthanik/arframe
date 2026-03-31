# ──────────────────────────────────────────────────────────────────────────────
# paginate.R — R-side row height calculation
#
# Computes per-row heights (in twips) for the page budget calculation used by
# build_keep_mask() in render-common.R. The actual keep-together logic is
# handled by RTF-native \keepn + \trkeep (render-rtf.R), not by R-side page
# break insertion.
#
# Architecture: Two-pass columnar scanner
#   Pass 1 (vectorized): Fast scan to identify multi-line candidates
#   Pass 2 (targeted): Expensive stri_wrap() only on candidates
# ──────────────────────────────────────────────────────────────────────────────

#' Compute available character width per column
#'
#' @param columns Named list of fr_col objects (visible only).
#' @param page fr_page object.
#' @return Named integer vector — character capacity per column.
#' @noRd
compute_column_char_widths <- function(columns, page) {
  # Approximate: column width in inches * characters-per-inch for the font
  # Use AFM space width as the average character width
  space_twips <- measure_text_width_twips(" ", page$font_family, page$font_size)
  vapply(
    columns,
    function(col) {
      col_twips <- inches_to_twips(col$width)
      max(1L, as.integer(col_twips / space_twips))
    },
    integer(1)
  )
}


#' Measure the height (in lines) of a single cell
#'
#' @param text Character scalar. Cell text content.
#' @param char_width Integer. Column character width capacity.
#' @return Integer. Number of lines this cell needs.
#' @noRd
measure_cell_height <- function(text, char_width) {
  if (!nzchar(text)) {
    return(1L)
  }

  # Split by explicit newlines first
  parts <- strsplit(text, "\n", fixed = TRUE)[[1L]]

  total <- 0L
  for (part in parts) {
    if (!nzchar(part)) {
      total <- total + 1L
      next
    }
    # Estimate line wrapping
    wrapped <- stringi::stri_wrap(part, width = char_width, simplify = TRUE)
    total <- total + length(wrapped)
  }

  max(1L, total)
}


#' Calculate row heights for a data frame (columnar two-pass scanner)
#'
#' Pass 1: Vectorized detection of multi-line candidates (newlines or wide text).
#' Pass 2: Expensive `stri_wrap()` only on candidate cells.
#'
#' Returns heights in **twips** (not line counts). Each row's height is
#' `max(lines_across_columns) * row_height_twips(font_size)`, giving exact
#' vertical space consumed — matching tabularray's approach.
#'
#' @param data Data frame (body data for one page group).
#' @param columns Named list of fr_col objects (visible only).
#' @param page fr_page object.
#' @return Integer vector of length `nrow(data)` — height in twips per row.
#' @noRd
compute_row_heights <- function(data, columns, page) {
  nr <- vctrs::vec_size(data)
  if (nr == 0L) {
    return(integer(0))
  }

  line_counts <- rep(1L, nr)
  col_chars <- compute_column_char_widths(columns, page)

  for (j in seq_along(columns)) {
    nm <- names(columns)[j]
    content <- data[[nm]]
    if (!is.character(content)) {
      next
    }

    # Strip sentinel tokens so width estimation uses visible text only
    content <- sentinel_to_plain_vec(content)

    # Pass 1: Vectorized candidate detection
    nl_count <- stringi::stri_count_fixed(content, "\n")
    char_count <- nchar(content)
    candidates <- which(nl_count > 0L | char_count > col_chars[j])
    if (length(candidates) == 0L) {
      next
    }

    # Pass 2: Expensive wrap only for candidates
    for (i in candidates) {
      h <- measure_cell_height(content[i], col_chars[j])
      line_counts[i] <- max(line_counts[i], h)
    }
  }

  # Convert line counts to twips
  one_row <- row_height_twips(page$font_size)
  as.integer(line_counts * one_row)
}


#' Calculate page budget in twips
#'
#' Returns the available vertical space for body rows in twips, accounting
#' for titles, column headers, spanning headers, footnotes, page chrome.
#' This is the tabularray-style approach: track exact twips remaining
#' rather than approximate line counts.
#'
#' @param spec A finalized fr_spec object.
#' @return Integer. Available body height in twips.
#' @noRd
compute_page_budget <- function(spec) {
  # Page height in twips
  page_dims <- paper_dims_twips(spec$page$paper, spec$page$orientation)
  page_height <- page_dims[["height"]]

  # Margins (top + bottom)
  margins <- spec$page$margins
  margin_top <- inches_to_twips(margins$top)
  margin_bottom <- inches_to_twips(margins$bottom)

  # Row height for one line at current font size
  one_row_twips <- row_height_twips(spec$page$font_size)

  # Column header rows: scan labels for embedded newlines
  col_labels <- vapply(
    spec$columns,
    function(col) col$label %||% "",
    character(1)
  )
  label_line_counts <- vapply(
    col_labels,
    function(lbl) length(strsplit(lbl, "\n", fixed = TRUE)[[1L]]),
    integer(1)
  )
  max_header_lines <- max(1L, max(label_line_counts))
  n_spanners <- n_spanner_levels(spec$header$spans)
  n_header_rows <- max_header_lines + n_spanners

  # Titles and spacing
  n_titles <- length(spec$meta$titles %||% list())
  titles_after <- spec$spacing$titles_after %||% 1L
  footnotes_before <- spec$spacing$footnotes_before %||% 1L

  # Separate footnotes by placement
  all_footnotes <- spec$meta$footnotes %||% list()
  n_footnotes_last <- sum(vapply(
    all_footnotes,
    function(fn) fn$placement == "last",
    logical(1)
  ))
  n_footnotes_every <- sum(vapply(
    all_footnotes,
    function(fn) fn$placement == "every",
    logical(1)
  ))

  # Chrome rows: titles + header + last-placement footnotes (in body area)
  chrome_rows <- n_titles +
    titles_after +
    n_header_rows +
    n_footnotes_last +
    if (n_footnotes_last > 0L) footnotes_before else 0L

  # Footer overflow: every-placement footnotes + pagefoot live in margin area
  footer_lines <- n_footnotes_every +
    (if (n_footnotes_every > 0L) footnotes_before else 0L) +
    (if (!is.null(spec$pagefoot)) max_chrome_lines(spec$pagefoot) else 0L)
  footer_twips <- footer_lines * one_row_twips
  footer_overflow <- max(0L, footer_twips - margin_bottom)

  # Header overflow: pagehead lives in margin area
  header_lines <- if (!is.null(spec$pagehead)) {
    max_chrome_lines(spec$pagehead)
  } else {
    0L
  }
  header_twips <- header_lines * one_row_twips
  header_overflow <- max(0L, header_twips - margin_top)

  # Available height for body in twips
  available_twips <- page_height -
    margin_top -
    margin_bottom -
    chrome_rows * one_row_twips -
    footer_overflow -
    header_overflow

  # Minimum 5 rows worth of space
  min_twips <- 5L * one_row_twips
  as.integer(max(min_twips, available_twips))
}


#' Compute page break positions and skip rows for oversized groups
#'
#' Twips-based group-aware pagination for groups that exceed the page budget.
#' Returns page break row indices and suppressed blank row indices.
#' This function is only called for oversized groups; normal groups are kept
#' together via RTF-native `\keepn` + `\trkeep` (see `build_keep_mask()`).
#'
#' Groups smaller than the page budget are **never split** across pages —
#' they are handled entirely by `build_keep_mask()`. Groups larger than one
#' page are split here with orphan/widow protection.
#'
#' Blank suppression: trailing blanks at page bottom and leading blanks
#' at page top are added to `skip_rows` (not rendered).
#'
#' @param row_heights Integer vector — height in twips per body row.
#' @param budget Integer — available body height in twips per page.
#' @param data Data frame (for group key computation).
#' @param group_by Character vector of grouping column names.
#' @param orphan_min Integer. Min rows to keep at bottom of page.
#' @param widow_min Integer. Min rows to carry to next page.
#' @return List with:
#'   - `page_breaks`: integer vector of row indices where page breaks occur
#'   - `skip_rows`: integer vector of row indices to suppress (blank rows at
#'     page boundaries)
#'   - `n_pages`: total page count (1 + length(page_breaks))
#' @noRd
paginate_rows <- function(
  row_heights,
  budget,
  data,
  group_by,
  orphan_min = 3L,
  widow_min = 3L
) {
  nr <- length(row_heights)
  if (nr == 0L) {
    return(list(page_breaks = integer(0), skip_rows = integer(0), n_pages = 0L))
  }

  page_breaks_list <- list()
  skip_rows_list <- list()
  used <- 0L

  # Deferred blank accounting: trailing blanks between groups are not counted
  # toward `used` immediately. At the next group, we check whether the pending
  # blank + group fit. If only the group fits, the blank is dropped (invisible
  # at a page boundary). This avoids wasting 1 row of budget per group.
  pending_blank_idx <- NULL
  pending_blank_height <- 0L

  # Identify blank rows and group boundaries
  is_blank <- detect_blank_rows(data)
  group_by <- intersect(group_by, names(data))

  if (length(group_by) > 0L) {
    keys <- build_group_keys(data, group_by)
  } else {
    keys <- rep("__all__", nr)
  }

  # Find group start positions
  is_header <- !is_blank & c(TRUE, keys[-1L] != keys[-length(keys)])
  header_positions <- which(is_header)

  for (g_idx in seq_along(header_positions)) {
    g_start <- header_positions[g_idx]
    g_end <- if (g_idx < length(header_positions)) {
      header_positions[g_idx + 1L] - 1L
    } else {
      nr
    }

    # Skip trailing blanks from group end
    while (g_end > g_start && is_blank[g_end]) {
      g_end <- g_end - 1L
    }

    group_rows <- g_start:g_end
    group_height <- sum(row_heights[group_rows])

    # Deferred blank logic: 4 branches
    if (used + pending_blank_height + group_height <= budget) {
      # Branch 1: pending blank + group both fit on current page
      used <- used + pending_blank_height + group_height
    } else if (used + group_height <= budget) {
      # Branch 2: group fits without blank — suppress blank at boundary
      if (!is.null(pending_blank_idx)) {
        skip_rows_list[[length(skip_rows_list) + 1L]] <- pending_blank_idx
      }
      used <- used + group_height
    } else if (group_height <= budget) {
      # Branch 3: group doesn't fit current page — move to next page
      page_breaks_list[[length(page_breaks_list) + 1L]] <- g_start
      used <- group_height
    } else {
      # Branch 4: group larger than one page — split with orphan/widow
      # control and blank suppression at page boundaries.
      if (used > 0L) {
        # Start group on a fresh page
        page_breaks_list[[length(page_breaks_list) + 1L]] <- g_start
        used <- 0L
      }

      n_group <- length(group_rows)
      gi <- 1L # group-local index

      while (gi <= n_group) {
        # --- Skip leading blank rows at page start (suppress) ---
        while (gi <= n_group && is_blank[group_rows[gi]] && used == 0L) {
          skip_rows_list[[length(skip_rows_list) + 1L]] <- group_rows[gi]
          gi <- gi + 1L
        }
        if (gi > n_group) {
          break
        }

        # --- Greedy scan: find how many rows fit on this page ---
        scan_end <- gi
        scan_used <- 0L
        while (scan_end <= n_group) {
          h <- row_heights[group_rows[scan_end]]
          if (scan_used + h > budget && scan_end > gi) {
            break
          }
          scan_used <- scan_used + h
          scan_end <- scan_end + 1L
        }
        # [gi, scan_end - 1] fit on this page

        # If everything remaining fits, done with this group
        if (scan_end > n_group) {
          used <- scan_used
          break
        }

        # --- Trim trailing blank rows from this page's slice ---
        trimmed_end <- scan_end - 1L
        while (trimmed_end > gi && is_blank[group_rows[trimmed_end]]) {
          trimmed_end <- trimmed_end - 1L
        }

        # --- Widow/orphan check ---
        n_data_remaining <- sum(
          !is_blank[group_rows[(trimmed_end + 1L):n_group]]
        )
        n_data_on_page <- sum(!is_blank[group_rows[gi:trimmed_end]])

        if (n_data_remaining > 0L && n_data_remaining < widow_min) {
          steal <- widow_min - n_data_remaining
          if (n_data_on_page - steal >= orphan_min) {
            stolen <- 0L
            while (stolen < steal && trimmed_end > gi) {
              if (!is_blank[group_rows[trimmed_end]]) {
                stolen <- stolen + 1L
              }
              trimmed_end <- trimmed_end - 1L
            }
            # Re-trim trailing blanks after stealing
            while (trimmed_end > gi && is_blank[group_rows[trimmed_end]]) {
              trimmed_end <- trimmed_end - 1L
            }
          }
        }

        # Next page starts after trimmed_end
        gi <- trimmed_end + 1L
        # Skip leading blanks at the top of the next page (suppress them)
        while (gi <= n_group && is_blank[group_rows[gi]]) {
          skip_rows_list[[length(skip_rows_list) + 1L]] <- group_rows[gi]
          gi <- gi + 1L
        }
        if (gi <= n_group) {
          page_breaks_list[[length(page_breaks_list) + 1L]] <- group_rows[gi]
        }
        used <- 0L
      }
    }

    # Defer trailing blanks (between groups or at end of data)
    # Capture ALL consecutive blank rows after the group, not just the first
    pending_blank_idx <- NULL
    pending_blank_height <- 0L
    trail <- g_end + 1L
    while (trail <= nr && is_blank[trail]) {
      if (is.null(pending_blank_idx)) {
        # Only the first blank is "deferred" for budget calculation
        pending_blank_idx <- trail
        pending_blank_height <- row_heights[trail]
      } else {
        # Additional trailing blanks are always suppressed
        skip_rows_list[[length(skip_rows_list) + 1L]] <- trail
      }
      trail <- trail + 1L
    }
  }

  # Final pending blank — suppress (no group follows)
  if (!is.null(pending_blank_idx)) {
    skip_rows_list[[length(skip_rows_list) + 1L]] <- pending_blank_idx
  }

  page_breaks <- unlist(page_breaks_list, use.names = FALSE) %||% integer(0)
  skip_rows <- unique(unlist(skip_rows_list, use.names = FALSE) %||% integer(0))

  n_pages <- length(page_breaks) + 1L
  list(
    page_breaks = page_breaks,
    skip_rows = skip_rows,
    n_pages = n_pages
  )
}
