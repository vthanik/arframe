# ──────────────────────────────────────────────────────────────────────────────
# render.R — Top-level rendering dispatch
#
# fr_render() is the terminal verb in the pipeline. It takes a fully
# configured fr_spec and writes the output file.
#
# Pipeline:
#   fr_spec → finalize_spec() → prepare_pages() → render_backend()
# ──────────────────────────────────────────────────────────────────────────────

#' Render a Table to File
#'
#' @description
#'
#' The terminal verb in every **arframe** pipeline. Takes a fully configured
#' `fr_spec` object and writes the rendered table to a file. Supports RTF,
#' LaTeX, and PDF output.
#'
#' Row pagination within the RTF output is delegated to the Word/LibreOffice
#' rendering engine via native RTF properties (`\trhdr` for repeating
#' headers). The LaTeX backend uses tabularray's `longtblr` environment with
#' `rowhead` for repeating headers. R handles `page_by` group splitting
#' (section/page breaks between groups) and `col_split` (column panel
#' calculation for wide tables).
#'
#' @param spec An `fr_spec` object from [fr_table()], configured with
#'   `fr_*()` pipeline verbs.
#' @param path Character scalar. Output file path. The file extension
#'   determines the format unless `format` is specified:
#'   * `.rtf` — Rich Text Format.
#'   * `.html` / `.htm` — Self-contained HTML preview.
#'   * `.tex` — LaTeX source (tabularray/XeLaTeX).
#'   * `.pdf` — PDF via XeLaTeX compilation (requires XeLaTeX on PATH or
#'     **tinytex**). First-time setup: call [fr_install_latex_deps()] to
#'     install required LaTeX packages (notably **tabularray**). For
#'     custom fonts without system-wide installation, set the
#'     `ARFRAME_FONT_DIR` environment variable (see [fr_page()]).
#' @param format Character scalar or `NULL`. Output format: `"rtf"`, `"html"`,
#'   `"latex"`, or `"pdf"`. If `NULL` (default), detected from the file
#'   extension of `path`.
#' @param ... Reserved for future backend-specific options.
#'
#' @return Invisibly returns `path` (the output file path).
#'
#' @section Page-by groups:
#' When `fr_rows(page_by = "col_name")` is set, the data is split by the
#' unique values of that column. Each group is rendered as a separate section
#' with its own table, separated by `\sect` (section break) in RTF. The
#' page-by column value is printed as a bold label above each group's table.
#'
#' @section Column splitting:
#' When `fr_cols(.split = TRUE)` is set and the total column width
#' exceeds the printable page area, columns are automatically split into
#' panels. Stub columns (set via `fr_col(stub = TRUE)`) are repeated
#' in every panel. Each panel is rendered as a separate section.
#'
#' @examples
#' # Render to a temporary RTF file
#' out <- file.path(tempdir(), "demog.rtf")
#' tbl_demog |>
#'   fr_table() |>
#'   fr_render(out)
#' unlink(out)
#'
#' # Full pipeline (in study programs, hlines/pagehead/pagefoot go in fr_theme())
#' out <- file.path(tempdir(), "demog_full.rtf")
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles("Table 14.1.1", "Summary of Demographics") |>
#'   fr_footnotes("Source: ADSL", "Program: t_demog.R") |>
#'   fr_cols(.width = "auto") |>
#'   fr_hlines("header") |>
#'   fr_page(orientation = "landscape") |>
#'   fr_pagehead(left = "{program}", right = "{datetime}") |>
#'   fr_pagefoot(center = "Page {thepage} of {total_pages}") |>
#'   fr_render(out)
#' unlink(out)
#'
#' # Multiple format rendering in sequence (RTF then LaTeX source)
#' rtf_out <- file.path(tempdir(), "ae_soc.rtf")
#' tex_out <- file.path(tempdir(), "ae_soc.tex")
#' spec <- tbl_ae_soc |>
#'   fr_table() |>
#'   fr_titles("Table 14.3.1", "Adverse Events by SOC") |>
#'   fr_hlines("header") |>
#'   fr_page(orientation = "landscape")
#' fr_render(spec, rtf_out)
#' fr_render(spec, tex_out)
#' unlink(c(rtf_out, tex_out))
#'
#' # Listing rendering — individual AE records
#' out <- file.path(tempdir(), "ae_listing.rtf")
#' adae |>
#'   fr_listing() |>
#'   fr_cols(
#'     USUBJID = fr_col("Subject ID", width = 1.2),
#'     AEDECOD = fr_col("Preferred Term"),
#'     AESEV   = fr_col("Severity", width = 1.0)
#'   ) |>
#'   fr_rows(sort_by = c("USUBJID", "ASTDT"),
#'           repeat_cols = "USUBJID") |>
#'   fr_titles("Listing 16.2.7", "Adverse Events") |>
#'   fr_footnotes("Source: ADAE") |>
#'   fr_render(out)
#' unlink(out)
#'
#' # Figure rendering (requires ggplot2)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(adsl, ggplot2::aes(x = AGE)) +
#'     ggplot2::geom_histogram(binwidth = 5) +
#'     ggplot2::labs(title = NULL, x = "Age (years)", y = "Count")
#'   out <- file.path(tempdir(), "age_dist.rtf")
#'   p |>
#'     fr_figure() |>
#'     fr_titles("Figure 14.1.1", "Distribution of Age") |>
#'     fr_footnotes("Source: ADSL") |>
#'     fr_page(orientation = "landscape") |>
#'     fr_render(out)
#'   unlink(out)
#' }
#'
#' @seealso [fr_table()] to start a pipeline, [fr_page()] for page layout,
#'   [fr_rows()] for pagination control.
#'
#' @export
fr_render <- function(spec, path, format = NULL, ...) {
  call <- caller_env()
  check_fr_spec(spec, call = call)
  check_scalar_chr(path, arg = "path", call = call)

  # Clear per-render provenance cache (recomputed once per render call)
  fr_env$cached_source_path <- NULL
  fr_env$cached_timestamp <- NULL

  format <- format %||% detect_format(path, call = call)

  # Figure dispatch — separate render path

  if (identical(spec$type, "figure")) {
    if (format == "pdf") {
      render_figure_pdf(spec, path)
    } else if (format == "rtf") {
      render_figure_rtf(spec, path)
    } else if (format == "html") {
      render_figure_html(spec, path)
    } else {
      cli_abort(
        c(
          "Figure rendering is only supported for {.val pdf}, {.val rtf}, and {.val html} output.",
          "x" = "You requested format {.val {format}}."
        ),
        call = call
      )
    }
    return(invisible(path))
  }

  spec <- finalize_spec(spec)
  page_groups <- prepare_pages(spec)
  col_panels <- calculate_col_panels(spec)

  # When split produces multiple panels, scale based on width mode
  if (isTRUE(spec$columns_meta$split) && length(col_panels) > 1L) {
    wm <- spec$columns_meta$width_mode
    if (identical(wm, "fit")) {
      spec <- fit_panel_widths(spec, col_panels)
    } else if (identical(wm, "equal")) {
      spec <- equal_panel_widths(spec, col_panels)
    }
    # "auto" and "fixed" — no panel scaling
  }

  backend <- get_backend(format, call = call)
  backend$render(spec, page_groups, col_panels, path)

  invisible(path)
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: Format Detection
# ══════════════════════════════════════════════════════════════════════════════

#' Detect output format from file extension
#' @noRd
detect_format <- function(path, call = caller_env()) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "") {
    cli_abort(
      c(
        "Cannot detect format from {.path {path}}.",
        "i" = "Provide a file extension (e.g. {.path .rtf}) or set {.arg format} explicitly."
      ),
      call = call
    )
  }
  format_map <- build_extension_map()
  result <- unname(format_map[ext])
  if (is.na(result)) {
    cli_abort(
      c(
        "Unsupported file extension {.val {ext}}.",
        "i" = "Supported extensions: {.val {names(format_map)}}."
      ),
      call = call
    )
  }
  result
}

#' Build extension-to-format map from backend registry (cached)
#' @noRd
build_extension_map <- function() {
  # Return cached map if registry hasn't changed
  reg <- fr_env$backends
  if (
    !is.null(fr_env$extension_map_cache) &&
      identical(names(reg), fr_env$extension_map_cache_keys)
  ) {
    return(fr_env$extension_map_cache)
  }
  format_map <- character(0)
  for (fmt in names(reg)) {
    for (e in reg[[fmt]]$extensions) {
      format_map[e] <- fmt
    }
  }
  fr_env$extension_map_cache <- format_map
  fr_env$extension_map_cache_keys <- names(reg)
  format_map
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: Backend Registry
# ══════════════════════════════════════════════════════════════════════════════

#' Get backend render function from registry
#' @noRd
get_backend <- function(format, call = caller_env()) {
  reg <- fr_env$backends
  backend <- reg[[format]]
  if (is.null(backend)) {
    available <- names(reg)
    cli_abort(
      c(
        "No backend available for format {.val {format}}.",
        "i" = "Available backends: {.val {available}}."
      ),
      call = call
    )
  }
  backend
}


# ══════════════════════════════════════════════════════════════════════════════
# Backend Registration API
# ══════════════════════════════════════════════════════════════════════════════

#' Register a Custom Render Backend
#'
#' @description
#'
#' Registers a new output format backend so that [fr_render()] can produce
#' output in that format. Third-party packages can use this to add HTML,
#' DOCX, or other formats without modifying arframe source.
#'
#' @param format Character scalar. Format identifier (e.g., `"html"`).
#' @param extensions Character vector. File extensions that map to this
#'   format (e.g., `c("html", "htm")`).
#' @param render_fn Function. The render function with signature
#'   `function(spec, page_groups, col_panels, path)`.
#' @param description Character scalar. Human-readable description.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' # Register a custom CSV backend
#' fr_register_backend(
#'   format = "csv",
#'   extensions = "csv",
#'   render_fn = function(spec, page_groups, col_panels, path) {
#'     utils::write.csv(spec$data, path, row.names = FALSE)
#'   },
#'   description = "CSV export (data only)"
#' )
#'
#' # Verify it appears in the backend list
#' fr_backends()
#'
#' # Use the custom backend
#' out <- file.path(tempdir(), "demo.csv")
#' tbl_demog |> fr_table() |> fr_render(out)
#' readLines(out, n = 3)
#' unlink(out)
#' fr_unregister_backend("csv")
#'
#' @seealso [fr_backends()] to list registered backends, [fr_render()].
#' @export
fr_register_backend <- function(
  format,
  extensions,
  render_fn,
  description = ""
) {
  call <- caller_env()
  check_scalar_chr(format, arg = "format", call = call)
  if (!is.character(extensions) || length(extensions) == 0L) {
    cli_abort(
      c(
        "{.arg extensions} must be a non-empty character vector.",
        "x" = "You supplied {.obj_type_friendly {extensions}}."
      ),
      call = call
    )
  }
  if (!is.function(render_fn)) {
    cli_abort(
      c(
        "{.arg render_fn} must be a function.",
        "x" = "You supplied {.obj_type_friendly {render_fn}}."
      ),
      call = call
    )
  }
  check_scalar_chr(description, arg = "description", call = call)

  fr_env$backends[[format]] <- list(
    render = render_fn,
    extensions = extensions,
    description = description
  )
  # Invalidate cached extension map
  fr_env$extension_map_cache <- NULL
  fr_env$extension_map_cache_keys <- NULL
  invisible(NULL)
}


#' Remove a Custom Render Backend
#'
#' Removes a previously registered backend by format name. Built-in backends
#' (rtf, latex, pdf) can also be removed if needed.
#'
#' @param format Character scalar. The format name to remove (e.g., `"csv"`).
#' @return `NULL` (invisible). Called for side effect.
#'
#' @examples
#' fr_register_backend("csv", "csv",
#'   render_fn = function(spec, page_groups, col_panels, path) {
#'     utils::write.csv(spec$data, path, row.names = FALSE)
#'   })
#' "csv" %in% fr_backends()$format
#' fr_unregister_backend("csv")
#' "csv" %in% fr_backends()$format
#'
#' @seealso [fr_register_backend()], [fr_backends()]
#' @export
fr_unregister_backend <- function(format) {
  call <- caller_env()
  check_scalar_chr(format, arg = "format", call = call)
  fr_env$backends[[format]] <- NULL
  fr_env$extension_map_cache <- NULL
  fr_env$extension_map_cache_keys <- NULL
  invisible(NULL)
}


#' List Registered Render Backends
#'
#' @description
#'
#' Returns a data frame listing all registered render backends, including
#' built-in (RTF, LaTeX, PDF) and any custom backends added via
#' [fr_register_backend()].
#'
#' @return A data frame with columns `format`, `extensions`, and `description`.
#'
#' @examples
#' # List built-in backends
#' fr_backends()
#'
#' # Register a custom backend, then view updated list
#' fr_register_backend(
#'   format = "csv",
#'   extensions = "csv",
#'   render_fn = function(spec, page_groups, col_panels, path) {
#'     utils::write.csv(spec$data, path, row.names = FALSE)
#'   },
#'   description = "CSV export (data only)"
#' )
#' fr_backends()
#' fr_unregister_backend("csv")
#'
#' @seealso [fr_register_backend()], [fr_render()]
#' @export
fr_backends <- function() {
  reg <- fr_env$backends
  if (length(reg) == 0L) {
    return(vctrs::new_data_frame(list(
      format = character(0),
      extensions = character(0),
      description = character(0)
    )))
  }
  vctrs::new_data_frame(list(
    format = names(reg),
    extensions = vapply(
      reg,
      function(b) paste(b$extensions, collapse = ", "),
      character(1)
    ),
    description = vapply(reg, function(b) b$description %||% "", character(1))
  ))
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: Spec Finalization
#
# Resolves columns, widths, and styles to produce a render-ready spec.
# ══════════════════════════════════════════════════════════════════════════════

#' Finalize spec for rendering
#'
#' 1. Build columns if empty (auto-generate from data)
#' 2. Resolve widths
#' 3. Filter to visible columns only for rendering
#'
#' @noRd
finalize_spec <- function(spec) {
  spec <- finalize_columns(spec)
  spec <- finalize_labels(spec)
  spec <- finalize_rows(spec)

  # Apply default footnote placement from config/theme to entries without one
  if (!is.null(spec$meta$footnote_placement)) {
    for (i in seq_along(spec$meta$footnotes)) {
      if (is.null(spec$meta$footnotes[[i]]$placement)) {
        spec$meta$footnotes[[i]]$placement <- spec$meta$footnote_placement
      }
    }
  }

  # Warn if continuation is set but no pagination is configured
  if (
    !is.null(spec$page$continuation) &&
      length(spec$body$page_by) == 0L &&
      length(spec$body$group_by) == 0L
  ) {
    cli::cli_warn(c(
      "{.arg continuation} label is set but neither {.arg page_by} nor {.arg group_by} is configured.",
      "i" = "The continuation label only appears on multi-page tables with pagination via {.fn fr_rows}."
    ))
  }

  # Pre-compute decimal alignment geometry (used by both RTF and LaTeX)
  spec$decimal_geometry <- compute_all_decimal_geometry(spec)

  spec
}


#' Finalize column structure: build defaults, resolve widths, inject gaps
#' @noRd
finalize_columns <- function(spec) {
  # If no columns configured, auto-generate defaults
  if (length(spec$columns) == 0L) {
    spec$columns <- build_default_columns(
      data = spec$data,
      configured = list(),
      default_width = NULL,
      width_mode = "auto",
      default_align = NULL,
      label_fn = NULL,
      labels = NULL,
      page = spec$page
    )
  }

  # Ensure all data columns have a column spec
  for (nm in names(spec$data)) {
    if (is.null(spec$columns[[nm]])) {
      auto_align <- if (is.numeric(spec$data[[nm]])) "right" else "left"
      col_def <- fr_col(label = nm, align = auto_align)
      col_def$id <- nm
      col_def$width <- estimate_col_width(spec$data, nm, nm, spec$page)
      spec$columns[[nm]] <- col_def
    }
  }

  # Resolve percentage widths to absolute inches
  printable_w <- printable_area_inches(spec$page)[["width"]]
  for (nm in names(spec$columns)) {
    w <- spec$columns[[nm]]$width
    if (is_fr_pct(w)) {
      spec$columns[[nm]]$width <- unclass(w) * printable_w
    }
  }

  # Ensure all columns have a resolved numeric width
  for (nm in names(spec$columns)) {
    col <- spec$columns[[nm]]
    if (is.null(col$width) || identical(col$width, "auto")) {
      label <- col$label
      if (!nzchar(label)) {
        label <- nm
      }
      spec$columns[[nm]]$width <- estimate_col_width(
        spec$data,
        nm,
        label,
        spec$page
      )
    }
  }

  # Resolve column visibility:
  # - page_by columns with visible = NULL -> FALSE (auto-hide)
  # - all other columns with visible = NULL -> TRUE (default visible)
  # - explicit TRUE/FALSE always respected
  page_by_cols <- spec$body$page_by
  for (nm in names(spec$columns)) {
    vis <- spec$columns[[nm]]$visible
    if (is.null(vis)) {
      spec$columns[[nm]]$visible <- !(nm %in% page_by_cols)
    }
  }

  # Apply theme/config stub column names (deferred until columns are built)
  theme_stubs <- spec$columns_meta$stub
  if (is.character(theme_stubs) && length(theme_stubs) > 0L) {
    for (nm in theme_stubs) {
      if (!is.null(spec$columns[[nm]]) && !isTRUE(spec$columns[[nm]]$stub)) {
        spec$columns[[nm]]$stub <- TRUE
      }
    }
    # Clean up — stubs are now on the column objects
    spec$columns_meta$stub <- NULL
  }

  # Auto-infer stub columns when split is enabled but none marked
  split_mode <- spec$columns_meta$split
  if (!identical(split_mode, FALSE) && !is.null(split_mode)) {
    stub_names <- stub_column_names(spec$columns)
    if (length(stub_names) == 0L) {
      auto_stubs <- unique(c(spec$body$group_by, spec$body$indent_by))
      if (length(auto_stubs) == 0L) {
        auto_stubs <- names(spec$columns)[1L]
      }
      for (nm in auto_stubs) {
        if (!is.null(spec$columns[[nm]])) {
          spec$columns[[nm]]$stub <- TRUE
        }
      }
    }
  }

  # Scale auto widths to fit printable area (skip when split is on —
  # columns must keep natural widths so calculate_col_panels() can split them)
  if (identical(split_mode, FALSE) || is.null(split_mode)) {
    spec$columns <- distribute_auto_widths(spec$columns, spec$page)
  }

  # Generate auto-spans from fr_col(group = ...) before inject_span_gaps
  spec <- generate_group_spans(spec)

  # Inject gap columns between adjacent spanning headers
  spec <- inject_span_gaps(spec)

  spec
}


#' Generate spanning headers from fr_col(group = ...) annotations
#'
#' Scans all columns for `group` values and creates level-1 spans.
#' Explicit `fr_spans()` at the same label override auto-generated spans.
#'
#' @noRd
generate_group_spans <- function(spec) {
  cols <- spec$columns
  col_names <- names(cols)

  # Collect unique group names preserving first-column-appearance order
  groups <- character(0)
  group_cols <- list()
  for (nm in col_names) {
    grp <- cols[[nm]]$group
    if (!is.null(grp)) {
      if (!grp %in% groups) {
        groups <- c(groups, grp)
        group_cols[[grp]] <- nm
      } else {
        group_cols[[grp]] <- c(group_cols[[grp]], nm)
      }
    }
  }
  if (length(groups) == 0L) {
    return(spec)
  }

  # Get existing span labels to check for overrides
  existing_spans <- spec$header$spans %||% list()
  existing_labels <- vapply(existing_spans, function(s) s$label, character(1))

  for (grp in groups) {
    # Skip if an explicit fr_spans() exists at this label
    if (grp %in% existing_labels) {
      next
    }
    spec$header$spans <- c(
      spec$header$spans,
      list(new_fr_span(label = grp, columns = group_cols[[grp]], level = 1L))
    )
  }

  spec
}


#' Finalize header labels and N-count resolution
#' @noRd
finalize_labels <- function(spec) {
  n_meta <- spec$columns_meta$n
  fmt <- spec$columns_meta$n_format

  # ── Per-column fr_col(n = ...) — highest priority ───────────────────────
  # Apply per-column N-counts first (these have highest priority)
  cols_with_n <- character(0)
  if (!is.null(fmt)) {
    for (nm in names(spec$columns)) {
      col <- spec$columns[[nm]]
      if (!is.null(col$n)) {
        base_label <- col$label %||% nm
        if (!nzchar(base_label)) {
          base_label <- nm
        }
        row_data <- list(label = base_label, n = col$n)
        spec$columns[[nm]]$label <- tryCatch(
          as.character(glue::glue_data(row_data, fmt)),
          error = function(e) base_label
        )
        cols_with_n <- c(cols_with_n, nm)
      }
    }
  } else {
    # Check if any columns have n set without format → warn
    has_any_n <- any(vapply(
      spec$columns,
      function(c) !is.null(c$n),
      logical(1)
    ))
    if (has_any_n) {
      cli_warn(
        c(
          "Column{?s} have {.arg n} set but no {.arg .n_format} is defined.",
          "i" = "Set {.arg .n_format} in {.fn fr_cols} or config YAML to display N-counts."
        )
      )
    }
  }

  # ── Bulk .n from columns_meta — lower priority ─────────────────────────
  if (!is.null(n_meta) && !is.null(fmt)) {
    if (is.data.frame(n_meta)) {
      parsed <- parse_df_n_counts(n_meta, spec)
      if (parsed$type == "global") {
        matched_col <- match_trt_to_columns(parsed$counts, spec$columns)
        matched_span <- match_trt_to_spans(
          parsed$counts,
          spec$columns,
          spec$header$spans %||% list()
        )
        # Remove columns that already got per-column n
        matched_col <- matched_col[!names(matched_col) %in% cols_with_n]
        spec <- apply_n_counts(spec, matched_col, fmt)
        spec <- apply_span_n_counts(spec, matched_span, fmt)
      } else {
        # 3-col df → store for per-group resolution in render loop
        spec$columns_meta$._df_result <- parsed
      }
    } else if (is.numeric(n_meta)) {
      # Named numeric vector — match by display label
      matched_col <- match_trt_to_columns(n_meta, spec$columns)
      matched_span <- match_trt_to_spans(
        n_meta,
        spec$columns,
        spec$header$spans %||% list()
      )
      matched_col <- matched_col[!names(matched_col) %in% cols_with_n]
      spec <- apply_n_counts(spec, matched_col, fmt)
      spec <- apply_span_n_counts(spec, matched_span, fmt)
    } else if (is.list(n_meta) && !is.data.frame(n_meta)) {
      # Named list — per-group, resolved in render loop (no global resolution)
    }
  }

  # Warn if per-group n (list or 3-col df) is used without page_by
  has_df_pergroup <- !is.null(spec$columns_meta$._df_result) &&
    spec$columns_meta$._df_result$type == "per_group"
  is_pergroup_list <- is.list(n_meta) &&
    !is.numeric(n_meta) &&
    !is.data.frame(n_meta)
  if (
    (is_pergroup_list || has_df_pergroup) &&
      length(spec$body$page_by) == 0L
  ) {
    cli_warn(
      "Per-group {.arg .n} requires {.fn fr_rows} with {.arg page_by}. N-counts ignored."
    )
  }

  # Apply header-level defaults to columns (from fr_header() or config)
  spec <- apply_header_defaults(spec)

  spec
}


#' Finalize row structure: sort, repeat-suppress, blank rows, indent
#' @noRd
finalize_rows <- function(spec) {
  # Sort data if sort_by is specified (listings)
  sort_by <- spec$body$sort_by
  if (length(sort_by) > 0L) {
    order_args <- lapply(sort_by, function(col) spec$data[[col]])
    order_idx <- inject(order(!!!order_args))
    spec$data <- spec$data[order_idx, , drop = FALSE]
    rownames(spec$data) <- NULL
  }

  # Suppress repeated values in repeat_cols (listings)
  repeat_cols <- spec$body$repeat_cols
  if (length(repeat_cols) > 0L) {
    for (col in repeat_cols) {
      if (!col %in% names(spec$data)) {
        next
      }
      vals <- spec$data[[col]]
      if (length(vals) > 1L) {
        is_repeat <- c(FALSE, vals[-1L] == vals[-length(vals)])
        # Also handle NAs
        is_repeat[is.na(is_repeat)] <- FALSE
        spec$data[[col]][is_repeat] <- ""
      }
    }
  }

  # Inject group header rows when group_label is set.
  # Must happen BEFORE blank_after (so headers are part of the group) and
  # BEFORE style remapping (so user styles apply to correct positions).
  group_label_col <- spec$body$group_label
  group_cols <- spec$body$group_by
  page_by_cols <- spec$body$page_by
  if (!is.null(group_label_col) && length(group_cols) > 0L) {
    gl_result <- inject_group_headers(
      spec$data,
      group_cols,
      group_label_col,
      preserve_cols = page_by_cols
    )
    spec$data <- gl_result$data

    # Remap existing style indices to account for injected header rows
    if (length(gl_result$header_rows) > 0L) {
      spec$cell_styles <- remap_style_indices_for_injected(
        spec$cell_styles,
        gl_result$header_rows
      )
    }

    # Apply group_bold: inject row-level bold style for header rows
    if (isTRUE(spec$body$group_bold) && length(gl_result$header_rows) > 0L) {
      spec$cell_styles <- c(
        spec$cell_styles,
        list(new_fr_row_style(rows = gl_result$header_rows, bold = TRUE))
      )
    }
  }

  # Insert blank rows at group boundaries (explicit blank_after only).
  blank_cols <- spec$body$blank_after
  blank_result <- insert_blank_after(
    spec$data,
    blank_cols,
    preserve_cols = page_by_cols
  )
  spec$data <- blank_result$data

  # Remap style row indices to account for inserted blank rows
  if (length(blank_result$insert_positions) > 0L) {
    spec$cell_styles <- remap_style_indices(
      spec$cell_styles,
      blank_result$insert_positions
    )
  }

  # Convert leading spaces to paragraph-level indent (after blank_after so
  # indices are correct, before indent_by so indent_by wins for shared columns)
  spec <- apply_leading_indent(spec)

  # Apply indent_by as cell styles (after leading indent so it wins)
  spec <- apply_indent_by(spec)

  spec
}


#' Apply N-counts to column labels using a format string
#'
#' Builds glue labels from n_counts + format for each matching column,
#' updating `spec$columns[[nm]]$label` in place. Delegates label
#' computation to `build_label_overrides()`.
#'
#' @param spec fr_spec object with resolved columns.
#' @param n_counts Named integer/numeric vector keyed by column name.
#' @param fmt Glue format string with `{label}` and `{n}` tokens.
#' @return Modified spec.
#' @noRd
apply_n_counts <- function(spec, n_counts, fmt) {
  overrides <- build_label_overrides(n_counts, fmt, spec$columns)
  if (!is.null(overrides)) {
    for (nm in names(overrides)) {
      spec$columns[[nm]]$label <- overrides[[nm]]
    }
  }
  spec
}


#' Apply N-counts to spanner labels using a format string
#'
#' Modifies `spec$header$spans[[i]]$label` in place for matching spans.
#'
#' @param spec fr_spec object with resolved spans.
#' @param span_counts Named integer vector keyed by span label.
#' @param fmt Glue format string with `{label}` and `{n}` tokens.
#' @return Modified spec.
#' @noRd
apply_span_n_counts <- function(spec, span_counts, fmt) {
  if (length(span_counts) == 0L) {
    return(spec)
  }
  spans <- spec$header$spans %||% list()
  overrides <- build_span_label_overrides(span_counts, fmt, spans)
  if (!is.null(overrides)) {
    for (i in seq_along(spec$header$spans)) {
      orig <- spec$header$spans[[i]]$label
      if (orig %in% names(overrides)) {
        spec$header$spans[[i]]$label <- overrides[[orig]]
      }
    }
  }
  spec
}


#' Resolve header labels for a specific page_by group
#'
#' Returns a named character vector of label overrides for one group,
#' or NULL when no per-group resolution is needed (global numeric n is
#' already resolved in finalize_spec).
#'
#' @param spec Finalized fr_spec object.
#' @param group_data Data frame — the page_by subset for this group.
#' @param group_label Character scalar — the page_by composite key,
#'   or NULL when no page_by is set.
#' @return Named character vector of column label overrides, or NULL.
#' @noRd
resolve_group_labels <- function(spec, group_data, group_label) {
  n_input <- spec$columns_meta$n
  fmt <- spec$columns_meta$n_format
  if (is.null(fmt)) {
    return(NULL)
  }

  # Check for cached per-group data frame result (3-col df from finalize_labels)
  df_res <- spec$columns_meta$._df_result
  if (!is.null(df_res) && df_res$type == "per_group") {
    df <- df_res$df
    page_vals <- as.character(df[[df_res$page_col]])
    mask <- tolower(page_vals) == tolower(as.character(group_label))
    group_df <- df[mask, , drop = FALSE]
    if (nrow(group_df) == 0L) {
      cli_warn(c(
        "No N-counts found for page group {.val {group_label}}.",
        "i" = "Available groups in {.arg .n}: {.val {unique(page_vals)}}.",
        "i" = "Ensure column 1 values match {.arg page_by} group values (case-insensitive)."
      ))
      return(NULL)
    }

    n_counts <- set_names(
      as.integer(group_df[[df_res$count_col]]),
      as.character(group_df[[df_res$trt_col]])
    )
    n_counts_col <- match_trt_to_columns(n_counts, spec$columns)
    n_counts_span <- match_trt_to_spans(
      n_counts,
      spec$columns,
      spec$header$spans %||% list()
    )
    col_ov <- build_label_overrides(n_counts_col, fmt, spec$columns)
    span_ov <- build_span_label_overrides(
      n_counts_span,
      fmt,
      spec$header$spans %||% list()
    )
    return(list(columns = col_ov, spans = span_ov))
  }

  if (is.list(n_input) && !is.numeric(n_input) && !is.data.frame(n_input)) {
    # Per-group static list — match by display label
    n_counts_raw <- n_input[[group_label]]
    if (is.null(n_counts_raw)) {
      return(NULL)
    }
    n_counts_col <- match_trt_to_columns(n_counts_raw, spec$columns)
    n_counts_span <- match_trt_to_spans(
      n_counts_raw,
      spec$columns,
      spec$header$spans %||% list()
    )
    col_ov <- build_label_overrides(n_counts_col, fmt, spec$columns)
    span_ov <- build_span_label_overrides(
      n_counts_span,
      fmt,
      spec$header$spans %||% list()
    )
    return(list(columns = col_ov, spans = span_ov))
  }

  # Global numeric already resolved in finalize_labels; data frame already
  # handled above via ._df_result
  NULL
}


#' Parse an N-count data frame into a standardised internal format
#'
#' Converts a 2-col or 3-col data frame to either "global" (2-col) or
#' "per_group" (3-col) format for deferred N-count resolution.
#'
#' @param df A 2- or 3-column data frame (already validated by
#'   `validate_n_param()`).
#' @param spec The fr_spec (used for column label matching).
#' @return A list with `$type` ("global" or "per_group") and type-specific
#'   fields.
#' @noRd
parse_df_n_counts <- function(df, spec) {
  if (ncol(df) == 2L) {
    counts <- set_names(as.integer(df[[2L]]), as.character(df[[1L]]))
    return(list(type = "global", counts = counts))
  }

  # 3-col: page_by group × treatment × count
  list(
    type = "per_group",
    df = df,
    page_col = 1L,
    trt_col = 2L,
    count_col = 3L
  )
}


#' Match treatment labels to column labels (case-insensitive)
#'
#' Takes a named vector of treatment counts and matches them to columns
#' by comparing against column display labels (case-insensitive).
#' Only matches direct column labels. For spanner label matching, use
#' `match_trt_to_spans()`.
#'
#' @param trt_counts Named numeric vector. Names can be either display labels
#'   (e.g. `c("Placebo" = 45)`) or data column names (e.g. `c(placebo = 45)`).
#'   Label match is tried first; column name match is the fallback. Both are
#'   case-insensitive.
#' @param columns Named list of `fr_col` objects.
#' @return Named integer vector keyed by column name.
#' @noRd
match_trt_to_columns <- function(trt_counts, columns) {
  col_names <- names(columns)
  col_labels <- vapply(
    col_names,
    function(nm) {
      lbl <- columns[[nm]]$label
      if (is.null(lbl) || !nzchar(lbl)) nm else lbl
    },
    character(1)
  )
  labels_lower <- tolower(col_labels)
  names_lower <- tolower(col_names)

  result <- integer(0)
  for (trt in names(trt_counts)) {
    trt_lower <- tolower(trt)
    # Try label match first, then column name match
    idx <- match(trt_lower, labels_lower)
    if (is.na(idx)) {
      idx <- match(trt_lower, names_lower)
    }
    if (!is.na(idx)) {
      result[col_names[idx]] <- as.integer(trt_counts[trt])
    }
  }

  result
}


#' Match treatment labels to spanner labels (case-insensitive)
#'
#' For treatments that don't match any column label directly, checks
#' if they match a spanner label. Returns named integer keyed by
#' **span label** (not column name).
#'
#' @param trt_counts Named numeric vector (e.g. `c("Placebo" = 45)`).
#' @param columns Named list of `fr_col` objects (used to exclude
#'   treatments that already matched columns).
#' @param spans List of `fr_span` objects.
#' @return Named integer vector keyed by span label.
#' @noRd
match_trt_to_spans <- function(trt_counts, columns, spans) {
  if (length(spans) == 0L) {
    return(integer(0))
  }

  # Get column labels to exclude treatments that already matched columns
  col_labels_lower <- tolower(vapply(
    columns,
    function(c) {
      lbl <- c$label
      if (is.null(lbl) || !nzchar(lbl)) "" else lbl
    },
    character(1)
  ))

  result <- integer(0)
  for (trt in names(trt_counts)) {
    trt_lower <- tolower(trt)
    if (trt_lower %in% col_labels_lower) {
      next
    }
    for (span in spans) {
      if (tolower(span$label) == trt_lower) {
        result[span$label] <- as.integer(trt_counts[trt])
        break
      }
    }
  }
  result
}


#' Build label overrides from N-counts and format string
#'
#' @param n_counts Named integer vector keyed by column name.
#' @param fmt Glue format string with `{label}` and `{n}` tokens.
#' @param columns Named list of `fr_col` objects.
#' @return Named character vector of label overrides, or NULL.
#' @noRd
build_label_overrides <- function(n_counts, fmt, columns) {
  overrides <- character(0)
  for (nm in names(n_counts)) {
    if (!nm %in% names(columns)) {
      next
    }
    base_label <- columns[[nm]]$label %||% nm
    if (!nzchar(base_label)) {
      base_label <- nm
    }
    row_data <- list(
      label = base_label,
      n = as.integer(n_counts[[nm]])
    )
    overrides[nm] <- tryCatch(
      as.character(glue::glue_data(row_data, fmt)),
      error = function(e) base_label
    )
  }
  if (length(overrides) == 0L) NULL else overrides
}


#' Build span label overrides from N-counts and format string
#'
#' @param n_counts Named integer vector keyed by span label.
#' @param fmt Glue format string with `{label}` and `{n}` tokens.
#' @param spans List of `fr_span` objects.
#' @return Named character vector of label overrides, or NULL.
#' @noRd
build_span_label_overrides <- function(n_counts, fmt, spans) {
  overrides <- character(0)
  for (span_label in names(n_counts)) {
    row_data <- list(
      label = span_label,
      n = as.integer(n_counts[[span_label]])
    )
    overrides[span_label] <- tryCatch(
      as.character(glue::glue_data(row_data, fmt)),
      error = function(e) span_label
    )
  }
  if (length(overrides) == 0L) NULL else overrides
}


#' Apply header-level styling defaults to columns
#'
#' Propagates fr_header(align = ...) and fr_header(align = list(...))
#' to columns that don't have a per-column header_align set.
#'
#' Precedence (highest wins):
#'   1. fr_col(header_align = ...)  — per-column override
#'   2. align_map (tidyselect)      — applied here
#'   3. scalar align                — blanket default applied here
#'   4. column body align           — inherited (default)
#'
#' @noRd
apply_header_defaults <- function(spec) {
  h <- spec$header
  if (is.null(h)) {
    return(spec)
  }

  # Capture which columns have explicit fr_col(header_align=) BEFORE we

  # apply any defaults. These must not be overridden.
  explicit_ha <- vapply(
    spec$columns,
    function(c) !is.null(c$header_align),
    logical(1)
  )

  # Step 1: Apply scalar align as blanket default (lowest priority)
  if (!is.null(h$align)) {
    for (nm in names(spec$columns)) {
      if (!explicit_ha[nm]) {
        spec$columns[[nm]]$header_align <- h$align
      }
    }
  }

  # Step 2: Apply align_map (tidyselect) — overrides scalar but not fr_col
  align_map <- h$align_map
  if (!is.null(align_map) && length(align_map) > 0L) {
    for (nm in names(align_map)) {
      if (is.null(spec$columns[[nm]])) {
        next
      }
      if (!explicit_ha[nm]) {
        spec$columns[[nm]]$header_align <- align_map[[nm]]
      }
    }
  }

  spec
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: Page-by Grouping
# ══════════════════════════════════════════════════════════════════════════════

#' Split data by page_by column(s)
#'
#' @param spec An fr_spec object.
#' @return List of lists, each with `data` and `group_label`.
#' @noRd
prepare_pages <- function(spec) {
  page_by <- spec$body$page_by

  if (length(page_by) == 0L) {
    return(list(list(
      data = spec$data,
      group_label = NULL,
      orig_rows = NULL
    )))
  }

  # Validate page_by columns exist
  missing <- setdiff(page_by, names(spec$data))
  if (length(missing) > 0L) {
    cli_abort(
      c(
        "{.arg page_by} column{?s} not found in data: {.val {missing}}.",
        "i" = "Available columns: {.val {names(spec$data)}}."
      )
    )
  }

  # Build composite key preserving original row order (split() would
  # convert to factor and sort alphabetically, losing data order)
  keys <- inject(paste(!!!spec$data[page_by], sep = " / "))
  unique_keys <- unique(keys)

  lapply(unique_keys, function(k) {
    mask <- keys == k
    group_data <- spec$data[mask, , drop = FALSE]
    group_overrides <- resolve_group_labels(spec, group_data, k)
    list(
      data = group_data,
      orig_rows = which(mask),
      group_label = k,
      label_overrides = if (is.list(group_overrides)) {
        group_overrides$columns
      } else {
        group_overrides
      },
      span_overrides = if (is.list(group_overrides)) {
        group_overrides$spans
      } else {
        NULL
      }
    )
  })
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: Column Panel Calculation
# ══════════════════════════════════════════════════════════════════════════════

#' Calculate column panels for wide tables
#'
#' When col_split is enabled and total width exceeds printable area,
#' splits columns into panels with stub columns repeated in each.
#'
#' @param spec An fr_spec object (finalized).
#' @return List of character vectors (column names per panel).
#' @noRd
calculate_col_panels <- function(spec) {
  split_mode <- spec$columns_meta$split
  if (identical(split_mode, FALSE) || is.null(split_mode)) {
    # Single panel with all visible columns
    vis_names <- names(visible_columns(spec$columns))
    return(list(vis_names))
  }

  printable <- printable_area_inches(spec$page)[["width"]]
  # Get stub columns from column-level stub = TRUE
  vis_cols <- visible_columns(spec$columns)
  vis_names <- names(vis_cols)
  stub_cols <- stub_column_names(vis_cols)

  # Calculate stub width
  stub_width <- sum(vapply(
    vis_cols[intersect(stub_cols, vis_names)],
    function(c) c$width,
    numeric(1)
  ))
  available <- printable - stub_width

  if (available <= 0) {
    cli_abort("Stub columns exceed the printable page width.")
  }

  # Data columns (non-stub)
  data_cols <- setdiff(vis_names, stub_cols)

  # Check if all columns fit in one panel
  total_data_width <- sum(vapply(
    vis_cols[data_cols],
    function(c) c$width,
    numeric(1)
  ))
  if (total_data_width <= available) {
    return(list(vis_names))
  }

  # Build atomic column groups from level-1 spans (spanner-aware packing)
  groups <- build_atomic_groups(data_cols, spec$header$spans)

  # Greedy left-to-right packing of groups
  panels <- list()
  current <- character(0)
  current_width <- 0

  for (grp in groups) {
    grp_width <- sum(vapply(vis_cols[grp], function(c) c$width, numeric(1)))
    if (current_width + grp_width > available && length(current) > 0L) {
      panels <- c(panels, list(c(intersect(stub_cols, vis_names), current)))
      current <- grp
      current_width <- grp_width
    } else {
      current <- c(current, grp)
      current_width <- current_width + grp_width
    }
  }
  if (length(current) > 0L) {
    panels <- c(panels, list(c(intersect(stub_cols, vis_names), current)))
  }

  panels
}


#' Build atomic column groups from level-1 spans
#'
#' Returns a list of character vectors where each vector is an atomic group
#' of columns that must stay together during panel splitting. Columns covered
#' by a level-1 span form one group; unspanned columns are singletons.
#'
#' @param data_cols Character vector of non-stub visible column names.
#' @param spans List of fr_span objects (may be empty).
#' @return Ordered list of character vectors.
#' @noRd
build_atomic_groups <- function(data_cols, spans) {
  if (length(spans) == 0L || length(data_cols) == 0L) {
    return(as.list(data_cols))
  }

  # Get level-1 spans (lowest/innermost level)
  lvl1_spans <- Filter(function(s) s$level == 1L, spans)
  if (length(lvl1_spans) == 0L) {
    return(as.list(data_cols))
  }

  assigned <- character(0)
  groups <- list()

  for (col in data_cols) {
    if (col %in% assigned) {
      next
    }

    # Check if this column belongs to a level-1 span
    matching_span <- NULL
    for (sp in lvl1_spans) {
      if (col %in% sp$columns) {
        matching_span <- sp
        break
      }
    }

    if (!is.null(matching_span)) {
      # Take all columns from this span that are in data_cols
      grp <- intersect(matching_span$columns, data_cols)
      groups <- c(groups, list(grp))
      assigned <- c(assigned, grp)
    } else {
      # Singleton (unspanned column)
      groups <- c(groups, list(col))
      assigned <- c(assigned, col)
    }
  }

  groups
}


#' Scale column widths per panel to fill the printable page width
#'
#' When col_split produces multiple panels, each panel typically has
#' stub columns + a few data columns — far narrower than the page.
#' This function scales each panel's data columns proportionally so
#' the total panel width fills the printable area (AutoFit to Window).
#'
#' Stub columns keep their original width (shared across panels).
#' Only the data (non-stub) columns within each panel are scaled up.
#'
#' @param spec Finalized fr_spec with resolved column widths.
#' @param col_panels List of character vectors (column names per panel).
#' @return Modified fr_spec with adjusted column widths.
#' @noRd
fit_panel_widths <- function(spec, col_panels) {
  printable <- printable_area_inches(spec$page)[["width"]]
  stub_cols <- stub_column_names(spec$columns)

  # Compute stub width (constant across panels)
  stub_width <- sum(vapply(
    spec$columns[intersect(stub_cols, names(spec$columns))],
    function(c) c$width,
    numeric(1)
  ))
  available <- printable - stub_width
  if (available <= 0) {
    return(spec)
  }

  # For each panel, find its data columns and compute the scale factor.
  # Data columns are unique per panel (non-stub), so scaling won't conflict.
  for (panel_cols in col_panels) {
    data_cols <- setdiff(panel_cols, stub_cols)
    if (length(data_cols) == 0L) {
      next
    }

    panel_data_width <- sum(vapply(
      spec$columns[data_cols],
      function(c) c$width,
      numeric(1)
    ))
    if (panel_data_width <= 0) {
      next
    }

    scale_factor <- available / panel_data_width
    for (nm in data_cols) {
      spec$columns[[nm]]$width <- spec$columns[[nm]]$width * scale_factor
    }
  }

  spec
}


#' Distribute equal widths per panel for unfixed data columns
#'
#' When `.width = "equal"` + `.split = TRUE`, each panel's unfixed data
#' columns get an equal share of the remaining space after stub and
#' fixed-width columns.
#'
#' @param spec Finalized fr_spec with resolved column widths.
#' @param col_panels List of character vectors (column names per panel).
#' @return Modified fr_spec with adjusted column widths.
#' @noRd
equal_panel_widths <- function(spec, col_panels) {
  printable <- printable_area_inches(spec$page)[["width"]]
  stub_cols <- stub_column_names(spec$columns)

  stub_width <- sum(vapply(
    spec$columns[intersect(stub_cols, names(spec$columns))],
    function(c) c$width,
    numeric(1)
  ))

  for (panel_cols in col_panels) {
    data_cols <- setdiff(panel_cols, stub_cols)
    if (length(data_cols) == 0L) {
      next
    }

    parts <- separate_fixed_auto_cols(spec$columns, data_cols)
    if (length(parts$auto_names) == 0L) {
      next
    }

    remaining <- printable - stub_width - parts$fixed_sum
    if (remaining <= 0) {
      next
    }
    equal_w <- remaining / length(parts$auto_names)
    for (nm in parts$auto_names) {
      spec$columns[[nm]]$width <- equal_w
    }
  }

  spec
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: Span Gap Column Injection
# ══════════════════════════════════════════════════════════════════════════════

#' Inject narrow gap columns between adjacent spanning headers
#'
#' When `spec$header$span_gap` is TRUE, inserts empty gap columns at
#' boundaries where two spans at the same level are adjacent. The gap
#' column creates a visual break between span hlines in both RTF and
#' LaTeX without relying on border trimming.
#'
#' Called in `finalize_spec()` after width distribution, before header
#' label resolution.
#'
#' @param spec An fr_spec object.
#' @return Modified fr_spec with gap columns inserted.
#' @noRd
inject_span_gaps <- function(spec) {
  if (!isTRUE(spec$header$span_gap)) {
    return(spec)
  }
  spans <- spec$header$spans
  if (length(spans) == 0L) {
    return(spec)
  }

  col_names <- names(spec$columns)
  vis_names <- col_names[vapply(
    spec$columns,
    function(c) !isFALSE(c$visible),
    logical(1)
  )]
  nc <- length(vis_names)

  # For each level, find boundaries between adjacent spans
  levels <- sort(unique(vapply(spans, function(s) s$level, integer(1))))
  gap_after <- character(0)

  for (lvl in levels) {
    lvl_spans <- Filter(function(s) s$level == lvl, spans)
    for (sp in lvl_spans) {
      sp_cols <- intersect(sp$columns, vis_names)
      if (length(sp_cols) == 0L) {
        next
      }
      last_col <- sp_cols[length(sp_cols)]
      last_idx <- match(last_col, vis_names)
      if (last_idx < nc) {
        next_col <- vis_names[last_idx + 1L]
        for (sp2 in lvl_spans) {
          sp2_cols <- intersect(sp2$columns, vis_names)
          if (length(sp2_cols) > 0L && sp2_cols[1L] == next_col) {
            gap_after <- c(gap_after, last_col)
            break
          }
        }
      }
    }
  }

  gap_after <- unique(gap_after)
  if (length(gap_after) == 0L) {
    return(spec)
  }

  # Gap width: font_size * 0.5 / 72 inches
  fs <- spec$page$font_size %||% 9
  gap_width <- fs * 0.5 / fr_env$points_per_inch

  # Insert gap columns in reverse order to preserve indices
  gap_positions <- match(gap_after, names(spec$columns))
  sorted_idx <- order(gap_positions, decreasing = TRUE)

  for (i in sorted_idx) {
    pos <- gap_positions[i]
    gap_name <- paste0(".__span_gap_", pos, "__")

    # Create gap fr_col
    gap_col <- fr_col(label = "", width = gap_width, align = "center")
    gap_col$id <- gap_name
    gap_col$is_gap <- TRUE
    gap_col$gap_type <- "span"

    # Insert into columns list
    cols <- spec$columns
    n_cols <- length(cols)
    gap_list <- list(gap_col)
    names(gap_list) <- gap_name
    new_cols <- c(cols[seq_len(pos)], gap_list)
    if (pos < n_cols) {
      new_cols <- c(new_cols, cols[(pos + 1L):n_cols])
    }
    spec$columns <- new_cols

    # Insert empty column into data
    spec$data <- insert_gap_column(spec$data, pos, gap_name)

    # Expand higher-level spans that straddle this gap
    spec$header$spans <- expand_spans_for_gap(
      spec$header$spans,
      gap_name,
      gap_after[i]
    )
  }

  spec
}


#' Insert an empty character column into a data frame at a given position
#' @noRd
insert_gap_column <- function(df, after_pos, col_name) {
  nr <- nrow(df)
  gap_data <- rep_len("", nr)
  n_cols <- ncol(df)

  # Build new data frame with gap column inserted after position
  before <- df[, seq_len(after_pos), drop = FALSE]
  gap_df <- vctrs::new_data_frame(set_names(list(gap_data), col_name))

  if (after_pos < n_cols) {
    after <- df[, (after_pos + 1L):n_cols, drop = FALSE]
    cbind(before, gap_df, after)
  } else {
    cbind(before, gap_df)
  }
}


#' Expand spans that straddle a gap insertion point
#'
#' If a higher-level span covers columns on BOTH sides of the gap,
#' the gap column is added to its columns vector so it renders as
#' part of the merged cell.
#'
#' @param spans List of fr_span objects.
#' @param gap_name Name of the new gap column.
#' @param after_col Name of the column after which the gap was inserted.
#' @return Modified list of fr_span objects.
#' @noRd
expand_spans_for_gap <- function(spans, gap_name, after_col) {
  lapply(spans, function(sp) {
    cols <- sp$columns
    idx <- match(after_col, cols)
    if (!is.na(idx) && idx < length(cols)) {
      # This span has columns on both sides of the gap — include the gap
      sp$columns <- c(
        cols[seq_len(idx)],
        gap_name,
        cols[(idx + 1L):length(cols)]
      )
    }
    sp
  })
}
