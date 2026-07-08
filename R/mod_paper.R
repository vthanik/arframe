# The typeset sheet (design spec #3/#4/#7): the render payoff. Renders the
# SELECTED output through the exact same arpillar::render_spec()/
# render_ggplot() seam the export leg uses -- screen == paper, one spec
# object feeds both surfaces, never a second parallel renderer. When the
# output is not ready, the ghost shell (utils_ghost.R) stands in; when a
# ready render throws, the GOV.UK error summary renders instead and the id
# is flagged in `rv$broken` (mod_contents.R's TOC stamp already reads that
# flag). Region click delegation (arframe.js) routes every click on the
# page's furniture -- real or ghost -- through `open_card()`.

# ---- title block ------------------------------------------------------

#' The TLF number line: `"<number_label> <number>"`, falling back to the
#' generator's own kind label (`"Table"`/`"Figure"`/`"Listing"`) when
#' `options$number_label` is absent -- an object added by hand (outside
#' both `add_from_preset()`/`add_from_generator()`) still gets a sensible
#' label rather than showing a blank line.
#' @noRd
.number_line <- function(object) {
  label <- object@options$number_label
  if (is.null(label) || !nzchar(label)) {
    gen <- tryCatch(arpillar::generator(object@type), error = function(e) NULL)
    label <- if (!is.null(gen)) .kind_number_label(gen$kind) else "Table"
  }
  number <- object@options[["number"]] %||% ""
  trimws(paste(label, number))
}

#' The real (non-ghost) title block: number line and title only -- what
#' the options actually carry, nothing fabricated (the old
#' footnote-promoted "population line" was an audit liability: the canvas
#' showed a line the spec did not, 2026-07-04). Wrapped in the `title`
#' region so clicking it opens the card routed there. When the output
#' carries filters, the Population tag (Task 12) renders below, wearing
#' its OWN `filters` region.
#' @noRd
.title_block <- function(object) {
  shiny::tags$div(
    class = "ar-paper-title-block ar-mono",
    shiny::tags$div(class = "ar-paper-number", .number_line(object)),
    shiny::tags$div(
      class = "ar-paper-title-text",
      if (nzchar(object@title)) object@title else "Untitled output"
    ),
    if (length(object@filters) > 0L) {
      shiny::tags$div(
        class = "ar-paper-filtertag",
        paste0("Population: ", .filters_tag_label(object@filters))
      )
    }
  )
}

# ---- source line ---------------------------------------------------------

# Canvas flip (2026-07-04, supersedes decision #7): the canvas shows
# tabular's FULL page render -- running header/footer bands included
# (`chrome_onscreen = "auto"`, tabular's default). A ready table needs no
# arframe-side title/source markup; `.title_block()`/`.source_line()`
# remain for the ghost/stale/error paths and the figure leg, where no
# tabular spec exists to carry them.

#' The provenance TEXT: `Source: <dataset> - arframe <version> - <date>`.
#' The one composition of the source line, shared by the screen leg (below)
#' and the export legs (mod_card's .rtf download, fct_export's package
#' build), which bake it into `options$source` so arpillar renders the SAME
#' string on paper -- the date is stamped here, arframe-side, keeping the
#' engine's emit byte-deterministic.
#' @noRd
.source_text <- function(object) {
  ver <- as.character(utils::packageVersion("arframe"))
  paste0(
    "Source: ",
    object@dataset,
    " - arframe ",
    ver,
    " - ",
    format(Sys.Date(), "%Y-%m-%d")
  )
}

#' The faint provenance line at the foot of the sheet, from
#' `.source_text()`. Wrapped in the `source` region so clicking it opens
#' the card's read-only provenance / "View code" panel (design spec #4's
#' `source` row).
#' @noRd
.source_line <- function(object) {
  shiny::tags$div(
    class = "ar-paper-source ar-mono",
    .source_text(object)
  )
}

#' A copy of `object` with the screen's source line baked into
#' `options$source`, so an RTF render carries the same provenance string
#' the canvas paints. Applied at EXPORT time only -- the live report in the
#' store never carries a stamped date.
#' @noRd
.with_source <- function(object) {
  opts <- object@options
  opts$source <- .source_text(object)
  S7::set_props(object, options = opts)
}

#' The Appendix-I footer datetime stamp, `ddMMMyyyy:hh:mm:ss`. Built from
#' `month.abb` so the month abbreviation never follows the session locale.
#' @noRd
.chrome_stamp <- function(now = Sys.time()) {
  lt <- as.POSIXlt(now)
  sprintf(
    "%02d%s%04d:%02d:%02d:%02d",
    lt$mday,
    toupper(month.abb[lt$mon + 1L]),
    lt$year + 1900L,
    lt$hour,
    lt$min,
    as.integer(lt$sec)
  )
}

#' A copy of `object` with the render-time chrome tokens substituted as
#' LITERALS into the pagehead/pagefoot band strings. arpillar REJECTS
#' those tokens (they would break its byte-deterministic emit), so the
#' app stamps them at render/export time -- the same seam as
#' `.with_source()`. The backend-resolved `{page}`/`{npages}` field
#' codes pass through untouched, as do unknown `{tokens}` -- never
#' silently drop what a user typed.
#'
#' Recognised tokens:
#'   * Chrome (independent of theme): `{datetime}`, `{program}`,
#'     `{program_path}`.
#'   * Study meta (from `theme$study`): `{sponsor}`, `{protocol}`,
#'     `{study}`, `{study_id}`, `{indication}`, `{data_date}`,
#'     `{status}`.
#'   * Population label: `{analysis-set}` -- the object's population
#'     override, else `theme$default_population`, looked up in
#'     `theme$populations`.
#'   * Arm label: `{arm-label}` -- from `theme$arm$label` (per-object
#'     arm-column resolution is a render-leg concern).
#'
#' `now` is injectable for tests; `theme` defaults to empty so callers
#' outside a report context stay correct.
#' @noRd
.with_chrome <- function(object, now = NULL, theme = list()) {
  opts <- object@options
  if (is.null(opts$pagehead) && is.null(opts$pagefoot)) {
    return(object)
  }
  study <- theme$study %||% list()
  tokens <- c(
    datetime = .chrome_stamp(now %||% Sys.time()),
    program = paste0("programs/", .output_slug(object), ".R"),
    program_path = "programs",
    sponsor = as.character(study$sponsor %||% ""),
    protocol = as.character(study$protocol %||% ""),
    study = as.character(study$study %||% ""),
    study_id = as.character(study$study_id %||% ""),
    indication = as.character(study$indication %||% ""),
    data_date = as.character(study$data_date %||% ""),
    status = as.character(study$status %||% "")
  )
  as_lbl <- .resolve_analysis_set(object, theme)
  if (!is.null(as_lbl)) {
    tokens[["analysis-set"]] <- as_lbl
  }
  arm_lbl <- .resolve_arm_label(object, theme)
  if (!is.null(arm_lbl)) {
    tokens[["arm-label"]] <- arm_lbl
  }
  sub_band <- function(b) {
    if (!is.list(b)) {
      return(b)
    }
    lapply(b, function(v) {
      v <- as.character(v)
      for (tok in names(tokens)) {
        v <- gsub(paste0("{", tok, "}"), tokens[[tok]], v, fixed = TRUE)
      }
      v
    })
  }
  opts$pagehead <- sub_band(opts$pagehead)
  opts$pagefoot <- sub_band(opts$pagefoot)
  S7::set_props(object, options = opts)
}

#' `{analysis-set}` resolver: the population attached to `object` (via
#' `object@options$population`) wins; else `theme$default_population`;
#' looked up in `theme$populations` for the display label. Returns NULL
#' when no population applies -- caller drops the token from the token
#' map so an unresolved `{analysis-set}` passes through unchanged.
#' @noRd
.resolve_analysis_set <- function(object, theme) {
  pop_id <- object@options$population
  if (is.null(pop_id) || !nzchar(as.character(pop_id))) {
    pop_id <- theme$default_population
  }
  if (is.null(pop_id) || !nzchar(as.character(pop_id))) {
    return(NULL)
  }
  pops <- theme$populations %||% list()
  entry <- pops[[as.character(pop_id)]]
  if (is.null(entry)) {
    return(NULL)
  }
  as.character(entry$label %||% "")
}

#' `{arm-label}` resolver: `theme$arm$label` when set (the arframe
#' Setup > Page-and-Style band's default is "Treatment" per
#' `.SPEC_ARM`). Returns NULL when unset so the token passes through.
#' @noRd
.resolve_arm_label <- function(object, theme) {
  lbl <- theme$arm$label
  if (is.null(lbl) || !nzchar(as.character(lbl))) {
    return(NULL)
  }
  as.character(lbl)
}

#' A copy of `object` with `@KEY` footnote references resolved against
#' `theme$footnotes` (a keyword -> text register mirroring BMS
#' QC_MasterFile.xlsm's Footnote sheet). Applied at render/export time,
#' same seam as `.with_chrome()`, so the raw `@KEY` reference stays in
#' `emit_code()` output for reproducibility. An entry that is exactly
#' `@KEY` resolves to the registered text; a mixed line like
#' `"@SAFPOP; N is the enrolled count"` also resolves the leading `@KEY`
#' at the start of the string. `@UNKNOWN` (no matching register entry)
#' passes through as a literal -- never silently drop what a user typed.
#' @noRd
.with_footnotes <- function(object, theme = list()) {
  register <- theme$footnotes %||% list()
  fns <- as.character(object@footnotes)
  if (length(fns) == 0L) {
    return(object)
  }
  resolved <- vapply(
    fns,
    function(fn) .expand_key_ref(fn, register),
    character(1),
    USE.NAMES = FALSE
  )
  if (identical(resolved, fns)) {
    return(object)
  }
  S7::set_props(object, footnotes = resolved)
}

# Expand a `@KEY` reference at the start of a footnote line into the
# registered text. Supported shapes:
#   * "@KEY"           -> register[["KEY"]]
#   * "@KEY something" -> paste(register[["KEY"]], "something")
# Unregistered keys pass through as-is. Non-`@`-prefixed strings pass
# through unchanged.
.expand_key_ref <- function(fn, register) {
  m <- regmatches(fn, regexec("^@([A-Za-z_][A-Za-z0-9_]*)(.*)$", fn))[[1L]]
  if (length(m) < 3L) {
    return(fn)
  }
  key <- m[[2L]]
  rest <- m[[3L]]
  hit <- register[[key]]
  if (is.null(hit)) {
    return(fn)
  }
  paste0(as.character(hit), rest)
}

#' `.with_source()` + `.with_chrome()` mapped over every output of a
#' report -- the export package's whole-report leg (both the async JSON
#' handoff and the sync fallback build from the same injected copy).
#' One `now` for the whole package so every output carries the same
#' stamp; the report's own `theme` supplies study-meta tokens.
#' @noRd
.report_with_source <- function(report) {
  now <- Sys.time()
  theme <- report@theme
  pages <- lapply(report@pages, function(pg) {
    S7::set_props(
      pg,
      objects = lapply(pg@objects, function(o) {
        .with_chrome(
          .with_footnotes(.with_source(o), theme = theme),
          now = now,
          theme = theme
        )
      })
    )
  })
  S7::set_props(report, pages = pages)
}

# ---- code view (v5, decision #8) -----------------------------------------

#' Build the code-view panel: a filename bar (Copy / Download .R / Close)
#' above the `emit_code()` reproduction script in a mono `<pre>`. The
#' script regenerates THIS output's RTF from bare arpillar -- the "R code
#' to reproduce it" a regulator or independent QC programmer runs. Copy is
#' pure client JS (`[data-ar-copy]` targets the `<pre>`'s id); Download and
#' Close are server-wired in `mod_paper_server()`.
#' @noRd
.code_panel <- function(store, ns, object) {
  script <- tryCatch(
    arpillar::emit_code(store$con, object),
    error = function(e) {
      paste0("# Could not emit code:\n# ", conditionMessage(e))
    }
  )
  pre_id <- ns("code_pre")
  fname <- paste0(.output_slug(object), ".R")
  shiny::tags$div(
    class = "ar-code",
    shiny::tags$div(
      class = "ar-code-bar ar-mono",
      shiny::tags$span(class = "ar-code-name", fname),
      shiny::tags$div(class = "ar-bar-spacer"),
      shiny::tags$button(
        type = "button",
        class = "ar-code-act",
        `data-ar-copy` = pre_id,
        .icon("copy", 11),
        "Copy"
      ),
      shiny::downloadLink(
        ns("code_dl"),
        label = shiny::tagList(.icon("export", 11), "Download .R"),
        class = "ar-code-act"
      ),
      .action_btn(
        ns("code_close"),
        shiny::tagList(.icon("close", 11), "Close"),
        variant = "link",
        class = "ar-code-act"
      )
    ),
    shiny::tags$pre(
      id = pre_id,
      class = "ar-code-body ar-mono",
      shiny::HTML(.hl_r(script))
    )
  )
}

#' Lightweight server-side R syntax highlighting: comments, strings,
#' numbers, keywords, and function calls wrapped in `ar-hl-*` spans.
#' Everything is HTML-escaped piecewise, so the pre's textContent (what
#' the Copy button reads) stays byte-identical to the script.
#' @noRd
.hl_r <- function(code) {
  # ponytail: one-alternation tokenizer, not a grammar -- strings and
  # comments win by pattern order; nested quotes inside strings are the
  # known ceiling (emit_code never produces them).
  pat <- paste0(
    "\"[^\"\n]*\"", # string
    "|#[^\n]*", # comment
    "|(?<![\\w.])\\d+(?:\\.\\d+)?(?![\\w.])", # number
    "|\\b(?:library|function|if|else|for|TRUE|FALSE|NULL|NA)\\b",
    "|[A-Za-z_.][A-Za-z0-9_.]*(?=\\()" # function call
  )
  esc <- function(x) htmltools::htmlEscape(x)
  m <- gregexpr(pat, code, perl = TRUE)[[1]]
  if (identical(m[1L], -1L)) {
    return(esc(code))
  }
  lens <- attr(m, "match.length")
  out <- character(0)
  pos <- 1L
  for (i in seq_along(m)) {
    if (m[i] > pos) {
      out <- c(out, esc(substr(code, pos, m[i] - 1L)))
    }
    tok <- substr(code, m[i], m[i] + lens[i] - 1L)
    cls <- if (startsWith(tok, "\"")) {
      "ar-hl-str"
    } else if (startsWith(tok, "#")) {
      "ar-hl-com"
    } else if (grepl("^[0-9]", tok)) {
      "ar-hl-num"
    } else if (
      tok %in%
        c(
          "library",
          "function",
          "if",
          "else",
          "for",
          "TRUE",
          "FALSE",
          "NULL",
          "NA"
        )
    ) {
      "ar-hl-kw"
    } else {
      "ar-hl-fn"
    }
    out <- c(out, sprintf('<span class="%s">%s</span>', cls, esc(tok)))
    pos <- m[i] + lens[i]
  }
  if (pos <= nchar(code)) {
    out <- c(out, esc(substr(code, pos, nchar(code))))
  }
  paste(out, collapse = "")
}

# ---- stale notice (run semantics, decision #8) ----------------------------

#' The stale-proof notice: stands in for the table/figure content when a
#' heavy edit (roles/filters) invalidated an already-typeset proof. The
#' page stays a complete shell (title block and source line render around
#' it); Run in the inspector footer re-typesets.
#' @noRd
.stale_panel <- function() {
  shiny::tags$div(
    class = "ar-paper-stale ar-mono",
    shiny::tags$p(class = "ar-paper-stale-word", "STALE"),
    shiny::tags$p("Roles or filters changed since the last typeset."),
    # U+2318 PLACE OF INTEREST SIGN + U+21B5 CARRIAGE RETURN -- the Run
    # shortcut glyphs (mod_card_ui's footer); \u escapes keep R/
    # ASCII-clean (R CMD check portability rule).
    shiny::tags$p("Run (\u2318\u21b5) re-typesets this proof.")
  )
}

# ---- error summary (GOV.UK pattern) --------------------------------------

#' One jump link in the error summary: the `validate_output()` message,
#' clicking it posts the SAME region-click input a furniture/ghost region
#' posts (`ns("region")`), routed through `.ghost_region()` -- so a jump
#' link and clicking the corresponding ghost slot land on the identical
#' card.
#' @noRd
.error_jump_link <- function(ns, region, message) {
  click_js <- sprintf(
    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
    ns("region"),
    region
  )
  shiny::tags$li(
    shiny::tags$a(
      href = "#",
      onclick = paste0(click_js, "; return false;"),
      message
    )
  )
}

#' Split a (possibly multi-line, cli-formatted) `conditionMessage()` into a
#' plain headline and cleaned-up detail lines. A cli condition message
#' (e.g. `arpillar_error_input`) is one headline followed by `\n`-joined
#' bullet lines, each prefixed with a cli glyph (`x` -> "✖", `i` ->
#' "ℹ", `*`/bullet -> "•", `v` -> "✔", `!` -- see
#' `cli::cli_abort()`) and a space; rendering the raw string in a single
#' `<p>` collapses the `\n`s into one run-on line with the glyph bleeding
#' into the prose. The headline is `parts[[1]]` verbatim; each detail line
#' has its leading glyph + surrounding whitespace stripped.
#' @noRd
.split_error_message <- function(msg) {
  parts <- strsplit(msg, "\n", fixed = TRUE)[[1]]
  # ✖/ℹ/•/✔ = the "x"/"i"/bullet/"v" cli glyphs
  # (\u escapes, not literal UTF-8 bytes, so R CMD check's file-wide
  # non-ASCII-characters scan of executable code stays clean).
  glyph_re <- "^\\s*[\u2716\u2139\u2022\u2714!]\\s*"
  list(
    headline = parts[[1]],
    detail = sub(glyph_re, "", parts[-1])
  )
}

#' The GOV.UK-pattern error summary rendered at the top of the paper when a
#' ready-status render throws anyway (the static oracle predicts
#' acceptance but the actual data does not match, e.g. a role names a
#' column absent from the bound dataset). `role="alert"` + `tabindex="-1"`
#' so `session$sendCustomMessage("ar-focus", ...)` (the same handler
#' `mod_frame.R`'s undo/redo buttons already register) can move focus onto
#' it. `msg` is split into a headline `<p>` plus one muted `<p>` per detail
#' line (`.split_error_message()`) rather than rendered raw, so a
#' multi-line cli message reads as a short list, not a run-on line with a
#' bullet glyph bleeding into the prose.
#' @noRd
.error_summary <- function(ns, id, object, msg) {
  v <- arpillar::validate_output(object)
  links <- if (nrow(v) > 0L) {
    lapply(seq_len(nrow(v)), function(i) {
      .error_jump_link(ns, .ghost_region(v$control_id[[i]]), v$message[[i]])
    })
  } else {
    list(.error_jump_link(ns, "title", "Check the output configuration."))
  }
  parsed <- .split_error_message(msg)
  detail_p <- lapply(parsed$detail, function(line) {
    shiny::tags$p(class = "ar-mono ar-problem-detail", line)
  })
  shiny::tags$div(
    class = "ar-problem",
    id = id,
    role = "alert",
    tabindex = "-1",
    shiny::tags$h2("There is a problem"),
    shiny::tags$p(class = "ar-mono", parsed$headline),
    detail_p,
    shiny::tags$ul(links)
  )
}

# ---- UI ---------------------------------------------------------------

#' The paper module UI (v5): the content-hugging galley artifact -- the
#' sheet slot (server-rendered end to end via `uiOutput`) plus a
#' mounted-but-hidden `plotOutput` for the figure leg -- both containers
#' stay mounted, a class flip on the sheet root picks which one shows
#' (mirrors `mod_frame.R`'s "all bodies mount, CSS picks one" pattern).
#' No fit/page toolbar (decision #7): the artifact hugs its content; page
#' width is export's business.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_paper_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    id = ns("desk"),
    class = "ar-desk-col",
    # The canvas toolbar (2026-07-04): Run / .rtf / Output|Code at the top
    # of the desk -- the Preact component mounts into this child module.
    mod_toolbar_ui(ns("toolbar")),
    shiny::div(
      id = ns("sheet"),
      class = "ar-paper",
      `data-ar-paper` = ns(NULL),
      shiny::uiOutput(ns("sheet_html_slot")),
      shiny::div(
        class = "ar-paper-figure-slot",
        # A FIXED pixel height, not "auto": `plotOutput(height = "auto")`
        # sizes the graphics device off the CONTAINER's CSS height, and the
        # container has no image inside it (so no intrinsic height) until
        # the first successful render -- a DRAFT/error figure's `req()`
        # inside `renderPlot()` never draws anything, so an "auto"-height
        # container collapses toward zero and the base graphics device
        # throws "figure margins too large" trying to open at ~0px. A
        # fixed height keeps the device area valid in every state (draft,
        # error, ready).
        shiny::plotOutput(ns("sheet_figure"), height = "460px")
      )
    ),
    # The code view (decision #8): an alternate desk surface holding the
    # `emit_code()` reproduction script. Mounts alongside the sheet; the
    # `ar-showing-code` class on the desk (flipped by `rv$code_view`)
    # picks which shows -- no unmount, so returning to the artifact is a
    # class toggle, not a re-render.
    shiny::uiOutput(ns("code_slot"))
  )
}

# ---- server -------------------------------------------------------------

#' The paper module server: renders the selected output's content (table
#' or figure) through the export-identical seam, or the ghost shell / error
#' summary when it is not ready / fails.
#' @param id *The module namespace, matching `mod_paper_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_paper_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_toolbar_server("toolbar", store)

    # The whole sheet body: running head, title block (real or ghost), the
    # table/figure content (or its ghost), footnotes, source line. Gated to
    # the same two triggers the brief specifies -- a report mutation
    # (role/options/filters edit) or a selection change -- NOT a bare
    # `store$rv$report` read with no bindEvent, so an UNRELATED store write
    # (e.g. `rv$log`) never forces a re-render.
    #
    # Concurrent-mutator drag guard: `document.body.dataset.arDragging` is
    # set (arframe.js) for the physical duration of a Contents TOC drag.
    # This observer stays a plain `bindEvent` -- it already only fires on
    # the two named triggers, not continuously -- because the guard cannot
    # live server-side at all: `req(!isTRUE(...))` has no way to read a DOM
    # dataset attribute, and the risk this guards against (a region click,
    # real or ghost, firing WHILE the flag is set) is a CLIENT input that
    # never needs to reach Shiny in the first place. The guard is therefore
    # entirely in the JS bridge: `arRegionClick()` queues the click instead
    # of calling `Shiny.setInputValue()` while `arDragging` is set, and
    # `arFlushDeferredRegionClicks()` (called from `arInitSortables()`'s
    # `onEnd`, right after it clears the flag) posts any queued click the
    # instant the drag ends -- see arframe.js's "THE CONCURRENT-MUTATOR
    # DRAG GUARD" section. No input this observer's `renderUI` depends on
    # (`store$rv$report`/`store$rv$selected`) can itself be driven by a
    # region click, so gating the click at its source is sufficient; there
    # is no separate mid-drag `rv$report` mutator to guard against today
    # (mod_contents.R's own reorder-drop posts once on `onEnd`, after the
    # flag is already cleared).
    output$sheet_html_slot <- shiny::renderUI({
      .render_sheet(store, session, ns)
    }) |>
      shiny::bindEvent(store$rv$report, store$rv$selected, store$rv$run_nonce)

    output$sheet_figure <- shiny::renderPlot(
      {
        obj <- selected_object(store)
        shiny::req(obj)
        shiny::req(arpillar::output_status(obj) == "ready")
        shiny::req(.is_figure_type(obj@type))
        p <- tryCatch(
          arpillar::render_ggplot(store$con, obj),
          arpillar_error_input = function(e) NULL,
          error = function(e) NULL
        )
        shiny::req(p)
        p
      },
      res = 96
    ) |>
      shiny::bindEvent(store$rv$report, store$rv$selected, store$rv$run_nonce)

    # The class flip: table content shows the HTML slot, a figure shows the
    # plot slot, "not ready"/no-selection shows neither's real content
    # (the ghost markup lives INSIDE the HTML slot in that case, see
    # `.render_sheet()`) -- one class read off the live object, applied via
    # a session message so neither container is ever unmounted/remounted.
    shiny::observe({
      obj <- selected_object(store)
      kind <- if (is.null(obj)) {
        "none"
      } else if (.is_figure_type(obj@type)) {
        "figure"
      } else {
        "table"
      }
      session$sendCustomMessage(
        "ar-paper-kind",
        list(id = ns("sheet"), kind = kind)
      )
    }) |>
      shiny::bindEvent(store$rv$report, store$rv$selected)

    # Region clicks (real furniture, ghost slots, and error-summary jump
    # links all post to this one input -- see the `[data-ar-region]`
    # delegated handler in arframe.js). The contract is a plain region
    # STRING; anything else (a stray browser script, an extension) is
    # dropped -- a malformed payload must never take the session down.
    shiny::observeEvent(input$region, {
      region <- input$region
      if (!is.character(region) || length(region) != 1L || !nzchar(region)) {
        return()
      }
      open_card(store, region)
    })

    # The canvas context menu (bridge.js, 2026-07-04): "Add output" posts
    # add_first; "Delete output" posts ctx_remove and is only offered by
    # the client when a TOC row is active, but the guard here keeps a
    # stale click honest.
    shiny::observeEvent(input$add_first, {
      store$rv$adding <- TRUE
    })

    shiny::observeEvent(input$ctx_remove, {
      id <- store$rv$selected
      if (is.null(id)) {
        return()
      }
      remove_output(store, id)
    })

    # ---- code view (v5) ----
    # The panel content: the emit_code() script for the current selection.
    # Rebuilt on a report edit (the roles/options that shape the script) or
    # a selection change; empty when nothing is selected.
    output$code_slot <- shiny::renderUI({
      obj <- selected_object(store)
      if (is.null(obj)) {
        return(NULL)
      }
      .code_panel(store, ns, obj)
    }) |>
      shiny::bindEvent(store$rv$report, store$rv$selected)

    # Flip the desk between artifact and code view on every code_view
    # change -- a class toggle, so neither surface is remounted.
    shiny::observe({
      session$sendCustomMessage(
        "ar-code-view",
        list(id = ns("desk"), on = isTRUE(store$rv$code_view))
      )
    }) |>
      shiny::bindEvent(store$rv$code_view)

    shiny::observeEvent(input$code_close, {
      store$rv$code_view <- FALSE
    })

    # Download the reproduction script -- the same emit_code() the panel
    # shows, written to disk (path arg) so it is byte-identical to the view.
    output$code_dl <- shiny::downloadHandler(
      filename = function() {
        obj <- selected_object(store)
        if (is.null(obj)) "output.R" else paste0(.output_slug(obj), ".R")
      },
      content = function(file) {
        obj <- selected_object(store)
        if (is.null(obj)) {
          .abort_app("No output is selected.")
        }
        arpillar::emit_code(store$con, obj, path = file)
      }
    )

    # Born hidden (the app opens in Data mode, 2026-07-04): the desk's
    # surfaces must keep computing so Report mode never opens blank.
    shiny::outputOptions(output, "sheet_html_slot", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "sheet_figure", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "code_slot", suspendWhenHidden = FALSE)
    # `.ar-hidden-dl` is display:none, which suspends the download binding
    # -- href stays `#`, so the programmatic click falls back to the page
    # URL and saves `download.html`. See mod_toolbar.R.
    shiny::outputOptions(output, "code_dl", suspendWhenHidden = FALSE)

    invisible(NULL)
  })
}

# ---- sheet body dispatch ---------------------------------------------------

#' Build the whole sheet body: dispatches on selection / output_status /
#' render success. The sheet is ALWAYS a complete page shell, never a bare
#' spinner or blank div. A READY table is tabular's own full page render
#' (canvas flip 2026-07-04); every other path (ghost, stale, error,
#' figure) still paints the arframe title block / source line around its
#' content.
#' @noRd
.render_sheet <- function(store, session, ns) {
  obj <- selected_object(store)

  if (is.null(obj)) {
    # The paper only shows when the LoC is drilled, which always carries a
    # selection -- so a NULL here is the hidden list view (the renderUI still
    # fires while hidden). Render nothing; the List-of-Contents owns the
    # empty state now, and fabricating one here would just be dead markup.
    return(NULL)
  }

  status <- arpillar::output_status(obj)
  if (!identical(status, "ready")) {
    return(shiny::tagList(
      ghost_shell(obj),
      .source_line(obj)
    ))
  }

  # Run semantics (decision #8): a heavy edit marked this proof stale --
  # never auto re-collect from DuckDB; the notice stands in until Run.
  if (obj@id %in% store$rv$stale) {
    return(shiny::tagList(
      .title_block(obj),
      .stale_panel(),
      .source_line(obj)
    ))
  }

  if (.is_figure_type(obj@type)) {
    result <- .try_render_figure(store, obj)
  } else {
    result <- .try_render_table(store, obj)
  }

  if (!result$ok) {
    store$rv$broken <- union(store$rv$broken, obj@id)
    log_line(store, paste0("render failed: ", obj@id, " -- ", result$message))
    err_id <- ns("problem")
    session$sendCustomMessage("ar-focus", list(id = err_id))
    return(shiny::tagList(
      .error_summary(ns, err_id, obj, result$message),
      .title_block(obj),
      .source_line(obj)
    ))
  }

  store$rv$broken <- setdiff(store$rv$broken, obj@id)
  if (.is_figure_type(obj@type)) {
    return(shiny::tagList(
      .title_block(obj),
      result$content,
      .source_line(obj)
    ))
  }
  # Canvas flip (2026-07-04, supersedes decision #7's chrome-free galley):
  # a READY table is tabular's own full page render -- title block,
  # footnotes, source, and any running header/footer bands all come from
  # the spec, so the sheet adds NOTHING around it. Painting arframe's own
  # title/source here again would double-print them.
  result$content
}

#' Render a table output through the export-identical seam
#' (`cached_ard()` -> `arpillar::render_spec()` -> `htmltools::as.tags()`),
#' catching an `arpillar_error_input` (a role names a column absent from
#' the bound dataset -- the STATIC oracle predicts acceptance but the
#' actual data does not match) or any other render-time error. Returns
#' `list(ok, content, message)` so the caller never has to branch on
#' whether `content` is meaningful.
#' @noRd
.try_render_table <- function(store, object) {
  tryCatch(
    {
      ard <- cached_ard(store, object)
      # Paper parity on screen: the canvas spec carries the SAME stamped
      # source line and chrome literals the .rtf download gets -- tabular
      # renders the whole page (title block through source), the sheet
      # adds nothing.
      spec <- arpillar::render_spec(
        ard,
        .with_chrome(
          .with_footnotes(.with_source(object), theme = store$rv$report@theme),
          theme = store$rv$report@theme
        )
      )
      orient <- object@options$orientation %||% "landscape"
      list(
        ok = TRUE,
        content = shiny::div(
          class = "ar-paper-table-wrap",
          `data-ar-orient` = if (identical(orient, "portrait")) {
            "portrait"
          } else {
            "landscape"
          },
          htmltools::as.tags(spec)
        ),
        message = NULL
      )
    },
    arpillar_error_input = function(e) {
      list(ok = FALSE, content = NULL, message = conditionMessage(e))
    },
    error = function(e) {
      list(ok = FALSE, content = NULL, message = conditionMessage(e))
    }
  )
}

#' Render a figure output: `output$sheet_figure` (a `renderPlot()`) already
#' calls `render_ggplot()` for the ACTUAL pixels, so this leg only needs to
#' confirm the render will not throw (a dry run) to decide ok/error and
#' surface the right message -- `plotOutput` cannot report a render
#' exception back into `renderUI`'s own return value, so the dry run is
#' what lets the SAME error-summary path used for tables also cover
#' figures. `content` is the (already class-flipped, via
#' `mod_paper_ui()`'s `ar-paper-figure-slot`) figure container placeholder
#' -- the real pixels come from `output$sheet_figure` mounted alongside it.
#' @noRd
.try_render_figure <- function(store, object) {
  tryCatch(
    {
      arpillar::render_ggplot(store$con, object)
      list(ok = TRUE, content = shiny::tagList(), message = NULL)
    },
    arpillar_error_input = function(e) {
      list(ok = FALSE, content = NULL, message = conditionMessage(e))
    },
    error = function(e) {
      list(ok = FALSE, content = NULL, message = conditionMessage(e))
    }
  )
}
