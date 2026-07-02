# The QC sheet (design spec 5.4, plan Task 15): the proof-check + run log.
# QC mode swaps the desk to a paper-styled proof-check sheet -- one row per
# output (mono number + title + the SAME status stamp the TOC shows), each
# not-ready output's validate_output() gaps listed as jump links, a summary
# line, and below a rule the run log (newest first). QC is a document too, so
# it wears the page chrome (a running head) the on-screen galley artifact
# deliberately drops (decision #7). Every status read routes through
# `.toc_rows()` -> `output_status()` -- no second predicate lives here.

# ---- problems -------------------------------------------------------------

#' The per-problem jump list for one output, given its folded TOC status.
#'
#' A `"ready"` output has none. A `"broken"` render or a `"stale"` proof has
#' no `validate_output()` row (both are "ready" to the static oracle), so
#' each gets ONE synthesized entry routed to the title block -- the jump
#' still lands somewhere actionable. Every other status (`"draft"`,
#' `"needs_data"`) lists its real `validate_output()` gaps, each mapped to
#' the page region that would fill it (`.ghost_region()`, shared with the
#' ghost shell and the paper error summary).
#' @noRd
.qc_problems <- function(object, status) {
  if (identical(status, "ready")) {
    return(list())
  }
  if (identical(status, "broken")) {
    return(list(list(
      region = "title",
      message = "Render failed; open the output to inspect."
    )))
  }
  if (identical(status, "stale")) {
    return(list(list(
      region = "title",
      message = "Proof is stale; open the output and Run."
    )))
  }
  v <- arpillar::validate_output(object)
  if (nrow(v) == 0L) {
    return(list())
  }
  lapply(seq_len(nrow(v)), function(i) {
    list(
      region = .ghost_region(v$control_id[[i]]),
      message = v$message[[i]]
    )
  })
}

# ---- sheet builder --------------------------------------------------------

#' One jump link: the problem message as an anchor whose click posts
#' `qc-jump` (the output id + the mapped region) for the server to route.
#' Priority `event` so re-clicking the same link re-fires; `return false`
#' stops the `#` href from scrolling the sheet.
#' @noRd
.qc_jump_link <- function(ns, id, region, message) {
  click_js <- sprintf(
    "Shiny.setInputValue('%s', {id: '%s', region: '%s'}, {priority: 'event'})",
    ns("jump"),
    id,
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

#' One proof-check row: mono number + title + status stamp, with a jump-link
#' list below when the output has unmet requirements.
#' @noRd
.qc_row <- function(ns, report, row) {
  obj <- .find_object(report, row$id)
  problems <- if (is.null(obj)) list() else .qc_problems(obj, row$status)
  shiny::tags$div(
    class = "ar-qc-row",
    shiny::tags$div(
      class = "ar-qc-row-head",
      shiny::tags$span(class = "ar-qc-number ar-mono", row$number),
      shiny::tags$span(class = "ar-qc-title", row$title),
      .stamp(row$status)
    ),
    if (length(problems) > 0L) {
      shiny::tags$ul(
        class = "ar-qc-problems",
        lapply(problems, function(p) {
          .qc_jump_link(ns, row$id, p$region, p$message)
        })
      )
    }
  )
}

#' The run-log block: the newest line first, a mono list below the RUN LOG
#' micro-label. Empty state is an explicit note rather than a blank gap.
#' @noRd
.qc_log <- function(log) {
  shiny::tags$div(
    class = "ar-qc-log",
    .label("RUN LOG"),
    if (length(log) == 0L) {
      shiny::tags$p(class = "ar-qc-log-empty ar-mono", "Nothing logged yet.")
    } else {
      shiny::tags$div(
        class = "ar-qc-log-lines ar-mono",
        lapply(rev(log), function(line) {
          shiny::tags$div(class = "ar-qc-log-line", line)
        })
      )
    }
  )
}

#' The whole proof-check sheet: running head, summary line, one row per
#' output, a rule, then the run log. `broken`/`stale` fold into the row
#' status exactly as the TOC does (`.toc_rows()`), so a stamp here can never
#' disagree with the same output's stamp in the Contents column.
#' @noRd
.qc_sheet <- function(ns, report, broken, stale, log) {
  rows <- .toc_rows(report, broken, stale)
  ready_n <- sum(vapply(
    rows,
    function(r) identical(r$status, "ready"),
    logical(1)
  ))
  total_n <- length(rows)
  shiny::tags$div(
    class = "ar-qc-sheet",
    # U+2014 EM DASH -- \u escape keeps R/ ASCII-clean (portability rule).
    shiny::tags$div(
      class = "ar-qc-head ar-mono",
      paste0("Quality control \u2014 ", report@name)
    ),
    shiny::tags$div(
      class = "ar-qc-summary ar-mono",
      sprintf(
        "%d of %d output%s ready",
        ready_n,
        total_n,
        if (total_n == 1L) "" else "s"
      )
    ),
    if (total_n == 0L) {
      shiny::tags$p(class = "ar-qc-empty ar-mono", "No outputs yet.")
    } else {
      shiny::tags$div(
        class = "ar-qc-rows",
        lapply(rows, function(r) .qc_row(ns, report, r))
      )
    },
    shiny::tags$div(class = "ar-qc-rule"),
    .qc_log(log)
  )
}

# ---- UI --------------------------------------------------------------------

#' The QC mode body: a scrolling desk holding the server-rendered
#' proof-check sheet.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_qc_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "ar-qc",
    shiny::uiOutput(ns("sheet"))
  )
}

# ---- server ----------------------------------------------------------------

#' The QC server: renders the proof-check sheet from the store's report +
#' broken/stale/log state, and routes a jump click back to Report mode.
#' @param id *The module namespace, matching `mod_qc_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_qc_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$sheet <- shiny::renderUI({
      .qc_sheet(
        ns,
        store$rv$report,
        store$rv$broken,
        store$rv$stale,
        store$rv$log
      )
    })
    # The QC body is shown/hidden by the custom `ar-mode` class, NOT a Shiny
    # tabset, so Shiny never learns the body became visible and would leave a
    # suspended output blank when the user switches to QC. Force it to
    # compute while hidden (the same fix the inspector panes needed).
    shiny::outputOptions(output, "sheet", suspendWhenHidden = FALSE)

    # A jump link: select the output, flip to Report mode (mirror the class
    # to the client -- setting rv$mode directly bypasses the frame's own
    # `ar-mode` send), and open the inspector on the mapped region.
    shiny::observeEvent(input$jump, {
      store$rv$selected <- input$jump$id
      store$rv$mode <- "report"
      session$sendCustomMessage("ar-mode", "report")
      open_card(store, input$jump$region)
    })

    invisible(NULL)
  })
}
