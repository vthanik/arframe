# The canvas toolbar (2026-07-04, explorer-style): Run / .rtf / Output|Code
# live at the TOP of the desk, not in the inspector footer. The visible
# controls are a Preact component (srcjs/toolbar.js) rendered into the
# mount div below; this module owns everything server-side -- the Run
# re-typeset, the per-output RTF download, the code-view flag -- and
# pushes display state to the client via the "ar-toolbar" custom message.
# The Preact side posts plain namespaced inputs (view / run / rtf_click),
# so the store stays the only state owner (all state in rv, never DOM).

#' The canvas toolbar UI: the Preact mount point plus a hidden download
#' link (a browser download must be initiated by an <a>; the Preact .rtf
#' button posts `rtf_click` and the server relays an `ar-click` to this
#' link -- the same pattern as the frame's Export package).
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_toolbar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "ar-toolbar",
    shiny::div(
      id = ns("mount"),
      class = "ar-toolbar-mount",
      `data-ar-toolbar` = ns(NULL)
    ),
    shiny::tagAppendAttributes(
      shiny::downloadLink(ns("rtf"), label = NULL, class = "ar-hidden-dl"),
      `aria-hidden` = "true",
      tabindex = "-1"
    )
  )
}

#' The canvas toolbar server: Run (drops the ARD memo and bumps
#' `run_nonce` so the paper re-typesets fresh), the per-output `.rtf`
#' download through the export-identical seam, the Output|Code segmented
#' view driving `rv$code_view`, and the state push that keeps the Preact
#' component honest (code_view / ready / stale; `running` is reserved --
#' Run is synchronous today).
#' @param id *The module namespace, matching `mod_toolbar_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_toolbar_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Run: drop every memoized ARD so the rebuild is honest (a stale
    # upstream parquet re-collects rather than replaying the memo), clear
    # the stale-proof flags (Run IS the re-typeset), then bump the nonce
    # the paper's renderers bind to.
    shiny::observeEvent(input$run, {
      keys <- grep("^ard::", ls(store$cache), value = TRUE)
      rm(list = keys, envir = store$cache)
      store$rv$stale <- character(0)
      store$rv$run_nonce <- store$rv$run_nonce + 1L
      log_line(store, "run: re-typeset requested")
    })

    # The Output|Code segmented control posts "output" or "code"; the desk
    # flip itself is mod_paper's ar-code-view class toggle off this flag.
    shiny::observeEvent(input$view, {
      store$rv$code_view <- identical(input$view, "code")
    })

    # The Preact .rtf button cannot be an <a>; relay its click to the
    # hidden download link.
    shiny::observeEvent(input$rtf_click, {
      session$sendCustomMessage("ar-click", list(id = ns("rtf")))
    })

    # The per-output RTF -- the SAME render seam as export (decision #7's
    # one-spec rule): tables through render_rtf, figures through
    # render_figure_rtf.
    output$rtf <- shiny::downloadHandler(
      filename = function() {
        obj <- selected_object(store)
        if (is.null(obj)) "output.rtf" else paste0(.output_slug(obj), ".rtf")
      },
      content = function(file) {
        obj <- selected_object(store)
        if (is.null(obj)) {
          .abort_app("No output is selected.")
        }
        # Paper parity: bake the screen's own source line into the emitted
        # RTF (options$source; the engine renders it verbatim and never
        # stamps a date itself), and stamp the running-band chrome tokens
        # to literals. The ARD memo key ignores options, so the cached ARD
        # is reused as-is.
        theme <- store$rv$report@theme
        obj <- .with_chrome(
          .with_footnotes(.with_source(obj), theme = theme),
          theme = theme
        )
        if (.is_figure_type(obj@type)) {
          arpillar::render_figure_rtf(store$con, obj, file)
        } else {
          ard <- cached_ard(store, obj)
          arpillar::render_rtf(ard, obj, file)
        }
      }
    )
    # Keep the download output non-suspended: the anchor lives under
    # `.ar-hidden-dl` (`display: none`), which Shiny normally treats as
    # not-visible and suspends -- leaving the <a>'s href stuck at `#`, so
    # a programmatic .click() falls back to the current page URL and
    # macOS's save dialog offers `download.html`. Must run AFTER the
    # `output$rtf <-` assignment; `outputOptions()` errors otherwise.
    shiny::outputOptions(output, "rtf", suspendWhenHidden = FALSE)

    # State push: everything the Preact component displays. The client
    # buffers the last message until the component mounts, so ordering
    # against the first render is not a race.
    shiny::observe({
      obj <- selected_object(store)
      sel <- store$rv$selected
      ready <- !is.null(obj) &&
        identical(arpillar::output_status(obj), "ready") &&
        !(sel %in% store$rv$broken)
      session$sendCustomMessage(
        "ar-toolbar",
        list(
          id = ns("mount"),
          state = list(
            code_view = isTRUE(store$rv$code_view),
            ready = ready,
            stale = !is.null(sel) && sel %in% store$rv$stale,
            running = FALSE
          )
        )
      )
    }) |>
      shiny::bindEvent(
        store$rv$report,
        store$rv$selected,
        store$rv$code_view,
        store$rv$stale,
        store$rv$broken,
        ignoreNULL = FALSE
      )

    invisible(NULL)
  })
}
