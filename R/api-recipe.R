# ──────────────────────────────────────────────────────────────────────────────
# api-recipe.R — Pipeline composition: fr_recipe, fr_apply, c()
#
# Captures reusable verb chains that can be applied to any fr_spec.
# Recipes store calls (not closures), so they are serializable via
# saveRDS/readRDS.
# ──────────────────────────────────────────────────────────────────────────────

#' Create a Reusable Table Recipe
#'
#' @description
#'
#' Captures a sequence of `fr_*()` verb calls as an `fr_recipe` object that
#' can be applied to any `fr_spec`. Recipes enable team-wide standardisation:
#' define your company style once, reuse it across hundreds of tables.
#'
#' Apply a recipe to a spec with [fr_apply()]. Compose multiple recipes with
#' `c()`: `c(recipe_a, recipe_b)` creates a combined recipe where `recipe_b`
#' overrides `recipe_a` for verbs that replace (most verbs), and appends for
#' verbs that accumulate (`fr_spans`, `fr_styles`).
#'
#' @param ... Unevaluated `fr_*()` verb calls. Each call must be a pipeline
#'   verb that takes an `fr_spec` as its first argument. The `spec` argument
#'   is omitted — it is injected when the recipe is applied.
#'
#' @return An `fr_recipe` object (list of captured call expressions).
#'
#' @section Application:
#' * **`fr_apply()`**: `spec |> fr_apply(recipe)` — pipe-friendly
#' * **`c()`**: `c(recipe1, recipe2)` — compose two recipes
#'
#' @section Serialisation:
#' Recipes store R language objects (calls), not closures, so they survive
#' `saveRDS()` / `readRDS()` roundtrips:
#' ```r
#' saveRDS(company_style, "company_style.rds")
#' company_style <- readRDS("company_style.rds")
#' ```
#'
#' @examples
#' ## ── Recipe for page layout ────────────────────────────────────────────────
#'
#' page_recipe <- fr_recipe(
#'   fr_page(orientation = "landscape", font_size = 9),
#'   fr_pagehead(left = "{program}", right = "{datetime}"),
#'   fr_pagefoot(center = "Page {thepage} of {total_pages}")
#' )
#' page_recipe
#'
#' ## ── Recipe for styling ────────────────────────────────────────────────────
#'
#' style_recipe <- fr_recipe(
#'   fr_hlines("header"),
#'   fr_header(bold = TRUE, align = "center")
#' )
#' style_recipe
#'
#' ## ── Apply a recipe then add per-table customisation ───────────────────────
#'
#' company_style <- c(page_recipe, style_recipe)
#'
#' spec <- tbl_demog |>
#'   fr_table() |>
#'   fr_apply(company_style) |>
#'   fr_titles("Table 14.1.1", "Demographics")
#' spec
#'
#' ## ── Compose: base + override for AE tables ────────────────────────────────
#'
#' ae_style <- c(company_style, fr_recipe(
#'   fr_hlines("box"),
#'   fr_page(font_size = 8)
#' ))
#' ae_spec <- tbl_ae_soc |>
#'   fr_table() |>
#'   fr_apply(ae_style) |>
#'   fr_titles("Table 14.3.1", "AE Summary by SOC")
#'
#' @seealso [fr_apply()] for pipe-friendly application,
#'   [fr_theme()] for session-wide defaults.
#'
#' @export
fr_recipe <- function(...) {
  calls <- enexprs(...)
  if (length(calls) == 0L) {
    cli_abort(
      c(
        "At least one verb call is required.",
        "i" = "Example: {.code fr_recipe(fr_page(font_size = 9), fr_hlines(\"header\"))}"
      ),
      call = caller_env()
    )
  }

  # Validate that each element looks like a function call
  for (i in seq_along(calls)) {
    if (!is.call(calls[[i]])) {
      cli_abort(
        "Argument {i} in {.fn fr_recipe} must be a function call (e.g., {.code fr_page(...)}).",
        call = caller_env()
      )
    }
  }

  structure(calls, class = "fr_recipe")
}


#' Apply a Recipe to a Spec
#'
#' @description
#'
#' Applies an `fr_recipe` to an `fr_spec`, evaluating each captured verb call
#' in sequence with the spec as the first argument. This is the primary way
#' to use recipes in a pipeline.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param recipe An `fr_recipe` object from [fr_recipe()].
#'
#' @return A modified `fr_spec`.
#'
#' @examples
#' # Basic application
#' style <- fr_recipe(fr_hlines("header"), fr_header(bold = TRUE))
#' spec <- tbl_demog |> fr_table() |> fr_apply(style)
#'
#' # Apply a recipe and then override one parameter with a direct verb call
#' spec <- tbl_ae_soc |>
#'   fr_table() |>
#'   fr_apply(style) |>
#'   fr_header(bold = FALSE)   # override the recipe's bold = TRUE
#'
#' # Apply to different datasets with the same recipe
#' demog_spec <- tbl_demog |> fr_table() |> fr_apply(style)
#' disp_spec  <- tbl_disp  |> fr_table() |> fr_apply(style)
#'
#' @seealso [fr_recipe()]
#' @export
fr_apply <- function(spec, recipe) {
  call <- caller_env()
  check_fr_spec(spec, call = call)
  if (!inherits(recipe, "fr_recipe")) {
    cli_abort(
      c(
        "{.arg recipe} must be an {.cls fr_recipe} object (created by {.fn fr_recipe}).",
        "x" = "You supplied {.obj_type_friendly {recipe}}."
      ),
      call = call
    )
  }

  env <- caller_env()
  for (expr in recipe) {
    # Inject spec as the first argument
    expr <- as.call(c(expr[[1L]], quote(.spec), as.list(expr[-1L])))
    spec <- eval(expr, envir = list(.spec = spec), enclos = env)
  }
  spec
}


#' Combine Recipes
#'
#' @description
#' Concatenates two or more `fr_recipe` objects into a single recipe.
#' Later recipes override earlier ones for verbs that replace (most verbs),
#' and append for verbs that accumulate ([fr_spans()], [fr_styles()]).
#'
#' @param ... `fr_recipe` objects to combine.
#'
#' @return A combined `fr_recipe` object.
#'
#' @examples
#' # Compose two recipes
#' base_style <- fr_recipe(fr_page(orientation = "landscape"))
#' ae_style <- fr_recipe(fr_hlines("box"))
#' combined <- c(base_style, ae_style)
#' combined
#'
#' # Compose and apply in one pipeline
#' layout <- fr_recipe(
#'   fr_page(orientation = "landscape", font_size = 9),
#'   fr_pagehead(left = "{program}")
#' )
#' rules <- fr_recipe(
#'   fr_hlines("header"),
#'   fr_header(bold = TRUE)
#' )
#' full_style <- c(layout, rules)
#' full_style   # 4 verbs
#'
#' out <- file.path(tempdir(), "composed.rtf")
#' tbl_tte |>
#'   fr_table() |>
#'   fr_apply(full_style) |>
#'   fr_titles("Table 14.2.1", "Time-to-Event Analysis") |>
#'   fr_render(out)
#' unlink(out)
#'
#' @seealso [fr_recipe()], [fr_apply()]
#' @export
c.fr_recipe <- function(...) {
  recipes <- list(...)
  for (r in recipes) {
    if (!inherits(r, "fr_recipe")) {
      cli_abort(
        "All arguments to {.fn c} must be {.cls fr_recipe} objects. Got {.cls {class(r)}}.",
        call = caller_env()
      )
    }
  }
  combined <- unlist(lapply(recipes, unclass), recursive = FALSE)
  structure(combined, class = "fr_recipe")
}


#' @export
print.fr_recipe <- function(x, ...) {
  n <- length(x)
  cli::cli_h3("fr_recipe ({n} verb{?s})")
  for (i in seq_along(x)) {
    fn_name <- as.character(x[[i]][[1L]])
    # Extract key arguments (skip first which is the function name)
    args <- x[[i]][-1L]
    if (length(args) > 0L) {
      arg_strs <- vapply(
        seq_along(args),
        function(j) {
          nm <- names(args)[j]
          val <- deparse(args[[j]], width.cutoff = 30L)[1L]
          if (!is.null(nm) && nzchar(nm)) paste0(nm, "=", val) else val
        },
        character(1)
      )
      arg_str <- paste(arg_strs, collapse = ", ")
      if (nchar(arg_str) > 50L) {
        arg_str <- paste0(substr(arg_str, 1, 47), "...")
      }
      cli::cli_text("  {i}. {fn_name}({arg_str})")
    } else {
      cli::cli_text("  {i}. {fn_name}()")
    }
  }
  invisible(x)
}
