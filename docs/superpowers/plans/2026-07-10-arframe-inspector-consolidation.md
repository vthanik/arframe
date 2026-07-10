# Inspector Consolidation + Slug Filenames + downlit Code View — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement `docs/superpowers/specs/2026-07-10-arframe-inspector-consolidation-design.md` — three-pane inspector with a top pill strip and accordions, Ranks folded into an Options ORDER section, an in-depth help-modal system, slug filenames for json/R/rtf, downlit-highlighted code view.

**Architecture:** Two repos. arpillar gains exported `output_slug()`/`output_slugs()` and slug-named `outputs/*.json` with stale-file cleanup; arframe consumes them, restructures the inspector (`mod_card*.R`), adds `utils_help.R` + `.accordion_section()` atoms, and swaps `.hl_r()` for `downlit::highlight()`.

**Tech Stack:** R, Shiny modules, S7, native `<details>` accordions, downlit 0.4.5, testthat.

## Global Constraints

- `arframe()` is the only arframe export; every new function is `@noRd` internal. arpillar's two new functions ARE exported (full roxygen per `~/.claude/rules/roxygen.md`).
- ID-less `onchange` posts for value-carrying per-object controls (CLAUDE.md 2026-07-10 data-loss rule); shared observers, never per-row observers in `renderUI`.
- Em-dash `—` in user-facing strings, never `--`; ASCII-only inside `cli_abort()` messages.
- Store is the sole inter-module channel; no arpillar/DBI calls inside `render*`/`observe` bodies beyond the existing patterns.
- Dev loop after every task: `Rscript -e 'devtools::document()'`, `devtools::test()`, `air format R/ tests/`; full `devtools::check()` at Tasks 5 and 12. 0E/0W/0N.
- Golden gates: RTF byte goldens re-pointed at slug paths, never regenerated.
- No `Co-Authored-By` trailers. Commit per task.

## Fallout audit (every `.output_slug` caller — Task 3 must touch all)

`R/fct_async.R:41`, `R/fct_export.R:30,105`, `R/mod_toolbar.R:78`, `R/mod_paper.R:94,405,795`, `R/fct_project.R:.emit_programs` (Task 4), plus deletion of `R/utils_atoms.R:411-417`.

---

### Task 1: arpillar — exported `output_slug()` / `output_slugs()`

**Repo:** `/Users/vignesh/projects/r/arpillar`

**Files:**
- Create: `R/slug.R`
- Test: `tests/testthat/test-slug.R`

**Interfaces:**
- Produces: `output_slug(object)` → `character(1)`; `output_slugs(report)` → named `character` (`id -> slug`, collision-suffixed).

- [ ] **Step 1: Failing tests**

```r
# tests/testthat/test-slug.R
test_that("output_slug composes kind-number-title", {
  obj <- object(
    id = "out005", type = "listing", dataset = "ADAE",
    title = "Adverse Event Listing",
    options = list(number = "16.2.7", number_label = "Listing")
  )
  expect_identical(output_slug(obj), "l-16-2-7-adverse-event-listing")
})

test_that("output_slug falls back to id when number+title are empty", {
  obj <- object(id = "out009", type = "summary", dataset = "ADSL", title = "")
  expect_identical(output_slug(obj), "out009")
})

test_that("output_slugs suffixes collisions with the id", {
  a <- object(id = "out001", type = "summary", dataset = "ADSL",
              title = "Demographics", options = list(number = "14.1.1"))
  b <- object(id = "out002", type = "summary", dataset = "ADSL",
              title = "Demographics", options = list(number = "14.1.1"))
  rep <- report(pages = list(page(objects = list(a, b))))
  slugs <- output_slugs(rep)
  expect_identical(unname(slugs["out001"]), "t-14-1-1-demographics")
  expect_identical(unname(slugs["out002"]), "t-14-1-1-demographics-out002")
})

test_that("output_slug rejects a non-object", {
  expect_error(output_slug(1), class = "arpillar_error_input")
})
```

(Adapt constructor calls to arpillar's real `object()`/`report()`/`page()` signatures — read `R/aaa_class.R` first; do not guess.)

- [ ] **Step 2: Run** `Rscript -e 'testthat::test_file("tests/testthat/test-slug.R")'` — FAIL, `output_slug` not found.

- [ ] **Step 3: Implement `R/slug.R`** (roxygen per the standard — two narrated `@examples`, `@seealso` to `report_to_folder()`/`emit_code()`; body:)

```r
output_slug <- function(object) {
  if (!S7::S7_inherits(object, arpillar::object)) {
    cli::cli_abort(
      c(
        "{.arg object} must be an {.cls object}.",
        "x" = "You supplied {.obj_type_friendly {object}}."
      ),
      class = "arpillar_error_input",
      call = rlang::caller_env()
    )
  }
  label <- object@options$number_label %||% "Table"
  kind <- tolower(substr(label, 1, 1))
  raw <- paste(kind, object@options[["number"]] %||% "", object@title)
  slug <- tolower(gsub("[^a-zA-Z0-9]+", "-", trimws(raw)))
  slug <- gsub("^-+|-+$", "", slug)
  # A bare kind letter means number AND title were empty: not identifying.
  if (!nzchar(slug) || identical(slug, kind)) object@id else slug
}

output_slugs <- function(report) {
  objs <- unlist(lapply(report@pages, function(p) p@objects), recursive = FALSE)
  ids <- vapply(objs, function(o) o@id, character(1))
  slugs <- vapply(objs, output_slug, character(1))
  dup <- duplicated(slugs) | duplicated(slugs, fromLast = TRUE)
  slugs[dup] <- paste0(slugs[dup], "-", ids[dup])
  stats::setNames(slugs, ids)
}
```

(`output_slugs` gets the same S7 input check on `report`.)

- [ ] **Step 4: Run tests** — PASS. `devtools::document()`; NAMESPACE gains both exports.
- [ ] **Step 5: Commit** `feat(slug): exported output_slug()/output_slugs() for auditable filenames`

---

### Task 2: arpillar — `report_to_folder()` writes slug-named specs + stale cleanup

**Repo:** `/Users/vignesh/projects/r/arpillar`

**Files:**
- Modify: `R/serialize.R:578-615` (`report_to_folder`)
- Test: `tests/testthat/test-serialize.R` (append)

**Interfaces:**
- Consumes: `output_slugs(report)` (Task 1).
- Produces: `outputs/<slug>.json` on disk; any other `*.json` in `outputs/` deleted. `report_from_folder()` untouched (already globs).

- [ ] **Step 1: Failing tests**

```r
test_that("report_to_folder names specs by slug and round-trips", {
  dir <- withr::local_tempdir()
  report_to_folder(demo_report, dir)   # use the suite's existing report fixture
  files <- list.files(file.path(dir, "outputs"))
  slugs <- output_slugs(demo_report)
  expect_setequal(files, paste0(unname(slugs), ".json"))
  back <- report_from_folder(dir)
  expect_setequal(
    vapply(unlist(lapply(back@pages, function(p) p@objects), FALSE),
           function(o) o@id, character(1)),
    names(slugs)
  )
})

test_that("report_to_folder removes stale spec files after a rename", {
  dir <- withr::local_tempdir()
  report_to_folder(demo_report, dir)
  # retitle the first output -> new slug; the old file must vanish
  renamed <- retitle_first(demo_report, "Completely New Title")  # helper via S7::set_props
  report_to_folder(renamed, dir)
  files <- list.files(file.path(dir, "outputs"))
  expect_setequal(files, paste0(unname(output_slugs(renamed)), ".json"))
})
```

- [ ] **Step 2: Run** — FAIL (files are `<id>.json`; stale file survives).
- [ ] **Step 3: Implement** — in `report_to_folder()` replace the per-object write loop:

```r
slugs <- output_slugs(report)
written <- character(0)
for (ob in objs) {
  fname <- paste0(slugs[[ob@id]], ".json")
  object_to_json(ob, path = file.path(outputs_dir, fname))
  written <- c(written, fname)
}
stale <- setdiff(list.files(outputs_dir, pattern = "\\.json$"), written)
unlink(file.path(outputs_dir, stale))
```

- [ ] **Step 4: Full arpillar dev loop** — `document()`, `test()`, `check(args = "--no-manual")`, `air format R/ tests/`. 0/0/0.
- [ ] **Step 5: Commit** `feat(serialize): slug-named outputs/*.json with stale-file cleanup`

---

### Task 3: arframe — consume arpillar slugs everywhere

**Files:**
- Modify: `R/utils_atoms.R:405-417` (delete `.output_slug`), `R/fct_async.R:41`, `R/fct_export.R:30,105`, `R/mod_toolbar.R:78`, `R/mod_paper.R:94,405,795`
- Test: `tests/testthat/test-utils_atoms.R` (drop slug tests), `tests/testthat/test-fct_export.R`

**Interfaces:**
- Consumes: `arpillar::output_slug()`, `arpillar::output_slugs()`.
- Produces: no arframe-local slug logic remains (grep-clean).

- [ ] **Step 1:** Reinstall arpillar so the new exports are visible: `Rscript -e 'devtools::install("/Users/vignesh/projects/r/arpillar", quick = TRUE)'`.
- [ ] **Step 2:** Delete `.output_slug()` from `utils_atoms.R`; mechanical replace each caller with `arpillar::output_slug(...)`. In `R/fct_async.R:.export_names` and `R/fct_export.R:.zip_export`, switch to one `arpillar::output_slugs(report)` lookup instead of per-object calls (collision-safe where a whole report is in scope).
- [ ] **Step 3:** Move the old slug unit tests to assert via `arpillar::output_slug`; run `devtools::test()` — PASS; `grep -rn "\.output_slug" R/` returns nothing.
- [ ] **Step 4: Commit** `refactor(slug): consume arpillar::output_slug(s), drop the local copy`

---

### Task 4: arframe — `.emit_programs()` slug filenames + stale cleanup

**Files:**
- Modify: `R/fct_project.R:129-162`
- Test: `tests/testthat/test-fct_project.R` (create if absent)

**Interfaces:**
- Produces: `programs/<slug>.R` per output + `programs/run-all.R`; stale `.R` (except `run-all.R`) deleted. Matches the `{program}` chrome token `mod_paper.R:94` already stamps.

- [ ] **Step 1: Failing test**

```r
test_that(".emit_programs writes slug-named programs and prunes stale ones", {
  store <- local_demo_store()          # suite's existing store fixture helper
  dir <- store$rv$path
  arframe:::.emit_programs(store)
  slugs <- arpillar::output_slugs(store$rv$report)
  progs <- list.files(file.path(dir, "programs"))
  expect_setequal(progs, c(paste0(unname(slugs), ".R"), "run-all.R"))
  # rename -> old program pruned
  first <- names(slugs)[[1]]
  update_object(store, first, function(o) S7::set_props(o, title = "Renamed"),
                label = "retitle")
  arframe:::.emit_programs(store)
  progs2 <- list.files(file.path(dir, "programs"))
  expect_setequal(
    progs2,
    c(paste0(unname(arpillar::output_slugs(store$rv$report)), ".R"), "run-all.R")
  )
})
```

- [ ] **Step 2: Run** — FAIL. **Step 3:** In `.emit_programs()`:

```r
slugs <- arpillar::output_slugs(store$rv$report)
written <- character(0)
for (obj in objs) {
  fname <- paste0(slugs[[obj@id]], ".R")
  out <- file.path(prog_dir, fname)
  tryCatch(
    { arpillar::emit_code(store$con, obj, path = out, theme = theme)
      written <- c(written, fname) },
    error = function(e) NULL
  )
}
stale <- setdiff(list.files(prog_dir, pattern = "\\.R$"), c(written, "run-all.R"))
unlink(file.path(prog_dir, stale))
```

- [ ] **Step 4:** Tests PASS. **Step 5: Commit** `fix(programs): programs/<slug>.R matches the paper's {program} claim; prune stale`

---

### Task 5: arframe — RTF slug cleanup + golden re-point + mid-plan check

**Files:**
- Modify: the async-export completion handler (find with `grep -n "export_mirai" R/*.R`; prune in the main process, never in the daemon)
- Test: `tests/testthat/test-fct_async.R`, `tests/testthat/test-fct_export.R`

- [ ] **Step 1: Failing test** — after an export completes into `out_dir`, a pre-seeded `stale-name.rtf` in `out_dir` is gone and `list.files(out_dir, pattern = "\\.rtf$")` equals `paste0(unname(slugs_of_ready), ".rtf")`.
- [ ] **Step 2:** Implement: in the completion handler, `stale <- setdiff(list.files(out_dir, pattern = "\\.rtf$"), unlist(names_map)); unlink(file.path(out_dir, stale))`.
- [ ] **Step 3:** Re-point RTF byte goldens: goldens compare CONTENT against a rendered file — update any fixture PATHS that assumed `<id>.rtf`; byte expectations untouched. `devtools::test()` — the golden gate passes unmodified.
- [ ] **Step 4:** Full `devtools::check(args = "--no-manual")` — 0/0/0. **Commit** `fix(export): slug-named RTFs pruned of stale renders; goldens re-pointed`

---

### Task 6: downlit code view

**Files:**
- Modify: `R/mod_paper.R:397-437` (`.code_panel`), delete `.hl_r()` (`mod_paper.R:439-~500`); `inst/www/arframe.css:1634-1652`; `DESCRIPTION` (Imports: downlit)
- Test: `tests/testthat/test-mod_paper.R`

**Interfaces:**
- Produces: `.code_html(script)` internal returning downlit HTML with plain-escape fallback.

- [ ] **Step 1: Failing tests**

```r
test_that(".code_html highlights via downlit with textContent parity", {
  script <- "x <- adsl |> head(2) # note\ny <- \"str\""
  html <- arframe:::.code_html(script)
  expect_match(html, "class='fu'|class=\"fu\"")
  txt <- rvest::html_text2(rvest::read_html(paste0("<pre>", html, "</pre>")))
  expect_identical(txt, script)
})

test_that(".code_html falls back to escaped text when highlighting fails", {
  bad <- "x <- ("           # unparseable -> downlit returns NA
  expect_identical(arframe:::.code_html(bad), as.character(htmltools::htmlEscape(bad)))
})
```

(If rvest is not in Suggests, strip tags with `gsub("<[^>]+>", "", ...)` + entity-decode instead — no new dependency.)

- [ ] **Step 2:** FAIL. **Step 3:** Implement in `mod_paper.R`:

```r
.code_html <- function(script) {
  out <- tryCatch(
    downlit::highlight(script, classes = downlit::classes_pandoc()),
    error = function(e) NA_character_
  )
  if (is.na(out)) as.character(htmltools::htmlEscape(script)) else out
}
```

`.code_panel()` swaps `shiny::HTML(.hl_r(script))` for `shiny::HTML(.code_html(script))`; delete `.hl_r()`. `usethis::use_package("downlit")`. CSS: replace the `.ar-hl-*` block with pandoc-class rules scoped to `.ar-code-body` — `.fu`/`.kw` purple `#7c3aed`-family token, `.op` red, `.st` green, `.fl`/`.dv` blue, `.co` gray italic, `.va` ink; `a` inherits color, underline on hover, `target` handled by downlit's own markup.
- [ ] **Step 4:** PASS + eyeball the code view against the datasetviewer Code modal. **Commit** `feat(code-view): downlit highlighting (pkgdown mechanism), .hl_r removed`

---

### Task 7: `.accordion_section()` atom

**Files:**
- Modify: `R/utils_atoms.R` (new atom), `inst/www/arframe.css`
- Test: `tests/testthat/test-utils_atoms.R`

**Interfaces:**
- Produces: `.accordion_section(label, body, icon = NULL, open = TRUE, help = NULL, count = NULL)` → `<details class="ar-acc" open><summary>…</summary><div class="ar-acc-body">…</div></details>`; `help` is a pre-built tag (the Task 10 `.help_icon()`), slotted into the summary row.

- [ ] **Step 1: Failing tests** — renders `<details ... open>`; `open = FALSE` omits the attribute; summary carries label + chevron icon + optional help tag; `NULL` body returns `NULL` (matches `.opt_section` elision).
- [ ] **Step 2:** FAIL. **Step 3:**

```r
.accordion_section <- function(label, body, icon = NULL, open = TRUE,
                               help = NULL, count = NULL) {
  body <- Filter(Negate(is.null), if (is.list(body)) body else list(body))
  if (length(body) == 0L) return(NULL)
  args <- list(
    class = "ar-acc",
    shiny::tags$summary(
      class = "ar-acc-head",
      if (!is.null(icon)) shiny::tags$span(class = "ar-acc-chip", .icon(icon, 13)),
      shiny::tags$span(class = "ar-label ar-acc-label", label),
      if (!is.null(count)) shiny::tags$span(class = "ar-acc-count ar-mono", count),
      shiny::tags$span(class = "ar-bar-spacer"),
      help,
      .icon("chevron", 12)
    ),
    shiny::tags$div(class = "ar-acc-body", body)
  )
  if (isTRUE(open)) args$open <- NA
  do.call(shiny::tags$details, args)
}
```

CSS: `.ar-acc` hairline-separated inside the pane; `.ar-acc-head` = pointer cursor, hover wash, chevron rotates 90° on `[open]`, soft-tinted `.ar-acc-chip` (token-fed fill), generous padding per the reference language; `summary::-webkit-details-marker { display: none }`.
- [ ] **Step 4:** PASS. **Commit** `feat(atoms): native-details accordion section`

---

### Task 8: inspector frame — top pill strip, rail removed, toolbar toggle

**Files:**
- Modify: `R/mod_card.R` (UI 106-152, server 181-232), `R/mod_toolbar.R` (panel-toggle button), `inst/www/arframe.css`, `R/fct_store.R:636` area (`toggle_insp()` comment)
- Test: `tests/testthat/test-mod_card.R`, `tests/testthat/test-mod_toolbar.R`

**Interfaces:**
- Consumes: existing `ar-insp-tab` custom message, `store$rv$insp_tab/insp_collapsed`.
- Produces: `.INSP_TABS <- c(roles = "Roles", options = "Options", filters = "Filters")`; strip markup `div.ar-insp-strip > button.ar-insp-tab[data-ar-insp-tab]`; toolbar button id `panel_toggle`.

- [ ] **Step 1: Failing tests** — `mod_card_ui("c")` HTML has no `ar-insp-tabs` rail, has `ar-insp-strip` as the FIRST child of `.ar-insp-main` with exactly three buttons; no `ranks` pane div; toolbar UI contains `panel_toggle`.
- [ ] **Step 2:** FAIL. **Step 3:** Drop `ranks` from `.INSP_TABS`; delete the rail div + `.insp_tab_btn` icon (strip buttons are text pills:)

```r
.insp_tab_btn <- function(ns, tab) {
  shiny::tags$button(
    id = ns(paste0("tab_", tab)), type = "button",
    class = "ar-insp-tab action-button", `data-ar-insp-tab` = tab,
    .INSP_TABS[[tab]]
  )
}
```

UI: `.ar-insp-strip` (segmented pill group) above `.ar-insp-body`; remove `mod_card_ranks_ui` pane div and `mod_card_ranks_server` mount. Server: tab-click observer keeps switch semantics but drops the click-active-to-collapse branch (the strip is inside the folding pane now); `mod_toolbar.R` gains a quiet icon button toggling `store$rv$insp_collapsed` + the existing `ar-collapse` message. CSS: pill strip per the design language (rounded-full group, filled active pill), delete rail styles.
- [ ] **Step 4:** PASS. **Step 5:** `Rscript -e 'shiny::runApp(arframe::arframe(...))'` smoke + screenshot the drilled inspector. **Commit** `feat(inspector): three-pane top pill strip replaces the icon rail`

---

### Task 9: Ranks → Options ORDER section; delete `mod_card_ranks.R`

**Files:**
- Modify: `R/mod_card_options.R` (new `.opt_order_section()` + three relocated observers into `mod_card_options_server`), delete `R/mod_card_ranks.R`
- Test: relocate `tests/testthat/test-mod_card_ranks.R` content into `tests/testthat/test-mod_card_options.R`, then delete the file

**Interfaces:**
- Consumes: `.reorder_slot()`, `.opt_levels_control()`, `.role_for_slot()`, `update_object()` (all existing).
- Produces: `.opt_order_section(con, ns, object)` returning the accordion-wrapped ORDER section or `NULL` (km).

- [ ] **Step 1:** Move the three ranks regression tests (row-block reorder commit, `hier_sort` default-elision commit, `x_order` commit) into `test-mod_card_options.R`, retargeting namespaced inputs from `ranks-*` to `options-*`. Run — FAIL.
- [ ] **Step 2:** Implement `.opt_order_section()` in `mod_card_options.R` by relocating `.ranks_items_section` / `.ranks_hier_section` / `.ranks_xorder_section` bodies (renamed `.order_*`), with the occurrence control restyled as a two-way pill (`freq`/`alpha`, `.opt_choice_named` idiom, ID-less onchange posting `{field:"hier_sort", value, nonce}` to the existing shared `opt` observer). Append `.opt_order_section(store$con, ns, obj)` to the pane `tagList` after `.opt_schema_sections(...)`. Move the three `observeEvent`s (`rank_items`, `hier_sort` via the shared post, `opt_reorder_x_order` already handled by the generic levels path — verify, don't duplicate). Delete `R/mod_card_ranks.R`; purge `mod_card_ranks_ui/server` references (Task 8 already unmounted).
- [ ] **Step 3:** Tests PASS; `grep -rn "ranks" R/` only hits comments/history. **Commit** `feat(options): ORDER section absorbs Ranks; mod_card_ranks deleted`

---

### Task 10: help system — registry, `?` icons, modals

**Files:**
- Create: `R/utils_help.R`
- Modify: `R/mod_card.R` (one shared observer), `R/mod_setup.R:1220` area (card headers get the icon), `R/mod_card_roles.R` / `mod_card_options.R` / `mod_card_filters.R` / `mod_card_listing.R` (pass `help =` into sections; delete every `ar-opt-hint` paragraph — 16 across the five files), `inst/www/arframe.css` (modal + chip styles)
- Test: `tests/testthat/test-utils_help.R`

**Interfaces:**
- Produces: `.HELP_TOPICS` (named list, `topic -> function() shiny::tagList(...)`); `.help_icon(ns, topic)` (button class `ar-help-btn`, onclick posts `{topic, nonce}` to `ns("help_open")`); `.show_help(topic)` (calls `shiny::showModal`); helpers `.help_p(...)`, `.help_code(x)` (inline chip), `.help_block(x)` (bordered block), `.help_h(x)`.

- [ ] **Step 1: Failing tests**

```r
test_that("every inspector section and Setup card has a help topic", {
  expect_setequal(names(arframe:::.HELP_TOPICS), arframe:::.HELP_REQUIRED)
})
test_that("help entries are substantive tutorials, not tooltips", {
  for (topic in names(arframe:::.HELP_TOPICS)) {
    html <- as.character(arframe:::.HELP_TOPICS[[topic]]())
    expect_gt(nchar(gsub("<[^>]+>", "", html)), 400)   # real prose
    expect_match(html, "ar-help-code", info = topic)   # at least one example
  }
})
test_that(".help_icon posts topic without toggling the accordion", {
  tag <- arframe:::.help_icon(shiny::NS("x"), "filters")
  expect_match(as.character(tag), "event.preventDefault\\(\\); event.stopPropagation\\(\\)")
})
```

`.HELP_REQUIRED` is the explicit vector of section/card ids: Setup — `study`, `paths`, `populations`, `analysis_sets`, `treatment`, `page_style`, `summaries`, `footnotes`, `team`, `sources`, `preferences`; inspector — `roles`, `order`, `title`, `footnotes_out`, `options_<generator-shared>` (one per Options schema section actually rendered: enumerate from `.opt_schema_sections` labels), `listing_sort`, `listing_stack`, `filters`, `population`.
- [ ] **Step 2:** FAIL. **Step 3:** Implement the mechanism:

```r
.help_icon <- function(ns, topic) {
  shiny::tags$button(
    type = "button", class = "ar-help-btn",
    `aria-label` = paste("Help:", topic),
    onclick = sprintf(
      "event.preventDefault(); event.stopPropagation(); Shiny.setInputValue('%s', {topic: '%s', nonce: Date.now()}, {priority: 'event'})",
      ns("help_open"), topic
    ),
    "?"
  )
}
.show_help <- function(topic) {
  entry <- .HELP_TOPICS[[topic]]
  if (is.null(entry)) return(invisible(NULL))
  shiny::showModal(shiny::modalDialog(
    entry(), easyClose = TRUE, footer = NULL, class = "ar-help-modal"
  ))
}
```

One `observeEvent(input$help_open, .show_help(input$help_open$topic))` in `mod_card_server` and one in `mod_setup_server` (namespaces differ; both post to their own `help_open`).
**Content bar (spec §4, user call):** every entry is an in-depth tutorial ANY user level can follow — bold heading, plain-language prose on what the section does and why it exists in a submission workflow, inline `.help_code` chips, 2–4 worked ADaM examples, and (for Options topics) a "what changes on the rendered table" line per choice. Exemplar to match (filters):

```r
filters = function() shiny::tagList(
  .help_h("Filters — subset the rows this output analyses"),
  .help_p("A filter keeps only the records that match a condition before any
    statistics are computed. The population (above) applies the study's
    analysis set; these ad-hoc filters stack on top of it."),
  .help_p("Pick a variable, an operator, and a value. Conditions combine with
    AND — every listed condition must hold for a row to survive."),
  .help_block('AGE >= 18'),
  .help_p("keeps adult subjects only, computed from the dataset's AGE column."),
  .help_block('SAFFL = "Y"'),
  .help_p("keeps rows flagged into the safety population. Flag variables end
    in FL and hold \"Y\"/\"N\"."),
  .help_block('AEBODSYS is not na'),
  .help_p("drops rows with a missing body system — use this to exclude
    unmapped events from an occurrence table."),
  .help_p("The telemetry line under the inspector shows how many records
    survive the active filters, so you can sanity-check a condition the
    moment you add it.")
)
```

Write every `.HELP_REQUIRED` topic to this bar (treatment covers `TRT01A` vs `TRT01P` + estimand basis; order covers freq-vs-alpha with a pooled-incidence example; each Options schema topic explains its choices' rendered effect). Then wire `help = .help_icon(ns, "<topic>")` into every `.accordion_section()` call (Tasks 9/11) and `.card(title = tagList(title, .help_icon(...)))` for Setup cards; delete all `ar-opt-hint` paragraphs (their information must be absorbed into the topic, not dropped — diff each hint against the new entry before deleting).
- [ ] **Step 4:** PASS. **Commit** `feat(help): in-depth per-section help modals replace inline hints`

---

### Task 11: wrap all pane sections in accordions

**Files:**
- Modify: `R/mod_card_options.R:558-568` (`.opt_section` delegates to the atom), `R/mod_card_roles.R:739-780` (`.slot_fieldset` summary row), `R/mod_card_filters.R:464-525` (POPULATION + FILTERS sections), `R/mod_card_listing.R` (sort/stack editors)
- Test: existing pane tests updated for the `<details>` wrapper

**Interfaces:**
- Consumes: `.accordion_section()` (Task 7), `.help_icon()` (Task 10).

- [ ] **Step 1:** Update pane snapshot/structure tests: each section root is `details.ar-acc[open]`. FAIL.
- [ ] **Step 2:** `.opt_section(label, rows, help = NULL, icon = NULL)` becomes a thin wrapper over `.accordion_section()` (all existing call sites keep working; add `help`/`icon` at each call site). Roles: `.slot_fieldset()` keeps the `<fieldset>` for a11y INSIDE the accordion body; its legend text moves to the summary. Filters: POPULATION and FILTERS become two accordion sections. Listing sort/stack editors wrap likewise. Drag-and-drop check: sortable containers live in the accordion BODY — verify `data-ar-sortable` still initialises after a details toggle (bridge.js binds by delegation; confirm, else re-scan on `toggle` event).
- [ ] **Step 3:** PASS + interactive smoke: fold/unfold every section, drag a role row, run an output. **Commit** `feat(inspector): accordion sections across Roles/Options/Filters`

---

### Task 12: design-grade polish + verification sweep

**Files:**
- Modify: `inst/www/tokens.css`, `inst/www/arframe.css`
- Test: `tests/testthat/test-tokens.R` (token presence), full suite

- [ ] **Step 1:** Load the `frontend-design` skill. Token pass per the spec's design language: two-layer card shadow pair, 14-16px radius scale, chip fill scale, pill-control tokens; apply to inspector strip, accordions, help modal, code pane. No inline styles.
- [ ] **Step 2:** Full gates in BOTH repos: `document()`, `test()`, `check(args = "--no-manual")` 0/0/0, `air format R/ tests/`, `covr` per-file ≥ 95% on touched files.
- [ ] **Step 3:** Screenshot eyeball (standing rule; real CDISC pilot mounts): Setup (help modal open), Report LoC, drilled inspector on each pane incl. ORDER for an occurrence + a line output, code view, a fresh save's `outputs/` + `programs/` + `output/` listing (slug triplet, no stale files). Compare against the nine reference shots + datasetviewer modals.
- [ ] **Step 4:** Update `CLAUDE.md` (inspector = 3 tabs + accordions + help registry; slug filenames; downlit) and `NEWS`-equivalent handoff notes; `tasks/todo.md` review section.
- [ ] **Step 5: Commit** `feat(ui): reference-grade inspector polish; docs updated`

---

## Self-review notes

- Spec coverage: §1→Task 8, §2→Task 9, §3→Tasks 7+11, §4→Task 10, §5→Tasks 1-5, §6→Task 6, design language→Task 12 (+ per-task CSS). Testing section items each land in a task's Step 1.
- Ordering: arpillar first (1-2), consumption (3-5), independent UI (6), atoms→frame→content (7-11), polish (12).
- Known risk: `test-mod_card.R` asserts rail markup (will fail loudly in Task 8 Step 1 — intended); `folder_to_report` fixtures in arframe that hand-seed `outputs/out00N.json` keep loading (read side is glob-based).
