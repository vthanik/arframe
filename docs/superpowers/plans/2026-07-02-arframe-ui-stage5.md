# arframe Stage 5 — Galley UI Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the arframe submission-native report builder in the **Galley** design (paper-first proof room: TOC + stamps + typeset page + summonable/pinnable galley card) on the completed arpillar engine — exceeding teal / blockr / rapido / Posit apps on design and quality.

**Binding design doc:** `docs/superpowers/specs/2026-07-02-arframe-galley-design-system.md` — every token, geometry number, region name, stamp, and interaction in this plan comes from it; when in doubt, that spec wins. The product paradigm + module/store architecture (§5.1–§5.5) of `2026-07-01-arframe-ui-redesign-design.md` still binds; its visual/layout sections are superseded.

**Architecture:** Three arpillar prerequisite exports (declarative `templates()`/`option_schema()`, the single `output_status()`/`validate_output()` oracle, the `render_spec()` screen==paper seam), then arframe as a strict signature.py-style module tree: one injected structured store (S7 `report` + selection pointers, ALL draft state out of the DOM), `mod_<region>.R` modules that only wire UI↔store, every heavy computation a pure arpillar call. Paper preview = `htmltools::as.tags(render_spec(...))` for tables (verified shiny.tag.list from the SAME pipeline that exports) and `renderPlot(render_ggplot(...))` for figures (same ggplot object both places).

**Tech Stack:** shiny 1.13.0 · bslib 0.10.0 · htmltools 0.5.9 · fontawesome 0.5.3 · arpillar (all 31 exports) · tabular 0.2.0.9000 (`as.tags` embed) · datasetviewer 0.1.0 (Data-mode grid) · vendored SortableJS 1.15.6 (MIT) · self-hosted IBM Plex Sans/Mono woff2 (OFL) · zip 2.3.3 (bundle export) · mirai (Task 16, install first — NOT currently installed) · shinytest2.

## Global Constraints

- **Two repos.** Tasks 1–3 run in `/Users/vignesh/projects/r/arpillar`; Tasks 4–17 in `/Users/vignesh/projects/r/arframe`. Never mix a commit across repos.
- **Dev loop per task (in order):** `Rscript -e 'devtools::document()'` → `Rscript -e 'devtools::test()'` → `air format R tests` → commit. Full `devtools::check(args = "--no-manual")` (0 errors / 0 warnings) at Tasks 3, 9, 14, 17.
- **Goldens are inviolable:** arpillar's demographics RTF byte-golden and 3 vdiffr figure goldens must pass untouched after every arpillar task. No `*.new` snapshot files.
- **Reinstall before any shinytest2 / screenshot step:** `R CMD INSTALL --no-multiarch --no-docs --no-byte-compile .` — arpillar first if it changed, then arframe. S7 print dispatch only registers in an INSTALLED package.
- **Every UI task ends with a screenshot** written to `.local/screens/` (gitignored) for the user's eyeball — never claim visual correctness from code reading. Measure DOM (`getBoundingClientRect`) for any alignment claim.
- **Errors:** classed cli conditions only — `arpillar_error_input` (engine), `arframe_error_input` (app). ASCII inside cli message strings. Always `call = rlang::caller_env()`.
- **Naming:** exports are bare verbs/nouns, snake_case, never shadow base R. arframe exports ONLY `arframe()`; every module/helper is internal. CSS prefix `ar-`, JS/data attrs `data-ar-*`, tokens `--ar-*`.
- **No `Co-Authored-By: Claude` / AI attribution anywhere.** Never push without explicit per-session approval.
- **Store is the only inter-module channel.** Modules never read each other's inputs; all draft/edit state lives in the store, never in the DOM. Each control writes through its own `observeEvent` with an explicit event.
- **Logic lives in arpillar.** No `DBI`/`cards`/`ggplot2`/`tabular` call inside an arframe `render*`/`observe`/`reactive` — modules call arpillar exports.
- **roxygen2 house standard** (`~/.claude/rules/roxygen.md`) for every new arpillar export; arframe internals get `@noRd` one-liners.
- **Fonts:** self-host IBM Plex Sans (400/500/600) + IBM Plex Mono (400/500) latin woff2 subsets in `inst/www/fonts/` — OFL license, recorded in `inst/COPYRIGHTS`. `@font-face` lives in `tokens.css` (LINKED stylesheet so relative font URLs resolve). System fallbacks always declared. Never bundle a commercial font (the explorer Avenir mistake).
- **The paper is mono.** Everything rendered on the page (title block, table, footnotes, source line) uses the mono face — the RTF deliverable is Courier; the preview is honest to it.
- **Stamps are the status vocabulary:** `READY / DRAFT / NO DATA / ERROR` — mono caps, colored border + text, transparent fill, full-sentence `aria-label`. Colour never alone.
- **Engine render seam sees only `WORK`** (figure legs hardcode it) — every render-bound dataset registers into the default `WORK` library.
- **`options$decimals` is a length-1 integer** (a named vector is silently ignored by the engine) — the Options pane must emit a scalar.

## Grounded interface facts (verified 2026-07-02 — do not re-derive)

- Role slots the render legs extract (all errors class `arpillar_error_input`):
  - `summary` / `crosstab`: slot `"treatment"` OR `"group"` (exactly 1 item) + slot `"summarize"` OR `"row"` (>= 1 item); crosstab additionally needs >= 1 item with `role_type == "category"`. `object@dataset` must be non-empty.
  - `line` / `box`: slots `"x"`, `"y"` (exactly 1 each) + `"group"` OR `"treatment"` (exactly 1).
  - `km`: slots `"time"`, `"censor"` (exactly 1 each) + `"group"` OR `"treatment"` OR `"strata"` (exactly 1).
- Figure option keys consumed by the engine (defaults in parentheses): `error_type` ("ci"), `ci_level` (0.95), `legend_position` ("bottom"), `palette` ("Set2"), `mean_diamond` (FALSE, box), `event_value` (0, km), `ci` (FALSE, km), `risk_table` (TRUE, km), `censor_marks` (TRUE, km), `x_order` (chr vec, line/box), `x_label`/`y_label` (chr(1)), `time_breaks` (num vec, km). Table key: `decimals` (1L).
- Filter predicate: `list(column = chr(1), op = chr(1), value = vector, include_missing = lgl(1) optional)`; ops exactly `"==", "!=", "%in%", ">", "<", ">=", "<=", "is.na", "not.na"`; drop-tolerant (half-built predicates silently compile to nothing — the UI must do its own completeness display, the engine will not error).
- `data_items(con, name)` → `name` chr, `type` chr ("measure"|"category"|"date"), `sql_type` chr, `label` chr (ALWAYS `NA_character_` today — do not build UI that requires labels).
- `catalog_grid(con)` → `name, library, rows <dbl>, cols <int>, size <dbl bytes>, modified <chr>, loaded <lgl>` in registration order.
- `value_counts()` → NAMED integer vector (desc count); `filter_count()` → `list(matched, total)`.
- `htmltools::as.tags(<tabular_spec>)` → `shiny.tag.list` (scoped style + `div.tabular-doc`), round-trips through a tempfile per call — cache the ARD, rebuild spec only on option edits.
- `km` with `risk_table = TRUE` (default) returns a **patchwork**, not a ggplot — `renderPlot` prints both fine; class checks must accept both.
- ExtendedTask: `ExtendedTask$new(function(...) mirai::mirai({...}))`; func must NOT read reactives; `bslib::bind_task_button` relays busy state but does NOT auto-invoke. mirai NOT installed — Task 17 installs it.
- DuckDB connections cannot cross a mirai daemon — pass `dataset_path()` strings; the daemon opens its own `engine_open()`.
- No `unregister` export exists in arpillar; duplicate registration errors. The store must track what is registered.
- SortableJS bridge (adapted from explorer, prefix renamed): container attrs `data-ar-sortable`, `-handle`, `-item`, `-attr`, `-input`, `-extra`; JS posts `Shiny.setInputValue(id, {order: [...], nonce: Date.now(), ...extra}, {priority: "event"})`; re-init on `shiny:value shiny:idle` (jQuery events) + `el._arSortable` double-init guard; NEVER re-render a sortable list on its own reorder event.

## File structure (final state)

```
arpillar/R/fct_templates.R        templates(), template(), option_schema()   (Task 1)
arpillar/R/fct_status.R           output_status(), validate_output()         (Task 2)
arpillar/R/fct_render_table.R     + render_spec() split out of render_rtf()  (Task 3)

arframe/R/app.R                   arframe(project, data) launcher            (Task 6)
arframe/R/theme.R                 ar_theme(), head assets                    (Task 4)
arframe/R/utils_atoms.R           .icon(), .label(), .type_chip(), .stamp(), .action_btn() (Task 4)
arframe/R/fct_store.R             new_store() + mutators + undo + ard cache  (Task 5)
arframe/R/utils_report.R          pure S7-tree helpers (.replace_object etc) (Task 5)
arframe/R/utils_demo.R            .demo_catalog() fixture builder (internal) (Task 5)
arframe/R/mod_frame.R             app bar + desk frame + status bar          (Task 6)
arframe/R/mod_contents.R          the TOC: numbers, leaders, stamps, reorder (Task 7)
arframe/R/mod_add_output.R        Add-output overlay + Recommended (Suggest) (Task 8)
arframe/R/mod_paper.R             the page: render, ghost shell, regions     (Task 9)
arframe/R/utils_ghost.R           ghost shell (pure fn of template + object) (Task 9)
arframe/R/mod_card.R              galley card frame: summon, pin, route      (Task 10)
arframe/R/mod_card_roles.R        columns/rows/axes region content           (Task 10)
arframe/R/mod_card_options.R      title/footnotes/stat/figure option rows    (Task 11)
arframe/R/mod_card_filters.R      presets + builder + live count             (Task 12)
arframe/R/mod_data_mode.R         data furniture: catalog, grid, profile     (Task 13)
arframe/R/mod_project.R           save/open/autosave + Export package        (Task 14)
arframe/R/mod_qc.R                the proof-check sheet (validation + log)   (Task 15)
arframe/R/fct_async.R             ExtendedTask export runner (mirai)         (Task 16)
arframe/inst/www/tokens.css       the --ar-* token contract + @font-face     (Task 4)
arframe/inst/www/arframe.css      chrome + module layers (sectioned)         (Task 4+)
arframe/inst/www/arframe.js       region clicks, card pin, sortable bridge   (Task 6+)
arframe/inst/www/fonts/           IBM Plex Sans/Mono woff2 (OFL)             (Task 4)
arframe/inst/www/Sortable.min.js  vendored SortableJS 1.15.6 MIT             (Task 7)
arframe/inst/COPYRIGHTS           vendored JS + fonts record                 (Task 4)
```

One test file per source file, name mirrored (`tests/testthat/test-<name>.R`). Variable pickers live INSIDE the galley card regions (no separate data rail); Suggest lives inside Add-output; Review is the QC sheet; Ranks is a disabled "coming" row inside the relevant card region.

---

### Task 1: arpillar — `templates()` + `option_schema()` (the declarative registry)

**Repo:** `/Users/vignesh/projects/r/arpillar`

**Files:**
- Create: `R/fct_templates.R`
- Test: `tests/testthat/test-templates.R`

**Interfaces:**
- Consumes: nothing (static registry; mirrors the grounded role-slot + option facts above).
- Produces: `templates()` → named list of template lists; `template(id)` → one template (unknown id → `arpillar_error_input`); `option_schema(type)` → data.frame `key <chr>, label <chr>, kind <chr: "int"|"choice"|"flag"|"text"|"levels"|"numvec">, default <list-col>, choices <list-col>` (unknown type → `arpillar_error_input`). Template shape (a plain list — no S7 needed for a static registry):
  `list(id, label, kind = "table"|"figure", type, description, slots = list(list(slot, label, accepts <chr>, min <int>, max <int|Inf>)), title, footnotes)`.

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test-templates.R
test_that("templates() lists the v1 registry keyed by id", {
  tp <- templates()
  expect_named(tp, c("demographics", "crosstab", "mean_line", "box", "km"))
  for (t in tp) {
    expect_true(all(c("id", "label", "kind", "type", "description", "slots",
                      "title", "footnotes") %in% names(t)))
    expect_true(t$kind %in% c("table", "figure"))
    for (s in t$slots) {
      expect_true(all(c("slot", "label", "accepts", "min", "max") %in% names(s)))
    }
  }
})

test_that("template slot names match what the render legs extract", {
  # Grounded: summary/crosstab want treatment+summarize; line/box x/y/group;
  # km time/censor/group. The registry MUST use those exact slot strings.
  expect_setequal(vapply(template("demographics")$slots, `[[`, "", "slot"),
                  c("treatment", "summarize"))
  expect_setequal(vapply(template("mean_line")$slots, `[[`, "", "slot"),
                  c("x", "y", "group"))
  expect_setequal(vapply(template("km")$slots, `[[`, "", "slot"),
                  c("time", "censor", "group"))
})

test_that("template() aborts on an unknown id", {
  expect_error(template("nope"), class = "arpillar_error_input")
})

test_that("option_schema covers exactly the engine-consumed keys", {
  expect_setequal(option_schema("summary")$key, "decimals")
  expect_setequal(
    option_schema("line")$key,
    c("error_type", "ci_level", "legend_position", "palette",
      "x_order", "x_label", "y_label")
  )
  expect_setequal(
    option_schema("box")$key,
    c("mean_diamond", "legend_position", "palette", "x_order", "x_label", "y_label")
  )
  expect_setequal(
    option_schema("km")$key,
    c("event_value", "ci", "risk_table", "censor_marks",
      "legend_position", "palette", "x_label", "y_label", "time_breaks")
  )
  expect_error(option_schema("waterfall"), class = "arpillar_error_input")
})

test_that("every schema default matches the engine default", {
  line <- option_schema("line")
  d <- function(df, k) df$default[[match(k, df$key)]]
  expect_identical(d(line, "error_type"), "ci")
  expect_identical(d(line, "ci_level"), 0.95)
  expect_identical(d(line, "legend_position"), "bottom")
  expect_identical(d(line, "palette"), "Set2")
  km <- option_schema("km")
  expect_identical(d(km, "event_value"), 0)
  expect_identical(d(km, "risk_table"), TRUE)
  expect_identical(d(option_schema("summary"), "decimals"), 1L)
})
```

- [ ] **Step 2: Run to verify failure** — `Rscript -e 'devtools::test(filter = "templates")'` → FAIL, `templates` not found.

- [ ] **Step 3: Implement `R/fct_templates.R`**

```r
# The declarative output-template registry.
#
# Templates are the single contract three UI surfaces share: the Outputs rail
# (what can be added), the Roles pane (which slots exist and what they accept),
# and the Options pane (which knobs exist, their kinds and defaults). The slot
# names and option keys are pinned to what the render legs actually extract --
# they are the render contract, not a UI convenience.

.SLOT <- function(slot, label, accepts, min = 1L, max = 1L) {
  list(slot = slot, label = label, accepts = accepts, min = min, max = max)
}

.TEMPLATES <- list(
  demographics = list(
    id = "demographics", label = "Demographics Summary", kind = "table",
    type = "summary",
    description = "Descriptive statistics and counts by treatment arm.",
    slots = list(
      .SLOT("treatment", "Treatment arms", "category"),
      .SLOT("summarize", "Summarize", c("measure", "category"), 1L, Inf)
    ),
    title = "Demographics and Baseline Characteristics",
    footnotes = "Safety Population."
  ),
  crosstab = list(
    id = "crosstab", label = "Categorical Crosstab", kind = "table",
    type = "crosstab",
    description = "Counts and percentages of categories by treatment arm.",
    slots = list(
      .SLOT("treatment", "Treatment arms", "category"),
      .SLOT("summarize", "Tabulate", "category", 1L, Inf)
    ),
    title = "Summary of Categorical Variables",
    footnotes = "Safety Population."
  ),
  mean_line = list(
    id = "mean_line", label = "Mean Over Time", kind = "figure", type = "line",
    description = "Mean with interval by visit and treatment arm.",
    slots = list(
      .SLOT("x", "X axis (visit)", c("category", "date")),
      .SLOT("y", "Analysis value", "measure"),
      .SLOT("group", "Treatment arms", "category")
    ),
    title = "Mean Over Time", footnotes = character(0)
  ),
  box = list(
    id = "box", label = "Box Plot", kind = "figure", type = "box",
    description = "Distribution of a measure by group and treatment arm.",
    slots = list(
      .SLOT("x", "X axis", c("category", "date")),
      .SLOT("y", "Analysis value", "measure"),
      .SLOT("group", "Treatment arms", "category")
    ),
    title = "Distribution by Treatment", footnotes = character(0)
  ),
  km = list(
    id = "km", label = "Kaplan-Meier", kind = "figure", type = "km",
    description = "Survival curve with at-risk table by treatment arm.",
    slots = list(
      .SLOT("time", "Time to event", "measure"),
      .SLOT("censor", "Censor flag", c("measure", "category")),
      .SLOT("group", "Treatment arms", "category")
    ),
    title = "Kaplan-Meier Estimate", footnotes = character(0)
  )
)

.OPT <- function(key, label, kind, default, choices = NULL) {
  list(key = key, label = label, kind = kind, default = default,
       choices = choices)
}

.OPTION_SCHEMA <- list(
  summary = list(.OPT("decimals", "Decimal places", "int", 1L)),
  crosstab = list(.OPT("decimals", "Decimal places", "int", 1L)),
  line = list(
    .OPT("error_type", "Interval", "choice", "ci", c("ci", "se", "sd")),
    .OPT("ci_level", "Confidence level", "choice", 0.95, c(0.9, 0.95, 0.99)),
    .OPT("legend_position", "Legend", "choice", "bottom",
         c("bottom", "right", "top", "left", "none")),
    .OPT("palette", "Palette", "choice", "Set2",
         c("Set2", "Set1", "Dark2", "Paired")),
    .OPT("x_order", "X level order", "levels", NULL),
    .OPT("x_label", "X axis label", "text", NULL),
    .OPT("y_label", "Y axis label", "text", NULL)
  ),
  box = list(
    .OPT("mean_diamond", "Mark means", "flag", FALSE),
    .OPT("legend_position", "Legend", "choice", "bottom",
         c("bottom", "right", "top", "left", "none")),
    .OPT("palette", "Palette", "choice", "Set2",
         c("Set2", "Set1", "Dark2", "Paired")),
    .OPT("x_order", "X level order", "levels", NULL),
    .OPT("x_label", "X axis label", "text", NULL),
    .OPT("y_label", "Y axis label", "text", NULL)
  ),
  km = list(
    .OPT("event_value", "Event value of censor", "int", 0),
    .OPT("ci", "Confidence band", "flag", FALSE),
    .OPT("risk_table", "At-risk table", "flag", TRUE),
    .OPT("censor_marks", "Censor marks", "flag", TRUE),
    .OPT("legend_position", "Legend", "choice", "bottom",
         c("bottom", "right", "top", "left", "none")),
    .OPT("palette", "Palette", "choice", "Set2",
         c("Set2", "Set1", "Dark2", "Paired")),
    .OPT("x_label", "X axis label", "text", NULL),
    .OPT("y_label", "Y axis label", "text", NULL),
    .OPT("time_breaks", "At-risk timepoints", "numvec", NULL)
  )
)
```

Then the three exports (full roxygen per the house standard — one-line purpose,
2 progressive `@examples` ending on the printed value, `@seealso` linking the
trio + `build_ard`):

```r
templates <- function() {
  .TEMPLATES
}

template <- function(id) {
  .check_scalar_chr(id, "id")
  t <- .TEMPLATES[[id]]
  if (is.null(t)) {
    cli::cli_abort(
      c(
        "Unknown output template.",
        "x" = "No template is named {.val {id}}.",
        "i" = "Available: {.val {names(.TEMPLATES)}}."
      ),
      class = "arpillar_error_input",
      call = rlang::caller_env()
    )
  }
  t
}

option_schema <- function(type) {
  .check_scalar_chr(type, "type")
  sch <- .OPTION_SCHEMA[[type]]
  if (is.null(sch)) {
    cli::cli_abort(
      c(
        "No option schema for this output type.",
        "x" = "Type {.val {type}} is not renderable.",
        "i" = "Renderable types: {.val {names(.OPTION_SCHEMA)}}."
      ),
      class = "arpillar_error_input",
      call = rlang::caller_env()
    )
  }
  data.frame(
    key = vapply(sch, `[[`, "", "key"),
    label = vapply(sch, `[[`, "", "label"),
    kind = vapply(sch, `[[`, "", "kind"),
    default = I(lapply(sch, `[[`, "default")),
    choices = I(lapply(sch, `[[`, "choices")),
    stringsAsFactors = FALSE
  )
}
```

- [ ] **Step 4: document + test + format** — `Rscript -e 'devtools::document()'`; `Rscript -e 'devtools::test()'` → ALL pass (prior 258 + new); `air format R tests`. No `*.new` goldens.
- [ ] **Step 5: Commit** — `git add -A && git commit -m "feat(templates): declarative output-template registry + option schema"`

---

### Task 2: arpillar — `output_status()` + `validate_output()` (the single oracle)

**Repo:** `/Users/vignesh/projects/r/arpillar`

**Files:**
- Create: `R/fct_status.R`
- Test: `tests/testthat/test-status.R`

**Interfaces:**
- Consumes: `object` S7 class; the grounded role-slot rules (NOT `templates()` — the oracle keys off `object@type` so it works for any object, template-born or not).
- Produces:
  - `validate_output(object)` → data.frame `control_id <chr>, message <chr>, order_index <int>` — zero rows == complete. `control_id` values are stable tokens the UI namespaces: `"dataset"`, `"roles-treatment"`, `"roles-summarize"`, `"roles-x"`, `"roles-y"`, `"roles-group"`, `"roles-time"`, `"roles-censor"`, `"type"`.
  - `output_status(object)` → `"ready" | "draft" | "needs_data"` (`"broken"` is a runtime flag the app layers on after a render error — the static oracle cannot know it).
- **The oracle property (the load-bearing test):** for every renderable type, `output_status(obj) == "ready"` ⟺ the render leg does NOT throw `arpillar_error_input` for role/dataset reasons. This kills the audit's "two conflicting is-configured checks" permanently.

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test-status.R
.st_items <- function(...) lapply(list(...), function(nm) data_item(name = nm))

.st_obj <- function(type, dataset = "ADSL", roles = list()) {
  object(id = "o1", type = type, dataset = dataset, roles = roles)
}

test_that("needs_data when the object names no dataset", {
  o <- .st_obj("summary", dataset = "")
  expect_identical(output_status(o), "needs_data")
  v <- validate_output(o)
  expect_identical(v$control_id[[1L]], "dataset")
})

test_that("draft lists each unfilled slot in slot order", {
  o <- .st_obj("summary")
  v <- validate_output(o)
  expect_identical(v$control_id, c("roles-treatment", "roles-summarize"))
  expect_identical(v$order_index, c(1L, 2L))
  expect_identical(output_status(o), "draft")

  half <- .st_obj("summary", roles = list(
    role(slot = "treatment", items = .st_items("TRT01P"))
  ))
  expect_identical(validate_output(half)$control_id, "roles-summarize")
})

test_that("group/row/strata aliases satisfy their slots", {
  o <- .st_obj("summary", roles = list(
    role(slot = "group", items = .st_items("TRT01P")),
    role(slot = "row", items = .st_items("AGE"))
  ))
  expect_identical(output_status(o), "ready")
})

test_that("a slot holding too many items is reported, not ready", {
  o <- .st_obj("line", roles = list(
    role(slot = "x", items = .st_items("AVISIT", "AVISITN")),
    role(slot = "y", items = .st_items("AVAL")),
    role(slot = "group", items = .st_items("TRT01P"))
  ))
  expect_identical(output_status(o), "draft")
  expect_match(validate_output(o)$message[[1L]], "exactly one")
})

test_that("crosstab requires at least one category item", {
  o <- .st_obj("crosstab", roles = list(
    role(slot = "treatment", items = .st_items("TRT01P")),
    role(slot = "summarize", items = list(
      data_item(name = "AGE", role_type = "measure")
    ))
  ))
  expect_identical(output_status(o), "draft")
})

test_that("an unrenderable type reports control_id 'type'", {
  o <- .st_obj("waterfall")
  expect_identical(output_status(o), "draft")
  expect_identical(validate_output(o)$control_id[[1L]], "type")
})

test_that("ORACLE: ready predicts render acceptance, draft predicts rejection", {
  skip_if_not_installed("nanoparquet")
  dir <- withr::local_tempdir()
  df <- data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    TRT01P = rep(c("A", "B"), 4), AGE = c(30, 40, 50, 60, 55, 33, 44, 51),
    AVISIT = rep(c("W1", "W2"), 4), AVAL = as.double(1:8),
    CNSR = rep(0:1, 4), stringsAsFactors = FALSE
  )
  pq <- file.path(dir, "adsl.parquet")
  nanoparquet::write_parquet(df, pq)
  con <- engine_open(); withr::defer(engine_close(con))
  register_dataset(con, "ADSL", pq)

  ready_tbl <- .st_obj("summary", roles = list(
    role(slot = "treatment", items = .st_items("TRT01P")),
    role(slot = "summarize", items = list(
      data_item(name = "AGE", role_type = "measure")
    ))
  ))
  expect_identical(output_status(ready_tbl), "ready")
  expect_no_error(build_ard(con, ready_tbl))

  draft_tbl <- .st_obj("summary")
  expect_identical(output_status(draft_tbl), "draft")
  expect_error(build_ard(con, draft_tbl), class = "arpillar_error_input")

  ready_km <- .st_obj("km", roles = list(
    role(slot = "time", items = list(data_item(name = "AVAL", role_type = "measure"))),
    role(slot = "censor", items = .st_items("CNSR")),
    role(slot = "group", items = .st_items("TRT01P"))
  ))
  expect_identical(output_status(ready_km), "ready")
  expect_no_error(render_ggplot(con, ready_km))
})
```

- [ ] **Step 2: Run to verify failure** — `Rscript -e 'devtools::test(filter = "status")'` → FAIL, `output_status` not found.

- [ ] **Step 3: Implement `R/fct_status.R`**

```r
# The single is-configured oracle.
#
# output_status()/validate_output() are the ONE place completeness is decided:
# the Outline status dots, the canvas ghost-vs-render switch, the commit gates,
# and the Review rail all read this pair. The slot requirements MIRROR what the
# render legs extract (fct_render_ard.R .arm_var/.summarize_items,
# fct_render_ggplot.R .figure_roles, fct_render_km.R .km_roles) -- the oracle
# test pins that equivalence so the two can never drift.

.SLOT_REQS <- list(
  summary = list(
    list(id = "roles-treatment", slots = c("treatment", "group"), n = 1L,
         what = "a treatment variable"),
    list(id = "roles-summarize", slots = c("summarize", "row"), n = NA_integer_,
         what = "at least one variable to summarize")
  ),
  crosstab = list(
    list(id = "roles-treatment", slots = c("treatment", "group"), n = 1L,
         what = "a treatment variable"),
    list(id = "roles-summarize", slots = c("summarize", "row"), n = NA_integer_,
         what = "at least one category variable to tabulate")
  ),
  line = list(
    list(id = "roles-x", slots = "x", n = 1L, what = "an x-axis variable"),
    list(id = "roles-y", slots = "y", n = 1L, what = "an analysis variable"),
    list(id = "roles-group", slots = c("group", "treatment"), n = 1L,
         what = "a treatment variable")
  ),
  box = list(
    list(id = "roles-x", slots = "x", n = 1L, what = "an x-axis variable"),
    list(id = "roles-y", slots = "y", n = 1L, what = "an analysis variable"),
    list(id = "roles-group", slots = c("group", "treatment"), n = 1L,
         what = "a treatment variable")
  ),
  km = list(
    list(id = "roles-time", slots = "time", n = 1L, what = "a time variable"),
    list(id = "roles-censor", slots = "censor", n = 1L, what = "a censor variable"),
    list(id = "roles-group", slots = c("group", "treatment", "strata"), n = 1L,
         what = "a treatment variable")
  )
)

validate_output <- function(object) {
  # NOT .check_object() -- that helper ALSO aborts on an empty dataset, which is
  # exactly the needs_data state this oracle must REPORT. Use a type-only guard
  # and let the empty-dataset case fall through to the add("dataset", ...) branch.
  .check_is_object(object)
  probs <- list()
  add <- function(id, msg) {
    probs[[length(probs) + 1L]] <<- list(control_id = id, message = msg)
  }

  if (!nzchar(object@dataset)) {
    add("dataset", "Choose a dataset for this output.")
  }
  reqs <- .SLOT_REQS[[object@type]]
  if (is.null(reqs)) {
    add("type", paste0(
      "Output type '", object@type, "' cannot be rendered yet."
    ))
  } else {
    for (r in reqs) {
      items <- .slot_items(object, r$slots)
      if (is.na(r$n)) {
        if (length(items) == 0L) {
          add(r$id, paste0("Assign ", r$what, "."))
        }
      } else if (length(items) == 0L) {
        add(r$id, paste0("Assign ", r$what, "."))
      } else if (length(items) != r$n) {
        add(r$id, paste0("This role must hold exactly one variable; it holds ",
                         length(items), "."))
      }
    }
    if (object@type == "crosstab" && length(probs) == 0L) {
      cats <- Filter(
        function(it) identical(it@role_type, "category"),
        .slot_items(object, c("summarize", "row"))
      )
      if (length(cats) == 0L) {
        add("roles-summarize",
            "A crosstab needs at least one category variable.")
      }
    }
  }

  data.frame(
    control_id = vapply(probs, `[[`, "", "control_id"),
    message = vapply(probs, `[[`, "", "message"),
    order_index = seq_along(probs),
    stringsAsFactors = FALSE
  )
}

output_status <- function(object) {
  v <- validate_output(object)
  if (nrow(v) == 0L) {
    return("ready")
  }
  if ("dataset" %in% v$control_id) {
    return("needs_data")
  }
  "draft"
}

#' All items across the first role whose slot matches one of `slots`.
#' @noRd
.slot_items <- function(object, slots) {
  for (r in object@roles) {
    if (r@slot %in% slots) {
      return(r@items)
    }
  }
  list()
}

#' The S7 type check from `.check_object`, WITHOUT its empty-dataset abort.
#'
#' `.check_object` (fct_render_ard.R) aborts when `@dataset` is empty; the status
#' oracle must instead REPORT that as `needs_data`, so it needs a type-only guard.
#' Arg is `x` (not `object`) so the bare `object` here resolves to the S7 class.
#' @noRd
.check_is_object <- function(x) {
  if (!S7::S7_inherits(x, object)) {
    cli::cli_abort(
      c(
        "{.arg object} must be an {.cls object}.",
        "x" = "You supplied {.obj_type_friendly {x}}."
      ),
      class = "arpillar_error_input",
      call = rlang::caller_env()
    )
  }
}
```

- [ ] **Step 4: document + test + format; all prior tests + goldens green.**
- [ ] **Step 5: Commit** — `git commit -m "feat(status): output_status + validate_output single configured-oracle"`

---

### Task 3: arpillar — `render_spec()` (the screen==paper seam)

**Repo:** `/Users/vignesh/projects/r/arpillar`

**Files:**
- Modify: `R/fct_render_table.R` (split `render_rtf` into spec-build + emit)
- Test: `tests/testthat/test-render_table.R` (extend)

**Interfaces:**
- Consumes: the existing internal spec-assembly inside `render_rtf` (tabular `cols`/`col_spec(align = "decimal")`/`headers("Treatment Group" = arms)`/`tabular(titles =, footnotes =)`).
- Produces: `render_spec(ard, object)` → the `tabular_spec` (everything `render_rtf` builds short of `emit()`); `render_rtf(ard, object, path)` becomes `tabular::emit(render_spec(ard, object), path, format = "rtf")` — behavior identical.
- **Gate:** the demographics RTF byte-golden must remain BYTE-IDENTICAL (the refactor cannot change one byte).

- [ ] **Step 1: Write the failing test**

```r
# append to tests/testthat/test-render_table.R
test_that("render_spec returns the same spec render_rtf emits (byte gate)", {
  skip_if_not_installed("nanoparquet")
  # This file builds its fixture INSIDE each test_that (there is no shared
  # file-scope con/obj) -- mirror that pattern here.
  dir <- withr::local_tempdir()
  df <- data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    TRT01P = rep(c("A", "B"), 4), AGE = c(30, 40, 50, 60, 55, 33, 44, 51),
    SEX = c("M", "F", "M", "F", "F", "M", "M", "F"), stringsAsFactors = FALSE
  )
  pq <- file.path(dir, "adsl.parquet")
  nanoparquet::write_parquet(df, pq)
  con <- engine_open(); withr::defer(engine_close(con))
  register_dataset(con, "ADSL", pq)
  obj <- object(
    id = "t1", type = "summary", dataset = "ADSL", title = "Demographics",
    roles = list(
      role(slot = "treatment", items = list(data_item(name = "TRT01P"))),
      role(slot = "summarize", items = list(
        data_item(name = "AGE", role_type = "measure"),
        data_item(name = "SEX", role_type = "category")
      ))
    )
  )

  ard <- build_ard(con, obj)
  spec <- render_spec(ard, obj)
  # tabular's spec is S7; its class vector is "tabular::tabular_spec", not the
  # bare "tabular_spec" -- assert via S7, not expect_s3_class(bare-name).
  expect_true(S7::S7_inherits(spec, tabular::tabular_spec))

  # as.tags is the Shiny embed seam -- must return renderable tags.
  tags <- htmltools::as.tags(spec)
  expect_s3_class(tags, "shiny.tag.list")
  expect_match(as.character(tags), "tabular-doc", fixed = TRUE)

  # emitting the spec ourselves == render_rtf byte-for-byte.
  a <- file.path(dir, "a.rtf"); b <- file.path(dir, "b.rtf")
  render_rtf(ard, obj, a)
  tabular::emit(spec, b, format = "rtf")
  expect_identical(readBin(a, "raw", file.info(a)$size),
                   readBin(b, "raw", file.info(b)$size))
})
```

This test builds its OWN fixture (the pattern every `test_that` in this file
uses; there is no shared `con`/`obj`). The demographics BYTE-GOLDEN snapshot
lives in a separate file, `test-golden-demographics.R` — Step 4's "golden
byte-identical" gate is satisfied there. The `render_spec` extraction must not
change one byte of `render_rtf`'s output (it is a pure extraction, so it won't),
which that golden file confirms.

- [ ] **Step 2: Run to verify failure** — `render_spec` not found.
- [ ] **Step 3: Implement.** In `R/fct_render_table.R`, move the spec-assembly body of `render_rtf` into `render_spec(ard, object)` (exported, house roxygen; `@seealso` `render_rtf`, `render_display`), leaving:

```r
render_rtf <- function(ard, object, path) {
  .check_rtf_path(path)                 # keep the existing path guard
  spec <- render_spec(ard, object)
  tabular::emit(spec, path, format = "rtf")
  invisible(path)
}
```

`render_spec` performs the exact assembly previously inline (render_display →
`tabular::tabular(...)` with the same titles/footnotes defaults → `cols` /
`col_spec(align = "decimal")` on arm columns → `headers("Treatment Group" = arms)`)
and returns the spec. Move the path guard OUT of the spec build (it belongs to
the emit leg only). Add `htmltools` to `Suggests` (test-only usage; tabular
registers the `as.tags` S3 method).

- [ ] **Step 4: Verify the byte gate** — full `devtools::test()`: demographics golden byte-identical, zero `*.new`. Then `Rscript -e 'devtools::check(args = "--no-manual")'` → 0 errors / 0 warnings.
- [ ] **Step 5: Commit** — `git commit -m "feat(render): render_spec seam -- same tabular spec for screen preview and RTF emit"`

---

## ADDENDUM (2026-07-02) — Outputs model reframe: generators + presets + the occurrence engine

The Outputs catalog is **generators (reusable engine macros) + presets (named configs)**, not template-per-table (decided with the user; see memory [[arframe-outputs-model]]). Two new arpillar tasks (Task E1, Task E2) reopen `feat/ui-prereqs` and run BEFORE resuming arframe. Task 8 (Add-output) is reframed to a generator-picker + preset-library + Save-as-preset. Everything else (Tasks 4–7, 9–17) stands; the store's `add_output` gains a preset path.

### TLF numbering model (decided with the user 2026-07-02)

TLF numbers (`14.1.1`, `14.3.1`, ...) are **editable output metadata, preset-seeded** — NOT auto-derived from output type (real numbering is domain/SAP-shell driven per ICH E3: 14.1 demographics, 14.2 efficacy, 14.3 safety, 16.2 listings; figures follow their domain, not "all 14.2"). Model:
- Store on the output as **`options$number`** (chr, e.g. `"14.3.1"`) + **`options$number_label`** (chr: `"Table"`/`"Figure"`/`"Listing"`). No S7 change (lives in `options`, like population/subject_id).
- **Presets seed it** (E2): each preset carries a canonical `number` + `number_label` in its options prefill (Demographics -> Table 14.1.1, AE incidence -> Table 14.3.1, KM -> Figure 14.2.1...).
- **`add_output` (store)**: from a preset -> copy its number; from a bare generator -> auto-suggest the next free number within that kind group (next `14.1.x` for a table) as a starting value. Always editable after.
- **Editable** in the Options pane title region (Task 11): a text field for the number + a label-word select.
- **Displayed** from `options$number`, never re-derived: the **TOC groups by kind (Tables/Figures/Listings) but shows `options$number`** (Task 7 RETROFIT — replace the current auto `14.1.n` index with `options$number %||% <auto-suggest>`), and the **paper title block** (Task 9) shows `"<number_label> <number>"` above the title.
- **Retrofit note (Task 7 is already built):** its TOC auto-derives the number by kind today; when Task 9 (title block) lands, retrofit both display surfaces to read `options$number`. Small, tracked in the ledger.

### Task E1: arpillar — the occurrence generator (AE / SOC▸PT)

**Repo:** `/Users/vignesh/projects/r/arpillar` (branch `feat/ui-prereqs`)

**Files:**
- Create: `R/fct_render_occurrence.R` (the occurrence ARD + display legs) — or extend `fct_render_ard.R`/`fct_render_table.R` if that fits the existing structure better; implementer decides after reading them.
- Modify: `R/fct_render_ard.R` (`build_ard` dispatch: add `"occurrence"`), `R/fct_render_table.R` (`render_display`/`render_spec` handle the hierarchy shape), `R/fct_status.R` (`.SLOT_REQS$occurrence`), `R/fct_templates.R` (`option_schema("occurrence")`)
- Test: `tests/testthat/test-render_occurrence.R`, `tests/testthat/test-golden-occurrence.R` (byte-golden)

**Grounding — DONE.** Full source-grounded brief (explorer pattern + cards 0.8.0 API verified + exact arpillar change-sites + a hand-checkable ADAE fixture) is in the grounding agent's output file: `/private/tmp/claude-501/-Users-vignesh-projects-r-explorer/358cef77-6981-4ea5-b5c4-2a64d5aca11b/tasks/a1614031dfa971963.output` (read it first — it is your implementation brief). Key verified facts: `cards::ard_stack_hierarchical(data, variables, by, id, denominator, ...)` — `id`+`denominator` REQUIRED; first `variables` entry = top level; subject-level dedup is automatic (last-row-per-group); `pivot_across` on the result gives `soc, label, row_type, <arms>` columns.

**DECISIONS (locked — do not re-litigate):**
1. **Hierarchy role = a dedicated `"hierarchy"` slot** (ordered list of category items, first = top/SOC), read by a new `.hierarchy_items(object)` via the existing `.find_role` — NOT role_type-gated, so `aaa_class.R` `data_item`/validator is UNTOUCHED.
2. **Two-dataset (the critical finding):** `object@dataset` = the event frame (ADAE). The subject-level denominator comes from a SECOND registered dataset named by **`object@options$population`** (a dataset id string), pulled via its own `.collect_filtered` — reusing `data` as the denominator gives wrong (event-count) N's (verified). Subject id = **`object@options$subject_id %||% "USUBJID"`**. Keep `object`'s S7 class unchanged (population/subject_id live in `options`, matching the explorer) — no new S7 property.
3. **v1 scope = 1 or 2 hierarchy levels** (PT-only, or SOC▸PT — the canonical AE table). `validate_output` rejects >2 with a clear "3+ levels coming" message; the cards call is N-capable for free, but the display walk is built for ≤2 now (N-level display is a fast-follow, noted, not built).
4. **Render = indent-via-label-text** (arpillar's existing contract): SOC header row (`label="GI"`) + `"  "`-indented PT rows (`label="  Nausea"`, cells `"n (pct)"`, population-N base), reusing `.row`/`.arm_levels`/`.fmt_count_cell`. Do NOT port tabular's `col_spec(usage="group")` machinery — the leading-space idiom is arpillar's way and is simpler.

**Contract:**
- `object@type == "occurrence"`. Roles: `treatment`/`group` (exactly 1 arm) + **`hierarchy`** (an ORDERED ≥1 list of classification variables, e.g. AEBODSYS ▸ AEDECOD; 1 level = flat PT-only, 2 = SOC▸PT, N generalizes). Population denominator = the filtered subject frame (incidence % base = population N per arm, matching the header N).
- `build_ard(con, occurrence_obj)` → cards ARD via `ard_stack_hierarchical` over the ordered hierarchy vars, `.by = arm`, subject-level (distinct-subject incidence, not event rows). `render_display` → SOC header rows + indented PT rows, `n (pct)` per arm (pct 1dp, population-N base); ordered by descending leaf frequency by default (`options$hier_sort`). `render_spec`/`render_rtf` emit it (indentation via the stub column). An occurrence RTF **byte-golden** (frozen, deterministic — like demographics).
- `option_schema("occurrence")`: at least `hier_sort` (choice: freq/alpha, default freq), `cutoff` (numeric incidence % threshold on the leaf, default 0), `top_n` (int, leaf, default 0 = all), `overall_row` (flag, an "any event" row). Match whatever the render leg actually reads (pin keys = the byte-golden contract, per the option_schema discipline).
- `.SLOT_REQS$occurrence`: `roles-treatment` (treatment|group, exactly 1) + `roles-hierarchy` (hierarchy, ≥1). `output_status` "ready" ⟺ build_ard accepts (the oracle property extends to occurrence).

**Steps (TDD):** ground → failing tests (ARD shape: SOC/PT rows, incidence counts hand-checked on a small ADAE fixture; display rows; the golden) → run to fail → implement → document + `devtools::test()` (prior 326 + new; ALL goldens incl. the new occurrence one byte-stable) → `devtools::check(args="--no-manual")` 0/0 (known time NOTE only) → `air format` → commit `feat(occurrence): SOC▸PT adverse-event generator + byte-golden`.

**Check:** a 2-level SOC▸PT occurrence table renders to a deterministic RTF byte-golden; incidence %s use the population-N base per arm; `output_status` predicts render acceptance for occurrence objects.

### Task E2: arpillar — reframe `templates()` into `generators()` + `presets()`

**Repo:** `/Users/vignesh/projects/r/arpillar` (branch `feat/ui-prereqs`)

**Files:**
- Modify/replace: `R/fct_templates.R` → `R/fct_generators.R` (`generators()`, `generator(id)`) + `R/fct_presets.R` (`presets()`, `preset(id)`); keep `option_schema()` (add occurrence). Update `NAMESPACE`, the `.Rd`, and `tests/testthat/test-templates.R` → `test-generators.R` + `test-presets.R`.

**Interfaces:**
- `generators()` → named list of the engine macros; each: `id` (= the engine `type`: `summary`/`crosstab`/`occurrence`/`km`/`line`/`box`), `label`, `kind` (`table`/`figure`), `description`, `slots` (`list(slot, label, accepts, min, max)`). This is the current `templates()` content REPURPOSED as generators, PLUS the occurrence generator (slots: treatment + hierarchy [min 1, max Inf, accepts category]).
- `presets()` → named list of the STARTER library; each: `id`, `label`, `domain` (`"Safety"`/`"Efficacy"`/`"PK"`/`"General"`), `generator` (a `generators()` id), `roles` (prefill: slot → variable names, using CDISC-canonical names as sensible defaults, e.g. TRT01P/AGE/SEX/AEBODSYS/AEDECOD/AVAL/AVISIT/CNSR), `options`, `filters`, `title`, `footnotes`. Ship ~15–20 across domains: demographics, disposition, AE overall summary, AE by SOC/PT, SAE, deaths, exposure, vital signs by visit, labs by visit, KM (OS), mean-over-time, box-by-visit, a categorical crosstab, etc. `preset(id)` aborts `arpillar_error_input` on unknown id.
- `option_schema(type)` gains `"occurrence"` (from Task E1).

**Steps (TDD):** failing tests (generators keyed by the 6 engine types with correct slots incl. occurrence's hierarchy slot; presets each reference a real generator id and only fill slots that generator exposes; every preset's `domain` in the known set; `preset("nope")` aborts) → implement → document + test + `air format` → commit `feat(catalog): generators + presets replace the templates registry`.

**Check:** every preset's `generator` is a real `generators()` id and its prefilled `roles` slots are a subset of that generator's slots; `generators()` includes `occurrence`.

---

### Task 4: arframe — Galley tokens, fonts, theme, atoms

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Create: `inst/www/tokens.css`, `inst/www/arframe.css`, `inst/www/fonts/` (6 woff2), `inst/COPYRIGHTS`, `R/theme.R`, `R/utils_atoms.R`
- Modify: `DESCRIPTION` (Imports + `fontawesome`, `htmltools`), `.gitignore` (add `.DS_Store`, `.local/`)
- Test: `tests/testthat/test-theme.R`, `tests/testthat/test-utils_atoms.R`

**Interfaces:**
- Produces: `ar_theme()` → `bslib::bs_theme` (brand vars only: `primary = "#2D5FA8"`, `bg = "#FFFFFF"`, `fg = "#1B1F23"`, `danger = "#B3261E"`, Plex font collections, `"border-radius" = "2px"`); `.head_assets()` → registers `arwww` once, links `tokens.css` + `arframe.css` (+ JS added Task 6); atoms `.icon(name, size = 16)` (fontawesome, margins pinned `"0px"`), `.label(text)` (micro-label div `.ar-label`), `.type_chip(role_type)` (measure `#` violet / date calendar amber / else `A` blue, 18×18), `.stamp(status)` (maps `ready→READY/--ar-ready`, `draft→DRAFT/--ar-draft`, `needs_data→NO DATA/ink-4`, `broken→ERROR/--ar-error`; mono caps, 1px colored border, transparent fill, `aria-label` full sentence), `.action_btn(id, label, variant, ...)` (Shiny-bindable, NO btn-default).
- `.fa_names` starts with: `plus, close = "xmark", pin = "thumbtack", pencil = "pen", grip = "grip-vertical", kebab = "ellipsis-vertical", search = "magnifying-glass", table = "table", figure = "chart-line", listing = "list", database, import = "file-import", export = "download", code, check, warn = "triangle-exclamation", undo = "arrow-rotate-left", redo = "arrow-rotate-right", open = "folder-open", save = "floppy-disk", arrow_right = "arrow-right", calendar, eye`.

- [ ] **Step 1: Fetch fonts (exact verified paths — releases carry NO standalone woff2, only per-family zips).** Download and extract:
  - `https://github.com/IBM/plex/releases/download/%40ibm%2Fplex-sans%401.1.0/ibm-plex-sans.zip` → take `ibm-plex-sans/fonts/split/woff2/IBMPlexSans-{Regular,Medium,SemiBold}-Latin1.woff2` (400/500/600).
  - `https://github.com/IBM/plex/releases/download/%40ibm%2Fplex-mono%402.5.0/ibm-plex-mono.zip` → take `ibm-plex-mono/fonts/split/woff2/IBMPlexMono-{Regular,Medium}-Latin1.woff2` (400/500).
  Place the five `*-Latin1.woff2` files in `inst/www/fonts/` (total ≈ 101 KB, well under the < 500 KB gate); the `@font-face` `src` URLs in `tokens.css` reference these exact filenames. The Latin1 subset covers Western/ASCII UI text — the declared system fallbacks handle anything outside it. Verify each is a real woff2 (`file` output). Write `inst/COPYRIGHTS`:

```
IBM Plex Sans, IBM Plex Mono (subset: latin, woff2)
License: SIL Open Font License 1.1
Copyright IBM Corp.
https://github.com/IBM/plex
Bundled in inst/www/fonts/

SortableJS (added in Task 7)
```

- [ ] **Step 2: Failing tests**

```r
# tests/testthat/test-theme.R
test_that("ar_theme is Bootstrap brand variables only, Galley values", {
  th <- ar_theme()
  expect_s3_class(th, "bs_theme")
  expect_identical(bslib::bs_get_variables(th, "primary")[["primary"]], "#2D5FA8")
})

test_that("head assets link both stylesheets from one resource path", {
  html <- as.character(htmltools::tagList(.head_assets()))
  expect_match(html, "arwww/tokens.css", fixed = TRUE)
  expect_match(html, "arwww/arframe.css", fixed = TRUE)
})

test_that("Galley ink/paper pairs hold WCAG AA", {
  lum <- function(hex) {
    v <- strtoi(c(substr(hex, 2, 3), substr(hex, 4, 5), substr(hex, 6, 7)), 16L) / 255
    v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    sum(v * c(0.2126, 0.7152, 0.0722))
  }
  cr <- function(a, b) (max(lum(a), lum(b)) + 0.05) / (min(lum(a), lum(b)) + 0.05)
  expect_gte(cr("#1B1F23", "#FFFFFF"), 4.5)  # ink on paper
  expect_gte(cr("#5C6670", "#FFFFFF"), 4.5)  # ink-3 on paper (footnotes, source)
  expect_gte(cr("#5C6670", "#E9EBEA"), 4.5)  # ink-3 on desk = the floor for ALL
  #                                           readable small text (micro-labels,
  #                                           group headers, TOC numbers). ink-4 /
  #                                           ink-5 are decoration only (leader
  #                                           dots, hairlines) -- never info text.
  expect_gte(cr("#B3261E", "#FFFFFF"), 4.5)  # error on paper
  # Stamp text sits in the TOC on the desk, so each stamp hex must clear 4.5:1
  # THERE (mockup greens/ambers are indicative; tune the three hexes to pass).
  for (hex in c("#2E7D4F", "#9A6B0B", "#B3261E")) {  # ready / draft / error text
    expect_gte(cr(hex, "#E9EBEA"), 4.5)
  }
})

# tests/testthat/test-utils_atoms.R
test_that(".stamp maps the four oracle states to letterpress stamps", {
  s <- as.character(.stamp("ready"))
  expect_match(s, "READY", fixed = TRUE)
  expect_match(s, "ar-stamp-ready", fixed = TRUE)
  expect_match(s, "aria-label", fixed = TRUE)
  expect_match(as.character(.stamp("needs_data")), "NO DATA", fixed = TRUE)
  expect_match(as.character(.stamp("broken")), "ERROR", fixed = TRUE)
  expect_error(.stamp("nope"), class = "arframe_error_input")
})

test_that(".icon pins fontawesome margins; .type_chip maps three kinds", {
  fa <- as.character(.icon("pin"))
  expect_match(fa, "margin-left:0", fixed = TRUE)
  expect_match(as.character(.type_chip("measure")), "ar-chip-meas", fixed = TRUE)
  expect_match(as.character(.type_chip("date")), "ar-chip-date", fixed = TRUE)
  expect_match(as.character(.type_chip("category")), "ar-chip-cat", fixed = TRUE)
})

test_that(".action_btn is Shiny-bindable without btn-default", {
  b <- as.character(.action_btn("go", "Go"))
  expect_match(b, "action-button", fixed = TRUE)
  expect_no_match(b, "btn-default")
})
```

- [ ] **Step 3: Verify failure.**
- [ ] **Step 4: Implement.** `inst/www/tokens.css` — the FULL token table from the Galley spec §1 verbatim (desk/chrome/paper/card/rules/ink 1–5/accent/stamps/focus/chips/radii/spacing/motion) + the two `@font-face` families (5 faces, `font-display: swap`) + the reduced-motion block. `inst/www/arframe.css` sections `00 base` + `01 atoms`:

```css
/* ============ 00 base ============ */
body { font-family: "IBM Plex Sans", system-ui, -apple-system, sans-serif;
  font-size: 13px; color: var(--ar-ink); background: var(--ar-desk); }
.ar-mono { font-family: "IBM Plex Mono", ui-monospace, SFMono-Regular, Menlo, monospace; }
.form-control, .form-select, .selectize-input {
  font-size: 12px; border-color: var(--ar-rule); border-radius: var(--ar-radius); }
.form-control:focus, .form-select:focus, .selectize-input.focus {
  border-color: var(--ar-accent); box-shadow: 0 0 0 3px var(--ar-accent-weak); }
input[type="checkbox"], input[type="radio"] { accent-color: var(--ar-accent); }
:focus-visible { outline: 3px solid var(--ar-focus); outline-offset: 0;
  box-shadow: 0 0 0 5px #0b0c0c; }
/* ============ 01 atoms ============ */
.ar-label { text-transform: uppercase; letter-spacing: 0.12em; font-size: 11px;
  font-weight: 500; color: var(--ar-ink-3);  /* ink-3, not ink-4: AA on the desk */
  font-family: "IBM Plex Mono", ui-monospace, monospace; margin: 2px 0 6px; }
.ar-stamp { display: inline-block; font-family: "IBM Plex Mono", monospace;
  font-size: 11px; letter-spacing: 0.08em; line-height: 1.4; padding: 0 4px;
  border: 1px solid currentColor; border-radius: var(--ar-radius-sm);
  background: transparent; }
.ar-stamp-ready { color: var(--ar-ready); }
.ar-stamp-draft { color: var(--ar-draft); border-color: #C9A227; }
.ar-stamp-needs_data { color: var(--ar-ink-4); }
.ar-stamp-broken { color: var(--ar-error); }
.ar-chip { display: inline-flex; align-items: center; justify-content: center;
  flex: 0 0 auto; width: 18px; height: 18px; border-radius: var(--ar-radius);
  font-family: "IBM Plex Mono", monospace; font-size: 9.5px; font-weight: 600;
  line-height: 1; margin-right: 6px; }
.ar-chip-cat { color: var(--ar-char-fg); background: var(--ar-char-bg); }
.ar-chip-meas { color: var(--ar-num-fg); background: var(--ar-num-bg); }
.ar-chip-date { color: var(--ar-date-fg); background: var(--ar-date-bg); }
.ar-icon-btn { width: 26px; height: 26px; display: inline-flex; align-items: center;
  justify-content: center; background: transparent; color: var(--ar-ink-3);
  border: 1px solid transparent; border-radius: var(--ar-radius); }
.ar-icon-btn:hover { background: rgba(27,31,35,0.05); border-color: var(--ar-rule);
  color: var(--ar-ink); }
```

`R/theme.R` + `R/utils_atoms.R` per Interfaces (atoms mirror the explorer shapes; `.stamp` aborts `arframe_error_input` on an unknown status; the app error helper `.abort_app(msg, ...)` wrapping `cli::cli_abort(class = "arframe_error_input")` lives in `utils_atoms.R`).

- [ ] **Step 5: document + test + format; commit** — `git commit -m "feat(theme): Galley token contract, IBM Plex, stamp/chip/icon atoms"`

---

### Task 5: arframe — the store (draft state out of the DOM) + demo fixtures

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Create: `R/fct_store.R`, `R/utils_report.R`, `R/utils_demo.R`
- Modify: `DESCRIPTION` (Imports + `S7`; Suggests + `nanoparquet`, `withr`)
- Test: `tests/testthat/test-fct_store.R`, `tests/testthat/test-utils_report.R`

**Interfaces (every later module consumes EXACTLY these):**

```r
new_store(con, report = NULL)
# -> list(con, rv = shiny::reactiveValues(...), undo = env, cache = env)
# rv fields (Galley pointers):
#   report   S7 report          selected chr id | NULL   (TOC selection = the page shown)
#   region   chr | NULL         (selected page region: "title"|"columns"|"rows"|
#                                "footnotes"|"source"|"axes"|"series"|"legend"|"filters")
#   card     lgl (galley card open)      pinned  lgl (card docked)
#   mode     "report"|"data"|"qc"        dataset chr | NULL (Data-mode selection)
#   bridge_dataset chr | NULL   (Use-in-new-output pre-seed, consumed once)
#   adding   lgl (Add-output overlay open)   filter_draft list (per-selection filter rows)
#   path chr | NULL   dirty lgl   saved_at chr | NULL
#   broken chr ids    log chr     catalog_nonce int
commit(store, new_report, label = "")      # undo push (cap 50), report<-, dirty<-TRUE
undo(store); redo(store); can_undo(store); can_redo(store)
selected_object(store)                     # S7 object | NULL
update_object(store, id, fn, label = "")   # fn(object)->object; rebuild + commit
add_output(store, template_id, dataset)    # object from template; select; -> id
remove_output(store, id); move_output(store, id, to); rename_output(store, id, title)
cached_ard(store, object)                  # keyed build_ard memo (two-stage seam)
log_line(store, msg)                       # timestamped line onto rv$log
open_card(store, region); close_card(store); toggle_pin(store)
# pure (utils_report.R): .find_object(report, id), .replace_object(report, id, obj),
# .remove_object(report, id), .move_object(report, id, to), .all_objects(report),
# .object_from_template(tpl, dataset, id), .next_id(report)  # sprintf("out%03d", n)
```

- ARD cache key: `rlang::hash(list(object@dataset, object@type, <roles as plain lists>, object@filters))` — options EXCLUDED (display-stage edits reuse the ARD: the two-stage seam). Verify `rlang::hash` exists first (`Rscript -e 'rlang::hash(1)'`).
- New report default: `report(id = "report1", name = "Untitled report", pages = list(page(id = "p1")))`.
- `.demo_catalog(dir = tempdir())` (internal): writes ADSL (USUBJID/TRT01P/AGE/SEX/SAFFL), ADVS (USUBJID/TRT01P/AVISIT/AVISITN/PARAMCD/AVAL), ADTTE (USUBJID/TRT01P/AVAL/CNSR) parquet via nanoparquet; returns an opened, registered catalog. Shared by tests, shinytest2 apps, screenshots.

- [ ] **Step 1: Failing tests.** Cover: store shape; commit/undo/redo round-trip (`identical()` on `report_to_json` strings); undo cap 50; `add_output` copies template type/title/footnotes, binds dataset, selects; `update_object` edits one object, siblings untouched; remove clears a dangling `selected`; move/rename; `open_card/close_card/toggle_pin` (pin survives `close_card`? NO — pinned card ignores close until unpinned: `close_card` is a no-op while `pinned`); `cached_ard` HIT on options-only change + MISS on filter/role change (assert via `sum(startsWith(ls(store$cache), "ard::"))` before/after, with `.demo_catalog()`); and **THE SUSPEND-CONTRACT REGRESSION**:

```r
test_that("REGRESSION: config committed via the store survives with no UI mounted", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_output(store, "demographics", "ADSL"))
  shiny::isolate(update_object(store, id, function(o) {
    S7::set_props(o, options = list(decimals = 2L))
  }))
  json <- arpillar::report_to_json(shiny::isolate(store$rv$report))
  back <- arpillar::report_from_json(json)
  expect_identical(.find_object(back, id)@options$decimals, 2L)
})
```

All store tests run under `shiny::isolate()` / `shiny::reactiveConsole(TRUE)` — no session.

- [ ] **Step 2: Verify failure.**
- [ ] **Step 3: Implement** — `utils_report.R` pure walkers over `report@pages[[i]]@objects` rebuilding with `S7::set_props`; `fct_store.R`:

```r
new_store <- function(con, report = NULL) {
  if (is.null(report)) {
    report <- arpillar::report(
      id = "report1", name = "Untitled report",
      pages = list(arpillar::page(id = "p1"))
    )
  }
  undo <- new.env(parent = emptyenv())
  undo$stack <- list(); undo$redo <- list()
  list(
    con = con,
    rv = shiny::reactiveValues(
      report = report, selected = NULL, region = NULL,
      card = FALSE, pinned = FALSE, mode = "report", dataset = NULL,
      bridge_dataset = NULL, adding = FALSE, filter_draft = list(),
      path = NULL, dirty = FALSE, saved_at = NULL,
      broken = character(0), log = character(0), catalog_nonce = 0L
    ),
    undo = undo,
    cache = new.env(parent = emptyenv())
  )
}

commit <- function(store, new_report, label = "") {
  store$undo$stack <- c(store$undo$stack, list(store$rv$report))
  n <- length(store$undo$stack)
  if (n > 50L) store$undo$stack <- store$undo$stack[(n - 49L):n]
  store$undo$redo <- list()
  store$rv$report <- new_report
  store$rv$dirty <- TRUE
  invisible(new_report)
}

cached_ard <- function(store, object) {
  roles <- lapply(object@roles, function(r) {
    list(slot = r@slot, items = lapply(r@items, function(it) {
      list(name = it@name, label = it@label, role_type = it@role_type)
    }))
  })
  # Prefix the key space: the ONE cache env also memoizes Add-output
  # recommendations ("rec::") and Data-mode profiles ("profile::"). Prefixing
  # keeps the two-stage-seam tests honest -- they count ard entries via
  # sum(startsWith(ls(store$cache), "ard::")), never bare ls() length.
  key <- paste0("ard::", rlang::hash(list(
    object@dataset, object@type, roles, object@filters
  )))
  hit <- store$cache[[key]]
  if (!is.null(hit)) {
    return(hit)
  }
  ard <- arpillar::build_ard(store$con, object)
  store$cache[[key]] <- ard
  ard
}
```

(remaining mutators 5–15 lines each over the walkers; `open_card(store, region)` sets `region`, `card <- TRUE`; `close_card` no-ops while `pinned`.)

- [ ] **Step 4: document + test + format.**
- [ ] **Step 5: Commit** — `git commit -m "feat(store): injected structured store -- S7 report, undo, ARD cache, galley pointers, suspend-contract regression"`

---

### Task 6: arframe — the frame (app bar · desk · status bar) + launcher

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Create: `R/mod_frame.R`, `inst/www/arframe.js`
- Modify: `R/app.R` (real launcher), `R/theme.R` (`.head_assets()` adds `arframe.js`), `inst/www/arframe.css` (append `02 frame`)
- Test: `tests/testthat/test-mod_frame.R`, `tests/testthat/apps/frame/app.R` (shinytest2)

**Interfaces:**
- Produces: `arframe(project = NULL, data = NULL)` export; `mod_frame_ui(id, report_body, data_body, qc_body)` / `mod_frame_server(id, store)`. The frame is layout-only: app bar + status bar + a `.ar-body` that mounts ALL THREE mode bodies at once; CSS shows the one matching `rv$mode`. Each mode body is a full furniture set composed in `app.R`; the report body is itself the three-region contents/desk/card layout.
- Frame anatomy (spec §3): 42px app bar (`.ar-bar`, `--ar-chrome`, hairline bottom): mono wordmark `arframe` · 1px divider · report title `span.ar-title` (click swaps to a text input via JS class flip; Enter/blur commits through `observeEvent(input$name)`) + pencil · spacer · quiet text buttons `Data` / `QC` (`.action_btn` link-style; active = accent + 2px underline) · mono `⌘K` hint (inert until v1.1) · `Export package` (solid ink `.ar-btn-ink`). The **report body** `.ar-body-report` is the three-region row: contents slot (280px, transparent — sits ON the desk) · desk (flex-1, centers the paper slot) · card slot (absolute right when floating; flush 320px column when `.ar-pinned`). The **data body** `.ar-body-data` and **QC body** `.ar-body-qc` are sibling full-body sets. Status bar 26px mono faint. The mode class on the workspace root (`.ar-mode-report` / `.ar-mode-data` / `.ar-mode-qc`) selects which body `display`s; all three stay MOUNTED (draft state lives in the store, never the DOM, so unmounting is never needed — mounting also preserves the grid/scroll state of the hidden modes).
- `arframe.js` (v1 of the bridge):

```js
$(document).on("click", "[data-ar-mode]", function () {
  Shiny.setInputValue("frame-mode", this.getAttribute("data-ar-mode"),
    { priority: "event" });
});
Shiny.addCustomMessageHandler("ar-mode", function (m) {
  var ws = document.querySelector(".ar-workspace");
  ws.className = "ar-workspace ar-mode-" + m;
});
Shiny.addCustomMessageHandler("ar-focus", function (m) {
  var el = document.getElementById(m.id);
  if (el) { el.scrollIntoView({ block: "nearest" }); el.focus(); }
});
Shiny.addCustomMessageHandler("ar-disable", function (m) {
  var el = document.getElementById(m.id);
  if (el) el.toggleAttribute("disabled", !!m.disabled);
});
```

(sortable + region-click handlers appended in Tasks 7/9; card pin in Task 10.)
- `arframe(project, data)`: opens `engine_open()`, registers `data` named paths, seeds `new_store` (project JSON via `report_from_json` when given), composes `mod_frame_ui("frame", report_body = tagList(mod_contents_ui(...), mod_paper_ui(...), mod_card_ui(...)), data_body = mod_data_mode_ui(...), qc_body = mod_qc_ui(...))` — with `div.ar-slot-placeholder` stand-ins for the bodies whose modules land later (paper T9, card T10, data T13, qc T15); `onStop(engine_close)`.
- Server: `observeEvent(input$mode)` → `rv$mode` + `ar-mode` message; undo/redo buttons wired to `undo(store)/redo(store)` with `ar-disable` sync; title edit commits `S7::set_props(report, name =)`.

- [ ] **Step 1: Failing tests** — testServer: `input$mode <- "data"` sets `rv$mode`; `input$name` commits the report name and `can_undo(store)` flips TRUE; undo restores. UI smoke: `mod_frame_ui` HTML contains `.ar-bar`, both `data-ar-mode` buttons, `.ar-statusbar`, and the three mode-body containers (`.ar-body-report` / `.ar-body-data` / `.ar-body-qc`).
- [ ] **Step 2: Verify failure.**
- [ ] **Step 3: Implement** (CSS `02 frame`: bar/desk/status geometry per spec §3; `.ar-btn-ink { background: var(--ar-ink); color: var(--ar-chrome); border-radius: var(--ar-radius); font-size: 11.5px; padding: 5px 12px; }`).
- [ ] **Step 4: shinytest2 + screenshot** — `tests/testthat/apps/frame/app.R` = `arframe(data = <demo paths>)`; assert bar renders, mode click flips the workspace class; `AppDriver$get_screenshot(".local/screens/06-frame.png")`. Reinstall arpillar-then-arframe first.
- [ ] **Step 5: Commit** — `git commit -m "feat(frame): Galley app bar, desk, status bar, mode switching, launcher"`

---

### Task 7: arframe — Contents (the TOC: numbers, leaders, stamps, reorder)

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Create: `R/mod_contents.R`, `inst/www/Sortable.min.js` (vendored 1.15.6, MIT header intact)
- Modify: `inst/COPYRIGHTS` (SortableJS entry), `inst/www/arframe.js` (append `arInitSortables` — the exact bridge from the Grounded facts section), `R/theme.R` (`.head_assets()` adds Sortable BEFORE arframe.js), `inst/www/arframe.css` (append `03 contents` + sortable drag states), `R/app.R` (compose into the contents slot)
- Test: `tests/testthat/test-mod_contents.R`

**Interfaces:**
- Consumes: `.all_objects()`, `arpillar::output_status()`, `arpillar::template()`/`templates()` (kind lookup by type), `.stamp()`, store mutators + `rv$broken`.
- Produces: `mod_contents_ui(id)` / `mod_contents_server(id, store)`. Anatomy per spec §3: `CONTENTS` micro-label; groups `TABLES / FIGURES / LISTINGS` (kind from the type→template map; empty groups omitted); entry = mono number (kind-scoped index for v1: `14.1.n` tables · `14.2.n` figures · `16.2.n` listings) + sans title + dotted leader + `.stamp(status)` where status = `if (id %in% rv$broken) "broken" else output_status(obj)`. Active = 2px accent left bar + weak wash (pre-reserved transparent border). Hover reveals grip + kebab (rename / duplicate / remove via `.ar-pop` popovers — labelled input + Apply; destructive confirm for remove). Row click → `rv$selected <- id`. Bottom: `+ Add output` → fires the Task-8 overlay (`rv$adding <- TRUE`, the field declared in Task 5's store).
- Sortable: container `data-ar-sortable` + `-handle=".ar-toc-grip"` + `-item=".ar-toc-row"` + `-attr="data-ar-id"` + `-input=ns("reorder")`; server reconciles `c(intersect(ord, ids), setdiff(ids, ord))` → `move_output`. **The list renderUI is keyed on membership/titles/statuses ONLY — never on order** (SortableJS owns DOM order between renders).

- [ ] **Step 1: Failing tests** — testServer with 3 demo outputs: rows grouped + numbered correctly; reorder input applies (assert `.all_objects` ids), stale id dropped, missing appended; rename commits; delete removes + clears dangling selection; stamps match the oracle (+ a `rv$broken` id shows ERROR); row click selects. JS smoke: `arframe.js` contains `_arSortable` and `shiny:value`.
- [ ] **Step 2: Verify failure.**
- [ ] **Step 3: Implement.** CSS `03 contents`: leader = `.ar-toc-leader { flex: 1; border-bottom: 1px dotted var(--ar-ink-5); margin: 0 5px 3px; }`; row grid `[grip 14px] [number] [title 1fr] [stamp]`; group label = `.ar-label` at `--ar-ink-5`; drag states `.ar-sortable-ghost { opacity: .4; background: var(--ar-accent-weak) !important; }` etc.
- [ ] **Step 4: document + test + format; reinstall; screenshot `.local/screens/07-contents.png` (3 outputs, mixed stamps, one active, drag grip visible on hover).**
- [ ] **Step 5: Commit** — `git commit -m "feat(contents): TLF table of contents -- leaders, stamps, drag reorder, rename/remove"`

---

### Task 8: arframe — Add-output overlay (+ the Recommended/Suggest section)

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Create: `R/mod_add_output.R`
- Modify: `inst/www/arframe.css` (append `04 overlay`), `R/app.R` (compose)   <!-- rv$adding is declared in Task 5's store; no fct_store change needed -->
- Test: `tests/testthat/test-mod_add_output.R`

**Interfaces:**
- Consumes: `arpillar::templates()`, `arpillar::catalog_grid()`, `arpillar::detect_structure()`, `add_output()`, `rv$adding`, `rv$bridge_dataset`.
- Produces: `mod_add_output_ui(id)` / `mod_add_output_server(id, store)`. A centred overlay (`.ar-overlay` fixed-free: an absolutely-positioned in-flow backdrop `rgba(20,28,40,0.26)` inside the workspace + a `--ar-card` dialog with the float shadow — the ONE shadowed surface). Contents: `ADD OUTPUT` micro-label + close ×; **Recommended for your data** section — for each catalog dataset, `detect_structure` maps `subject → demographics`, `bds → mean_line + box`, `occurrence → crosstab`, and a `CNSR` column (from `data_items`) → `km`; each recommendation renders "<template label> — from <DATASET>" with a one-click Add. Below: the full template list (kind glyph + label + description) and a dataset picker (empty by default; pre-seeded from `rv$bridge_dataset`, then cleared). Confirm = `add_output()` + `rv$adding <- FALSE`; the paper shows the new ghost shell immediately. Esc / backdrop click closes. Focus moves into the dialog on open and back to `+ Add output` on close (`ar-focus`).

- [ ] **Step 1: Failing tests** — testServer (demo catalog: ADSL subject, ADVS bds, ADTTE has CNSR): recommendations contain demographics-from-ADSL and km-from-ADTTE; clicking a recommendation adds the right template bound to the right dataset and closes; manual path: template `box` + dataset ADVS adds a box object; `rv$bridge_dataset <- "ADSL"` pre-selects then clears after add; overlay hidden while `rv$adding` FALSE.
- [ ] **Step 2: Verify failure.**
- [ ] **Step 3: Implement** — recommendations computed once per `rv$catalog_nonce` (memo under a `"rec::"`-prefixed key in `store$cache`); each rec/add button its own `observeEvent`.
- [ ] **Step 4: document + test + format; reinstall; screenshot `.local/screens/08-add-output.png` (overlay open, recommendations visible).**
- [ ] **Step 5: Commit** — `git commit -m "feat(add-output): template overlay with data-driven recommendations"`

---

### Task 9: arframe — the paper (render, ghost shell, regions, error summary)

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Create: `R/mod_paper.R`, `R/utils_ghost.R`
- Modify: `inst/www/arframe.css` (append `05 paper` + `06 ghost`), `inst/www/arframe.js` (region click delegation), `R/app.R` (compose into the desk slot), `DESCRIPTION` (Imports + `tabular`)
- Test: `tests/testthat/test-mod_paper.R`, `tests/testthat/test-utils_ghost.R`

**Interfaces:**
- Consumes: `selected_object()`, `cached_ard()`, `arpillar::render_spec()`, `arpillar::render_ggplot()`, `arpillar::output_status()`, `arpillar::validate_output()`, `arpillar::template()`, `htmltools::as.tags` (tabular S3 method — `tabular` in Imports registers it), `open_card()`, `rv$broken`.
- Produces: `mod_paper_ui(id)` / `mod_paper_server(id, store)`.
  - **The sheet** (`.ar-paper`): white, `--ar-paper-edge` border, centred; fit ⇄ page toggle (two mono icon buttons in a fieldset/legend "Preview width", `.ar-paper--fit` max-width 1100px / `.ar-paper--page` fixed 975px + 48px padding). Around the rendered content the module adds the Galley furniture: running head (report name left / `Page 1 of 1` right, hairline), and the **source line** (`Source: <dataset> · arframe <version> · <date>`, faint mono).
  - **Ready + table** → `output$sheet_html <- renderUI(htmltools::as.tags(arpillar::render_spec(cached_ard(store, obj), obj)))`, gated `bindEvent(store$rv$report, store$rv$selected)`. A scoped CSS override maps `.tabular-doc` typography onto the mono face.
  - **Ready + figure** → `renderPlot(arpillar::render_ggplot(store$con, obj), res = 96)` (km patchwork prints fine). Both containers stay mounted; class flip picks one.
  - **Not ready** → `ghost_shell(obj)`.
  - **Render error** → tryCatch adds id to `rv$broken` + `log_line`; renders the GOV.UK error summary at the paper top: `div(role = "alert", tabindex = "-1")` "There is a problem" + the engine message + one jump link per `validate_output` row, each firing `open_card(store, <control_id→region>)` (`roles-treatment→columns`, `roles-summarize→rows`, `roles-x/y/group→axes`, `roles-time/censor→axes`, `dataset→title`); focus moves to the summary (`ar-focus`). Success clears the id.
  - **Regions:** wrapper divs `data-ar-region="title|columns|rows|footnotes|source"` (figures: `axes|series|legend`) around the furniture + injected around the rendered table via the ghost/section builders; delegated JS posts `input$<ns>region`; server → `open_card(store, region)`. Hover = faint accent outline; selected = 1.5px accent outline (`.ar-region-active`).
- `ghost_shell(object)` (pure, session-free): the full page shell with, per unfilled slot (from `validate_output` + `template()` slot labels), a dashed ghost block IN PLACE — title block ghost when title empty, ghost header band "Treatment Group / <arm> <arm>" for `columns`, ghost stub rows for `rows`, ghost axes rectangle for figures — each `.ar-ghost-slot`: dashed `--ar-paper-edge` border + `+` glyph + mono hint (`assign treatment arms`), clickable (region attr). Empty report (no outputs at all): blank sheet + ghost title `Add your first output` + one CTA wired to `rv$adding`.
- JS append:

```js
$(document).on("click", "[data-ar-region]", function (e) {
  e.stopPropagation();
  var host = $(this).closest("[data-ar-paper]").attr("data-ar-paper");
  Shiny.setInputValue(host + "-region",
    this.getAttribute("data-ar-region"), { priority: "event" });
});
```

- [ ] **Step 1: Failing tests** — utils_ghost: one dashed hint per unfilled slot naming the slot label; filled slots ghost-free; figure ghost has the axes frame; empty-report shell has the CTA. mod_paper testServer (demo store): ready demographics → `output$sheet_html` contains `tabular-doc` + arm names + the source line; draft object → ghost markup; role-with-bogus-variable → id lands in `rv$broken`, alert markup lists jump links; `input$region <- "columns"` calls `open_card` (assert `rv$region == "columns"`, `rv$card`); options-only edit re-renders WITHOUT a new cache entry (two-stage seam assertion: `sum(startsWith(ls(store$cache), "ard::"))` unchanged).
- [ ] **Step 2: Verify failure.**
- [ ] **Step 3: Implement.** CSS `05 paper`: `.ar-paper { background: var(--ar-paper); border: 1px solid var(--ar-paper-edge); margin: 24px auto; padding: 32px 36px 20px; }`; running head + source line mono recipes; `.ar-region-active { outline: 1.5px solid var(--ar-accent); outline-offset: 1px; }`; `[data-ar-region]:hover { outline: 1px solid var(--ar-accent-weak); }`. `06 ghost`: `.ar-ghost-slot { border: 1.5px dashed var(--ar-paper-edge); border-radius: var(--ar-radius); color: var(--ar-ink-4); padding: 12px; display: flex; gap: 8px; align-items: center; cursor: pointer; font-family: "IBM Plex Mono", monospace; font-size: 11px; } .ar-ghost-slot:hover { border-color: var(--ar-accent); color: var(--ar-accent); background: var(--ar-accent-weak); }`; error summary `.ar-problem { border: 2px solid var(--ar-error); padding: 16px; margin-bottom: 16px; }`.
- [ ] **Step 4: document + test + format; reinstall BOTH; screenshots `.local/screens/09a-ghost.png`, `09b-table.png`, `09c-figure.png`, `09d-problem.png`.** Full `devtools::check()` both repos → 0/0.
- [ ] **Step 5: Commit** — `git commit -m "feat(paper): typeset sheet -- screen==paper render, on-page ghost shell, regions, problem summary"`

---

### Task 10: arframe — the galley card (summon, pin, route) + roles regions

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Create: `R/mod_card.R`, `R/mod_card_roles.R`
- Modify: `inst/www/arframe.css` (append `07 card`), `inst/www/arframe.js` (pin/Esc handlers), `R/app.R` (compose into the card slot)
- Test: `tests/testthat/test-mod_card.R`, `tests/testthat/test-mod_card_roles.R`

**Interfaces:**
- Consumes: `rv$card/pinned/region`, `selected_object()`, `arpillar::templates()` (`.template_for(object)` = first template with matching `$type`), `arpillar::data_items()`, `arpillar::validate_output()`, `update_object()`, `close_card()/toggle_pin()`.
- Produces:
  - `mod_card_ui(id)` / `mod_card_server(id, store)` — the card FRAME: `--ar-card` surface, paper-edge border, 320px; floating (absolute right of the desk, float shadow) vs `.ar-pinned` (flush right column, no shadow) — class flip via a custom message; header = region micro-label + pin toggle (`aria-pressed`) + close ×; Esc closes when unpinned (JS keydown → `close` input). The frame hosts region CONTENT modules; all mounted, class-flip visibility by `rv$region`.
  - `mod_card_roles_ui/server` — content for `columns` / `rows` / `axes` regions. Per slot (from `.template_for`): `fieldset` + `legend.ar-label` (slot label); assigned rows = `.type_chip(role_type)` + name + grip (multi-slots sortable, input `ns(paste0("reorder_", slot))` + extra `{"slot": ...}`) + visible remove × (`aria-label = "Remove <name> from <slot>"`); dashed `+ Add variable` row → inline rich picker: selectize over ELIGIBLE variables (`data_items()` filtered to `slot$accepts`, minus assigned), two-line options (chip + name + sql_type) via a shared `.picker_render()` (options packed `"NAME\x1fTYPE"` so search matches both), empty by default; selection commits via its own `observeEvent` (append `data_item(name, role_type = items$type[match])` to the alias-matched existing role — `treatment/group`, `summarize/row`, `group/treatment/strata` — else create `role(slot = <canonical>)`). A slot with a `validate_output` problem shows the inline message (11px, `--ar-error`, icon + the IDENTICAL validate string). Region routing: `columns` shows only the treatment slot; `rows` the summarize slot; `axes` the figure slots.

- [ ] **Step 1: Failing tests** — card frame: `open_card(store, "rows")` renders header `ROWS`, `input$close` clears `rv$card`; close is a NO-OP while pinned; `input$pin` toggles. Roles: draft demographics + `input$add_treatment <- "TRT01P"` puts the item on the object; picker for treatment offers NO measure (AGE absent); remove works; filling both slots flips the oracle to ready (and the paper test-double re-renders); reorder input reorders summarize items; a line object routes `axes` with x/y/group slots, y offering only measures.
- [ ] **Step 2: Verify failure.**
- [ ] **Step 3: Implement.** Region content renderUI keyed on `(rv$selected, roles digest)` — never on drag. CSS `07 card`: floating `position: absolute; right: 16px; top: 64px; box-shadow: var(--ar-shadow-float);` / pinned `position: static; flex: 0 0 320px; border-left: 1px solid var(--ar-rule); box-shadow: none; height: 100%;`.
- [ ] **Step 4: document + test + format; reinstall; screenshots `.local/screens/10a-card-roles.png` (floating, picker open), `10b-card-pinned.png`; re-shoot the assign flow: ghost → filled table live.**
- [ ] **Step 5: Commit** — `git commit -m "feat(card): summonable pinnable galley card + slot editor with eligible pickers"`

---

### Task 11: arframe — card regions: title, footnotes, stat + figure options

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Create: `R/mod_card_options.R`
- Modify: `inst/www/arframe.css` (append `08 options`), `R/app.R` (compose into the card frame)
- Test: `tests/testthat/test-mod_card_options.R`

**Interfaces:**
- Consumes: `arpillar::option_schema(object@type)`, `selected_object()`, `update_object()`, `arpillar::distinct_values()` (levels kind), `rv$region`.
- Produces: `mod_card_options_ui/server` — content for `title`, `footnotes`, and the option rows attached to `rows` (table stats) / `axes`/`series`/`legend` (figure options):
  - `title` region: Number-context line (read-only for v1), Title textInput, Population line textInput — S7 props (`set_props(o, title =)`); footnote-population is the first footnote by convention.
  - `footnotes` region: one textInput per footnote line + add/remove; commits `character` vector prop.
  - Option rows generated from `option_schema(type)` by kind — `int` → textInput `inputmode="numeric"` parsed `as.integer` (invalid → inline message, NOT committed; committed value ALWAYS scalar integer); `choice` → radios (default preselected); `flag` → checkbox; `text` → textInput (empty string REMOVES the key); `levels` → sortable list seeded `x_order %||% distinct_values(con, dataset, x_var)` (only when the x slot is filled); `numvec` → comma-separated textInput parsed + sorted. Option rows route to region: `decimals` under `rows`; `error_type/ci_level/mean_diamond/event_value/ci/risk_table/censor_marks/time_breaks` + `x_order/x_label/y_label` under `axes`; `palette` under `series`; `legend_position` under `legend`.
  - **Default-elision commit:** a value equal to the schema default REMOVES the key from `options` (keeps JSON + emitted code minimal, matches `emit_code` style).
  - Ranks placeholder: a quiet disabled row "Top-N and incidence cutoffs arrive with the AE hierarchy table" + `coming` tag under `rows` for crosstab.
- Title-region focus: when `open_card(store, "title")` fires, `ar-focus` the Title input.

- [ ] **Step 1: Failing tests** — demographics: `title` region edits commit props; `rows` shows exactly the decimals row; `"2"` commits `2L`; `"x"` shows inline error, no commit; back-to-default removes the key; **the two-stage assertion: an options-only commit leaves `sum(startsWith(ls(store$cache), "ard::"))` unchanged** (paper re-render is a cross-module effect — assert it in a Task-9 integration test, not here); km object: `axes` renders event_value/ci/risk_table/censor_marks/time_breaks + labels, `series` palette radios, `legend` position radios, engine defaults preselected; line + filled x: levels sortable seeded from distinct_values; numvec `"0, 6, 12"` commits `c(0, 6, 12)`; footnote add/remove round-trips.
- [ ] **Step 2: Verify failure.**
- [ ] **Step 3: Implement** — `.opt_control(ns, row, current)` generator; one `observeEvent` per key; option-value labels mono (`.ar-opt-row` grid `label 1fr / control auto`).
- [ ] **Step 4: document + test + format; reinstall; screenshot `.local/screens/11-card-options.png` (km axes region — the richest).**
- [ ] **Step 5: Commit** — `git commit -m "feat(card-options): schema-generated option rows with default-elision commits"`

---

### Task 12: arframe — card region: filters (presets + builder + live count)

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Create: `R/mod_card_filters.R`
- Modify: `inst/www/arframe.css` (append `09 filters`), `R/app.R` (compose), `R/mod_paper.R` (render the `Population: <preset/n filters>` tag under the title when `object@filters` non-empty, region `filters`)
- Test: `tests/testthat/test-mod_card_filters.R`

**Interfaces:**
- Consumes: `selected_object()`, `arpillar::data_items()`, `arpillar::distinct_values(include_missing = TRUE)`, `arpillar::filter_count()`, `update_object()`.
- Produces: `mod_card_filters_ui/server` — content for the `filters` region. Presets FIRST: "Safety population" (`SAFFL == "Y"`, shown only when SAFFL exists in `data_items()`), "Full set" (clears). Builder rows: column (rich picker) · op (select over the EXACT engine set `==, !=, %in%, >, <, >=, <=, is.na, not.na`) · value (multi selectize from `distinct_values(include_missing = TRUE)` for category/date — the NA token maps to `NA` in `value`; numeric text for measure comparisons; hidden for `is.na/not.na`) · include-missing checkbox · remove ×. Rows live in a store-side draft (`rv$filter_draft`, seeded from `object@filters` on selection change); a row commits ONLY when complete (`.complete(pred)` mirrors the engine's compilability rules) — incomplete rows show an honest `incomplete` badge (the engine is drop-tolerant and will NOT error). Live count `matched of total` beside the region label via `filter_count` on the COMPLETE predicates, debounced 300ms. Filters key the ARD cache, so a filter commit re-runs `build_ard` — correct.

- [ ] **Step 1: Failing tests** — preset visibility (demo ADSL has SAFFL) + one-click write; SEX `%in%` "F" commits + count matches `filter_count`; include-missing flag; `is.na` hides value + commits value-less; incomplete row NOT committed + badge shown; remove; Full set clears; the paper tag appears when filters exist and its click routes region `filters`.
- [ ] **Step 2: Verify failure.**
- [ ] **Step 3: Implement** (indexed row inputs `ns(paste0(field, "_", i))`, row-count-bounded observer registration).
- [ ] **Step 4: document + test + format; reinstall; screenshot `.local/screens/12-card-filters.png`.**
- [ ] **Step 5: Commit** — `git commit -m "feat(card-filters): preset-led filter region with live row counts"`

---

### Task 13: arframe — Data mode (catalog column, grid, profile card, bridge)

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Create: `R/mod_data_mode.R`
- Modify: `inst/www/arframe.css` (append `10 data`), `R/app.R` (fill the frame's `data_body` slot), `DESCRIPTION` (Imports + `datasetviewer`, `shinyFiles`; `Remotes: vthanik/arpillar, vthanik/datasetviewer`) — verify shinyFiles first (`Rscript -e 'packageVersion("shinyFiles")'`; install if absent)
- Test: `tests/testthat/test-mod_data_mode.R`

**Interfaces:**
- Consumes: `arpillar::catalog_grid()`, `register_dataset()`, `load_table()/unload_table()`, `dataset_path()`, `data_items()`, `value_counts(include_missing = TRUE)`, `column_precision()`, `detect_structure()`, `datasetviewer::datasetviewerOutput/renderDatasetViewer/dataset_viewer`, `shinyFiles`.
- Produces: `mod_data_mode_ui(id)` / `mod_data_mode_server(id, store)` — the `.ar-mode-data` body in the SAME Galley furniture:
  - **Contents column** lists datasets: name (mono, like a TLF number) + `rows × cols` sublabel + structure tag (mono, `subject / bds / occurrence / generic`) + a lazy/loaded stamp (`LOADED` accent-quiet / `LAZY` ink-4). Active = accent bar. Bottom: `+ Import` (`shinyFilesButton`, xpt/parquet/json) → `register_dataset` in tryCatch(`arpillar_error_input`) → `showNotification` + `log_line` + `rv$catalog_nonce` bump; duplicate error surfaced honestly.
  - **Desk**: the datasetviewer grid for `rv$dataset` — `renderDatasetViewer(datasetviewer::dataset_viewer(arpillar::dataset_path(store$con, rv$dataset)))` (path in; lazy DuckDB-WASM; NEVER `collect_data` for viewing).
  - **Galley card** (pinnable, same frame): variable profile — variable rich picker, then: type chip + `sql_type` · distinct count (`length(distinct_values(limit = 1000L))`, display `1000+` at cap) · missing count (NA bucket of `value_counts(include_missing = TRUE)`) · precision (measures) · top-10 values as mono rows with count + a 4px `--ar-accent-weak` proportion bar (count text ALWAYS beside the bar). Below: primary `Use in a new output →` → `rv$bridge_dataset <- rv$dataset`, `rv$mode <- "report"`, `rv$adding <- TRUE`.
  - Profile reads memoized per (dataset, variable) under a `"profile::"`-prefixed key in `store$cache`.

- [ ] **Step 1: Failing tests** — catalog rows for the 3 demo datasets with structure tags per `detect_structure`; SEX profile: distinct 2, missing 0, top values M/F; bridge sets all three store fields; duplicate-import notifies + logs without crashing; `output$grid` non-NULL (WASM behavior eyeballed in browser only).
- [ ] **Step 2: Verify failure.**
- [ ] **Step 3: Implement.**
- [ ] **Step 4: document + test + format; reinstall; screenshots `.local/screens/13a-data.png` (catalog + grid), `13b-profile.png`.**
- [ ] **Step 5: Commit** — `git commit -m "feat(data-mode): dataset contents, DuckDB-WASM grid, profile card, use-in-output bridge"`

---

### Task 14: arframe — project persistence + Export package

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Create: `R/mod_project.R`
- Modify: `R/mod_frame.R` (wire the Export split-menu + Save/Open into the bar), `inst/www/arframe.css` (append `11 menus`), `R/app.R` (compose), `DESCRIPTION` (Imports + `zip`)
- Test: `tests/testthat/test-mod_project.R`

**Interfaces:**
- Consumes: `arpillar::report_to_json/report_from_json`, `arpillar::render_rtf/render_figure_rtf/emit_code/emit_report_code`, `cached_ard()`, `output_status()`, `shinyFiles` (save/open dialogs), `store` (path/dirty/saved_at).
- Produces: `mod_project_server(id, store)` + bar affordances:
  - **Save / Save as / Open** (bar kebab next to the title): `report_to_json(report, path)` / `shinyFilesSaveButton` picks path / Open reads + `commit()` + resets undo; **autosave** = `observe` on `rv$dirty` debounced 2s writing to `rv$path` when set; `rv$saved_at <- format(Sys.time(), "%H:%M")` feeds the status bar (`saved 14:02` / `unsaved changes`).
  - **Export package** (the solid bar button, a small menu): "Current output (RTF)" — downloadHandler: tables `render_rtf(cached_ard(store, obj), obj, file)`, figures `render_figure_rtf(store$con, obj, file)`; "All outputs (zip)" — renders every READY output to a tempdir (`<number>-<id>.rtf`) + `emit_report_code` script + the project JSON, bundled with `zip::zipr` (declared in DESCRIPTION; never `utils::zip`, which shells out to a non-portable system binary); skipped non-ready outputs are LISTED in the completion notification (fail loud, no silent truncation); "Reproducible code (R script)" — `emit_report_code(con, report, file)`. Completion toasts via `showNotification`; every export `log_line`d.
- Export runs synchronously here; Task 16 swaps the all-outputs path onto ExtendedTask.

- [ ] **Step 1: Failing tests** — save→open round-trip re-renders identically (JSON `identical`); autosave writes after dirty (advance via `session$elapse` in testServer or direct observer flush); current-output download produces a non-empty RTF byte-identical to a direct `render_rtf` reference; package zip contains N ready RTFs + `report.R` + `project.json`, and the notification names the skipped drafts; the emitted `report.R` `parse()`s.
- [ ] **Step 2: Verify failure.**
- [ ] **Step 3: Implement.**
- [ ] **Step 4: document + test + format; reinstall; screenshot `.local/screens/14-export.png` (menu open).** Full `devtools::check()` → 0/0.
- [ ] **Step 5: Commit** — `git commit -m "feat(project): JSON save/open/autosave + Export package (RTF, zip bundle, R script)"`

---

### Task 15: arframe — the QC sheet (proof-check + run log)

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Create: `R/mod_qc.R`
- Modify: `inst/www/arframe.css` (append `12 qc`), `R/app.R` (fill the frame's `qc_body` slot)
- Test: `tests/testthat/test-mod_qc.R`

**Interfaces:**
- Consumes: `.all_objects()`, `arpillar::output_status()/validate_output()`, `rv$broken`, `rv$log`, `open_card()`.
- Produces: `mod_qc_ui/server` — the desk swaps to a paper-styled **proof-check sheet**: running head `Quality control — <report name>`; one row per output (mono number + title + stamp + per-problem jump links: click → `rv$mode <- "report"`, select the output, `open_card` at the mapped region); a summary line (`2 of 4 outputs ready`); below a rule, the RUN LOG (mono 11px, newest first). The whole sheet is print-shaped — QC is a document too.

- [ ] **Step 1: Failing tests** — 3 outputs mixed states: rows + stamps correct; problem link navigates (mode/selected/region all set); log renders newest first; summary counts match the oracle.
- [ ] **Step 2: Verify failure → implement → document + test + format; reinstall; screenshot `.local/screens/15-qc.png`.**
- [ ] **Step 3: Commit** — `git commit -m "feat(qc): proof-check sheet with jump links and run log"`

---

### Task 16: arframe — async export (mirai + ExtendedTask)

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Create: `R/fct_async.R`
- Modify: `R/mod_project.R` (all-outputs path onto the task), `R/app.R` (`arframe(daemons = 2)` arg; `daemons()` at startup, teardown `onStop`), `DESCRIPTION` (Imports + `mirai`)
- Test: `tests/testthat/test-fct_async.R`

**Interfaces:**
- **Precondition:** `install.packages("mirai")` (verified NOT installed). Then source-ground the exact `daemons()` signature from the INSTALLED package (`?mirai::daemons` — confirm `.compute` exists before using it; if the installed API differs, follow the installed docs — do not code from this plan's memory).
- Produces: `export_task()` — an `ExtendedTask$new(function(report_json, paths, out_dir) mirai::mirai({ ... }, report_json = report_json, paths = paths, out_dir = out_dir))` whose expression: `library(arpillar)`; `con <- engine_open()`; `register_dataset` each of `paths` (named `dataset_path()` strings — a DuckDB connection NEVER crosses the daemon boundary); `report_from_json(report_json)`; render every ready output; return the file list. **Busy state, driven manually** (NOT `bind_task_button`, which relays only to a `bslib::input_task_button` — the bar/menu buttons are custom `.action_btn`/`.ar-btn-ink` elements, so the relay would silently no-op): watch `task$status()` — `"running"` shows the inline mono `rendering n outputs...` line + a Cancel affordance and disables the menu item via the `ar-disable` message; `"success"` resolves the zip download + toast; `"error"` surfaces the condition message + `log_line`. (Alternative if a task button is wanted elsewhere: restyle a real `bslib::input_task_button` with the `ar-` classes.) Daemon pool: `mirai::daemons(daemons, .compute = "arframe")` inside `arframe()` (NEVER at load), `onStop(function() mirai::daemons(0, .compute = "arframe"))` — adjust to the installed API if `.compute` is absent.
- Test with `daemons(1, dispatcher = FALSE, .compute = "arframe")` + teardown (CRAN 2-core ceiling); `skip_on_cran()`.

- [ ] **Step 1: Install mirai + verify signatures from installed help; write failing test** — the task function (called directly, off-Shiny) renders a 2-output demo report in a fresh daemon and returns 2 existing RTFs byte-identical to in-session references.
- [ ] **Step 2: Verify failure → implement → document + test + format.**
- [ ] **Step 3: Commit** — `git commit -m "feat(async): mirai-backed export task -- paths cross the daemon, never connections"`

---

### Task 17: arframe — accessibility, responsive, final polish sweep

**Repo:** `/Users/vignesh/projects/r/arframe`

**Files:**
- Modify: `inst/www/arframe.css` (append `13 responsive`), `inst/www/arframe.js` (keyboard map), any module needing fixes
- Test: `tests/testthat/test-a11y.R`, shinytest2 viewport apps

**Steps:**
- [ ] **Keyboard:** ↑/↓ move the TOC selection; Enter opens the card on the first incomplete region; Esc closes the unpinned card. JS keydown map posting namespaced inputs; testServer assertions.
- [ ] **A11y audit test (`test-a11y.R`):** every `fieldset` in card regions has a `legend`; every stamp has `aria-label`; the pin has `aria-pressed`; icon-only buttons have `aria-label`; the problem summary has `role="alert"` — assert on rendered module HTML strings.
- [ ] **Responsive:** below 1100px the contents column collapses to a slim strip (numbers + stamps, flyout on hover/focus) and the galley card becomes a bottom sheet; shinytest2 viewport captures at 1000×700 / 1440×900 / 1920×1080 → `.local/screens/17-vp-*.png`; assert no horizontal scroll (`document.body.scrollWidth <= innerWidth` via `get_js`).
- [ ] **Final eyeball set:** the six money shots re-taken on the demo report (frame, contents, ghost, table, figure, card) into `.local/screens/final/`.
- [ ] **Full gates:** `devtools::check()` 0/0 both repos; arpillar full suite + goldens untouched; `air format`.
- [ ] **Commit** — `git commit -m "feat(polish): keyboard map, a11y audit, responsive collapse, viewport sweep"`

---

## Self-review (run after writing, before executing)

1. **Spec coverage** (Galley spec §1–§8 + product spec §5.1–§5.5 + §9–§10): tokens/fonts→T4; store/no-DOM-drafts→T5; frame/modes→T6; TOC+stamps+reorder→T7; Add-output+Suggest→T8; paper/ghost/regions/error-summary/screen==paper→T9; card+roles→T10; options/title/footnotes→T11; filters+presets+live-count→T12 (Ranks stub→T11); Data mode+profile+bridge→T13; JSON+code persistence+export→T14; QC/Review→T15; async §5.3→T16; a11y §5.4 + responsive §5.5→T17. Deferred per spec: ⌘K palette (v1.1), Actions/Rules, dark skin.
2. **Oracle single-source:** every status display (stamps T7, ghost T9, QC T15) reads `output_status`/`validate_output` — no second predicate anywhere.
3. **Two-stage seam assertions appear twice** (T5 cache test, T11 options-edit test) — both must key the cache on roles+filters and exclude options.
4. **Type consistency:** store field names (`report/selected/region/card/pinned/mode/dataset/bridge_dataset/adding/filter_draft/path/dirty/saved_at/broken/log/catalog_nonce` — all 16) and mutator signatures are quoted identically in T5–T15; region tokens (`title/columns/rows/footnotes/source/axes/series/legend/filters`) identical in T9/T10/T11/T12/T15 and the Galley spec §4.
5. **Every engine call in the plan exists in arpillar today** except the three T1–T3 exports this plan itself adds (`templates/template/option_schema`, `output_status/validate_output`, `render_spec`) — no other new engine surface is assumed.

