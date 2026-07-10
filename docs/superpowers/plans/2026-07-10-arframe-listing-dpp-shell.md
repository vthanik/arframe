# Listing rework — DPP shell GS_CSR_AE_L_002 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the listing generator reproduce the GS_CSR_AE_L_002 DPP shell exactly (stacked cells, level recodes, group-skip subject blocks), with picker-based editors and no duplicate/ARD-only controls.

**Architecture:** Engine work lands in arpillar's listing leg (`fct_render_listing.R`): item `@levels` recodes replace the `legends` option, and the id column gets tabular's `usage="group" / group_display="column" / group_skip` spec (followed through stacks). UI work lands in arframe: TRANSPOSE and STACKED COLUMNS pickers restricted to the selected list variables, VALUE LEGENDS deleted, ARD-only COLUMNS knobs hidden for listings.

**Tech Stack:** R, S7, DuckDB, tabular, Shiny/bslib, testthat.

## Global Constraints

- `arframe()` is the only arframe export; everything else `@noRd`.
- No tidyverse in Imports; base R + `cli`/`rlang`/`S7`; `::`-qualify; native pipe.
- ASCII-only inside `cli_abort()` message strings; em-dash as `—` escape in UI string literals.
- Golden gate: all **pre-listing** RTF goldens stay byte-identical. The listing golden (`_snaps/golden-listing/listing.rtf`, created 2026-07-10, uncommitted) is REWRITTEN by this plan — that is intended, not a violation.
- Dev loop per repo after each task: `Rscript -e 'devtools::document()'`, `Rscript -e 'devtools::test()'`; full `devtools::check(args = "--no-manual")` + `air format R/ tests/` at the repo gate tasks (4 and 9). arpillar must be re-INSTALLED before arframe work (`R CMD INSTALL --no-docs .` or `devtools::install(quick = TRUE)`).
- **No git commits in this plan.** Both repos carry the whole uncommitted listing epic from the previous session; interleaved commits would snapshot mixed state. The user gates commits/pushes (never push without approval). Flag at the end.
- Real-data verification only: `/Users/vignesh/projects/data/cdisc-adam-pilot` (ADAE). testthat fixtures stay minimal/bundled.
- Reference target: image cache `86875ae7-6c1e-403d-8dda-eacdebb18e4c/1.png` (shell) and `2.png` (DPP metadata). Grey-highlighted shell parts are optional; everything else must match.

## File structure

| File | Responsibility |
|---|---|
| `~/projects/r/arpillar/R/fct_render_listing.R` | engine: `.apply_item_levels()` replaces legend recode; `.stack_output_name()`; group-skip col_spec on the id column; delete `.legend_*` / `.filled_pairs` / `.apply_legend_recodes` |
| `~/projects/r/arpillar/R/fct_generators.R` | drop the `legends` row from `.OPTION_SCHEMA$listing` |
| `~/projects/r/arpillar/tests/testthat/test-render_listing.R` | levels-recode + group-skip unit tests; delete legend tests |
| `~/projects/r/arpillar/tests/testthat/test-generators.R` | pinned schema row list minus `legends` |
| `~/projects/r/arpillar/tests/testthat/test-golden-listing.R` | golden spec rewritten onto `@levels` recodes + group-skip; snapshot regenerated |
| `~/projects/r/arframe/R/mod_card_listing.R` | transpose choices from list vars; stacks picker rework; delete VALUE LEGENDS section + observers |
| `~/projects/r/arframe/R/mod_card_options.R` | hide Stub column header + Total column for listings |
| `~/projects/r/arframe/tests/testthat/test-mod_card_listing.R` | wire tests updated/added |

---

### Task 1: arpillar — item `@levels` recodes on the listing leg

The Roles-pane LEVELS editor writes `data_item@levels` (value / display / include / expected / the `__ARF_MISSING__` sentinel). The listing leg must consume it, or removing VALUE LEGENDS leaves the recode written-but-ignored.

Semantics for a listing (row data, no stat rows):
- `display` recode: cell values equal to a level `value` render as its `display`. Runs FIRST in `.listing_display()` (before stacks), so stacked cells glue the short codes — same position legends had.
- `include = FALSE`: rows with that value are DROPPED (the "keep" checkbox filters the listing).
- `expected = TRUE` and the Missing sentinel are inert (no zero rows / stat Missing row in a listing).

**Files:**
- Modify: `~/projects/r/arpillar/R/fct_render_listing.R` (`.listing_display()` + new helper)
- Test: `~/projects/r/arpillar/tests/testthat/test-render_listing.R`

**Interfaces:**
- Produces: `.apply_item_levels(display, object)` — data.frame in, data.frame out. Task 2 slots it where `.apply_legend_recodes()` sits today.
- Consumes: `.listing_id_item()`, `.listing_items()`, `.MISSING_LEVEL_VALUE` (fct_render_table.R), `%||%`.

- [x] **Step 1: Write the failing tests** — append to `test-render_listing.R` (reuse that file's existing object-builder helpers; mirror how the legend tests build a listing object with `id = USUBJID`, `columns = SEX/RACE` and set `@levels` on the SEX item):

```r
test_that("item @levels display recodes apply to listing cells", {
  # SEX item carries levels metadata: M/F displays
  # build listing object as in the file's other tests, but with
  # columns item SEX given:
  #   levels = list(list(value = "M", display = "Male"),
  #                 list(value = "F", display = "Female"))
  d <- build_ard(con, obj)
  disp <- render_display(d, obj)
  expect_true(all(disp$SEX %in% c("Male", "Female")))
})

test_that("include = FALSE levels drop the listing rows", {
  # SEX item levels: list(list(value = "M", include = FALSE))
  disp <- render_display(build_ard(con, obj), obj)
  expect_false(any(disp$SEX == "M"))
})

test_that("Missing sentinel and expected levels are inert on the listing leg", {
  # levels = list(list(value = arpillar:::.MISSING_LEVEL_VALUE, include = TRUE),
  #               list(value = "GHOST", expected = TRUE))
  disp <- render_display(build_ard(con, obj), obj)
  expect_false("GHOST" %in% disp$SEX)   # no zero rows
  expect_equal(nrow(disp), nrow_before) # sentinel changed nothing
})

test_that("levels recode runs before stacks (stacked cell glues recoded codes)", {
  # stack SEX+RACE with recodes M->"M." ; expect glued cell to carry "M."
})
```

- [x] **Step 2: Run to verify they fail** — `Rscript -e 'testthat::test_file("tests/testthat/test-render_listing.R")'` in arpillar. Expected: new tests FAIL (recodes ignored).

- [x] **Step 3: Implement** — in `fct_render_listing.R`, add after `.listing_labels()`:

```r
#' Apply each role item's `@levels` metadata to the listing frame: a
#' `display` recode maps the cell value to its paper label (run BEFORE
#' stacks, so a glued cell carries the short codes -- the position the
#' removed value-legend recode held), and `include = FALSE` drops that
#' value's ROWS (the LEVELS keep-checkbox as a listing filter). The
#' synthetic Missing sentinel and `expected` zero-levels are stat-table
#' concepts and are inert here.
#' @noRd
.apply_item_levels <- function(display, object) {
  items <- c(
    if (!is.null(it <- .listing_id_item(object))) list(it) else list(),
    .listing_items(object)
  )
  for (it in items) {
    meta <- Filter(
      function(m) {
        is.list(m) && !identical(as.character(m$value), .MISSING_LEVEL_VALUE)
      },
      it@levels %||% list()
    )
    if (!length(meta) || !(it@name %in% names(display))) {
      next
    }
    x <- as.character(display[[it@name]])
    drop_vals <- vapply(
      Filter(function(m) isFALSE(m$include), meta),
      function(m) as.character(m$value),
      character(1)
    )
    if (length(drop_vals)) {
      keep <- !(x %in% drop_vals)
      display <- display[keep, , drop = FALSE]
      x <- x[keep]
    }
    for (m in meta) {
      dsp <- m$display
      if (!is.null(dsp) && length(dsp) == 1L && !is.na(dsp) && nzchar(dsp)) {
        x[x == as.character(m$value)] <- as.character(dsp)
      }
    }
    display[[it@name]] <- x
  }
  display
}
```

and in `.listing_display()` add the call directly BEFORE the legend line (legends removed in Task 2):

```r
  disp <- .apply_item_levels(disp, object)
```

Note: recoding coerces the column to character — acceptable; only columns that carry levels metadata (categories) are touched, numeric columns keep their type.

- [x] **Step 4: Run tests to verify they pass**, plus the whole file — no regressions.

### Task 2: arpillar — remove value legends

**Files:**
- Modify: `~/projects/r/arpillar/R/fct_render_listing.R`, `~/projects/r/arpillar/R/fct_generators.R`
- Test: `test-render_listing.R`, `test-generators.R`

**Interfaces:**
- Consumes: Task 1's `.apply_item_levels()` already in place.
- Produces: `.OPTION_SCHEMA$listing` without a `legends` row; `.listing_display()` / `.render_listing_spec()` free of legend calls. A stale `options$legends` in an old spec is inert (options are not schema-validated on load) — verified by test.

- [x] **Step 1: Delete engine code** — remove from `fct_render_listing.R`: `.filled_pairs()`, `.legend_formatter()`, `.legend_footnotes()`, `.apply_legend_recodes()`; the `.apply_legend_recodes` call in `.listing_display()`; the `legends <- object@options$legends` + `.legend_footnotes(legends)` lines in `.render_listing_spec()` (footnotes become just `.rtf_footnotes(object)`). Update the file's header comment (legend recode → item levels recode).

- [x] **Step 2: Drop the schema row** — in `fct_generators.R` `.OPTION_SCHEMA$listing`, delete the `.OPT("legends", ...)` entry and its comment.

- [x] **Step 3: Update tests** — in `test-render_listing.R` delete/replace every legend test (grep `legends`); keep one regression:

```r
test_that("a stale options$legends key is inert", {
  # obj with options$legends set as the old editor wrote it
  expect_silent(disp <- render_display(build_ard(con, obj), obj))
  expect_true(any(disp$SEX == "MALE"))  # no recode, no error, no footnote
})
```

In `test-generators.R` update the pinned `option_schema("listing")` expectation (line ~183) to drop `"legends"`.

- [x] **Step 4: Run arpillar tests** — expect green except `test-golden-listing.R` (rewritten in Task 4).

### Task 3: arpillar — group-skip via the id column

tabular already provides the shell's pattern: `col_spec(usage = "group", group_display = "column", group_skip = TRUE)` = value prints once per consecutive group, blank row between groups. Emit it for the listing's id column, following the id through stacks (a stack consumes its vars and renames the surviving column).

**Files:**
- Modify: `~/projects/r/arpillar/R/fct_render_listing.R` (`.render_listing_spec()` + helper)
- Test: `test-render_listing.R`

**Interfaces:**
- Produces: `.stack_output_name(stacks, col)` → the display-column name that carries `col` after `.apply_stacks()` (the stack's `name`, else the first-entry-first-var name, else `col` untouched).
- Consumes: `options$group_skip` (layout schema flag, default `TRUE` — the "Blank row between blocks" toggle now governs the listing blank row).

- [x] **Step 1: Write the failing tests:**

```r
test_that("the id column gets group usage with suppression and skip", {
  # obj: id = USUBJID, columns = AEDECOD/ASTDT; no stacks
  spec <- render_spec(render_display(build_ard(con, obj), obj), obj)
  cs <- spec@cols[["USUBJID"]]      # adapt accessor to tabular's spec shape
  expect_identical(cs@usage, "group")
  expect_identical(cs@group_display, "column")
  expect_true(isTRUE(cs@group_skip))
})

test_that("group-skip follows the id through a stack", {
  # stack name "Unique Subject ID\n(Age/Sex/Race)" consuming USUBJID+AGE/SEX/RACE
  # expect the STACK column to carry usage = "group"
})

test_that("options$group_skip = FALSE turns the blank row off but keeps suppression", {
  # expect group_skip FALSE, usage still "group", group_display "column"
})

test_that("no id role means no group column", {})
```

(Adapt the col_spec accessor to how existing spec tests in the file read per-column specs — copy their pattern.)

- [x] **Step 2: Run to verify they fail.**

- [x] **Step 3: Implement** — add helper:

```r
#' The display-column name that carries `col` after `.apply_stacks()`:
#' the consuming stack's `name` (or its first-entry-first-var when the
#' stack is unnamed), else `col` itself when no stack consumes it.
#' @noRd
.stack_output_name <- function(stacks, col) {
  for (st in stacks %||% list()) {
    entries <- st$entries %||%
      list(list(vars = st$vars))
    vars <- as.character(unlist(lapply(entries, function(e) e$vars)))
    if (col %in% vars) {
      return(st$name %||% vars[[1L]])
    }
  }
  col
}
```

and in `.render_listing_spec()`, after the base `cols()` call and BEFORE `.apply_column_specs()` (so user column_specs can still override):

```r
  id <- .listing_id_item(object)
  if (!is.null(id)) {
    id_col <- .stack_output_name(object@options$stacks, id@name)
    if (id_col %in% body_cols) {
      gs <- !identical(object@options$group_skip, FALSE)
      spec <- tabular::cols(
        spec,
        "{id_col}" := tabular::col_spec(
          usage = "group",
          group_display = "column",
          group_skip = gs
        )
      )
    }
  }
```

(Use the file's existing dynamic-name idiom for `cols()` — if it builds named lists via `do.call(tabular::cols, ...)` elsewhere, do the same: `do.call(tabular::cols, c(list(spec), stats::setNames(list(...), id_col)))`.)

- [x] **Step 4: Run tests to verify they pass.**

### Task 4: arpillar — golden rewrite + repo gate + install

**Files:**
- Modify: `~/projects/r/arpillar/tests/testthat/test-golden-listing.R`
- Regenerate: `tests/testthat/_snaps/golden-listing/listing.rtf`

- [x] **Step 1: Rewrite the golden spec** — replace the `legends = list(...)` block (RACE/SEX code recodes) with `@levels` metadata on the RACE and SEX role items:

```r
# RACE item:
levels = list(
  list(value = "ASIAN", display = "A"),
  list(value = "BLACK OR AFRICAN AMERICAN", display = "B"),
  list(value = "WHITE", display = "C")
)
# SEX item:
levels = list(
  list(value = "MALE", display = "M"),
  list(value = "FEMALE", display = "F")
)
```

and move the decode lines into user footnotes (the DPP style, footnotes 1–2 of image 2):

```r
footnotes = c(
  "A=ASIAN,B=BLACK OR AFRICAN AMERICAN,C=WHITE",
  "M=MALE, F=FEMALE"
)
```

Keep the stack. The golden now ALSO pins group-skip (id `USUBJID` → suppressed repeats + blank rows) — the DPP accrual shape end to end.

- [x] **Step 2: Delete the stale snapshot and regenerate** — `rm tests/testthat/_snaps/golden-listing/listing.rtf`, run the golden test twice (double-render byte-identity assert stays). Eyeball the regenerated RTF: stacked id cell prints once per subject, blank row between subject blocks, `A`/`M` codes in cells, decode footnotes present.

- [x] **Step 3: Full gate** — `devtools::document()`, `devtools::test()` (ALL green; pre-listing goldens byte-identical — verify with `git status tests/testthat/_snaps/` showing only `golden-listing/` changed), `devtools::check(args = "--no-manual")` 0 err / 0 warn, `air format R/ tests/`.

- [x] **Step 4: Reinstall arpillar** — `Rscript -e 'devtools::install(quick = TRUE, upgrade = "never")'` so arframe sees the new engine.

### Task 5: arframe — TRANSPOSE offers only the selected list variables

**Files:**
- Modify: `~/projects/r/arframe/R/mod_card_listing.R` (`.listing_transpose_section()`)
- Test: `~/projects/r/arframe/tests/testthat/test-mod_card_listing.R`

**Interfaces:**
- Produces: `.listing_selected_items(object, items)` — the dataset items-meta frame filtered to the id + list-variable names, role order preserved. Task 6 reuses it.

- [x] **Step 1: Write the failing wire test** (follow the file's existing section-render test pattern):

```r
test_that("transpose selects offer only the selected list variables", {
  # obj: columns role = PARAM, AVAL, AVISIT ; dataset has many more cols
  html <- as.character(.listing_transpose_section(ns, obj, items))
  expect_true(grepl("PARAM", html))
  expect_false(grepl("USUBJID", html))          # unselected col absent
  # Value select: numeric list vars only
  # AVAL present in tr_value options; AVISIT absent
})
```

- [x] **Step 2: Run to verify it fails.**

- [x] **Step 3: Implement** — add the helper near `.listing_opt_list()`:

```r
#' The items-meta rows for the listing's SELECTED variables (id first,
#' then the list variables, role order) -- the choice set every listing
#' editor offers. A committed value no longer in the selection is
#' appended so a stale select still shows it.
#' @noRd
.listing_selected_items <- function(object, items, extra = character(0)) {
  sel <- c(
    vapply(
      .role_items_for(object, "id"),
      function(it) it@name,
      character(1)
    ),
    vapply(
      .role_items_for(object, "columns"),
      function(it) it@name,
      character(1)
    )
  )
  sel <- unique(c(sel, extra))
  items[match(intersect(sel, items$name), items$name), , drop = FALSE]
}
```

(If the module has no `.role_items_for(object, slot)` helper, use the same role lookup `mod_card_roles.R` uses — grep `.find_role` / `.role_for_slot` and reuse; do not invent a new lookup.)

In `.listing_transpose_section()`:

```r
  sel <- .listing_selected_items(
    object,
    items,
    extra = as.character(c(tr$param %||% character(0), tr$value %||% character(0)))
  )
  # Parameter: any selected list variable
  # Value: numeric selected list variables only
  numeric_cols <- sel$name[sel$type %in% "measure"]
  # choices become c("(none)" = "", sel$name) / c("(none)" = "", numeric_cols)
```

Keep the hint line; append: `"Choices are the selected list variables."`

- [x] **Step 4: Run the test file — green.**

### Task 6: arframe — STACKED COLUMNS picker rework

The current editor's failure mode (image 9): a new stack renders only its free-text NAME field, so users type `AGE/SEX/RAE` into it. Fix: stacks are built from pickers over the selected variables; the name field is explicitly the header label.

**Files:**
- Modify: `~/projects/r/arframe/R/mod_card_listing.R` (`.stack_block()`, `.stack_entry_row()`, `stk_add`/`stk_line_add` observers, `.listing_stacks_section()`)
- Test: `test-mod_card_listing.R`

**Interfaces:**
- Consumes: `.listing_selected_items()` from Task 5.

- [x] **Step 1: Write the failing wire tests:**

```r
test_that("a new stack starts with one empty entry line", {
  # drive the stk_add observer via testServer; expect
  # options$stacks[[1]]$entries == list(list(vars = character(0)))
})

test_that("a second stack line prefills the paren wrap", {
  # drive stk_line_add on a one-line stack; expect
  # entries[[2]]$prefix == "(" and suffix == ")"
})

test_that("stack pickers offer only the selected variables", {
  # id USUBJID + columns AGE/SEX; render .listing_stacks_section
  # expect AGE in picker choices, AEDECOD (unselected) absent
})
```

- [x] **Step 2: Run to verify they fail.**

- [x] **Step 3: Implement:**

1. `.listing_stacks_section()` / `.stack_block()` / `.stack_entry_row()`: pass `sel <- .listing_selected_items(object, items)` down and use it for every `.listing_add_picker()` in the stacks section (sort keys keep the full `items` — ORDER BY needs no display presence).
2. `stk_add` observer: seed the first line —

```r
    st[[length(st) + 1L]] <- list(
      name = NULL,
      indent = FALSE,
      entries = list(list(vars = character(0)))
    )
```

3. `stk_line_add` observer: prefill the DPP continuation wrap —

```r
    entries[[length(entries) + 1L]] <- if (length(entries) >= 1L) {
      list(vars = character(0), prefix = "(", suffix = ")")
    } else {
      list(vars = character(0))
    }
```

4. `.stack_block()` name field: placeholder `"Header label (\\n = line break)"` and move it visually AFTER the entry lines is NOT needed — keep position, the placeholder + seeded line 1 removes the ambiguity.
5. Section hint: `"Pick from the selected variables; line 2+ wraps in parentheses by default."`

- [x] **Step 4: Run the test file — green.** Also re-run any existing stack wire tests and adjust expectations for the seeded first line.

### Task 7: arframe — remove VALUE LEGENDS

**Files:**
- Modify: `~/projects/r/arframe/R/mod_card_listing.R`
- Test: `test-mod_card_listing.R`

- [x] **Step 1: Delete** `.listing_legends_section()`, `.legend_block()`, `.legend_pair_row()`, the `lgd_*` observers (`lgd_add`, `lgd_rm`, `lgd_col`, `lgd_field`, `lgd_pair_add`, `lgd_pair_rm`, `lgd_pair`), the section's entry in `.opt_listing_sections()`, and the `# ---- legends ----` comment blocks. Remove the now-unused `arpillar::distinct_values()` prefill call if nothing else uses it in this file. Update the file header comment (three structured kinds, not four).

- [x] **Step 2: Delete the legend wire tests** in `test-mod_card_listing.R` (grep `lgd_|legend`).

- [x] **Step 3: Run the test file + `devtools::test()`** — green; grep the repo for `legends` to confirm no arframe reference survives (ghost maps, `.ard_key`, css classes ok to leave if shared).

### Task 8: arframe — hide ARD-only COLUMNS knobs for listings

Stub column header (`stub_label`) and Total column (`total`) are stat-table concepts: a listing has no stub column and no pooled arm. "Blank row between blocks" (`group_skip`) STAYS — Task 3 wired it to the listing's subject-block blank row.

**Files:**
- Modify: `~/projects/r/arframe/R/mod_card_options.R` (`.opt_layout_sections()` COLUMNS section)
- Test: wherever the layout-section wire tests live (grep `Stub column header` in `tests/testthat/`)

- [x] **Step 1: Failing test:**

```r
test_that("listing COLUMNS section hides stub header and total", {
  html <- as.character(... .opt_layout_sections(con, ns, listing_obj) ...)
  expect_false(grepl("Stub column header", html))
  expect_false(grepl("Total column", html))
  expect_true(grepl("Blank row between blocks", html))
})
```

- [x] **Step 2: Implement** — in the COLUMNS `.opt_section()` list, gate the two rows and the total hint:

```r
  is_listing <- identical(object@type, "listing")
  # stub row / total row / "Pooled across arms" hint each wrapped:
  if (!is_listing) shiny::tags$div(... stub ...) else NULL,
  ...
  if (!is_listing) shiny::tags$div(... total ...) else NULL,
  if (!is_listing) shiny::tags$p(... pooled hint ...) else NULL
```

Also guard the `opt_total` / `opt_stub_label` observers if they are listing-reachable (they ride `.LAYOUT_GENERIC_KEYS` — the inputs simply won't exist for a listing, which Shiny tolerates; no observer change needed).

- [x] **Step 3: Run tests — green.**

### Task 9: arframe — repo gate

- [x] `Rscript -e 'devtools::document()'` (no Rd changes expected — all `@noRd`).
- [x] `Rscript -e 'devtools::test()'` — ALL green (baseline 1362+53 adjusts down by the deleted legend tests, up by the new ones).
- [x] `Rscript -e 'devtools::check(args = "--no-manual")'` — 0 err / 0 warn.
- [x] `air format R/ tests/` (hook enforces; run anyway).

### Task 10: Live verify against image 1 (the acceptance gate)

Build GS_CSR_AE_L_002 in the dev app on real ADAE and eyeball against `1.png`. Grey shell parts optional; everything else must match.

- [x] **Step 1: Launch** — `Rscript <scratchpad>/launch_arframe.R` (recreate from handoff if gone: dev app on :4321 mounting `~/projects/data/cdisc-adam-pilot`).
- [x] **Step 2: Compose the listing** (Add output → listing):
  - id: `USUBJID`; list variables: `AGE SEX RACE` + phase/visit (`APERIOD`/`EPOCH`, `AVISIT` — whichever ADAE carries) + `ASTDT AENDT ASTDY` + duration/type (`ADURN`, `AESER`) + `AESOC AEDECOD` + `AEREL AEACN` + `AESEV`/CTC.
  - Stacks: `Unique Subject ID\n(Age/Sex/Race)` = line 1 `USUBJID`, line 2 `AGE/SEX/RACE` (auto-parens); `Phase\nVisit`; `Onset\nResolution\nStudy Day` (indent); `DUR\nTRD\nType`; `System Organ Class\nPreferred Term` (indent); `REL\nACT`; `CAT\nTRT`.
  - LEVELS recodes on `RACE` (A/B/C/…), `SEX` (M/F), `AESER` (SAE/AE) via the peek editor — verify the RENDER changes (memory rule: written-but-ignored is the trap).
  - Sort: USUBJID asc, phase, visit, onset date, SOC, PT (image 2 programming notes).
  - Group banner: page-by the treatment/group column, banner `Group: {<col>}`.
  - Footnotes: type the decode footnotes (image 2 style).
  - Options: Blank row between blocks ON (default).
- [x] **Step 3: Verify each requirement visually** (screenshot every state):
  1. USUBJID stack prints ONCE per subject block, blank row between blocks (req 7).
  2. Stacked cells match image 1 (`IM…-2-1` over `(55/F/C)` shape).
  3. Transpose selects list only the selected list variables (req 3).
  4. No VALUE LEGENDS section; LEVELS recode shows in cells (req 4).
  5. Stack editor: new stack has a picker line, second line auto-parens (req 5).
  6. No Stub column header / Total column in the listing Options pane (req 6).
- [x] **Step 4: Export the RTF** and eyeball the paginated output once.

### Task 11: Report

- [x] Update `handoff.md` + `tasks/todo.md` work log.
- [x] Final message answers req 2 and req 6's "explain / why" questions:
  - **IDENTIFYING LABEL** = the listing's 0..1 `id` role: the leading column AND (now) the group-skip anchor — the column whose value blocks print once with blank separators. It earns its place; it is the shell's "Unique Subject ID" semantics, not decoration.
  - **Row limit** = `options$limit`, a DuckDB `LIMIT` pushdown. Not part of any DPP shell; earns its place only as a preview/perf cap on huge listings. Kept (harmless, one generic row), flagged for the user to drop if they disagree.
  - **Stub column header / Total column** were ARD-table concepts leaking through the shared layout schema; now hidden for listings.
- [x] Flag: nothing committed (two repos carry the whole uncommitted listing epic); user gates commits.

## Self-review

- Spec coverage: req 1 → Task 10; req 2 → Task 11; req 3 → Task 5; req 4 → Tasks 1+2+7 (recode wired engine-side so "one place" is real, not written-but-ignored); req 5 → Task 6; req 6 → Task 8; req 7 → Task 3 (+ golden in Task 4).
- Known adjacent gap NOT in scope: the SPANNING HEADER editor still says "Assign a treatment variable first." for listings (todo.md Gap 2) — untouched, surgical.
- Type consistency: `.apply_item_levels(display, object)`, `.stack_output_name(stacks, col)`, `.listing_selected_items(object, items, extra)` used consistently across tasks.
