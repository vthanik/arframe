# arframe — richer table shapes (nested `by`, column groups, per-variable missing, population estimand default)

**Status:** design spec (hardened). No production code in this document.
**Author:** Vignesh Thanikachalam.
**Date:** 2026-07-09.
**Repos in scope:** `arframe` (Galley UI), `arpillar` (engine), `tabular` (page/RTF renderer).
**Golden gate:** non-empty RTF/doc export byte-identical + every figure round-trips
`ggplot → figure() → golden`. This is the ONE preserved invariant. Every capability
below MUST be **inert when unconfigured** so the untouched baseline stays byte-identical;
re-golden is allowed only for outputs that OPT IN.

---

## 0. TL;DR and the load-bearing finding

The user's rule — *"always check tabular; it has a solution for all outputs"* — holds. **tabular
already renders every shape these four features need. No new tabular capability is required.**
All work is in `arpillar` (build the right wide frame + call the right tabular DSL) and `arframe`
(wire the UI to `object@options` / `data_item@levels` / `theme$populations`).

tabular's OWN examples already draw the two BDS targets:
- `headers.R` Example 3 (`R/headers.R:171-184`) is the "Values over Time" stub: `param` + `visit`
  as nested `usage = "group"` columns, `stat_label` leaf, arm columns under a `"Treatment Group"` band.
- `headers.R` Example 1 (`R/headers.R:106`) uses `col_spec(indent = "indent_level")` — a per-row
  depth column — for the SOC/PT nesting.
- `headers.R` Example 4 (`R/headers.R:204-211`) nests spanners to arbitrary depth via nested named lists.

One framing correction the research forced (see §2.4 and §7-D1): **`options$panels` is NOT a
column-group knob.** It maps to `tabular::paginate(spec, panels =)` — *horizontal width-pagination*
of a too-wide table (`R/paginate.R:75` "Number of horizontal panels for wide tables"). The
`.OPT("panels", "Panels (column groups)", "int", 1L)` **label is wrong**. Statistical column groups
(the "Value | %Change" spanners) are built with `tabular::headers()` over a wide frame that carries
stat columns — the machinery already half-wired in `arpillar::.apply_spans()` (arm-only today).

---

## 1. Problem & target outputs

arpillar's `summary`/`crosstab` generators today produce exactly one shape: **treatment arms as
columns, one flat stub, statistics as rows** (`fct_render_table.R:.measure_block` /
`.category_block`; the arm columns are pinned by `render_display`'s column contract, `fct_render_table.R:19-43`).
Real BDS "Values and Change from Baseline" and "Values over Time" summaries need a **multi-level
nested stub** and **multiple statistic column groups under spanning headers**. Four capabilities,
one coherent design:

1. **Nested `by` row grouping** (⑤⑥) — an ordered `by` slot (1..n category/date vars) on `summary`
   and `crosstab`, producing nested indented row bands (Phase ▸ Visit ▸ …), mirroring how
   `occurrence` nests SOC ▸ PT.
2. **Statistic column groups / spanning headers** (the `panels` mislabel) — a "Value" group and a
   "Percent Change From Baseline" group, each a set of stat COLUMNS under a spanner; or arm × stat groups.
3. **Per-variable missing control** (⑨) — a PER-VARIABLE override of the study-level
   `show_missing`, reachable in the Roles LEVELS pane as a synthetic "Missing" level.
4. **Population-driven estimand default** (⑩b) — the default `arm_mode` (actual→`TRT01A`/`TRTA`,
   planned→`TRT01P`/`TRTP`) driven by the bound analysis set instead of only the generator.

### 1.1 Target layout A — "Values and Change from Baseline" (stats-as-COLUMNS, two value-groups)

Capability 1 (nested stub) **and** Capability 2 (column groups).

```
                          |               Value                 |    Percent Change From Baseline    |
 Parameter / Visit        |  N   Mean    SD   Min   Q1  Med  Max IQR |  N   Mean   SE   Med   Min   Max  IQR |
 ---------------------------------------------------------------------------------------------------------
 Systolic BP (mmHg)                                                                                          <- group hdr (Parameter)
   Baseline               |  86  132.4  15.2   98  ...           |   —     —    —     —     —     —    —   |  <- Visit (indent 1)
   Week 4                 |  84  128.1  14.8  ...                |  84   -3.2  1.1  -2.9  ...             |
   Week 8                 |  80  126.7  14.1  ...                |  80   -4.5  1.3  ...                   |
 Diastolic BP (mmHg)                                                                                         <- group hdr (Parameter)
   Baseline               | ...                                 | ...                                   |
```

- **Stub axis** = Parameter ▸ Visit (Capability 1). Two `usage = "group"` columns, or Parameter as
  a `header_row` band + Visit indented under it.
- **Column axis** = two spanned groups (Capability 2). `"Value"` spans the AVAL stat columns;
  `"Percent Change From Baseline"` spans the CHG stat columns. Stats are COLUMNS here — a transpose
  of today's stats-as-rows frame.
- **Per-arm variant** = one page per arm (reuse `page_by` / `subgroup()`), OR arm as an even-outer
  column tier (`headers("Placebo" = c(val cols, chg cols), "Drug" = ...)` — nested spanners).

### 1.2 Target layout B — "Values over Time" (stats-as-ROWS, arm columns, nested visit stub)

Capability 1 ALONE. This is tabular's `headers.R` Example 3 verbatim — the smaller, higher-value first target.

```
                                     |          Treatment Group            |
 Parameter     Visit      Statistic  |  Placebo    Drug 50    Drug 100     |
                                     |  (N=53)     (N=54)     (N=55)       |
 ---------------------------------------------------------------------------
 Systolic BP   Baseline   n          |   53         54         55          |
                          Mean (SD)  |  132 (15)   131 (14)   130 (13)     |
                          Median     |  ...                                |
               Week 4     n          |  ...                                |
                          Mean (SD)  |  ...                                |
 Diastolic BP  Baseline   n          |  ...                                |
```

- **Stub axis** = Parameter ▸ Visit (Capability 1), stat_label as the leaf.
- **Column axis** = arms (already produced today), under the existing `"Treatment Group"` spanner
  (`.apply_spans`) with per-arm `(N=…)` (already produced by `.header_n_labels`).

**Layout B needs only Capability 1.** Layout A adds Capability 2. This orders the build (§5).

---

## 2. What tabular ALREADY provides (verified, with identifiers)

All three render primitives exist natively. Source: direct read of `R/headers.R`, `R/col_spec.R`,
`R/engine_headers.R`, `R/engine_group_display.R`, `R/engine_subgroup_split.R`, `R/aaa_class.R`.

### 2.1 Spanning column headers / column groups — `headers()`

- **Verb:** `tabular::headers(.spec, ...)` (`R/headers.R:228`). Each named `...` arg is one band:
  the **name is the spanner label**, the **value is column membership** — a character vector of
  data-column names (leaf band) OR a named list that recurses (nested band, **arbitrary depth**).
- **S7 class:** `header_node` (`R/aaa_class.R:453`) with props `label`, `span` (leaf col names),
  `children` (sub-bands), `style`. Stored at `tabular_spec@headers` (`R/aaa_class.R:1068`). A second
  `headers()` call **REPLACES** the tree (not additive).
- **Engine:** `engine_headers()` (`R/engine_headers.R:36`) flattens to a band-grid, one row per
  (node, depth).
- **HARD CONSTRAINT — contiguity:** a band's leaf columns must be **contiguous in data-frame column
  order**. A *visible* column splitting a band aborts with `tabular_error_input`
  (`R/engine_headers.R:84-102`); a *hidden* helper column between leaves is tolerated. → **arpillar
  must emit columns so each group's members are adjacent.**
- Peer bands render side by side (Example 3); nested named-list bands nest (Examples 2, 4).

### 2.2 Nested / multi-level stub — two native idioms

- **Idiom A — nested section-header bands (Phase ▸ Visit ▸ …).** Declare N columns as
  `col_spec(usage = "group", group_display = "header_row")`. Bands nest by data-frame column order
  (**outer = leftmost**), child rows auto-indent one level per band; depth = number of header_row
  group columns, **unbounded** (`R/col_spec.R:278-283`, `R/engine_group_display.R:262-337`).
- **Idiom B — one label column + per-row depth (SOC/PT shape).** `col_spec(indent = "<depth_col>")`
  reads a per-row non-negative integer depth from `spec@data[["<depth_col>"]]` and auto-hides that
  column (`R/col_spec.R:390-431`, `R/aaa_class.R:330-346`). Each level = `preset@indent_size` spaces
  (default 2, `R/aaa_class.R:828`).
- Indentation renders as **native cell padding** (HTML em / LaTeX pt / RTF twips) via the
  `cells_indent` integer sidecar — NOT leading spaces — on every paper backend
  (`R/engine_group_display.R:96-105`).
- **HARD CONSTRAINT — pre-sorted runs:** the group engine detects **run-length transitions of
  adjacent equal values**; it does NOT sort (`R/engine_group_display.R:429-451`). → **arpillar must
  emit rows pre-sorted so each group's rows are contiguous.**
- **Known limitation:** the single-member-group "collapse" (a 1-row group renders flush-left, no
  header) is implemented **single-band only** (`R/engine_group_display.R:301-302`, an explicit
  ponytail TODO). A nested two-`header_row` layout keeps a header per singleton. Cosmetic, not a blocker.

### 2.3 Indented row bands / group display — the two orthogonal engines

- `engine_group_display.R` — in-table indented bands, section-header rows, `group_skip` blank
  separators, driven by `col_spec@usage="group"` / `group_display` / `group_skip` / `indent`.
- `engine_subgroup_split.R` — orthogonal: partitions the WHOLE table into banner-headed,
  separately-paginated BY-group sections (SAS BY-group). This is what `page_by` / `.apply_subgroup`
  already use (`subgroup_spec`, `R/aaa_class.R:494`). NOT row indentation.

### 2.4 What `panels` actually is (the mislabel)

`tabular::paginate(.spec, panels = N)` (`R/paginate.R:75`, `:291`) splits a too-wide table into
`N` **horizontal panels** stacked down the page (the stub/group column repeats per panel). This is
width management, **not** statistical column grouping. arpillar wires it verbatim in
`.apply_panels()` (`fct_render_table.R:1752-1758`). Keep this feature; **rename the option** (§7-D1).

---

## 3. Design per capability

Notation: `resolve_option(object, theme, key, theme_path, engine_default)` (`arpillar/R/resolve.R:24`)
is the study→output→default resolver used at every theme-inherited site. `object@options` is a
free-form list (no first-class S7 slot for any of these keys — `aaa_class.R:158-183`), which is what
keeps the golden gate cheap: an absent key = engine default = byte-identical.

### 3.1 Capability 1 — nested `by` row grouping

**Insight:** the ARD machinery already stacks by a second grouping variable — `page_by` does exactly
this. `.stack_by_arm(data, quos, arm, page_by)` calls `cards::ard_stack(..., .by = c(arm, page_by))`
(`fct_render_ard.R:410-413`), producing `group2_level` on every stat row. `by` reuses this stack;
it only differs from `page_by` in the DISPLAY treatment — indented rows instead of page breaks
(the exact submission-convention distinction).

**(a) Generator / slot delta (`arpillar/R/fct_generators.R`).** Add an OPTIONAL `by` slot to
`summary` and `crosstab`:

```r
.SLOT("by", "Group rows by (nested)", c("category", "date"), min = 0L, max = Inf)
```

`min = 0L` keeps it optional (absent = today's shape). Slot name `"by"` is the render contract.

**(b) ARD change (`fct_render_ard.R`).** Read the ordered `by` items; extend the stack:

```r
by_vars <- vapply(.by_items(object), function(it) it@name, character(1))   # ordered, may be length 0
# .stack_by_arm(data, quos, arm, by = c(page_by, by_vars))  -> ard_stack(.by = c(arm, page_by, by_vars))
```

cards emits `group2_level`, `group3_level`, … one per by var (in order). `needed` (the projection
pull, `fct_render_ard.R:138`) gains `by_vars`. Percent base is unchanged (population-N denominator).
**When `by` is empty the `.by` vector is `c(arm)` — byte-identical to today.**

**(c) Display change (`fct_render_table.R`).** New path `.by_nested_block()` (dispatched only when
`by` is non-empty), producing a wide frame:

```
[ by1 , by2 , … , group , stat_label , <arm1> , <arm2> , … ]
   |     |          |         |            \_______ arm cells (existing .row builder)
   |     |          |         \_ variable label (existing group header, collapses to blank
   |     |          |            when a single measure — see Open Q O1)
   |     \_ inner nesting level (group column)
   \_ outer nesting level (group column)
```

Rules:
- Pivot arms → columns (existing `.row()` / `.stat_value*` lookups, keyed additionally on
  `group2_level`/`group3_level` the way `.occ_group2_is()` keys the SOC, `fct_render_table.R:601-606`).
- **Pre-SORT** rows so each nesting group is a contiguous run (tabular §2.2 constraint). Sort key =
  `(by1, by2, …, variable order, stat order)`. by-level order follows the item's `@levels` metadata
  when present (reuse `.resolve_levels`, `fct_render_table.R:453-478`), else ARD appearance order.
- Emit `by1..byN` as columns for tabular to consume as group columns.

**(d) tabular handoff (`.apply_cols`, `fct_render_table.R:1386`).** Extend the stub-spec builder:
when `by1..byN` columns are present, configure each as
`col_spec(usage = "group", group_display = "header_row", label = <by var label>)` (Idiom A). The
existing `group` (variable) column stays a group column; `stat_label` stays the `usage = "id"` leaf.
Column order in the emitted frame is `by1, …, byN, group, stat_label, arms` so tabular's
outer=leftmost nesting matches Parameter ▸ Visit ▸ variable. `.apply_spans` still bands the arms.
**No new tabular call** — same `cols()` / `headers()` verbs, more group columns.

**(e) arframe UI wiring.** `by` is a DATA ROLE, so it lives in the **Roles pane**
(`mod_card_roles.R`), NOT Options — the generator slot auto-surfaces because
`.slot_fieldset` builds inputs off `generator(type)$slots`. The add-picker is the shared
`.eligible_picker(ns, paste0("add_", "by"), .eligible_items(items_meta, slot, assigned))`
(`mod_card_roles.R:743`). Ordering/removal reuse the existing per-slot observers
(`mod_card_roles.R:1144-1184`). Per-level order/labels reuse the existing LEVELS editor. **A `by`
edit is an ARD-key change (STALE → Run re-typesets), not a cheap display edit** — it changes the
collect + stack; keep it in `.ard_key()` (unlike level metadata, which is display-only).

**(f) Schema delta.** None at S7 level (`by` is a `role` with `slot = "by"`; `role`/`data_item`
unchanged). Generator registry gains the slot. CDISC/ARS: the `by` slot ≈ **GroupingFactor**
(non-treatment) — see §6.

### 3.2 Capability 2 — statistic column groups (spanning headers)

**This is the transpose mode: stats become COLUMNS, grouped under spanners.** It is NOT `panels`.
Build it on `headers()` (§2.1) over a frame that carries stat columns.

**Model.** The two column groups come from summarizing TWO different measure variables — AVAL
(→ "Value") and CHG (→ "Percent Change From Baseline") — each with its own stat set. So the source
of the groups is the **`summarize` role holding ≥1 measure**, plus a per-output display toggle that
lays each measure's stats out as a COLUMN GROUP instead of stacking them as row blocks.

**(a) Options delta (`arpillar/R/fct_generators.R`, `.OPTION_SCHEMA$summary`).** Add:

```r
.OPT("stat_layout", "Statistics layout", "choice", "rows", c("rows", "columns"))
```

`"rows"` (default) = today's stacked stat rows → **byte-identical when unset**. `"columns"` = each
measure becomes a spanned column group; the per-measure stat set is the existing `options$stats` /
Setup continuous rows (`.stats_opt`, `fct_render_table.R:1176-1195`), but rendered horizontally.

**(b) ARD change.** None beyond Capability 1's stack. The ARD already carries every stat atom per
(variable × arm × [by levels]); `stat_layout` is purely a DISPLAY pivot (it re-runs only
`render_display`, never the collect — the two-stage seam's whole point, `fct_render_table.R:9-16`).

**(c) Display change.** New builder `.stat_columns_block()` (active only when
`stat_layout == "columns"`): emit one column per (measure, stat) — e.g. `Value_N`, `Value_Mean`,
`Value_SD`, …, `Chg_N`, `Chg_Mean`, `Chg_SE`, … — with rows = the by-nesting stub (Capability 1).
Cells reuse `.fmt_measure_cell` (`fct_render_table.R:696`). **Emit the columns of each group
CONTIGUOUSLY** (tabular §2.1 constraint). Carry the group membership forward as an attribute the
handoff reads.

**(d) tabular handoff — generalize `.apply_spans` (`fct_render_table.R:1768-1808`).** Today
`.spans_opt` intersects `s$cols` with **arms only** (`fct_render_table.R:1795`). Generalize it to
accept ANY display column so the value groups can be spanned:

```r
headers(spec,
  "Value"                        = c("Value_N","Value_Mean","Value_SD", ... ),
  "Percent Change From Baseline" = c("Chg_N","Chg_Mean","Chg_SE", ... )
)
```

Reuse the existing per-column `col_spec(align = "decimal")` on each stat column and per-column
labels ("N", "Mean", "SD", …) via `cols()`. The contiguity guard already in `.spans_opt`
(`fct_render_table.R:1800-1803`, drop non-contiguous bands forgivingly) is retained. **Per-arm
variant** = nested spanners: `headers("Placebo" = list("Value" = ..., "%Change" = ...), "Drug" = …)`
— arbitrary depth, natively supported (§2.1). One page per arm remains available via `page_by`.

**(e) arframe UI wiring.** Two controls in the **Options pane** (`mod_card_options.R`):
- `stat_layout` — auto-surfaces from `.OPTION_SCHEMA$summary` as a `choice` segmented control; the
  `for (opt_key in known)` observer loop (`mod_card_options.R:1534`) wires it with **zero new
  observer**. Optionally add to `.OPT_REGION` for section placement.
- Group labels — when `stat_layout == "columns"`, the value-group spanner labels ("Value",
  "Percent Change From Baseline") are per-measure and default from the measure's `data_item@label`;
  a user override reuses the existing **spans editor** pattern (`.opt_spans_section` / `.span_row` /
  `.commit_spans`, `mod_card_options.R:798-841,1334-1351`) — the best in-file template for a
  list-valued option, with default-elision to `NULL` when empty.

**(f) Schema delta.** `object@options$stat_layout` (+ optional `options$spans` reuse). No S7 change.
**Rename** `.OPT("panels", …)` label (§7-D1). CDISC/ARS: the value groups ≈ OutputDisplay column
structure; the AVAL/CHG split ≈ two `Analysis`es sharing a GroupingFactor.

### 3.3 Capability 3 — per-variable missing control

**Today:** `show_missing` is a single STUDY-level field
`theme$summaries$categorical$show_missing` ∈ {auto, always, never} (`mod_setup.R:108-111,2208-2214`),
consumed engine-side by `.cat_missing_row()` via `.cat_rules_opt()` (`fct_render_table.R:298-309,356-393`).
The engine default is being moved toward `"never"` (per your note); the arframe UI seed is already
`"never"`.

**Design — synthetic "Missing" LEVEL (recommended).** Reuse the existing `data_item@levels`
list and its include-checkbox UI rather than inventing a new per-item flag. A level record is
`list(value, display, include, expected)` (`aaa_class.R:39-46`). Add a synthetic entry with a
reserved sentinel value:

```r
list(value = "__ARF_MISSING__", display = "Missing", include = TRUE/FALSE)
```

- **include = TRUE** → force the Missing row ON for this variable (override study `show_missing`).
- **include = FALSE** → force it OFF.
- **absent** (no synthetic entry) → fall through to study `theme$summaries$categorical$show_missing`
  (today's behavior exactly).

**(a) arframe UI (`mod_card_roles.R:.levels_editor`, L388-493).** Inject one synthetic row into
`all_vals` (L398) for `category` items: append `"__ARF_MISSING__"` with display "Missing", rendered
by the SAME `.ar-level-row` markup and the SAME include checkbox (`.ar-level-include`, L426). The
checkbox posts the existing `input$lvl_include` → `.set_level_meta(lv, "__ARF_MISSING__", …)`
(L938-947, appends if absent). **No new observer, no new input, no new toggle concept** — this is
the whole reason to model it as a level. The row sorts last (after observed levels) and carries no
DISPLAY-AS / reorder affordance (it is synthetic). It stays a **cheap display edit** (levels are in
`.roles_digest` but excluded from `.ard_key`, L881-882) → repaints live, never STALE.

**(b) arpillar consumer (`fct_render_table.R:.category_block` / `.cat_missing_row`).** Before
computing the Missing row, check the item's `@levels` for the sentinel:
`.item_missing_override(it@levels)` → `TRUE` / `FALSE` / `NA`. Resolution:
`show_missing <- if (!is.na(override)) (if override "always" else "never") else .cat_rules_opt(theme)$show_missing`.
The sentinel value is filtered OUT of the ordinary level set in `.resolve_levels` (so it never renders
as a data level). **Empty `@levels` → `NA` → study rule → byte-identical.**

**(c) Schema delta.** None — reuses `data_item@levels`. Reserve the sentinel string in one place
(a constant `.MISSING_LEVEL_VALUE` in both repos; document it). CDISC/ARS: a per-variable Missing
override ≈ a DataSubset/Operation nuance on the Analysis; noted, not blocking.

**Rejected alternative — dedicated per-item flag** (`data_item@format$show_missing`). It works but
adds a SECOND per-variable "missing" concept beside the LEVELS list the user already edits, needs a
new control + observer in `.levels_editor`, and does not reuse the include-checkbox. The synthetic
level is strictly less surface area and matches the user's stated preference. **Chosen: synthetic level.**

### 3.4 Capability 4 — population-driven estimand default

**Today (`arpillar/R/resolve.R:84-120`, `theme.R:29`, `fct_presets.R:26-35`).** Estimand rides on
`object@options$arm_mode`, seeded PER GENERATOR (`occurrence → "actual"`, else `"planned"`) in
`.PRESET()`, and `resolve_arm()`'s own fallback is `%||% "planned"`. The rule table collapses to
`subject → TRT01{A|P}` vs everything-else → `TRT{A|P}`. **The code explicitly comments
(`resolve.R:38`, `theme.R:29`): "Estimand is a property of the ANALYSIS, not the study or the
population"**, and notes the awkward case: *demographics-on-safety-population is often planned.*

**The conflict, resolved.** The user wants the DEFAULT to track the bound analysis set
(safety/non-efficacy → actual; FAS/efficacy/ITT → planned). This does NOT overturn the comment: the
estimand VALUE stays a per-output property (`options$arm_mode` still wins). What changes is only the
**source of the DEFAULT** — from a coarse generator heuristic to the bound **AnalysisSet's `basis`**,
which is a better proxy for analysis intent (an AE table, an AE-severity crosstab, and an AE listing
all share the actual-treatment convention; the generator alone cannot see that). The one case the
comment names — demographics on the safety set wanting *planned* — is handled by an **explicit
per-output `arm_mode`** on that preset, which is more honest than a silent generator default. So:
**estimand ≠ population at the VALUE level (preserved); the DEFAULT is now set-aware.** (Decision
record §7-D2.)

**(a) theme schema delta (`arpillar/R/theme.R`).** A population record grows from
`list(label, dataset, filter)` to `list(label, dataset, filter, basis)`, `basis ∈ {"actual","planned"}`.
`.SPEC_POPULATIONS` is free-shaped `list()` (`theme.R:72-78`) so the validator accepts `basis` with
**zero code change**; only update the doc comment `theme.R:72-73` to name the new field.

**(b) `resolve_population` (`resolve.R:187-219`).** Return `basis` in the resolved list:
`basis = set$basis %||% NA_character_` (and `NA` for the legacy-literal and nothing-bound branches).

**(c) `resolve_arm` default chain (`resolve.R:84-120`).** New precedence (override still wins):

```
1. options$arm            (raw column-name override)             -- unchanged, wins absolutely
2. options$arm_mode       (explicit per-output estimand)         -- unchanged, wins over default
3. bound set's `basis`    (resolve_population(object,theme)$basis) -- NEW default source
4. generator heuristic    (occurrence -> actual, else planned)   -- retained as terminal fallback
5. "planned"                                                     -- unchanged final default
```

Implement by changing `mode <- object@options$arm_mode %||% "planned"` (`resolve.R:97`) to consult
`basis` then the generator heuristic before `"planned"`. **Move the generator heuristic OUT of the
`.PRESET()` pre-seed** (`fct_presets.R:29-35`) and INTO `resolve_arm` as fallback #4, so `arm_mode`
becomes a true (usually-absent) override rather than an always-present seed that pre-empts `basis`.
Presets whose analysis diverges from their population's basis (e.g. `demographics`/`exposure` on the
safety set = planned) set `options$arm_mode = "planned"` **explicitly** — enumerate and set these
(the summary/crosstab-on-safety presets in `.PRESETS`).

**(d) arframe UI (`mod_setup.R:.setup_analysis_sets`, L1770-1855).** Add a `BASIS` column to the
populations table: a bound `.seg_control(ns, "pop_basis_<i>", "Basis", c("actual","planned"), …)`
(same primitive as `cat_show_missing`). Seed `basis` by **name heuristic at creation**: in
`.POP_SEEDS` (L1710) `safety` → `"actual"`; in the `pop_add` new-row (L476) default `"planned"`, but
apply a heuristic on the label/id — `grepl("saf|as.?treated|per.?protocol|\\bpp\\b", x, ignore.case)`
→ actual; `grepl("fas|full|itt|effic", x, ignore.case)` → planned; else planned. Extend
`.collect_pops` (L1117-1137) with one line:
`basis = as.character(input[[paste0("pop_basis_", i)]] %||% "planned")`. **No new observer** — the
populations `observe` already rebuilds via `.collect_pops(input)`. Keep `pop_basis_<i>` in the
bound-input group (it is read by name, like `pop_filter_<i>`).

**(e) Schema delta.** `theme$populations[[id]]$basis`. CDISC/ARS: `basis` ≈ **AnalysisSet** metadata
(the analysis set that a `TRT01A`-vs-`TRT01P` GroupingFactor defaults from). Anticipates §6.

---

## 4. Golden-gate safety argument (per capability)

The gate: an output configured as it is TODAY must render byte-identically. Each capability is
inert-by-default because each new dimension is absent from `object@options` / `data_item@levels` /
`theme$populations[…]$basis` unless a user opts in, and every resolver already treats "absent" as
"engine default" (`resolve_option`, `%||%`).

- **Capability 1 (by nesting).** When the `by` slot is empty, the ARD stack is `.by = c(arm)` — the
  exact vector today's summary/crosstab produce (page_by absent). The display takes the existing
  `.measure_block`/`.category_block` path; `.apply_cols` sees no `by*` columns and emits the existing
  `group` + `stat_label` stub. **No new column, no reorder, no new tabular call.** Byte-identical.
  Guard it with a golden-strip case: empty `by` role vs no `by` role → identical RTF.
- **Capability 2 (stat columns).** `stat_layout` defaults `"rows"`. `.OPT` default `"rows"` +
  default-elision (`.commit_opt`, `mod_card_options.R:1044`) means an untouched output NEVER writes
  the key → `render_display` takes the stats-as-rows path → byte-identical. `.apply_spans`
  generalization keeps arm-only behavior when no stat columns exist (the `intersect` still lands on
  arms). Re-golden only outputs that set `stat_layout = "columns"`.
- **Capability 3 (per-var missing).** No synthetic level in `@levels` → `.item_missing_override`
  returns `NA` → the study `show_missing` rule applies exactly as today (`.cat_missing_row`
  unchanged path). The sentinel is filtered from ordinary levels, so it cannot leak into a data row.
  Byte-identical unless a user adds/toggles the Missing level. The existing golden-strip already
  pins `summaries.categorical.show_missing` — extend it (§5/§6-tests) for the override path.
- **Capability 4 (basis default).** **Verified inert on the existing goldens:** the golden
  demographics/occurrence objects set **no `arm_mode`** and bind **no basis-carrying registered set**
  (`test-golden-occurrence.R` uses `options = list(population = "ADSL")` — a legacy literal with no
  `basis`; grep finds no `arm_mode` in any golden). With `arm_mode` unset and `basis` NA, the new
  chain falls to fallback #4 (generator heuristic) → the SAME arm column today's code resolves
  (`TRT01P` via the role fallback / generator default) → byte-identical RTF. Moving the generator
  seed from `.PRESET` into `resolve_arm` fallback #4 preserves the resolved value for every
  no-basis/no-arm_mode output; the ONE behavioral change (summary/crosstab-on-safety presets) is
  made explicit and re-goldened deliberately. Guard with a golden-strip case: a population WITH
  `basis` set to the value the generator would have chosen anyway → identical RTF.

**Re-golden policy.** Allowed ONLY for outputs that opt into a new dimension (a configured `by`,
`stat_layout = "columns"`, a Missing-level override, or a basis-carrying bound set). The untouched
baseline goldens (`_snaps/golden-demographics/demographics.rtf`,
`_snaps/golden-occurrence/occurrence.rtf`, figures) MUST NOT move; if they do, a resolver diverged —
fix the resolver, never the golden (`test-golden_strip.R` header contract).

---

## 5. Staged build order (smallest blast radius first)

Each stage is independently shippable and verifiable. Order is by blast radius, not value.

**Stage 1 — Capability 4 (population estimand default).** Smallest, isolated to
`resolve.R` + `theme.R` doc + `fct_presets.R` + `mod_setup.R`. No ARD/display change; render path
untouched. **Verify:** golden-strip basis case byte-identical; a safety-bound summary resolves
`TRTA`; an FAS-bound summary resolves `TRTP`; per-output `arm_mode` still overrides; all baseline
goldens unchanged. Screenshot: Setup > Analysis sets shows the BASIS column, heuristic-seeded.

**Stage 2 — Capability 3 (per-variable missing).** Display-only, isolated to
`.category_block`/`.cat_missing_row` + `.levels_editor`. **Verify:** a variable with an included
Missing level shows the row regardless of study `show_missing`; excluded hides it; absent falls
through; baseline goldens unchanged (no sentinel). Screenshot: LEVELS pane shows the Missing row with
its include checkbox; toggling repaints live (no STALE).

**Stage 3 — Capability 1 (nested `by` grouping).** ARD stack extension + new
`.by_nested_block` display path + `.apply_cols` multi-group-column handoff + Roles `by` slot.
Unlocks **Layout B** (arms as columns, nested Parameter ▸ Visit stub). **Verify:** a two-level `by`
renders nested indented bands on ADaM BDS data; empty `by` byte-identical to today; re-golden a NEW
`by`-configured output; the spanner over arms still renders. Screenshot: the paper canvas shows
Parameter ▸ Visit nesting with per-arm (N=).

**Stage 4 — Capability 2 (statistic column groups).** Biggest. New `stat_layout = "columns"`
display mode + `.stat_columns_block` + generalized `.apply_spans` for stat groups + Options wiring +
`panels` relabel. Depends on Stage 3's nested stub for the row axis. Unlocks **Layout A**
("Value | Percent Change From Baseline"). **Verify:** an AVAL+CHG summarize role with
`stat_layout = "columns"` renders two spanned stat groups over a Parameter ▸ Visit stub; contiguity
holds; `stat_layout = "rows"` byte-identical; re-golden the NEW columns output. Screenshot: Layout A
end to end.

Sequencing rationale: 4 and 3 (capability numbers) are resolver-only / display-only and cannot move
a baseline golden; 1 touches the ARD but reuses page_by's stack; 2 is the only genuinely new display
mode and rides on 1's stub. Stages 1-2 can land in parallel (disjoint files); 3 precedes 4.

---

## 6. Test / golden plan

**Framework.** arpillar goldens: `expect_snapshot_file()` on the emitted RTF + an in-session
double-render `expect_identical(readBin(f1), readBin(f2))` (the RTF backend embeds no timestamp, so
bytes are a stable oracle — `test-golden-demographics.R:1-12`, `test-golden-occurrence.R:57-82`).
Byte-inertness of the baseline is pinned by `test-golden_strip.R`: for each wired resolver path,
render with (a) empty theme and (b) an explicit theme carrying the engine default, assert
byte-identical; a new resolver call site ADDS a strip case.

Per capability:

- **Capability 1.** `test-render_table.R`: `.by_nested_block` unit tests on a synthetic BDS frame
  (frame shape: `by1,by2,group,stat_label,arms`; assert contiguous runs, correct indent depth,
  arm-cell values). New byte golden `_snaps/golden-bds-by/` for a 2-level `by` output on the
  ADaM pilot. Golden-strip: empty `by` role == no `by` role.
- **Capability 2.** `test-render_table.R`: `.stat_columns_block` unit tests (column names per
  (measure,stat), contiguity, decimal align). New byte golden `_snaps/golden-bds-columns/` for an
  AVAL+CHG `stat_layout = "columns"` output. Assert `stat_layout = "rows"` == unset (strip case).
  A test that a non-contiguous span is dropped forgivingly (existing `.spans_opt` behavior).
- **Capability 3.** `test-render_table.R`: `.item_missing_override` returns TRUE/FALSE/NA per
  sentinel state; `.category_block` shows/hides the Missing row accordingly and independently of
  `theme$summaries$categorical$show_missing`; sentinel never appears as a data level. Golden-strip:
  no sentinel == today's study-rule render.
- **Capability 4.** `test-resolve.R`: the full precedence table (arm → arm_mode → basis → generator
  → planned) as a parametrized case matrix; `resolve_population` returns `basis`. Golden-strip: a
  population whose `basis` equals the generator's own choice → baseline RTF unchanged. A regression
  that the demographics-on-safety preset resolves `TRTP` via its explicit `arm_mode`.
- **arframe wiring.** `test-mod_setup_wire.R`: `pop_basis_<i>` round-trips into
  `theme$populations[[id]]$basis`; heuristic seeding on add. `test-mod_card_roles*`: the synthetic
  Missing level posts `lvl_include` and lands in `@levels`. `test-mod_card_options*`: `stat_layout`
  commits with default-elision.
- **Baseline invariants (every stage).** All of `test-golden-demographics.R`,
  `test-golden-occurrence.R`, `test-golden-figures.R`, `test-golden-pagechrome.R`,
  `test-golden_strip.R` stay green with **unchanged** snapshot bytes. Per the memory rule, eyeball a
  screenshot of every mode after each stage, not just green tests.

---

## 7. Open questions / decisions taken

**D1 (taken) — `panels` is not column groups; rename it.** `options$panels` →
`tabular::paginate(panels =)` is width-pagination (§2.4). Statistical column groups are built with
`headers()` (Capability 2). **Decision:** keep `panels` as the width knob, **relabel** its `.OPT`
from `"Panels (column groups)"` to `"Horizontal panels"`, and do NOT overload it. Flag: the task
brief's "flesh out `panels` into column groups" conflated two tabular features; this spec separates
them. (Surfaced per the "surface conflicts, pick one, explain why" convention.)

**D2 (taken) — estimand default may track the population without making estimand a population
property.** Resolves the `resolve.R:38` / `theme.R:29` comment: `options$arm_mode` stays the
authoritative per-output estimand (still wins); the population `basis` is only the DEFAULT SOURCE
(fallback #3), and divergent analyses (demographics-on-safety) set `arm_mode` explicitly. Update
both comments to read: *estimand VALUE is per-analysis; its DEFAULT tracks the bound AnalysisSet.*

**D3 (taken) — model per-variable missing as a synthetic level, not a new flag** (§3.3). Less
surface area; reuses the include checkbox; matches user preference.

**D4 (taken) — `by` is a Roles slot, not an Options key** (§3.1e). It is a data role (a variable in
a slot); page_by-style options stay for the "one page per level" case. A user wanting page breaks
uses `page_by`; a user wanting indented nesting uses `by`. Both can coexist (by nests within a page).

**O1 (open) — the variable `group` header when `by` is active and a single measure.** Today every
measure emits a `group` header row (the variable label). Under `by` with ONE analysis variable
(typical BDS), that header is redundant with the output title. **Proposed:** collapse the `group`
column to blank (or drop it) when a single summarize item is present under an active `by`; keep it
when ≥2 items (variable becomes an inner nesting level). Needs a UX call — flag for the build.

**O2 (open) — nesting depth cap.** cards' `.by` and tabular's group columns are N-capable, but
`occurrence` caps at 2 (`.hierarchy_items`, `fct_render_ard.R:549-559`). **Proposed:** support up to
3 `by` levels in v1 (Phase ▸ Visit ▸ Param covers the BDS canon), abort >3 with a clear message
(mirror the occurrence cap), lift later. The display walk and sort key are the only depth-bound code.

**O3 (open) — `stat_layout = "columns"` × arms.** Layout A per-arm can be (a) one page per arm
(`page_by` on the arm, simplest, reuses `subgroup()`) or (b) arm as an outer spanner tier over each
value group (nested `headers()`, denser). **Proposed:** ship (a) first (zero new machinery), add (b)
behind the same `stat_layout` once (a) is golden. Flag for the build.

**O4 (open) — Setup↔engine contract for `basis` and the Missing sentinel.** Per the standing risk
(several Setup theme fields shipped written-but-ignored), the engine-side READ must be verified, not
just the theme write. Each stage's acceptance includes a render-level assertion (RTF/display changes),
not only a theme-field round-trip test.

**O5 (open) — CHG availability.** Layout A's "Percent Change" group assumes a CHG (or PCHG) variable
in the BDS dataset. When the summarize role has only AVAL, `stat_layout = "columns"` renders a single
"Value" group (still valid). The Options UI should not offer a second group unless a second measure
is in the role. Flag for the wiring.

---

## 8. CDISC ARS spine alignment (anticipating backlog #12.4)

These four features pre-position the ARS `ReportingEvent` mapping (CLAUDE.local.md #12.4) so it need
not be re-cut later:

| This spec | ARS entity (backlog #12.4) | Note |
|---|---|---|
| `by` slot (§3.1) | **GroupingFactor** (non-treatment) + Groups | ordered nesting = ordered GroupingFactors |
| stat column groups (§3.2) | **OutputDisplay** column structure; AVAL/CHG ≈ two **Analysis**es | spanner labels = display metadata |
| per-var Missing (§3.3) | **Operation** / **DataSubset** nuance on an Analysis | per-variable missing handling |
| population `basis` (§3.4) | **AnalysisSet** metadata | the set a `TRT01A`/`TRT01P` GroupingFactor defaults from |

Keep the `by` items, the value-group labels, and the population `basis` addressable by a stable key
(the eventual `ARM_OID` linkage, arframe half) so the ARS emitter can cross-reference them. None of
these features BLOCKS on the ARS spine; each is a clean superset the spine can formalize.

---

## Appendix — key source anchors (for the implementer)

- tabular spanners: `tabular::headers()` `R/headers.R:228`; `header_node` `R/aaa_class.R:453`;
  contiguity guard `R/engine_headers.R:84-102`; examples `R/headers.R:106,171-184,204-211`.
- tabular nested stub: `col_spec(usage,"group",group_display,indent)` `R/col_spec.R:278-283,390-431`;
  group engine `R/engine_group_display.R:262-337,429-451`; indent sidecar `:96-105`.
- tabular width panels (the mislabel): `tabular::paginate(panels=)` `R/paginate.R:75,291`.
- arpillar ARD stack: `.stack_by_arm` / `ard_stack(.by=)` `fct_render_ard.R:410-413`; page_by
  precedent `:441-447`; occurrence 2-level cap `:549-559`.
- arpillar display seam: `render_display` `fct_render_table.R:148`; `.measure_block`/`.category_block`
  `:264-278,404-443`; `.cat_missing_row` `:356-393`; `.occ_nested_block` (SOC▸PT reference)
  `:542-578`; handoff `.apply_cols`/`.apply_spans`/`.apply_panels` `:1386,1768,1752`;
  `render_spec` pipeline `:1287-1311`.
- arpillar estimand: `resolve_arm` `resolve.R:84-120`; rule table `:45-54`; the anchoring comment
  `:36-63`; `resolve_population` `:187-219`; preset seed `fct_presets.R:26-35`; `resolve_option`
  `:24`.
- arpillar schema: `data_item@levels` `aaa_class.R:39-92`; `object@options` (free-form) `:158-183`;
  `.SPEC_POPULATIONS` (free-shaped) `theme.R:72-78`; `.OPTION_SCHEMA`/`.LAYOUT_SCHEMA`
  `fct_generators.R:93-222`.
- arframe wiring: LEVELS editor `mod_card_roles.R:388-493`, `.set_level_meta` `:938-947`, level
  observers `:1320-1367`; Options commit `mod_card_options.R:1011-1050`, known-key loop `:1534`,
  spans template `:798-841,1334-1351`; Setup populations `mod_setup.R:1770-1855`, `.collect_pops`
  `:1117-1137`, `.POP_SEEDS` `:1710`; `show_missing` `:108-111,2208-2214`; shared pickers
  `.ar_picker_select` `utils_atoms.R:370-401`, `.eligible_picker` `mod_card_roles.R:167-202`;
  render seam `fct_store.R:401,415-425`.
