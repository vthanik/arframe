# Audit False Positives — arframe

Findings from package audits that were investigated and confirmed as **not issues**.
Reference this before acting on future audit findings to avoid re-investigating.

Last updated: 2026-03-31

---

## Import Discipline

### `tools` not in DESCRIPTION Imports
**Finding:** `importFrom(tools, file_ext)` in NAMESPACE but `tools` not in DESCRIPTION Imports.
**Why false positive:** `tools` is a base R package — ships with every R installation. R CMD check does not require base packages in DESCRIPTION Imports. The NAMESPACE directive is sufficient.

---

## Naming Conventions

### Verb prefix mixing (parse_/compute_/build_/format_)
**Finding:** Functions use different verb prefixes for "similar" operations.
**Why false positive:** Each prefix describes a genuinely different operation:
- `parse_*` — extract structure from strings via regex
- `compute_*` — calculate/derive a numeric value
- `build_*` — construct/assemble an object from inputs
- `format_*` — transform a value into display representation
- `rebuild_*` — reconstruct an aligned string from parsed components

Using one prefix for all would lose semantic meaning. The distinction is intentional.

### Width dict key naming
**Finding:** Keys like `w_n`, `w_est_si`, `w_npct_pct_prefix` are "inconsistent."
**Why false positive:** They follow a deliberate semantic hierarchy: `w_{type}_{component}`. The naming encodes the stat type and component (integer, decimal, sign, prefix). Nested lists would add complexity to a switch-based dispatch system without benefit.

### S3 method naming
**Finding:** S3 methods lack consistent naming.
**Why false positive:** All S3 methods follow standard R conventions. `knit_print.fr_spec` follows knitr's naming; `print.fr_spec`, `format.fr_spec` follow base R. No custom generics violate naming rules.

---

## Redundancy

### na_to_empty() "underused"
**Finding:** 8+ files re-implement NA-to-empty-string logic.
**Why false positive:** Investigated every instance. The utility is already used consistently in all 7 vector-substitution call sites. The remaining patterns are:
- Scalar conditionals (`if (is.na(x)) "" else x`) — intentionally different from vectorized utility
- `ifelse(is.na(x), "continuous", ...)` — choosing between values, not NA-to-empty
- Error handler `return("")` — error recovery, not missing-value substitution

### Float/decimal regex "duplicated"
**Finding:** Regex patterns defined in multiple places.
**Why false positive:** Complex patterns (est_ci, est_ci_bracket) are pre-compiled in constants. Simple patterns (scalar_float: `^\d+\.\d+$`) are inline because they're self-documenting and used once. The stat_type_registry already centralizes detection patterns; parse patterns are necessarily different per type.

### Width computation "duplicated" across files
**Finding:** columns.R, decimal.R, paginate.R all compute widths.
**Why false positive:** Different computations for different purposes:
- `decimal.R` — character-count widths for alignment padding (nchar-based)
- `columns.R` — font-metric widths for column sizing (AFM twips-based)
- `paginate.R` — character-count widths for pagination budget (twips/space-based)
These are not the same operation and cannot share code.

### NULL coalescing `%||%` pattern "repeated"
**Finding:** `%||%` operator used 50+ times across codebase.
**Why false positive:** `%||%` is idiomatic R (from rlang). Extracting a helper for `x %||% default` would reduce readability. This is standard R style, not duplication.

---

## Architecture

### fr_page() replaces vs fr_pagehead() merges
**Finding:** Inconsistent verb semantics.
**Why false positive (as a bug):** This is by design. `fr_page()` sets the full page layout (orientation, paper, margins) — replace makes sense because these settings are interdependent. `fr_pagehead()`/`fr_pagefoot()` set individual chrome elements — merge is correct so you can call them independently for left/right. **Fixed as documentation issue** — added `@section Verb Behavior:` to fr_page() roxygen.

### fr_rows() collapse_hierarchy() post-processing
**Finding:** Post-processing side effect breaks pure verb pattern.
**Why false positive:** `collapse_hierarchy()` augments `spec$data` with `__display__` and `__row_level__` columns that later verbs (fr_styles, fr_col_style with `rows = "group_headers"`) depend on. Moving to finalize_spec() would break the pipeline because these columns must exist before styling verbs run.

### Optional fr_validate()
**Finding:** Validation should auto-run at render time.
**Why false positive:** `fr_render()` already validates internally. `fr_validate()` exists for users who want early feedback before committing to render. Auto-running strict validation would break valid edge cases (e.g., intentionally empty footnotes).

### Three HTML output modes
**Finding:** file/viewer/knit_print should be unified.
**Why false positive:** These are genuinely different targets:
- File mode writes standalone HTML with embedded CSS
- Viewer mode returns htmltools tags for RStudio viewer pane
- knit_print mode integrates with knitr/pkgdown via htmltools
Each has different requirements. Unifying would lose functionality.

---

## R-Specific

### 78 exports "too high"
**Finding:** Package has too many exports.
**Why false positive:** arframe is a feature-rich pipeline package. Every export is user-facing: `fr_*()` verbs (25), accessors (8), markup helpers (10), config functions (6), validation (4), rendering (3), datasets (11), S3 methods (11). No unused or dead exports found. All have tests and documentation.

### `detect_stat_type()` scalar version still exposed
**Finding:** Should only expose vectorized `detect_stat_types()`.
**Why false positive:** `detect_stat_type()` is `@noRd` (internal). It's useful for single-value debugging and is called from the vectorized version. Both serve different purposes.

### Redundant length checks before seq_along loops
**Finding:** Guard `if (length(x) == 0L)` before `for (x in ...)` is redundant.
**Why false positive:** In R, `for (x in character(0))` is a no-op — the guard is redundant but harmless. Removing adds risk for zero benefit. Some guards also return early with a meaningful value, which is different from a loop no-op.

### `vapply` without `USE.NAMES=FALSE`
**Finding:** Not all vapply calls set USE.NAMES.
**Why false positive:** Most calls that omit it intentionally return named vectors. The few that set `USE.NAMES = FALSE` do so because names would interfere with downstream logic.

### `paste()` vs `paste0()` mixing
**Finding:** Some `paste()` calls without explicit sep.
**Why false positive:** All `paste()` calls that omit `sep` use the default space separator intentionally (e.g., building prose error messages). No instance found where `paste()` was used where `paste0()` was intended.

---

## Failure Modes

### Zero-width column calculation
**Finding:** No validation that width > 0 after measurement.
**Why false positive:** The code already clamps minimum width to 0.5 inches for all-NA columns (line 210 in columns.R) and applies minimum content padding. Zero-width columns cannot occur in practice.

### resolve_cols_expr() error detection "fragile"
**Finding:** Uses string matching (`grepl("not found", ...)`) for error classification.
**Why false positive:** The function handles deferred column evaluation (columns that don't exist yet at call time but will at render time). The string matching covers all known R/rlang/tidyselect error messages. If a new R version changes error text, the fallback is to error immediately rather than silently misclassify — safe default.

### options() undocumented dependency
**Finding:** Check for hidden global options.
**Why false positive:** Package does not use `getOption()` or `options()` anywhere. All configuration is via `fr_config()`, `fr_theme()`, or `_arframe.yml`. No hidden options dependency.
