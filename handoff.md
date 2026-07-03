# handoff — arframe

Submission-native clinical report builder: the **Galley** UI (thin bslib Shiny
shell) on the **arpillar** engine. `arframe()` is the only export; everything
else is `@noRd`. Binding design spec + plan live in `docs/superpowers/specs/`
and `docs/superpowers/plans/`.

## STATUS (master @ `a060149`, 2026-07-03, evening session)

**The inspector full-depth redesign + RTF-parity session is DONE and merged.**
arframe `devtools::check` = **0/0/0** (929 tests green); arpillar
(`feat/ui-prereqs` @ `eb3af37`) = **0/0/0** (740 tests green). **Nothing is
pushed** — publish decision still HELD (see Integration).

Eyeballed on the real CDISC pilot mount (decision #9): 20/20 chromote
assertions, screenshots in `.local/screens/ws-0*.png`, RTF gates green.

## What this session shipped (user-reported issues → root causes → fixes)

1. **"Roles tab is empty"** — `.region_slots()`'s switch fallback returned ZERO
   slots for any stale non-roles region (e.g. `"title"` after a rename jump);
   plus the roles output was the only pane missing `suspendWhenHidden = FALSE`.
   Fixed: tab clicks clear `rv$region`; unknown regions fall back to ALL slots;
   a roles-empty narrowing (stale `"axes"` on a table) falls back too; suspend
   contract pinned like the sibling panes.
2. **Footnote "population" badge** — hardcoded row-1 span; deleted (+ CSS).
   Line 1 keeps its population-subtitle convention by position alone.
3. **RTF ≠ screen** — arpillar's `.rtf_titles()` dropped the number line +
   subtitle, `.rtf_footnotes()` dropped the source line, and `.apply_cols()`
   passed no widths (tabular autofit is EXACT-fit over NBSP-padded cells →
   Word's font substitution emergency-broke mid-cell). Fixed engine-side:
   title block = number line + title + footnotes[1]; footnotes += verbatim
   `options$source` (arframe stamps the date via `.with_source()` /
   `.report_with_source()` at export time — engine stays byte-deterministic);
   `.pin_col_widths()` pins every column to `tabular::as_grid()`'s own
   resolved width + 0.05in slack (slack shrinks to spare printable room).
   Goldens regenerated deliberately once. Both export legs (per-output .rtf
   download AND the export-package async/sync paths) carry the source line.
4. **"Rail is 5% of the depth"** — all four panes rebuilt on REAL engine
   capability:
   - **Roles**: SOURCE row (dataset + `detect_structure` + dims), cardinality
     hints from `generator()$slots` min/max, CDISC labels everywhere (see
     seam below), per-row **variable peek** (label editor = cheap commit;
     treat-as measure/category toggle for numeric cols = heavy commit;
     live distribution — `value_counts()` bars / `column_range()` +
     `column_precision()`), oracle problems strip, action-directing empty
     state. `.ard_key` no longer hashes labels (relabel renders live);
     `.roles_digest` now includes label + role_type.
   - **Options**: footnote drag-reorder (grips, index re-keyed after drop),
     decimals stepper + derived-precision hint, **stats membership+order
     editor** (new engine `stats` option, default-elided so goldens never
     churn), ordering keys filtered out via `.RANKS_KEYS`.
   - **Filters**: one chip per `*FL` category flag (CDISC map SAFFL/ITTFL/
     EFFFL/FASFL/PPROTFL/RANDFL/ENRLFL/COMPLFL; bare "<FL> = Y" fallback) via
     one shared `preset_flag` input; humanized operator labels over the EXACT
     engine values; paper Population tag recognizes any canonical flag.
   - **Ranks**: real module (`mod_card_ranks.R`) replaces the stub —
     summary/crosstab row-block order (SAME `.reorder_slot()` helper as
     Roles), occurrence `hier_sort` radios (default-elided), line/box
     relocated `x_order`, km honest empty state.
5. **Engine seams added (arpillar)**: `data_items()` now fills `label` from
   the artoo Dataset-JSON parquet sidecar (footer-only read, forgiving; the
   REAL pilot data already carries full CDISC labels); new export
   `column_range()` (min/median/max pushed down); `options$stats` for
   summary; `.rtf_source()`; width pinning. The demo catalog now stamps
   labels through the same sidecar seam.
6. **Robustness from the eyeball**: `input$region` drops non-string payloads
   (a malformed post used to CRASH the session via `open_card`); stepper
   input width outranks the generic 88px rule; population chips wrap (pilot
   ADSL has NINE flags); SOURCE dims never break mid-value.

## Gotchas learned this session (do not relearn)

- **tabular width mechanism**: autofit resolves EXACT-fit inches from AFM
  metrics over the decimal-padded (NBSP — never a legal break) cells;
  `tabular::as_grid(spec)@metadata$cols[[nm]]@width` is the resolved surface
  (a unit test pins its shape). `cols()` WARNS if you re-pass the grid's
  resolved col_specs (group_display baggage) — build fresh `col_spec()`s;
  an unset align resolves to NA (pass NULL). Content mode never shrinks
  below natural width; it warns and overflows — don't fight it.
- **`.mount_folder` names datasets `toupper(file stem)`**; `report_from_json`
  accepts a path; `arframe(project=)` + folders is the clean way to seed an
  eyeball session (`.local/screens/launcher.R` + `driver.R` are reusable).
- **The store is per-PROCESS**: a second chromote/driver run against the same
  app sees the first run's selection/peeks. Restart between drives.
- **`input$region` contract is a plain string** (jump links, ghosts).
- **testServer**: `outputOptions` pins must grep `deparse(body(<server>))`,
  not the source file (R/ is absent under installed check).
- The picker pack format is now `NAME\x1fTYPE\x1fLABEL` — the filters row
  re-seed MUST pack identically or selectize resets the row (regression
  test pins it).

## NEXT STEPS

- **Mockup roadmap (user steering, from the previous session)**: C (richer
  OUTLINE: grouped TABLES/FIGURES/LISTINGS, colored status dot + word),
  A (left activity bar — ask which of the 5 icons are real), B (canvas
  per-output tabs — biggest, confirm scope first). Order C → A → B.
- Possible inspector follow-ups (all engine-gated, none started):
  `total_column` option (ARD arm pooling — heavier), dataset switching
  in-place on the SOURCE row (mechanically trivial via update_object; roles
  keep stale names until validate), per-row filter match counts.
- `.ar-insp-slim` CSS is still vestigial; a few section-08 selectors remain
  dead (harmless) — later sweep.

## Integration / push — NOTHING PUSHED (decision pending)

Unchanged from last session: no GitHub repos exist for
arpillar/artoo/datasetviewer/arframe (only `vthanik/tabular`). Publishing is
a first-time multi-repo publish; user chose **HOLD**. Do NOT create repos or
push without an explicit per-session go-ahead + visibility choice + the
public-surface hygiene denylist check.

## How to run it (eyeball, real CDISC data — decision #9)

```r
devtools::load_all("/Users/vignesh/projects/r/arframe")
arframe(folders = c(
  "/Users/vignesh/projects/data/cdisc-adam-pilot",
  "/Users/vignesh/projects/data/cdisc-sdtm-pilot"
))
```

Headless: `.local/screens/launcher.R` (seeds Table 14.1.1 on the real ADSL,
pre-emits the source-injected RTF) + `.local/screens/driver.R` (20 chromote
assertions + per-tab screenshots). Kill port 7788 + relaunch between drives.

## Conventions in force

Unchanged: `\uXXXX` escapes for non-ASCII in R strings; em-dash `—` (never
`--`) in UI text; tokens.css vars only; observer-pool + `Date.now()` nonce
patterns; `air format` hook; gate `devtools::check` 0/0/0; no AI attribution;
**never push without explicit per-session approval**.
