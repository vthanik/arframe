# todo — Listing generator port (explorer list_table → arpillar + arframe, FULL parity)

## 2026-07-10 (later): DPP-shell rework (GS_CSR_AE_L_002) — DONE

Plan: `docs/superpowers/plans/2026-07-10-arframe-listing-dpp-shell.md`. All
seven requirements landed + live-verified on real ADAE (screenshots eyeballed
against the shell image):

- arpillar: item `@levels` recodes now apply on the LISTING leg
  (`.apply_item_levels()` — display recode before stacks, include=FALSE drops
  rows; Missing sentinel/expected inert). VALUE LEGENDS removed end to end
  (schema, engine, UI); decode footnotes are the user's own Footnotes lines.
- arpillar: id column emits `col_spec(usage="group", group_display="column",
  group_skip)` (followed through stacks via `.stack_output_name()`); the
  "Blank row between blocks" flag governs it; user column_specs override
  cleanly. Golden `_snaps/golden-listing/listing.rtf` REWRITTEN (levels
  recodes + group-skip pinned); baseline goldens untouched.
- arframe: TRANSPOSE + STACKED COLUMNS pickers restricted to id + selected
  list variables (`.listing_selected_items()`); new stack seeds a picker
  line; line 2+ prefills the paren wrap; Stub column header / Total column
  hidden for listings ("Blank row between blocks" kept).
- Gates: both repos 0 err / 0 warn (arpillar 1 known env NOTE); arpillar
  reinstalled. NOTHING COMMITTED (user gates).

**Known gap (decision needed):** date cells render ISO (`2014-01-03`), the
DPP shell wants `03JUN2003`-style. Listing leg has no per-column format knob
(stacks glue raw strings before col_spec formats). Needs a date-format
option (engine `.apply_stacks`/column_specs `format`) if exact-shell dates
are required.

Scope locked (user, 2026-07-10): FULL explorer parity — columns/id roles, sort,
limit, transpose (BDS PIVOT), stacks, value legends, column specs/order,
page_by, plus the universal layout schema. DPP reference: Accrual by Subject
Listing (GS_CSR_DM_L_001) et al. Prior epic (⑤/⑥ richer shapes) is DONE —
see git history / handoff.md.

## Locked design decisions

- `kind = "listing"` (third kind). arframe `.TOC_GROUPS` already has the
  LISTINGS/16.2 group; `.is_figure_type()` routes listings down the TABLE leg
  (build_ard → render_rtf) — zero arframe dispatch changes.
- Slots: `columns` (any type, 1..Inf), `id` (any type, 0..1, leads).
  `page_by` stays a LAYOUT option (arpillar convention), not a slot.
- Listing "ARD" = the collected raw frame (attr `listing_frame = TRUE`), so the
  build_ard → render_rtf seam, arframe's ARD cache, and emit_code all hold.
- Sort/limit/pivot push down to DuckDB (explorer semantics: sort keys need not
  be projected; missing sort cols dropped; pivot dups collapse via agg).
- number_label = "Listing", numbers 16.2.x via presets.
- Golden gate sacred: existing RTF goldens byte-identical throughout.

## Stages — ALL DONE except the two flagged gaps below

- Gap 1 (deliberate): NO UI for `column_specs` / `column_order` (engine honors
  both; explorer's SortableJS column manager maps to the future shared
  dashboard-table atom). Presets/import can still carry them.
- Gap 2 (found live): the SPANNING HEADER layout editor says "Assign a
  treatment variable first." for a listing — the spans editor is arm-oriented;
  the ENGINE spans listing body columns fine (`options$spans` honored,
  unit-tested). Small UI follow-up: feed body columns to the spans editor for
  listings.

- [x] 1. arpillar registry: `.GENERATORS$listing`, `.OPTION_SCHEMA$listing`,
      `.SLOT_REQS$listing`, roxygen.
      → check: option_schema("listing") works; pinned registry tests updated & pass.
- [x] 2. arpillar collect leg: `.build_listing_frame()` in fct_render_ard.R —
      projection (id + columns + page_by + sort keys), filters + population
      WHERE, ORDER BY, LIMIT, DuckDB PIVOT transpose; build_ard branch.
      → check: unit tests (sort, limit, missing-col tolerance, all-missing
      abort, pivot, filters+population).
- [x] 3. arpillar display: render_display listing branch — stacks (multi-line
      glued cells), column_order, legends cell recode.
      → check: unit tests.
- [x] 4. arpillar spec: `.render_listing_spec()` — numeric-right/text-left
      col_specs, column_specs merge, legend auto-footnotes, spans headers,
      page_by subgroup, preset + width pin; render_rtf unchanged.
      → check: unit tests; RTF renders.
- [x] 5. arpillar emit + presets: `.check_emit_type`, `.emit_run` table branch;
      listing presets (16.2.x, "Listing").
      → check: emit/preset tests.
- [x] 6. arpillar gate: test-golden-listing.R (double-render byte-identity +
      snapshot golden); FULL devtools::test — baseline goldens byte-identical;
      devtools::check 0 err/0 warn; air. Reinstall.
- [x] 7. arframe wiring: `.TYPE_ICONS$listing` glyph; LoC LISTINGS folder,
      roles pane (columns/id), scalar options auto-surface; pinned tests.
      → check: arframe tests pass.
- [x] 8. arframe structured option UI: SORT builder, LIMIT, TRANSPOSE, STACKS,
      LEGENDS inspector sections (Filters-pane row pattern; shared picker).
      → check: wire tests + commit round-trip tests.
- [x] 9. Live verify (real data, always): cdisc-adam-pilot, ADSL accrual-style
      listing + ADAE listing, screenshots, RTF export, eyeball.
- [x] 10. Self-critique + handoff.md update.

## Work log

- (start) Explorer engine + arpillar contract fully mapped (two subagent
  reports). DPP listing conventions extracted (scratchpad dpp.txt).

- (done) Stages 1-6 arpillar: registry + fct_render_listing.R (collect/display/
  spec legs) + emit + 3 presets + 43 unit tests + byte-golden
  (_snaps/golden-listing/listing.rtf, DPP accrual shape: stacked id cell,
  legend codes, decode footnotes). check 0/0/1-env. Installed.
- (done) Stage 7 arframe: listing glyph, rows-region slots (id/columns), ghost
  map, .ard_key gains sort/limit/transpose (listing-only, conditional), LoC
  test flipped from future-proof to real. 1362 tests green.
- (done) Stage 8 arframe: R/mod_card_listing.R (SORT / TRANSPOSE / STACKED
  COLUMNS / VALUE LEGENDS editors, 53 wire tests) + two-line integration in
  mod_card_options.R. Legend prefill via arpillar::distinct_values().
- (done) Stage 9 LIVE on cdisc-adam-pilot: ADAE listing built in-app
  (id USUBJID + AEDECOD/ASTDT/AESEV), band + 16.2.1 title, sort commit,
  STALE->Run->READY, legend prefill (MILD/MODERATE/SEVERE). Found+fixed live:
  blank-code legend pairs blanked cells -> .filled_pairs() inert-pair guard +
  regression test; golden unchanged.
