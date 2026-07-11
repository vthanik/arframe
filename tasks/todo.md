# todo — Listing generator port (explorer list_table → arpillar + arframe, FULL parity)

## 2026-07-10 (latest): inspector polish + date formats + title-replay fix — DONE, COMMITTED

Commits: arframe `b5cbb11` + `13a200f`; arpillar `3dd2632` (golden font
refresh, tabular #33) + `111e409`. Both repos 0/0/0, trees clean, NOT pushed.

- [x] SORT rows grid-aligned `[name 1fr][asc|desc][x]` — pills share one
      right edge.
- [x] STACKED COLUMNS redesign: one-row block header (name + Indent pill
      + x); one-row lines (chips + compact `+` picker); delim/prefix/suffix
      as SELECTS hidden behind a mono glue-preview toggle; hover-only line x.
      **Line 2+ seeds NO glue** (user call — supersedes the paren prefill
      below).
- [x] Indent is PROGRESSIVE engine-side (0/2/4… two spaces per line).
- [x] **Date-format gap CLOSED** (the "Known gap" below): DATE FORMATS
      section → `column_specs$format` → arpillar `.datetime_strftime()`
      (SAS names / mm-dd-yyyy patterns / strftime), applied BEFORE stacks
      so glued cells carry `03JAN2014`-style dates. Live-verified.
- [x] "Identifying label" slot → **"Group column"** + `.SLOT(hint=)` shown
      in the roles pane.
- [x] BUG FIX (data loss): TITLE-section number/title/number_label were
      raw Shiny inputs and replayed the previous output's values onto a
      newly drilled output on rebind — converted to id-less `title_edit`
      onchange posts; regression tests; convention in CLAUDE.md.
      Follow-up: TRANSPOSE `tr_*` selects are the same class, unconverted.
- [x] Demo ADSL gained `RANDDT`; pilot `out005.json` repaired after the
      corruption.

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

## Task 12 review — design-grade inspector polish + verification sweep (2026-07-10)

**Token/polish pass (surgical, tokens only):**
- `--ar-shadow-card` rebuilt to the reference two-layer pair — hairline
  `0 1px 2px` + wide diffuse `0 8px 24px` at low alpha (was two tight
  hairlines, too flat). Lifts every `.ar-panel` card off the canvas.
- New segmented-pill tokens (`--ar-pill-track`/`--ar-pill-pad`/`--ar-pill-radius`)
  applied to `.ar-insp-strip`/`.ar-insp-tab`: the Roles/Options/Filters strip is
  now a LUVAL-style rounded track with an individually-rounded filled active
  segment (was a clipped hard-cornered fill).
- New soft-tinted chip tokens (`--ar-chip-size`/`--ar-chip-radius`/`--ar-chip-fill`)
  applied to `.ar-acc-chip` (one size / one fill).
- Help modal overlay shadow → `--ar-shadow-float` token (dropped a hardcoded
  `0 16px 48px` rgba). Code pane already token-clean (left surgical).

**Gates:** arpillar 0/0/0 (1218 tests). arframe 0/0/0 (1500 tests after +2
regression tests). air clean. Per-file coverage on touched files: fct_export
98.1, mod_paper 96.3, mod_card_filters 95.6, mod_card_listing 95.9, mod_card
93.1, mod_card_options 92.4, utils_atoms 89.3, mod_card_roles 82.5, utils_help
69.2, fct_project 18.7. Sub-95 files are pre-existing Shiny server/UI (observers,
renderUI) not exercised by unit tests; this pass added no untested logic beyond
the two new regression tests. Flagged honestly, not silently accepted.

**Bug found + fixed during real-data eyeball (test-first):** Setup > Paths
serialises an unset dir as `""`; `%||%` only defaults on NULL, so `.emit_programs`
and `.sync_output_dir` spilled `programs/*.R` + `output/*.rtf` into the PROJECT
ROOT instead of the canonical subdirs. Added `.path_or_default()` (blank-safe),
+2 regression tests (red→green). After the fix the slug triplet lands correctly:
`outputs/<slug>.json` + `programs/<slug>.R` + `output/<slug>.rtf`, no root
spillage.

**Real-data screenshot eyeball (cdisc-adam-pilot, .local/screens/task12/):**
6 shots captured + eyeballed — Setup+help modal, Report LoC, drilled Roles,
Options (INCIDENCE-ORDER pill visible on occurrence), Filters, code view (slug
filename + downlit highlighting). All reference-grade; the segmented pill strip
and float-shadowed help modal are the visible wins. No CSS-level defects.
