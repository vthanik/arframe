# handoff — arframe v5 galley build

**Worktree:** `.claude/worktrees/galley-v5`, branch `feat/galley-v5` (off
`feat/galley-ui` @ `c638d51`). Main checkout untouched. Do NOT push.

## Goal

Implement the LOCKED v5 UI (CLAUDE.md binding decisions #7–#9): galley
canvas (no page cosplay), docked inspector, explorer Data mode,
Data|Report segmented toggle, real CDISC pilot data for all verification.
Reference design: the v5 `show_widget` mockup from the 2026-07-02 session
(sources tree + manage toolbar + explorer table; inspector with
Roles/Options/Filters/Ranks + Run ⌘↵/.rtf/`</>` footer + telemetry;
margin-mark region hover).

## STATUS: all 7 stages landed + eyeball-verified on real data (2026-07-02)

Branch `feat/galley-v5`: 6 commits (`2ef5722`→`61c7c7a`). arpillar got
`unregister_dataset()` (`333b8ce`, committed + locally installed; NOT pushed).
`devtools::check` = 0 errors / 0 warnings / 1 benign NOTE (clock). Full suite
green. Real CDISC ADaM(15 parquet)+SDTM(31 xpt) pilot folders mount and render
end-to-end (report ghost+inspector, data explorer 46 rows, drill grid on ADAE
1191×55 with typed column picker, mode toggle). Screenshots in
`.local/screens/v5/`.

**Coverage (per-file, 95% house target):** fct_export 97.5, fct_store 99.5,
mod_frame 100, mod_paper 96.6 (clear); mod_card 94.6, mod_data 94.1 (short by
the interactive shinyFiles import-parse handlers + pre-existing region
fallbacks); mod_card_roles 59.7 (PRE-EXISTING debt from the Task-10 wip, not
this session — flag for follow-up).

**Not pushed / follow-ups:** push arpillar (`unregister_dataset`) before CI;
mod_card_roles coverage; QC sheet (Task 15) + async export (Task 16) still
placeholder/sync; Options/Filters/Ranks inspector panes are "coming" stubs
(S3 wired the tabs, not their content — that's plan Tasks 11/12).

## Stage map (tasks #1–#7 in TaskList) — ALL COMPLETE

- [x] **S1 frame** (`2ef5722`): [Data|Report] segmented toggle (idempotent
  segments; QC keeps quiet-toggle), store-owned collapse
  (`rv$rail_collapsed`/`insp_collapsed`, `toggle_rail()/toggle_insp()`),
  `ar-collapse` message → workspace classes, delegated
  `[data-ar-collapse]` JS. Fixed `ar-mode` handler class-wipe bug.
- [x] **S2 paper** (`2874f08`): running head + fit/page toolbar DELETED;
  artifact content-hugging (`width: fit-content`); margin-mark chips =
  pure CSS `::before` off `data-ar-region`; JS `shiny:value` hook
  annotates tabular's OWN emitted structure (thead→columns, tbody→rows,
  .tabular-footnote→footnotes); on-screen title dup fixed
  (`.tabular-doc .tabular-title{display:none}` — spec title is for the
  export leg). arpillar spec carries NO page chrome today;
  `chrome_onscreen="off"` documented as the future knob (comment in
  mod_paper.R).
- [x] **S3 inspector** (`1850772`): mod_card = docked fixed-260px panel.
  Tab stack (`.INSP_TABS`, panes all mount, `ar-insp-tab-*` class flip via
  message), `rv$insp_tab` + `rv$run_nonce` in store, `open_card()` routes
  region→tab + un-collapses. Footer: Run (drops `ard::` memos, bumps
  nonce — paper renderers bind to it), `.rtf` downloadHandler
  (render_rtf / render_figure_rtf via `.output_slug()`), code btn
  (`input$code`, UNWIRED — S4). Telemetry: `arpillar::filter_count(con,
  dataset, filters)` → `list(matched, total)`. Float/pin/Esc JS + CSS
  removed; `close_card()`/`toggle_pin()` still in fct_store.R (unused —
  prune when QC keyboard work lands or in S7).
- [ ] **S4 code view**: `</>` swaps canvas → read-only mono panel with
  `arpillar::emit_code(con, object)` (guaranteed parse()-clean), filename
  bar + Copy + Download .R + Close. Suggest: `rv$code_view` flag; panel in
  mod_paper (desk swap) or new mod_code; downloadHandler for the .R.
- [ ] **S5 Data mode**: new mod_data per decision #8 (sources multi-folder
  tree, explorer detail table NAME/FOLDER/KIND/COLS/ROWS/SIZE/STATUS=LAZY/
  MODIFIED via file.info + catalog; toolbar Filter/View data/Import file/
  Import folder/Delete; dblclick OR View → grid + column picker; Delete
  unmounts). Dev mount: `/Users/vignesh/projects/data/cdisc-adam-pilot` +
  `cdisc-sdtm-pilot` (decision #9). shinyFiles for pickers.
- [ ] **S6 export package**: zip = outputs/*.rtf (ready only) +
  programs/*.R (emit_code) + run-all.R (emit_report_code) + report.json
  (report_to_json) + manifest.csv. Honest toast: n ready · n skipped.
- [ ] **S7 real-data eyeball + gates**: app on real pilot folders,
  screenshot every surface vs the v5 mockup, DOM-measure claims;
  document+test+check 0/0/0; covr ≥95%/file on new files.

## Key seams (verified, do not re-derive)

- `render_spec(ard, object)` → tabular spec (titles+footnotes at
  fct_render_table.R:648); `as.tags()` = screen; `emit()`/`render_rtf` =
  file. ONE spec, three surfaces.
- `render_rtf(ard, object, path)`; `render_figure_rtf(con, object, path)`;
  `emit_code(con, object, path=NULL)`; `emit_report_code(con, report,
  path=NULL)`; `filter_count(con, name, filters, library)` →
  `list(matched, total)`; `catalog(con)` exists (shape unchecked).
- tabular emits stable classes: tabular-title/-caption/-table/-footnote/
  -doc; per-render `#tabular-<hash>` scoped style → arframe.css overrides
  need `!important` (settled, don't churn).
- testServer quirk: `output$rtf` on a downloadHandler RUNS the download
  and returns the temp path (assert basename + content).
- JS contract test (test-mod_paper.R ~l.495) greps arframe.js for literal
  strings — update it when renaming handlers.

## Conventions in force

Store-first state (never DOM), classed cli conditions, air format hook,
test-first per stage, `.demo_catalog()` fixtures for tests but REAL pilot
data for any visual claim (decision #9). No AI attribution in commits. No
push without explicit approval.
