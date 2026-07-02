# handoff — arframe v5 galley build

**Branch state (2026-07-02, post-merge):** all v5 work consolidated onto
`master`. `feat/galley-v5` (worktree) and `feat/galley-ui` were fully merged
fast-forward and deleted; the worktree at `.claude/worktrees/galley-v5` is
removed. Do NOT push without explicit approval.

## Goal

Implement the LOCKED v5 UI (CLAUDE.md binding decisions #7–#9): galley
canvas (no page cosplay), docked inspector, explorer Data mode,
Data|Report segmented toggle, real CDISC pilot data for all verification.
Reference design: the v5 `show_widget` mockup from the 2026-07-02 session
(sources tree + manage toolbar + explorer table; inspector with
Roles/Options/Filters/Ranks + Run ⌘↵/.rtf/`</>` footer + telemetry;
margin-mark region hover).

## STATUS: all 7 stages landed, eyeball-verified on real data, MERGED

All stages (S1 frame → S7 gates) complete and green. Two post-merge bug
fixes landed after the user exercised the report path on folder-mounted
data (`5984c8e`): add-output errored because folder-library mounts
conflicted with the engine's WORK-only `@dataset` resolution (fix: mount
everything into WORK; folder = arframe-side provenance in
`store$sources`), and the Roles pane crashed on `switch(NULL)` when no
region was focused (fix: NULL/non-scalar guard in `.region_slots`).
`c7b8333` fixed an ASCII `--` in the Add-output recommendation label to a
`—` em-dash escape (R-CMD-check ASCII rule).

arpillar got `unregister_dataset()` (`333b8ce`, committed + locally
installed via `R CMD INSTALL`; **NOT pushed** — arframe CI cannot resolve
`Remotes: vthanik/arpillar` until it is).

`devtools::check` = 0 errors / 0 warnings / 1 benign NOTE (clock). Full
suite green. Real CDISC ADaM (15 parquet) + SDTM (31 xpt) pilot folders
mount and render end-to-end: report ghost + inspector, data explorer 46
rows, drill grid on ADAE 1191×55 with typed column picker, mode toggle,
Add output → demographics typeset (Placebo N=86 / Xanomeline High Dose
N=84), telemetry `adsl · 254 of 254 records`. Screenshots in
`.local/screens/v5/`.

**Coverage (per-file, 95% house target):** fct_export 97.5, fct_store 99.5,
mod_frame 100, mod_paper 96.6 (clear); mod_card 94.6, mod_data 94.1 (short by
the interactive shinyFiles import-parse handlers); mod_card_roles 59.7
(PRE-EXISTING Task-10 debt — spawn_task chip `task_d10a434e` open for it).

## Next steps (user picked: Options + Filters panes)

1. **Inspector Options pane** (plan Task 11): edit `options$number` /
   `number_label` / title / footnotes / decimals on the selected output;
   writes go through the store (never DOM), cheap edits re-render live.
2. **Inspector Filters pane** (plan Task 12): population/subset filters
   with live `filter_count` telemetry; heavy changes mark the proof STALE,
   Run re-typesets.
3. Then: QC sheet (Task 15), async mirai export (Task 16, mirai not
   installed), a11y/responsive sweep (Task 17), STALE-stamp run semantics,
   ⌘K palette (v1.1).
4. Integration when approved: push arpillar `333b8ce`, then arframe.

## Key seams (verified, do not re-derive)

- `render_spec(ard, object)` → tabular spec (titles+footnotes at
  fct_render_table.R:648); `as.tags()` = screen; `emit()`/`render_rtf` =
  file. ONE spec, three surfaces.
- `render_rtf(ard, object, path)`; `render_figure_rtf(con, object, path)`;
  `emit_code(con, object, path=NULL)`; `emit_report_code(con, report,
  path=NULL)`; `filter_count(con, name, filters, library)` →
  `list(matched, total)`; `unregister_dataset(con, name)` (new).
- Engine resolves an output's `@dataset` in WORK ONLY — never register
  datasets into per-folder libraries; folder provenance lives in
  `store$sources` / `store$kinds` (keyed by dataset NAME).
- tabular emits stable classes: tabular-title/-caption/-table/-footnote/
  -doc; per-render `#tabular-<hash>` scoped style → arframe.css overrides
  need `!important` (settled, don't churn).
- Data-mode outputs need `outputOptions(output, ..., suspendWhenHidden =
  FALSE)` AFTER the outputs are defined — the mode switch is a pure
  client-side class flip the server never sees.
- testServer quirk: `output$rtf` on a downloadHandler RUNS the download
  and returns the temp path (assert basename + content).
- JS contract test (test-mod_paper.R) greps arframe.js for literal
  strings — update it when renaming handlers.
- Non-ASCII in R strings = `\uXXXX` escapes (`"⌘↵"`, `"·"`,
  `"—"`); roxygen `[X | Y]` parses as a broken Rd link.

## Conventions in force

Store-first state (never DOM), classed cli conditions, air format hook,
test-first per stage, `.demo_catalog()` fixtures for tests but REAL pilot
data for any visual claim (decision #9). Eyeball verification is binding —
screenshot the running app for any UI claim (Preview MCP, launch.json in
main-repo `.claude/`, port 7788; kill the port between restarts). No AI
attribution in commits. No push without explicit approval.
