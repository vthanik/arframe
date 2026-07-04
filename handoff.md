# handoff -- 2026-07-04 (end of session 2: Global-Requirements parity DONE)

## Goal

Galley UI (arframe) on arpillar/tabular. Session 2 executed and FINISHED the
8-stage "Global Requirements for TLGs + explorer parity" plan
(`~/.claude/plans/users-vignesh-downloads-global-requirem-hashed-glade.md`),
including the end-to-end Appendix-I verification. **The next session starts
mockup piece C** (see "Next in queue").

## Current state

- **arframe** @ `60dc93b` (master), clean, `check` **0/0/0**, 1014 tests green.
- **arpillar** @ `4fb6225` (`feat/ui-prereqs`), clean, **0/0/0**, 824 green.
- **tabular untouched** this session. Nothing pushed anywhere (publish HELD --
  no GitHub repos exist yet for arpillar/artoo/datasetviewer/arframe).
- **Final proof:** `.local/screens/t-14-1-1-full.rtf` -- kitchen-sink Table
  14.1.1 (chrome bands, Total, arm reorder/recode, level recode + expected
  ASIAN zero row, Xanomeline span, SEX page-by with RIGHT-aligned banners +
  per-page Ns, stamped Program/datetime footer). 18/18 Appendix-I greps pass,
  byte-deterministic. **User still owes the Word eyeball** (font
  substitution / page fit) -- generator script: /tmp is gone, rebuild from
  this file's "Verification recipe" below if needed.
- Eyeball screenshots: `.local/screens/s1-blue-run-age-peek.png`, `s3-*`,
  `s4-canvas-page.png`, `s5-levels-editor.png`, `s7-spans.png`, `s8-pageby.png`.

## What landed (both repos, this session)

arframe (newest first): `60dc93b` handoff; `9d142ec` SUBGROUP/PAGE BY section
+ conditional page_by ARD key; `ebd734e` SPANNING HEADER section; `026b02d`
Total toggle + conditional total key; `7fe9059` LEVELS editor
(order/include/display-as/expected, cap 24); `ee8d01a` canvas flip (tabular's
full page render, 1056/816px sheet); `90e4a2b` Options layout sections
(titles/header N/page&output/running bands) + `.with_chrome()`; `14d580e`
explorer blue `#0378CD` + peek failure-not-memoized fix.

arpillar: `4fb6225` non-contiguous span drops forgivingly (live-repro);
`7bc1461` page-by var drops its own row block (live-repro); `74c4aeb`
page_by/page_n/banner/panels via tabular subgroup, banner RIGHT-aligned;
`b612968` options$spans bands; `175266a` pooled Total arm + arm order/recode;
`8f5bc19` data_item@levels; `ee51938` .LAYOUT_SCHEMA + Appendix-I chrome.

## Binding decisions from this session (CLAUDE.md already updated)

- **Canvas = tabular's FULL page render** (decision #7 FLIPPED 2026-07-04).
  A ready table gets NO arframe title/source markup (double-print);
  ghost/stale/error paths + figures keep theirs. `.tabular-title` CSS
  un-hidden (was display:none in the v5 dup-fix -- inverted).
- **Export = .rtf ONLY.** Org updated their standard; **no .lst emitter,
  ever** (user confirmed twice -- do not re-raise).
- **Subgroup banner is RIGHT-aligned** (user correction to Appendix I's
  centered-looking mock) -- engine-side via `cells_subgroup_labels()`.
- **Primary color = explorer blue `#0378CD`** (from ~/projects/r/explorer,
  theme.R:19 there).

## Key invariants (do NOT re-litigate)

- **No layout options = byte-identical legacy render** (no preset attached);
  pinned by tests. Goldens: `pagechrome.rtf`, `pageby.rtf` + originals
  (which passed UNCHANGED -- zero regen this whole session).
- **Determinism gate:** arpillar REJECTS `{datetime}`/`{program}` in
  pagehead/pagefoot (classed `arpillar_error_input`); arframe stamps
  literals via `.with_chrome()` (mod_paper.R) at canvas/.rtf/export legs;
  `{page}`/`{npages}` pass as Word field codes.
- **ARD key discipline:** `total`/`page_by` appended to `.ard_key()` ONLY
  when set (legacy hashes stable, regression-pinned). Level edits, spans,
  chrome = CHEAP (live re-render); total/page_by = HEAVY (auto Run-gated
  via the existing key-diff in `update_object` -- no bespoke stale code).
- **Levels model:** `data_item@levels` = list of
  `list(value, display, include, expected)`, list order = display order,
  empty = legacy bytes. Arms reuse it (Total pinned last; an unobserved arm
  is NEVER synthesized; non-contiguous span bands DROP, the fix is arm
  reorder). Percent denominators are population-N: exclusion never moves
  other cells (test-pinned).
- Serializer: `levels` is an additive field, NO schema bump (decoder `%||%`
  defaults it; v1-JSON regression test pinned).

## Deliberate cuts (add only when asked)

- page_n `"banner"` placement (modes now: off / headers).
- Occurrence page_by (silently unsupported, noted in layout_schema docs).
- Levels editor caps at 24 observed levels (flat cap, no paging).
- Ranks-pane rebase onto a shared sortable atom (works as-is; skipped).
- Auto-seeded `Protocol:` pagehead (protocol id is NOT modeled on report;
  user types it in RUNNING HEADER, or add report metadata later).

## Next in queue (the reset session starts HERE)

**Mockup piece C -- richer OUTLINE** (from the pre-font-detour steering,
order C -> A -> B):
- CONTENTS tree grouped into TABLES / FIGURES / LISTINGS sections.
- Colored status as dot + WORD (not dot alone) per row.
- Keep: ICH numbers mono, per-generator icons, chevron-collapse strip.
- Files: `R/mod_contents.R`, `inst/www/arframe.css` (TOC block), tests in
  `test-mod_contents.R`. Status colors already tokenized: `--ar-ready`,
  `--ar-draft`, `--ar-error`.

Then **A** (left activity bar -- ASK the user which of the 5 mockup icons are
real before building), then **B** (canvas per-output tabs -- CONFIRM scope
first, biggest piece).

Also pending externally: the **IBM Plex tabular session** (prompt was handed
over a session ago). arpillar/arframe deliberately kept `font_family`
generic (mono/sans/serif); when tabular lands recognition, follow the OLD
handoff's step 2/3 (preset font_family + figure base family + golden regen).

## Verification recipe (kitchen-sink RTF, if it needs rebuilding)

Object: summary on real ADSL (`~/projects/data/cdisc-adam-pilot`), options:
number 14.1.1, titles "Randomized Subjects", header_n "(N={n})",
total TRUE, spans list(label "Xanomeline", cols = the two Xanomeline arms),
page_by SEX + page_n headers + banner "Sex: {SEX}", pagehead
Protocol/Draft/Page {page} of {npages}, pagefoot "Program: {program}" /
"{datetime}"; treatment levels reorder Placebo, Xano High (display "Xano
High"), Xano Low (span needs contiguous arms!); RACE levels with recodes +
expected ASIAN. Render via `arframe:::.with_chrome(arframe:::.with_source(obj))`
then `arpillar::render_rtf` -- grep the 18 Appendix-I markers.

## Operating constraints (unchanged)

- `\uXXXX` escapes for non-ASCII in R strings; em-dash `—` in UI text
  (never `--`); tokens.css vars only.
- Gate: `devtools::check` 0/0/0 BOTH repos before commit (use
  `_R_CHECK_SYSTEM_CLOCK_=0` -- the NTP probe NOTE is environmental).
- After arpillar changes: `R CMD INSTALL --no-docs .` or arframe tests run
  against the stale installed copy.
- Eyeball harness: `.local/screens/launcher.R` (:7788, real CDISC mounts) +
  chromote driver; kill port 7788 between drives (store is per-process);
  in driver JS, mind quote collision (use double quotes inside `click('...')`).
- testServer suspend pins grep `deparse(body(<server>))`, not source files.
- Never push without explicit per-session approval. No AI attribution.
- `handoff.md` is TRACKED (committed, in `.Rbuildignore`) -- overwrite +
  commit, matching prior handoff commits.
