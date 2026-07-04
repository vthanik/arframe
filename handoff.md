# handoff -- 2026-07-04 (session 2: Global-Requirements parity + explorer port)

## Goal

Galley UI (arframe) on arpillar/tabular. This session executed the full 8-stage
"Global Requirements for TLGs + explorer parity" plan
(`~/.claude/plans/users-vignesh-downloads-global-requirem-hashed-glade.md`):
Appendix-I page chrome, explorer-blue theme, levels editor, Total column,
spanning headers, page-by. tabular was NOT modified.

## Current state

- **arframe** @ `9d142ec` (master), clean, `check` **0/0/0**, 1014 tests green.
- **arpillar** @ `7bc1461` (`feat/ui-prereqs`), clean, **0/0/0**, 822 green.
- **tabular untouched.** Nothing pushed anywhere (publish still HELD).
- Eyeball proof on real CDISC pilot: `.local/screens/s1-blue-run-age-peek.png`,
  `s3-*`, `s4-canvas-page.png`, `s5-levels-editor.png`, `s7-spans.png`,
  `s8-pageby.png`.

## Superseded decision (CLAUDE.md updated)

**Decision #7 flipped (2026-07-04):** canvas = tabular's FULL page render
(title block, footnotes, source, running bands all from the ONE spec the .rtf
emits); chrome-free galley is dead. `.rtf` is the org default export --
**no .lst emitter, ever** (user confirmed twice).

## What landed (newest first, per repo)

arframe: `9d142ec` PAGING section + conditional page_by key; `ebd734e`
SPANNING HEADER section; `026b02d` Total toggle + conditional total key;
`7fe9059` LEVELS editor (order/include/display-as/expected, cap 24);
`ee8d01a` canvas flip (page facsimile 1056/816px, .tabular-title un-hidden);
`90e4a2b` Options layout sections (titles/header N/page&output/running bands)
+ `.with_chrome()` token stamping; `14d580e` explorer blue #0378CD + peek
failure-not-memoized fix.

arpillar: `7bc1461` page-by var drops its own row block (live-repro fix);
`74c4aeb` page_by/page_n/banner/panels via tabular subgroup (banner
RIGHT-aligned -- user correction to Appendix I); `b612968` options$spans
bands; `175266a` pooled Total arm + arm order/recode; `8f5bc19`
data_item@levels; `ee51938` .LAYOUT_SCHEMA + Appendix-I chrome
(pagehead/pagefoot, {page}/{npages} field codes, {datetime} rejection,
printable-width pin, header_n) -- existing goldens passed UNCHANGED.

## Key invariants (do not re-litigate)

- **No layout options = byte-identical legacy render** (no preset attached);
  pinned by "no options => old golden" tests. New goldens:
  `pagechrome.rtf`, `pageby.rtf`.
- **Determinism gate:** arpillar REJECTS `{datetime}`/`{program}` in bands
  (classed error); arframe stamps literals via `.with_chrome()` (mod_paper.R)
  at canvas/.rtf/export legs; `{page}`/`{npages}` = Word field codes.
- **ARD key discipline:** `total`/`page_by` conditionally appended ONLY when
  set -- legacy hashes stable (regression-pinned). Level edits, spans, chrome
  = CHEAP; total/page_by = HEAVY (Run-gated) with zero new stale plumbing.
- **Levels:** `data_item@levels` = list(value, display, include, expected);
  empty = legacy bytes. Arms reuse it (Total pinned last, never synthesized).
- Percent denominators are population-N: level exclusion never moves other
  cells (test-pinned).

## Deliberate cuts (add when asked)

- page_n "banner" placement (banner shows label only; N modes: off/headers).
- Ranks-pane rebase onto a shared sortable atom (works as-is).
- Occurrence page_by (ignored, documented in layout_schema docs).
- Levels editor hard cap 24 observed levels, no paging.
- No auto-seeded Appendix-I pagehead (protocol id is not modeled in the
  report; user types it in RUNNING HEADER or we add report metadata later).

## What to do next (user steering)

1. Open an emitted .rtf in Word (Table 14.1.1 with chrome + page-by) --
   final Appendix-I line-by-line check (machine checks all pass).
2. IBM Plex font work: still waiting on the SEPARATE tabular session
   (prompt handed over previously); arpillar/arframe kept font_family
   generic (mono/sans/serif) deliberately.
3. Prior roadmap: mockup pieces C (richer OUTLINE) -> A (activity bar) ->
   B (canvas per-output tabs).

## Operating constraints (unchanged)

`\uXXXX` escapes; em-dash in UI text; check 0/0/0 both repos before commit;
eyeball on real CDISC (launcher :7788, kill port between drives); RTF byte
determinism is the golden gate; never push without per-session approval;
no AI attribution.
