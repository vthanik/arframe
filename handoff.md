# handoff — arframe inspector panes (Tasks 11/12 + run semantics)

**Branch state (2026-07-02):** `feat/inspector-panes` (worktree
`.claude/worktrees/inspector-panes`, branched off `master` @ f21be40)
carries the complete inspector build: Options pane (Task 11), Filters pane
(Task 12), and the locked STALE run semantics (decision #8). Four commits,
staged test-first, real-data eyeball sweep done. Do NOT push without
explicit approval.

## STATUS: Tasks 11 + 12 + run semantics COMPLETE, gate green

Executes the prior handoff's directive ("build the remaining inspector
panes the v5 way: worktree off master, staged, test-first, real-data
sweep, merge back when green").

- `devtools::check` = **0 errors / 0 warnings / 0 notes**; 763 tests pass.
- Coverage (95% per-file house bar): mod_card_options **95.6**,
  mod_card_filters **95.8**; fct_store 99.5, mod_paper 96.8,
  mod_contents 99.2. Pre-existing debt unchanged: mod_card_roles 66.1
  (chip `task_d10a434e` open), app.R 67.4, utils_atoms 75.4, mod_card 94.7.
- Real-data sweep on the CDISC pilot mounts (decision #9): demographics
  typeset (Placebo N=86 / High 84 / Low 84), cheap decimals edit re-renders
  live, role edit → STALE panel + TOC stamp → Run re-typesets, km options
  pane (AXES/SERIES/LEGEND, engine defaults preselected), Safety-population
  preset → committed row + `254 of 254` live count + paper Population tag.
  Screenshots: `.local/screens/v5/11-card-options.png`,
  `12-card-filters.png`, `12b-filters-run.png`, `12c-filters-stale.png`,
  `12d-stale-run-retypeset.png`.

## What landed (4 commits on feat/inspector-panes)

1. **Options pane** (`R/mod_card_options.R` + mirror test): TITLE section
   (editable TLF number + Table/Figure/Listing label select + title),
   FOOTNOTES line editor (line 1 = population, tagged), option rows
   generated from `arpillar::option_schema(type)` — int (numeric-text,
   invalid input → inline message, never committed), choice (radios),
   flag, text (empty removes), numvec (parsed + sorted), levels (sortable
   x_order seeded from `distinct_values`, only when x is filled).
   **Default-elision:** a value equal to the engine default REMOVES the
   key. Ranks tab keeps the Task-11 placeholder text + `coming` tag.
2. **Run semantics** (decision #8, `fct_store.R`/`mod_paper.R`/
   `mod_card.R`/`mod_contents.R`/`utils_atoms.R`): `.ard_key()` factored
   out of `cached_ard()` as the cheap/heavy oracle. `update_object()`
   marks `rv$stale` when the key moves on an output that was READY before
   the edit — the paper renders the STALE notice (full page shell kept),
   the TOC stamps STALE (broken outranks), Run clears flags + memos and
   re-typesets. Exempt: cheap edits (options/title/footnotes → live),
   draft fills (ghost→table payoff), broken-output fixes (error→fixed).
3. **Filters pane** (`R/mod_card_filters.R` + mirror test): Safety
   population / Full set presets (safety only when SAFFL exists), builder
   rows (column rich picker · exact engine op set · per-type value
   controls · include-missing · remove) in store-side `rv$filter_draft`
   seeded on selection change; only COMPLETE rows commit (incomplete rows
   wear an honest badge — the engine is drop-tolerant); live
   `filter_count` matched-of-total debounced 300ms; paper Population tag
   (own `filters` region, innermost delegation wins).
4. **Sweep fixes** (testServer-invisible, caught on real data):
   `suspendWhenHidden = FALSE` for the pane outputs (tab flip is a pure
   client-side class change — the mod_data lesson, now also pinned by
   body-deparse regression tests); filter column picker re-seed packed
   with SQL type (role-type packing matched nothing and the bind-post
   reset the committed row); selectize `item` renderer (closed picker
   shows the bare name); filter rows wrap.

## Plan deviations (deliberate, keep)

- **No separate Population input in the TITLE section** (plan line 1181):
  it would alias footnotes[1] with the footnotes editor's first row in the
  SAME pane; the footnotes editor's tagged line 1 is the single surface.
- **Options pane never narrows on `rv$region`** (v5 tabs supersede the
  floating-card model the plan predates); a `title` region click focuses
  the Title input instead. Roles still narrows (its three regions name
  slot subsets).
- **Editable number+label** per the addendum (L704) supersedes the older
  "read-only for v1" (L1181).
- **`.filter_complete/.filter_normalize`** pin the committed predicate to
  the engine's minimal shape so a hand-built safety row is `identical()`
  to the preset's.

## Known ceilings (ponytail-noted, not bugs)

- Undo/redo while the Options pane is open can leave control DISPLAY
  values stale until the next redraw trigger (store stays authoritative).
- Filters observer pool is a fixed 12-row registration; `+ Add filter`
  hides at the cap.
- The km_os/km_pfs presets name `TRT01P`; the pilot ADTTE carries
  TRTP/TRTA, so a km preset on real ADTTE shows the error summary until
  roles are re-assigned — the honest static-oracle-gap path, pre-existing.
- `arframe()` builds ONE store at app construction (all browser sessions
  share it) — fine for the single-user dev tool, noted while replaying
  screenshots.

## Next steps

1. Merge `feat/inspector-panes` → `master` (ff), delete branch + worktree
   (the standing pattern; merge may already be done — check `git log`).
2. Then (plan order): QC sheet (Task 15), async mirai export (Task 16 —
   mirai NOT installed, install first), a11y/responsive sweep (Task 17).
   Deferred per spec: ⌘K palette (v1.1), Actions/Rules, dark skin.
3. Integration when approved: push arpillar `333b8ce` (arframe CI cannot
   resolve `Remotes: vthanik/arpillar` until it is), then arframe.

## Key seams (verified this session, additive to v5's)

- `option_schema(type)` df: key/label/kind/default/choices; kinds =
  int/text/choice/flag/numvec/levels (km is the richest; occurrence keeps
  population+hier_sort — population is a plain text row for now).
- Engine filter predicate: `list(column, op, value, include_missing)`;
  op set `==, !=, %in%, >, <, >=, <=, is.na, not.na`;
  `arpillar:::.filter_one` silently DROPS incomplete/unknown predicates
  (that drop-tolerance is why the pane gates commits on completeness).
- `.ard_key(object)` (fct_store.R) = the memo key AND the cheap/heavy
  oracle; options are excluded by design.
- testServer gotcha: an outer variable named `id` is shadowed by the
  module server's own `id` arg inside the testServer expr (cost an hour;
  fixture ids are now named `out_id`).
- observeEvent + `ignoreInit=TRUE` swallows the first REAL event when no
  flush ran between observer creation and that event (testServer) — the
  no-op commit guard replaces ignoreInit in both new modules.
- `outputOptions()` is not introspectable under testServer's mock; the
  suspendWhenHidden regression tests pin the call via
  `deparse(body(<server>))` instead.

## Conventions in force

Store-first state (never DOM), classed cli conditions, air format hook,
test-first per stage, `.demo_catalog()` fixtures for tests but REAL pilot
data for any visual claim (decision #9). Eyeball verification is binding —
screenshot the running app for any UI claim (Preview MCP; `.claude/
launch.json` in the main repo runs `arframe(folders = <pilot dirs>)` on
port 7788; kill the port between restarts; chromote replay scripts in the
session scratchpad produced the .local/screens record). No AI attribution
in commits. No push without explicit approval.
