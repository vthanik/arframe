# handoff -- 2026-07-04

## Goal

Submission-native clinical report builder: the **Galley** UI (thin bslib Shiny
shell) on the **arpillar** engine, which renders tables/figures via **tabular**.
`arframe()` is the only export; everything else is `@noRd`. Binding design spec +
plan: `docs/superpowers/specs/` and `docs/superpowers/plans/`. This session
finished the inspector full-depth redesign + RTF parity, then scoped (but did not
yet implement) an "IBM Plex font everywhere" change.

## Current state

- **arframe** @ `8135c76` (master), working tree **clean**, `devtools::check`
  **0/0/0**, 929 tests green.
- **arpillar** @ `eb3af37` (branch `feat/ui-prereqs`), clean, **0/0/0**, 740 green.
- **tabular** @ its own HEAD -- **untouched this session** (only investigated).
- **Nothing pushed** anywhere -- publish decision still HELD (multi-repo first-time
  publish; no GitHub repos exist for arpillar/artoo/datasetviewer/arframe).
- Eyeball harness proven on the real CDISC pilot mount: `.local/screens/launcher.R`
  + `driver.R`, screenshots in `.local/screens/ws-0*.png`.

## What landed in the current session

The inspector redesign + RTF parity is **done and committed** (newest first):

- arframe `8135c76` handoff doc; `a060149` harden `input$region` (non-string
  payload used to crash the session), stepper width + chip-wrap polish from the
  eyeball; `5e07d85` a11y label coverage + installed-safe suspend pins +
  Rbuildignore `.claude`; `a617ff7` filters population-flag chips + humanized
  ops; `383347d` options footnote-reorder/decimals-stepper/stats-editor + Ranks
  real module; `57b51aa` Roles depth (source row, CDISC labels, variable peek,
  treat-as, relabel); `ee3f8ea` empty-Roles fix + footnote-badge removal + RTF
  screen-chrome parity wiring.
- arpillar `eb3af37` RTF paper-chrome parity + pinned column widths + `stats`
  option + label-sidecar/`column_range` metadata seams; goldens regenerated.

**Font work: investigated + a prompt drafted, NO code changed.** The user asked
to move tabular to IBM Plex (mono for tables, matching the app chrome), for the
HTML preview AND the RTF/DOCX export. They then chose to have **tabular natively
support IBM Plex** (rather than a downstream stack hack) and I handed them a
copy-paste **prompt for the tabular Claude Code session** (in chat, not a file).
Scope they picked earlier: **tables + figures (everything)**.

## Locked design decisions (do not re-litigate)

- **Inspector = the four panes rebuilt on REAL engine capability** (Roles peek /
  Options stats editor / Filters `*FL` chips / real Ranks module). All prior
  binding decisions in `CLAUDE.md` still hold.
- **RTF source line is stamped arframe-side** (`.with_source()` /
  `.report_with_source()` in mod_paper.R) into `options$source`; the arpillar
  emit path never calls `Sys.*` -- byte determinism is the golden-gate invariant.
- **Column widths are pinned to tabular's own resolved metrics + 0.05in slack**
  (`arpillar .pin_col_widths`), because tabular autofit is EXACT-fit over
  NBSP-padded cells and Word's font substitution otherwise breaks cells mid-value.
- **Font approach = let tabular own IBM Plex, not a downstream hack.** Reason:
  tabular's metric classifier (`.font_chain_family_class`, font_metrics.R:41)
  **defaults to serif** for any font it doesn't recognize by name, so a naive
  `font_family = "IBM Plex Mono"` would measure with Times metrics and wreck every
  column width. tabular must recognize IBM Plex as `mono`/`sans` first.

## Files in scope right now

- **Inspector (arframe, all committed):** `R/mod_card_roles.R`,
  `R/mod_card_options.R`, `R/mod_card_filters.R`, `R/mod_card_ranks.R` (new),
  `R/mod_card.R`, `R/fct_store.R`, `R/mod_paper.R`, `R/mod_frame.R`,
  `R/fct_export.R`, `R/utils_report.R`, `R/utils_demo.R`, `inst/www/arframe.css`,
  matching `tests/testthat/test-*`.
- **Engine (arpillar, committed):** `R/fct_render_table.R` (`render_spec`,
  `.rtf_titles/.rtf_footnotes`, `.pin_col_widths`, `.stats_opt`),
  `R/fct_render_ggplot.R`, `R/fct_dataitems.R` (`.sidecar_labels`, `column_range`),
  `R/fct_generators.R` (`stats` option), `tests/testthat/test-golden-*.R`.
- **Font work targets (NOT yet touched):** tabular `R/fonts.R`
  (`.stack_mono`, `.font_name_aliases`, `.resolve_font_stack`,
  `.font_generic_class`), tabular `R/font_metrics.R` (`.font_to_family_class`:31,
  `.font_chain_family_class`:41), tabular `R/aaa_class.R` (`font_family` default
  `"mono"`:813), tabular `R/backend_rtf.R` (`.rtf_font_table`:2841,
  `.rtf_falt`:3067 uses entry[[2]], `.rtf_family_class`:3081), tabular
  `R/backend_html.R`; downstream `arpillar R/fct_render_table.R render_spec`
  (fct_render_table.R:682-694) + `R/fct_render_ggplot.R` (figures).
- arframe already self-hosts the font: `inst/www/fonts/IBMPlexMono-{Regular,Medium}-Latin1.woff2`
  + IBMPlexSans, declared via `@font-face` in `inst/www/*.css`.

## What was tried and abandoned

- **Naive `font_family = "IBM Plex Mono"` (single name)** -- rejected: no fallback
  chain AND tabular's metric classifier defaults it to serif -> broken widths. The
  safe downstream form (if we ever do it without tabular support) is the explicit
  stack `c("IBM Plex Mono", "Liberation Mono", "Courier New", "monospace")` -- the
  Liberation anchor keeps the metric class = mono (Courier, 0.6em, exact) and the
  RTF family keyword = `\fmodern`.
- **Per-leg screen/export preset split** -- there is none in code today;
  `chrome_onscreen = "off"` is only a comment in mod_paper.R:76, never applied.
  `render_spec` is the ONE spec shared by the HTML preview and the RTF emit, so a
  single font change there covers both.
- **tabular bundling the font "for arframe"** -- redundant: arframe already
  self-hosts IBM Plex Mono, so the embedded preview picks it up the moment tabular
  merely NAMES it in the CSS. Bundling in tabular matters only for tabular's
  standalone HTML users (it's in the prompt for completeness).

## What to do next

1. **Wait for the tabular session** to land IBM Plex recognition (the prompt asks
   it to choose **A** = opt-in recognized family [recommended] vs **B** = change
   tabular's default `font_family`). Verify tabular emits
   `{\f0\fmodern\fprq1 IBM Plex Mono{\*\falt Liberation Mono};}` and that `\cellx`
   geometry is byte-identical to a `"mono"` render.
2. **Tables downstream:** if tabular picked **A**, add
   `spec <- tabular::preset(spec, font_family = "IBM Plex Mono")` to arpillar
   `render_spec()` (before `.pin_col_widths`) and regenerate the demographics +
   occurrence RTF goldens (font-table line only). If **B**, change nothing, just
   regen goldens. Add a unit test pinning the font-table line.
3. **Figures (independent of tabular's choice, can start now):** set arpillar
   `render_ggplot` base family to **IBM Plex Sans**; the font must be registered
   for the graphics device at render time (systemfonts/ragg -- check whether IBM
   Plex is installed system-wide or needs `systemfonts::register_font` pointing at
   a ttf/otf; the vendored woff2 may not suffice for the device). Regen figure
   goldens (vdiffr / byte RTF).
4. **Gate + eyeball:** `devtools::check` 0/0/0 both repos; screenshot the preview
   (IBM Plex table + figure) on real data; open an emitted RTF in Word to confirm.
5. **Larger roadmap (user steering, from before the font detour):** mockup pieces
   C (richer OUTLINE: grouped TABLES/FIGURES/LISTINGS + colored status dot+word),
   A (left activity bar -- ask which of the 5 icons are real), B (canvas
   per-output tabs -- biggest, confirm scope first). Order C -> A -> B.

## Operating constraints

- `\uXXXX` escapes for non-ASCII in R strings; em-dash `—` (never `--`) in
  user-facing UI text; tokens.css vars only; observer-pool + `Date.now()` nonce
  patterns for dynamic rows; `air format` PostToolUse hook; gate `devtools::check`
  0/0/0 before commit. No `Co-Authored-By: Claude` / AI attribution anywhere.
  **Never push without explicit per-session approval.**
- **testServer**: `outputOptions` suspend pins must grep `deparse(body(<server>))`,
  not the source file (R/ is absent under installed R CMD check).
- **Store is per-process**: restart the app (kill port 7788) between chromote
  drives or the second run sees the first's state.
- **RTF byte determinism** is a golden gate -- no timestamps in the arpillar emit
  path; dates get baked into `options$source` arframe-side.
- `handoff.md` is TRACKED here (committed, in `.Rbuildignore`), not gitignored --
  overwrite + commit it, matching prior handoff commits.

## Quick orient commands

```sh
cd /Users/vignesh/projects/r/arframe && git log --oneline -6 && git status --short
cd /Users/vignesh/projects/r/arpillar && git log --oneline -3
# font system in tabular (the next target):
grep -n "font_to_family_class\|font_chain_family_class\|default = \"serif\"" \
  /Users/vignesh/projects/r/tabular/R/font_metrics.R
# run the app on real CDISC data (interactive):
Rscript -e 'devtools::load_all("/Users/vignesh/projects/r/arframe"); \
  arframe(folders=c("/Users/vignesh/projects/data/cdisc-adam-pilot", \
  "/Users/vignesh/projects/data/cdisc-sdtm-pilot"))'
# headless eyeball: launcher.R (boots :7788, seeds Table 14.1.1) then driver.R
```
