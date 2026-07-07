# Handoff — arframe dashboard redesign (2026-07-07)

## Goal

Rescope arframe from a single-user builder to a **team-level tool** and lift the
visual language from the austere "Linear/GOV.UK" look to **dashboard-grade
UX/UI** (light-first: elevated cards, left sidebar, status pills, stat tiles),
modelled on the user's feel-good refs (Power BI, Sence.Point, Piduiteun, fitplan,
mota). Bar: exceed Posit's own Shiny apps. Full roadmap + locked decisions:
`~/.claude/plans/the-scope-now-is-curious-stroustrup.md` and **CLAUDE.md #12**.

Locked forks (2026-07-07): Report = **LoC table → drill into paper** (reverses
paper-first); **light-first** v1; **Foundation first**; **full CDISC ARS spine**;
keep GOV.UK a11y, drop its austerity.

## Current state — Foundation sub-project (#1) is DONE and verified

All 5 modes render cleanly under the new shell (screenshotted on real CDISC data
+ demo fixture; Setup/Data/Report/Review/Logs all clean). `devtools::test()` =
**FAIL 0 | PASS 1177**; the e2e mode-walk = **PASS 64**. `devtools::check()`: **Status: OK (0 errors / 0 warnings / 0 notes)**.

Sub-projects 2–5 (Setup→sectioned dashboard, Report→LoC table, full ARS spine,
dark theme) are NOT started — each gets its own spec→plan→build.

## What changed this session

**Design tokens** (`inst/www/tokens.css`, `R/theme.R`): added IBM Plex Sans
500/600/700 (`inst/www/fonts/`); retuned `--ar-*` to dashboard — elevation
shadow scale (`--ar-shadow-card/-lift/-float`), cooler canvas (`--ar-desk
#f5f7fa`), card radii (8/12/16/pill), spacing +40/48, 14px body + display scale
(`--ar-fs-page/-section/-stat`) + weight tokens, 5-state status set
(ready/draft/needs_data/broken/stale) with soft-fill pill bgs, categorical viz
ramp. bslib radius 4→8px.

**Atoms** (`R/utils_atoms.R` + `arframe.css` §01): new `.card()` (class
`.ar-panel` — `.ar-card` is taken by the inspector), `.stat_tile()`, `.avatar()`;
evolved `.stamp()` into a soft-fill **status pill** (dot via `::before`, no HTML
change → every caller inherits it). Added `review = "clipboard-check"` glyph.
Tests in `test-utils_atoms.R` (all pass).

**Shell** (`R/mod_frame.R` + `arframe.css` §02): replaced the top 5-mode
segmented switch with a **left `.ar-sidebar`** (brand + vertical `.ar-nav`) +
`.ar-main` (a `.ar-topbar` title+actions bar). `.frame_bar`→`.frame_sidebar`+
`.frame_topbar`; `.frame_section_switch`/`.seg`→`.frame_nav`/`.nav_item`. **No
server or bridge.js change** — nav items keep `data-ar-mode`; the `.ar-mode-*`
class-swap is unchanged. Responsive: sidebar folds to an icon rail <1024px.
Rewrote `test-mod_frame.R` for the new structure + made its e2e walk **all 5
modes** and screenshot each to `.local/screens/0N-<mode>.png` (get_screenshot
does NOT overwrite — file.remove first).

**Root-cause fix — Setup top gap**: `.ar-setup > *` (there to fight Shiny's
`display:contents` on the uiOutput) also un-hid the sibling
`<datalist id="ar-stat-atoms">`, whose options rendered as a ~250px list. Scoped
it to `.ar-setup > .shiny-html-output`. Verified via live DOM (`datalist
display=none`, content flush to top).

**Deletions**: dead `.ex-appbar`/`.ex-appbar-brand`/`.ex-section-switch`/
`.ex-seg*`/`.ex-appbar-user` CSS; and the whole **`R/mod_catalog_list.R`** +
its `.ex-manage-*`/`.ex-grid*`/`.ex-status*`/`.ex-viewer*`/`.ex-empty*` CSS — an
unwired, premature "shared surface" primitive (never mounted anywhere; Data uses
its own `.ar-dx-table`). The real shared table atom gets built in the Report/LoC
sub-project (#3) with two real consumers.

**Docs/memory**: CLAUDE.md #1/#8 rewritten + new #12 (redesign record + roadmap +
build state); memories `govuk-design-principles` (amended: a11y kept, visuals now
dashboard), `feedback-never-defer`, `feedback-screenshot-eyeball` added.

## Pre-existing issues fixed this session (were in HEAD, not new regressions)

- **`test-mod_setup_wire.R`** read `../../R/mod_setup.R`, absent in the R CMD
  check sandbox → guarded with `skip_if_not(file.exists(setup_src))` (fires on
  the real condition; `skip_on_cran` would NOT, since `devtools::check()` sets
  `NOT_CRAN=true`).
- **Non-ASCII WARNING**: `mod_setup.R` had user-facing `—`/`×` in *string
  literals* → escaped to `\uxxxx` (identical runtime output, ASCII source, per
  the check's own guidance). Comment-only non-ASCII in `mod_paper.R`/`theme.R`/
  `mod_card_filters.R` is check-excused ("except perhaps in comments"), left
  as-is per ascii.md.

## Next steps

1. **Sub-project #2 — Setup → sectioned dashboard**: brainstorm→spec. Split the
   one scroll page into trackable section-cards (`.card()`) + a Setup overview
   with `.stat_tile()`s (study completeness, subject/record counts).
2. **#3 — Report → List of Contents**: LoC table (number/type/title/population/
   status pills/actions, +Output/+Standard) → drill into paper+inspector
   (mirror Data's list→grid). Build the shared dashboard **table atom** here
   (reused by Data). Add Options/Prefs toolbar segment; Run→Review.
3. **#4 — full CDISC ARS spine** (mostly arpillar): ReportingEvent / Analysis /
   AnalysisMethod / Operation / GroupingFactor / AnalysisSet / DataSubset /
   OutputDisplay / ListOfContents / ARD + import/export; arframe surfaces
   ARM_OID/analysis columns. Couples to #3.
4. **#5 — dark "Instrument" theme** via token overrides.

## Verify / re-check

```bash
Rscript -e 'devtools::test()'                                   # FAIL 0
NOT_CRAN=true Rscript -e 'devtools::test(filter="mod_frame")'   # e2e + screenshots
Rscript -e 'devtools::check(args="--no-manual")'                # Status: OK (0/0/0)

# real-data eyeball (CLAUDE.md #9): boots on the CDISC ADaM pilot
Rscript .local/screens/launch.R    # serves 127.0.0.1:7910; drive Chrome to shoot each mode
# in-suite demo screenshots land in .local/screens/0N-<mode>.png
```
