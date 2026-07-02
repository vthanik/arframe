# CLAUDE.md — arframe

Project conventions for **arframe**, the submission-native clinical report
builder (the **Galley** UI on the **arpillar** engine). Inherits the global
`~/.claude/CLAUDE.md` and the R package rules in `~/.claude/rules/`. Records only
what is arframe-specific and user-decided — do not duplicate the house rules.

## What this is

Galley design: a paper-first "proof room" — a table of contents beside a typeset
page, proof-stamp statuses, and a summonable/pinnable galley card. The deliverable
(the TLF package) is the interface.

- **Binding design spec:** `docs/superpowers/specs/2026-07-02-arframe-galley-design-system.md`
- **Implementation plan:** `docs/superpowers/plans/2026-07-02-arframe-ui-stage5.md`
- Architecture: thin bslib Shiny shell on the pure `arpillar` engine
  (Imports via `Remotes: vthanik/arpillar`).

## Binding user decisions (2026-07-02) — do NOT re-litigate

1. **Design = Galley** (paper-first; NO dual rails, NO canvas tabs). Supersedes the
   earlier SAS-Viya dual-rail frame and the datasetviewer SAS-blue skin as the
   default. The dark "Instrument" skin is a later theme toggle, not v1.
2. **Outputs = generators + presets, NOT template-per-table.** ~6 reusable engine
   *generators* (summary / crosstab / occurrence / km / line / box) are the primary
   unit; a shippable *preset* library (~20, domain-grouped) + **Save-as-preset**
   layer on top. "50 tables by default" = presets (config), not 50 hard-coded
   templates. Non-standard output → pick a generator + configure; novel *shape* →
   add a generator to arpillar (auto-surfaces via the registry). The **occurrence /
   AE (SOC▸PT) generator is core** — AE tables are most of a safety package.
3. **TLF numbering = editable, preset-seeded** — `options$number` (e.g. `"14.3.1"`)
   + `options$number_label` (`Table`/`Figure`/`Listing`). NOT auto-derived from
   output type (real numbering is domain/SAP-shell driven per ICH E3: 14.1
   demographics, 14.2 efficacy, 14.3 safety, 16.2 listings). The TOC groups by kind
   but SHOWS `options$number`; the paper title block shows `"<label> <number>"`.
4. **Per-generator SVG icons.** A distinct house-style inline SVG per output type
   (summary = stat-list, crosstab = grid, occurrence = SOC▸PT hierarchy, km =
   step-down curve, line = trend, box = box-and-whisker; 16px, `currentColor`, 1.3
   stroke, round). The icon encodes the **type/family** (all AE presets share the
   occurrence icon; the label distinguishes the specific table). Never a repeated
   generic table glyph.
5. **Data mode is first-class**, not optional or last: dataset catalog + DuckDB-WASM
   grid (datasetviewer) + variable profile + import (artoo/shinyFiles) + the
   **"Use in a new output →"** bridge. It is the data on-ramp; pulled ahead of the
   finer roles/options/filters panes in the build order.
6. **Platform-specific UI resolves in the browser, never in R** — e.g. the ⌘K /
   Ctrl-K hint is set client-side from `navigator.platform` (a web app's server OS
   is not the client's).
7. **Canvas = galley, NOT page facsimile** (2026-07-02, supersedes Task 9's
   screen==paper canvas). The typeset-page look is tabular's brand; arframe never
   cosplays as paper. The canvas shows the live rendered proof (real table/figure,
   title block, footnotes, source — all clickable regions) on an app-native
   surface: NO running head (`Protocol … · Page N of N`), NO fit/page toggle, NO
   letter-page aspect, NO faux sheet. Page chrome appears only in export/QC
   preview (which is honestly tabular's render). Engine-enforced via tabular's
   `chrome_onscreen = "off"` preset knob on the screen leg only.
8. **v5 UI frame — LOCKED 2026-07-02** (five mockup iterations, converged):
   - Header: `arframe` wordmark · `[Data | Report]` segmented toggle (top-LEFT,
     modes are peers) · report name · QC · ⌘K · `Export package`.
   - Report mode: explorer CONTENTS tree (ICH numbers mono, per-generator icons,
     proof stamps; chevron-collapsible to a status-dot strip) | chrome-free
     artifact with proofreader **margin-mark** region hit-zones bound to
     tabular's own classes (`.tabular-title`, `thead`, `tbody`,
     `.tabular-footnote`) | fixed-width docked inspector (Roles / Options /
     Filters / Ranks tabs; chevron-collapsible to icon strip) with action footer
     **Run ⌘↵ · .rtf · `</>`** (code view = `arpillar::emit_code()` script,
     copy + download). Live telemetry line (`adsl · 254 subjects · 254 records`).
   - Data mode: datasetviewer manage-data, full-width (no inspector): SOURCES
     multi-folder tree (+ Add folder), toolbar (Filter · View data · Import file ·
     Import folder · Delete), explorer detail table (NAME / FOLDER / KIND / COLS /
     ROWS / SIZE / STATUS=LAZY / MODIFIED), double-click OR View data → grid with
     breadcrumb + column picker (typed badges); Delete unmounts.
   - Export package tree: `outputs/` + `programs/` (emit_code per output +
     run-all.R) + `report.json` + `manifest.csv`. Run semantics: cheap edits
     render live; heavy role/filter changes mark the proof STALE, Run re-typesets.
9. **Real data, always** (2026-07-02): dev sessions, eyeball verification, and
   screenshots ALWAYS mount `/Users/vignesh/projects/data/cdisc-adam-pilot`
   (15 ADaM parquet) and `/Users/vignesh/projects/data/cdisc-sdtm-pilot` (SDTM
   xpt) — never a synthetic demo catalog for any visual claim. testthat fixtures
   stay minimal/bundled (CRAN), but verification is real-data only.

## Working conventions (arframe-specific)

- **Eyeball verification is binding.** Screenshot the running app after any UI
  change; measure the DOM (`getBoundingClientRect`) for alignment/color claims.
  Never assert visual correctness from reading code.
- **`arframe()` is the only export**; everything else is internal (`@noRd`). ONE
  injected structured store is the sole inter-module channel; ALL draft/edit state
  lives in the store, never in the DOM (the audit's top data-loss risk).
- **Logic lives in arpillar**; modules only wire UI ↔ store. No
  DBI/cards/ggplot2/tabular call inside an arframe `render*`/`observe`/`reactive`.
- Tokens/skin by variable override (one `bslib::bs_theme()` + `inst/www/*.css`);
  no scattered inline styles. Fonts: self-hosted IBM Plex (OFL), never a commercial
  face.
- No `Co-Authored-By: Claude` / AI attribution anywhere. Never push without explicit
  per-session approval.
- Subagent execution hygiene: verify INLINE — do not spawn nested background
  sub-agents (they orphan into un-killable "Running" tasks).

## Golden gates (preserved through the rebuild)

Non-empty RTF export matching a byte/XML golden; every graph round-trips
ggplot → `figure()` → golden. See arpillar for the engine-side goldens.
