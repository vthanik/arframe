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
7. **Canvas = tabular's FULL page render** (2026-07-04, supersedes the 2026-07-02
   chrome-free galley after the Global-Requirements review: tabular already
   renders the compliant unified-format page, so the canvas shows it as-is).
   A READY table is tabular's own HTML render alone — title block, footnotes,
   source, and any running header/footer bands (`chrome_onscreen = "auto"`);
   arframe paints NO title/source markup around it (double-print). The sheet
   is page-width (letter landscape 1056px / portrait 816px via
   `data-ar-orient`), vertically continuous — the RTF is the paginated truth,
   the canvas never fakes page breaks. Ghost/stale/error paths and figures
   keep the arframe-side title block + source line (no spec exists there).
   Chrome tokens `{datetime}`/`{program}` are stamped to literals arframe-side
   (`.with_chrome()`); `{page}`/`{npages}` stay as field codes.
8. **v6 UI frame — LOCKED 2026-07-06** (supersedes v5 2026-07-02 after the
   Setup-as-team-hub redesign):
   - Header: `arframe` wordmark · **5-mode segmented switch** (Setup · Data ·
     Report · Review · Logs — all peers, `role="tablist"`) · click-to-edit
     report name (natively centered via flex) · Open folder · save chip ·
     Refresh · Undo/Redo · ⌘K palette hint · `Package`. Segments are pure
     CSS-toggled via `.ar-mode-*` on `.ar-workspace`; no server round-trip on
     switch. Presence avatars (heartbeat-lit ring) planned inline before
     Package but land in a follow-up.
   - Setup mode (the team hub, first stop on open — pre-Report): Study ·
     Sources (list-surface catalog with Add folder / Import file · shinyFiles
     wrapped in `.ar-picker` so Bootstrap variants own colour) · Populations ·
     Preferences + Paths (`programs_dir` / `output_rtf_dir` / `datasets_dir` /
     `logs_dir` written to `setup.yml` under `paths:`) · Team (roster from
     `.arframe/team.json`, activity feed from per-user `.arframe/activity/
     <slug>.jsonl`, presence rail from `.arframe/presence/<slug>.json`
     heartbeat). Same project folder opened by two users shows each other's
     edits and presence.
   - Report mode: explorer CONTENTS tree (ICH numbers mono, per-generator
     icons, proof stamps; chevron-collapsible to a status-dot strip) |
     chrome-free artifact with proofreader **margin-mark** region hit-zones
     bound to tabular's own classes (`.tabular-title`, `thead`, `tbody`,
     `.tabular-footnote`) | fixed-width docked inspector (Roles / Options /
     Filters / Ranks tabs; chevron-collapsible to icon strip) with action
     footer **Run ⌘↵ · .rtf · `</>`** (code view = `arpillar::emit_code()`
     script, copy + download). Live telemetry line
     (`adsl · 254 subjects · 254 records`).
   - Data mode: datasetviewer manage-data, full-width (no inspector): SOURCES
     multi-folder tree (+ Add folder), toolbar (Filter · View data · Import
     file · Import folder · Delete), explorer detail table (NAME / FOLDER /
     KIND / COLS / ROWS / SIZE / STATUS=LAZY / MODIFIED), double-click OR
     View data → grid with breadcrumb + column picker (typed badges); Delete
     unmounts. Import file/folder INTENTIONALLY duplicated with Setup >
     Sources — the two mounts share `.catalog_list_surface()`
     (`R/mod_catalog_list.R`), so the on-ramps do not drift.
   - Export package tree: `outputs/` (spec .json) + `programs/` (emit_code
     per output + run-all.R) + `report.json` + `manifest.csv`. The team
     folder `.arframe/` is EXCLUDED from the tarball (`.zip_export()` unlinks
     it before zipping) — sponsor deliverables never carry team activity.
     Run semantics: cheap edits render live; heavy role/filter changes mark
     the proof STALE, Run re-typesets.
9. **Real data, always** (2026-07-02): dev sessions, eyeball verification, and
   screenshots ALWAYS mount `/Users/vignesh/projects/data/cdisc-adam-pilot`
   (15 ADaM parquet) and `/Users/vignesh/projects/data/cdisc-sdtm-pilot` (SDTM
   xpt) — never a synthetic demo catalog for any visual claim. testthat fixtures
   stay minimal/bundled (CRAN), but verification is real-data only.

10. **Folder IS the format** (2026-07-06): the shared unit of collaboration is
    the project folder — opened by teammate A on Dropbox / SMB / a git-tracked
    dir, opened simultaneously by teammate B; each runs their own arframe
    session; the files are the source of truth. Canonical layout (mirrors SAS
    pharma `pgms/`, `output/`, `data/`, `logs/`):

    ```
    <project root>/
      setup.yml                     study config, sources, populations,
                                    preferences, paths (Setup writes here)
      outputs/<id>.json             spec — canonical source of truth
      programs/<id>.R               emit_code(spec) — team-visible artifact,
                                    regenerated every save
      programs/run-all.R            reproduces the whole package via
                                    `Rscript programs/run-all.R`
      output/<id>.rtf|.pdf          renders (destination configurable)
      data/                         inputs (destination configurable)
      report.json + manifest.csv    report-level metadata
      .arframe/                     TEAM STATE — excluded from tarball export
        team.json                   roster (name, email, initials, colour)
        activity/<slug>.jsonl       per-user activity log (no flock —
                                    concurrent writes eliminated by
                                    per-user files)
        presence/<slug>.json        30s heartbeat via `later::later()`;
                                    reader filters mtime > now - 60s
    ```

    Source of truth is the `.json` spec, not the `.R` script. The `.R` is
    deterministically re-emitted every save; external `.R` edits do NOT
    round-trip in v1 (would need parsing arbitrary R back into a spec —
    deferred). Generic effective user (`root`, `www-data`, `runner`, `user`,
    unset) shows the Setup > Team "Set your name" banner and emits ZERO
    activity/presence lines — prevents anonymous churn.

11. **Setup is first-class, first stop** (2026-07-06): `arframe()` opens in
    Setup mode (`.ar-workspace` gets `ar-mode-setup`), NOT Report — study
    config is the first thing a fresh project needs. Setup's Sources section
    is the "data on-ramp" that item 5 above called out; the `.list_surface`
    primitive is shared with Data mode so the two read visually identical.

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
- **No `--` in user-facing text.** Empty-value placeholders, printed cells, and
  any displayed string use the em-dash `—` (`"—"` in R strings to keep the
  source ASCII-clean; the literal `—` in JS/CSS, matching the existing `⌘`),
  never the ASCII double-hyphen `--`. `--` reads as a typo in the rendered UI.
  (This is a UI rule; `--` as an em-dash inside `#`/`#'` comments is still fine
  per `~/.claude/rules/ascii.md`.)
- No `Co-Authored-By: Claude` / AI attribution anywhere. Never push without explicit
  per-session approval.
- Subagent execution hygiene: verify INLINE — do not spawn nested background
  sub-agents (they orphan into un-killable "Running" tasks).

## Team-state file inventory (2026-07-06)

Modules for the team hub and the shared list surface:

- `R/fct_activity.R` — `.log_activity(dir, user, action, targets)`,
  `.read_activity(dir, tail_n)`, `.rotate_activity(dir)`. Per-user JSONL
  eliminates the append race.
- `R/fct_presence.R` — `.heartbeat(dir, user, mode, current_output)`,
  `.presence_list(dir, since_s = 60)`, `.gc_presence(dir, max_age_s = 86400)`.
- `R/fct_team.R` — `.read_team(dir)`, `.write_team(dir, team)`,
  `.ensure_team_member(dir, user)`, `.team_slug(user)` (filesystem-safe),
  `.user_is_generic(user)`.
- `R/mod_catalog_list.R` — `.catalog_list_surface(ns, tools)`,
  `.catalog_grid_table(rows)`, `.catalog_grid_row(ns, item, selected)`,
  `.catalog_type_chip()`, `.catalog_status_pill()`,
  `.catalog_items_from_grid(grid)`. Shared by Setup > Sources and Data mode.
- `R/mod_setup.R` (extended) — Sources, Team, Preferences + Paths sections
  on top of the original Study / Data / Populations / Page / Summaries.
- `R/fct_project.R` — `save_touched()` now emits `programs/<id>.R` per
  output + `programs/run-all.R` + one batched activity line + ensures the
  team roster contains the current user; new `.refresh_all(store)`
  consolidates the two prior refresh paths; new `.emit_programs()` and
  `.is_absolute_path()` helpers.
- `R/fct_export.R` — `.zip_export()` unlinks `.arframe/` before zipping so
  the tarball never leaks team activity.
- `R/app.R` — presence heartbeat via `later::later()` at 30s cadence.

All exports still enforced: `arframe()` is the only export; everything
above is `@noRd` internal.

## Golden gates (preserved through the rebuild)

Non-empty RTF export matching a byte/XML golden; every graph round-trips
ggplot → `figure()` → golden. See arpillar for the engine-side goldens.
