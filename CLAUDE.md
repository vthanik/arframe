# arframe — project conventions

Design decisions, LOCKED user calls, and working conventions for
**arframe** — the submission-native clinical report builder (the **Galley**
UI on the **arpillar** engine). Records only what is arframe-specific and
user-decided.

**Always check `CLAUDE.local.md` too** (gitignored, local). Its **Backlog &
requirements** section carries live progress and not-yet-specced work
(e.g. the #12.4 CDISC ARS spine); its **Session handoff** section governs
`handoff.md`. Read it at session start and keep it in sync as work lands.

## What this is

Galley design: a paper-first "proof room" — a table of contents beside a typeset
page, proof-stamp statuses, and a summonable/pinnable galley card. The deliverable
(the TLF package) is the interface.

- **Binding design spec:** `docs/superpowers/specs/2026-07-02-arframe-galley-design-system.md`
- **Implementation plan:** `docs/superpowers/plans/2026-07-02-arframe-ui-stage5.md`
- Architecture: thin bslib Shiny shell on the pure `arpillar` engine
  (Imports via `Remotes: vthanik/arpillar`).

## Binding user decisions (2026-07-02) — do NOT re-litigate

1. **Design = dashboard-grade** (SUPERSEDES the earlier "Galley paper-first, NO
   dual rails" call — see #12 for the 2026-07-07 redesign). Report is a
   **List-of-Contents table that drills into the paper + inspector** (mirrors
   Data's list->grid), NOT paper-first. Light-first dashboard visual language
   (elevated cards, left sidebar, status pills, stat tiles); tabular's paper
   render survives as the READY detail view. The dark "Instrument" skin is a
   later theme toggle, not v1.
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
   - Header — **top app bar** (LOCKED 2026-07-07, supersedes the 2026-07-07
     left-sidebar nav from #12.1 after the top-nav revision): `arframe`
     wordmark · **horizontal mode nav** (Setup · Data · Report · Review ·
     Logs — all peers, `role="tablist"`, active = accent underline) · flex
     gap · ⌘K palette hint · `Package`. **No left sidebar.** Mode tabs stay
     pure CSS-toggled via `.ar-mode-*` on `.ar-workspace` (delegated
     `[data-ar-mode]` click → `input$mode`; no server round-trip). The
     click-to-edit **report name moved to a page-header row** (`.ar-pagehead`)
     below the bar, present in every mode. **Refresh / Undo / Redo circle
     buttons removed** (2026-07-07, "too many, ugly"): Refresh was redundant
     (auto-fires on window-focus); Undo/Redo are now **⌘Z / ⌘⇧Z** keyboard
     shortcuts (bridge.js keydown → the same `frame-undo_btn`/`frame-redo_btn`
     inputs). Save chip kept. Presence avatars (heartbeat-lit ring) land in a
     follow-up in the actions cluster.
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
     Sources — both on-ramps will share one dashboard table atom built in the Report/LoC
     sub-project (#12); today Data uses `.ar-dx-table`. (The old unwired
     `.catalog_list_surface()` primitive was deleted 2026-07-07.)
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
    is the "data on-ramp" that item 5 above called out; Setup > Sources and
    Data will share one dashboard table atom (built in the Report/LoC
    sub-project, #12).

12. **Dashboard redesign — team-level tool (2026-07-07)** — supersedes #1
    (paper-first) and #8's top switch. Visual language moved from the austere
    Linear/GOV.UK look to **dashboard-grade UX/UI**: light-first, elevated
    rounded cards on a cool canvas, soft shadows, a **top app-bar** mode nav
    (2026-07-07 revision, superseding the short-lived left sidebar — see #8),
    status pills, stat tiles, IBM Plex Sans 400-700 + Plex Mono as the
    "instrument" face (TLF numbers, counts, IDs). GOV.UK a11y patterns kept,
    austerity dropped. Staged sub-projects:
    1. **Foundation (DONE)** — tokens (`inst/www/tokens.css`), atoms
       (`.card`/`.stat_tile`/status-pill/`.avatar`, `utils_atoms.R`), and the
       app shell (`mod_frame.R`; the shell's nav was **re-housed left-sidebar
       → top app bar** in the #2 pass — see #8). Plan:
       `~/.claude/plans/the-scope-now-is-curious-stroustrup.md`.
    2. **Setup → sectioned dashboard (DONE 2026-07-07)**
       — top-nav frame refactor + Setup re-housed as a section tab strip, one
       `.card()` per section, an overview stat strip, server-authoritative
       active tab. Spec/plan:
       `docs/superpowers/specs/2026-07-07-arframe-setup-dashboard-top-nav-design.md`,
       `docs/superpowers/plans/2026-07-07-arframe-setup-dashboard-top-nav.md`.
       3. Report -> **List-of-Contents table**
       drilling into paper+inspector (mirrors Data); +Output/+Standard,
       Options/Prefs segment, Run->Review.  4. **Full CDISC ARS spine** in
       arpillar (ReportingEvent/Analysis/AnalysisMethod/Operation/
       GroupingFactor/AnalysisSet/DataSubset/OutputDisplay/ListOfContents/ARD)
       + arframe ARM_OID columns.  5. Dark "Instrument" theme.

## Working conventions (arframe-specific)

- **`arframe()` is the only export**; everything else is internal (`@noRd`). ONE
  injected structured store is the sole inter-module channel; ALL draft/edit
  state lives in the store, never in the DOM (the audit's top data-loss risk).
- **Logic lives in arpillar**; modules only wire UI ↔ store. No
  DBI/cards/ggplot2/tabular call inside an arframe `render*`/`observe`/`reactive`.
- Tokens/skin by variable override (one `bslib::bs_theme()` + `inst/www/*.css`);
  no scattered inline styles. Fonts: self-hosted Avenir Next (sans, Light +
  Regular) + IBM Plex Mono (OFL). See `inst/COPYRIGHTS` for the sans
  licensing status.
- **No `--` in user-facing text.** Empty-value placeholders, printed cells, and
  any displayed string use the em-dash `—`, never the ASCII double-hyphen
  `--`. `--` reads as a typo in the rendered UI. (In `#` / `#'` comments and
  roxygen prose, em-dash `—` is preferred; ASCII `--` there is still
  tolerated.)
- **Variable / column / parameter pickers — ONE shared picker everywhere
    (2026-07-07). Every variable/param selector uses it; never a bare
    unlabeled `<select>` of column names, and never inline a selectize
    `render` blob in an R module.**
  - **The engine is `selectize` (Shiny's bundled one), NOT a new library.**
    The one option render — type-chip avatar + NAME + muted CDISC label on
    one line — is defined ONCE in the JS bundle as `window.arframePickerOption`
    / `window.arframePickerItem` (`srcjs/bridge.js`). R references it by name:
    `render = I("{ option: window.arframePickerOption, item:
    window.arframePickerItem }")` — zero render markup in the modules. Chip
    vocab (mirrors datasetviewer's `typeIcon` + arpillar's coarser taxonomy):
    `#` measure/number, calendar SVG date/datetime, clock SVG time, `A`
    category/string/bool, `P` param. (Tom Select was trialed and rejected
    2026-07-07 — do NOT re-suggest it.)
  - **`.ar_picker_select()` (`utils_atoms.R`) is the shared builder** — a
    `selectizeInput` wrapped in `.ar-picker`, choices a named vector whose
    NAMES are the packed `"name\x1ftype\x1flabel"` label the render splits and
    whose VALUES are what the server consumes. Two thin wrappers:
    - `.eligible_picker()` (`mod_card_roles.R`) — single/bind pick: Roles
      slot, Filters column, Populations subject-id, **Treatment variable**.
      Value = packed (server unpacks via `.unpack_item_name`) OR the bare
      column name (`bare_value = TRUE`, Treatment). Empty `selected` force-
      clears on init and just ADDS; a non-empty `selected` re-seeds a
      committed control (Filters, Treatment).
    - `.rich_picker()` (`mod_setup.R`) — per-row add: Continuous stats,
      Decimals-by. `onChange` posts `{i, value, nonce}` to a SHARED observer
      (row index baked in) then clears — no per-row Shiny observer to leak in
      the dynamic renderUI.
  - **Domain filters on the choices:** decimals-by offers **numeric columns
    only** (`type == "measure"`) + BDS params, with each param's PARAM value
    (from `SELECT DISTINCT PARAMCD, PARAM`) as the muted description;
    continuous stats enforce **global uniqueness** — a statistic is used once
    across ALL rows, so each row's picker excludes every atom used in any row.
  - **Long labels truncate with an ellipsis** — do NOT widen the dropdown to
    fit them: a wider control reads awkwardly, and a body-appended dropdown
    (`dropdownParent`) detaches from the control on scroll. The dropdown takes
    the control's natural width.
  - **Persistence rule:** inside a dynamic `renderUI`, never rely on selectize
    `selected` alone for a value that must survive re-render — either re-seed a
    single value with the exact packed choice (Filters / Treatment) or render
    the chosen state as server chips fed by an always-empty add-control
    (subject-id / decimals-by / continuous-stats).
  - Non-variable choices — segmented toggles (yes/no, orientation), short
    fixed enums, and the **dataset list** (`.select_input`, a native
    `<select>`) — stay as segmented controls or a native `<select>` and get
    no picker.

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
- `R/mod_catalog_list.R` — DELETED 2026-07-07 (unwired premature shared-surface
  primitive; the Report/LoC sub-project #12 builds the real shared table atom).
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
