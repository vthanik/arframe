# arframe / arpillar — UI redesign design spec

- **Status:** approved design (brainstorming complete); next step is `writing-plans`.
- **Date:** 2026-07-01
- **Supersedes:** the repo-root `design.md` and the datasetviewer/SAS-blue "north
  star" *as binding constraints*. This was a first-principles redesign; `design.md`
  was explicitly reopened by the user. The datasetviewer design system is retained
  as the **default light skin**, not as an unquestionable law.
- **Author:** Vignesh Thanikachalam.

---

## 1. What we are building

`arframe` is a local-first, R-native, open-source **submission-native clinical
report builder + data explorer**. It is modelled on SAS Viya's *Manage Data* +
*Explore & Visualize*, but re-centred on the clinical deliverable: a report is an
ordered set of **shell-conforming TLF outputs** (tables, listings, figures) exported
to RTF/PDF/HTML/DOCX and backed by reproducible R code.

Goal: beat SAS Viya, teal, blockr, and GSK rapido on **design and quality**. The
wedge is that none of them make the *deliverable itself* the primary object — they
assemble outputs and treat the report as an export afterthought. `arframe` makes the
report (the TLF package) the thing you edit.

## 2. Packages and naming

**The `AR` family.** `AR` = **Analysis Report / Analysis Result**. The packages carry
the `ar` prefix so they self-describe and read as one family (the `gt` / `sf`
pattern). This is a **deliberate departure** from the house "bare evocative single
word, no prefix" convention — chosen for self-describing discoverability across a
stats/clinical audience. Do not "correct" it back to an evocative single word; the
prefix is intentional. (It governs *package* names only; individual exported
functions still follow the house rule — bare verbs/nouns, no per-function prefix.)

`arframe` **reclaims the ecosystem name.** The earlier `arframe` in the roadmap
(*Herald / arframe / Mint / Loom*) was renamed to **`tabular`** (the render/export
layer), which frees the `arframe` name for this app. The global `~/.claude/CLAUDE.md`
roadmap line is stale on this point and should be updated separately.

| Package | Role |
|---|---|
| `arframe` | Thin bslib Shiny shell — the UI **frame**. `arframe()` opens/creates a report project. Section modules, `state.R` store, draft/commit helpers. |
| `arpillar` | Pure engine, no Shiny — the **pillar** the frame stands on. DuckDB catalog/engine, S7 document model, SQL compiler (pushdown), `render_plan()` seam, `render_ggplot`/`render_tabular`/`ard_cards`, JSON serialization + migrators. Unit-testable with no `session`. |

- Both names are CRAN-free (verified 2026-07-01). They shadow no base R (note: bare
  `ar()` is `stats::ar` — autoregression — but the prefixed names do not collide).
- Replaces the temporary `explorer`/`explorerengine` scaffolding and the `ex_`
  export prefix.
- The pairing `arframe` (UI) on `arpillar` (core) avoids an ugly `...engine` suffix.
- Not adopted: `arrender` (overlaps `tabular`, the existing render/export layer —
  composed, not reinvented) and `arkit` (an umbrella is redundant for a 2-package
  split — YAGNI until 3+ packages exist).

## 3. Paradigm — spec/shell-driven (submission-native)

The controlling decision. A report is an ordered list of **specified outputs**, each
mapped to an SAP-style **shell**. You do not build ad-hoc charts on a free canvas;
you **fill a shell**. The shell is visible from the first second (as ghosted
placeholder rows/cells) and fills in as data is assigned.

Rejected alternatives: BI free-form canvas (SAS Viya's own model — poor fit for
standardized TLFs, and competes with SAS on its home turf); task/catalog-only
(too close to the current Section B, less differentiated); explore-first notebook
(weak for reproducible standardized TLFs).

## 4. Information architecture

Two top-level **modes**, switched by a segmented toggle in the app bar:

- **Data** — the supporting explorer: dataset catalog, grid, variable profiles,
  import. Its job is to *feed* the report.
- **Report** — the deliverable: the report is a project holding **many outputs**,
  each independently editable. This is home.

The report is never a single output — the whole set of outputs travels together as
one project (see §9 persistence).

## 5. App shell — the SAS Viya dual-rail frame

The frame is SAS Viya's *Explore & Visualize* five-region shell, reskinned and
repurposed. Chosen over three invented alternatives (collapsible-outline+editor,
full-bleed focused, three-pane workbench) because it is the proven, familiar
structure for this audience and it holds everything already designed.

```
┌───────────────────────────────────────────────────────────────────────┐
│  app bar:  arframe · [Data | Report] · report name · undo/redo · Export │
├────┬──────────────┬───────────────────────────────┬──────────────┬─────┤
│ L  │ left pane    │  canvas: [output tabs] +       │ right pane   │  R  │
│ a  │ (contextual  │                                │ (contextual  │  a  │
│ c  │  to active   │   ┌─────────────────────────┐  │  to active   │  i  │
│ t  │  activity)   │   │  white paper shell      │  │  property)   │  l  │
│ i  │              │   │  preview (screen==paper)│  │              │     │
│ v  │              │   └─────────────────────────┘  │              │     │
│ «  │              │                                │              │  »  │
└────┴──────────────┴───────────────────────────────┴──────────────┴─────┘
```

| SAS Viya region | arframe mapping |
|---|---|
| Left rail: Data / Objects / Outline / Suggest / Review | **Data** (variables) · **Outputs** (clinical TLF templates) · **Outline** (the TLF deliverable list) · **Suggest** (recommend standard tables for the loaded ADaM) · **Review** (QC / shell-conformance / log) |
| Center canvas + page tabs | one **output per tab**; the white **paper shell preview** |
| Right rail: Options / Roles / Actions / Rules / Filters / Ranks | **Roles · Options · Filters · Ranks** for the selected output; **Actions/Rules** deferred (cross-output interactivity, later milestone) |
| Both rails collapse `«` `»` | same — collapse either rail to hand full width to a wide many-arm table |

The width cost of two rails + two panes is real; the mitigation is the same as SAS
Viya's — both rails and both panes collapse, giving a focused wide canvas on demand.

## 6. Left activity rail (what you add / navigate)

- **Data** — rich variable picker (type chip + name + label) scoped to the output's
  dataset(s). Also the drag source for role assignment.
- **Outputs** — the catalog of standard clinical TLF **templates** (the existing
  presets catalog; see the clinical table/graph catalogs). `+ Add output` picks from
  here. Each template declares the role slots it exposes (§8).
- **Outline** — the ordered TLF deliverable list, grouped Tables / Listings /
  Figures, numbered (14.2.1, 16.2.1, ...), each row carrying a live **status**
  (Ready / Draft / Needs data / Broken). Reorderable. This is the report's spine.
- **Suggest** — given the loaded ADaM datasets, recommend standard outputs
  (Demographics for ADSL, AE summary for ADAE, KM for ADTTE, ...).
- **Review** — QC / validation surface: shell-conformance per output, missing
  outputs, the run log.

## 7. Center canvas — the shell preview

- **Output tabs** across the top (one output per tab, `+` to add).
- **White paper shell preview.** Renders the actual output as it will appear on the
  page via the **same `render_plan → tabular`/`ggplot` seam that exports** — screen
  == paper, no dual-render divergence. A Fit-width ⇄ RTF-page toggle switches between
  screen fit and true paginated paper.
- **Ghost slots.** Unassigned roles render as ghosted placeholder rows/cells, so the
  mock shell is visible and fills in as data is assigned. An inline `+ Add variable`
  target sits in the stub.
- **Direct manipulation.** Clicking a part of the shell (banner, stub row, title,
  footnote) selects it and drives the right-rail property pane to that element's
  options. This is what beats teal's opaque wall of sidebar inputs: the preview *is*
  the encoding.

## 8. Right properties rail (how the selected output is configured)

Role slots are **derived from the output template** — Demographics exposes Treatment
arms / Summarize / Population; an AE table instead exposes a SOC▸PT hierarchy slot.
Each empty slot opens a picker showing **only eligible variables** (Treatment arms
won't offer a numeric).

- **Roles** — the template's role slots (the primary tab).
- **Options** — per-stat decimals, statistic recipes, labels, footnotes, format.
- **Filters** — population / subset filters.
- **Ranks** — top-N / incidence cutoff (e.g. AE tables).
- **Actions / Rules** — cross-output interactivity; deferred to a later milestone.

## 9. Persistence — JSON project + reproducible code

Two complementary saves; both matter and are not either/or.

- **JSON project (the reopenable build).** The S7 document model
  (`report → page → output → role → dataitem`) serialises to **JSON with a
  `schema_version`**, with forward **migrators**. Save, close, reopen later, keep
  working. Never `saveRDS` the live S7 tree. The whole set of outputs persists as one
  project.
- **Reproducible code (the deliverable).** Every output — and the whole report —
  emits **runnable R** (the `render_plan`/emit seam). Generated *from* the model so it
  can never drift from what is shown. This is the artifact for the submission and for
  QC / double-programming.

## 10. Data mode

- **Catalog** (left) — loaded/available datasets, lazy via DuckDB, row/col counts,
  in-memory vs lazy state indicator; `+ Import` (artoo ingest).
- **Grid** (center) — the datasetviewer DuckDB grid: lazy sample, sort, filter,
  type-chip column headers.
- **Variable profile** (right) — type, label, format, distinct values, distribution,
  missingness for the selected column.
- **The bridge** — a variable/dataset action **"Use in a new output →"** jumps to
  Report mode with that data pre-seeded, so exploration flows into the deliverable.

## 11. Visual language

- **Default skin: light SAS-blue (datasetviewer).** SAS blue `#0378cd`, 13px base,
  10.5px uppercase micro-label headers, 18×18 type chips (category=blue, measure=
  violet, date=amber), 3px radius, 2px blue left-stripe selection, no solid-blue
  selectize fill. Familiar to the SAS/clinical audience, honours the north star.
- **Dark modern skin: a later theme toggle.** Indigo-violet accent, tabular figures,
  keyboard-driven (⌘K palette, shortcut hints), output rendered as a white paper
  sheet on a dark canvas. The frame is skin-agnostic, so shipping this as a toggle is
  cheap once the light default lands. Not required for v1.
- **Recorded defaults** (flagged during design, changeable): light default + dark
  toggle; ship a light mode primarily; ⌘K palette, undo/redo, autosave are in;
  accent hex pending a proper token pass.

## 12. What changes vs the current app

- **Demotes the Section A / Section B peer structure.** Manage Data becomes the
  **Data** mode; the Report Builder becomes **Report** mode with the dual-rail frame.
- The current `task_catalog` / `setup_pane` / roles concepts survive but move into the
  rail structure: task catalog → **Outputs** rail; setup pane → **right properties
  rail**; the deliverable list → **Outline** rail.
- The `ex_` prefix and `explorer`/`explorerengine` names are retired in favour of
  bare exports and the `arframe` / `arpillar` packages.

## 13. Out of scope for this design / deferred

- Cross-output **Actions/Rules** interactivity (later milestone).
- The dark skin toggle (post-v1).
- A standalone template-**Library** management screen (templates are reached via the
  Outputs rail / Add-output; a full manage screen is YAGNI until needed).
- Connectors to heavy DB drivers (stay in `Suggests`).

## 14. Golden-gate invariants (preserved through the rebuild)

- Non-empty RTF export matching a byte/XML golden.
- Every graph round-trips ggplot → `figure()` → golden.

## 15. Next step

Hand to `writing-plans` to decompose this into a staged implementation (this is a
large rebuild: package rename to `arframe`/`arpillar`, shell/dual-rail frame, the
five left activities, the canvas shell-preview + direct manipulation, the right
properties rail, Data mode, JSON persistence + migrators, the light skin, and the
code-export seam).
