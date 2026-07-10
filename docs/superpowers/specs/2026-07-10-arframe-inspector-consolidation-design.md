# arframe — inspector consolidation, help system, auditable file names

Date: 2026-07-10. Status: approved direction, spec for implementation planning.
Supersedes nothing wholesale; amends the inspector half of the #12.3 LoC design
(`2026-07-07-arframe-setup-dashboard-top-nav-design.md` stays in force for the
frame and Setup).

## Source feedback (2026-07-10 screenshots)

1. Remove the **Ranks** tab — roles already drag-drop; ordering controls move
   into Options. Keep exactly three panes: **Roles · Options · Filters**.
2. Remove the inspector's **right icon rail** entirely; the pane switch becomes
   a horizontal strip at the top of the panel.
3. Every inspector section becomes an **accordion** the user can fold.
4. Every Setup section and inspector section gets a small **`?` help icon**
   opening an in-depth help modal (datasetviewer Filter-help style). Inline
   gray helper text is removed.
5. `outputs/*.json`, `programs/*.R`, and `output/*.rtf` share one **slug
   filename** (`l-16-2-7-adverse-event-listing.*`) — `out005.R` is not
   auditable.
6. Code view highlighting must match datasetviewer quality — and must **not**
   be hand-rolled: use the ecosystem package (posit/r-lib mechanism).

Design bar (user call, 2026-07-10): **no compromise on visual quality** — the
result must read like the best contemporary dashboard products (nine reference
shots reviewed in `/Users/vignesh/Downloads/*.webp`), exceeding stock
Posit/Shiny apps.

## Design language (distilled from the nine references)

All references — light CRM dashboards, Sence.Point, Mota, TWISTY, and the two
dark NFT shells — share one system. arframe adopts the light variant (dark
"Instrument" stays a later theme):

- **Canvas & cards.** Cool light-gray canvas; content lives on white cards,
  radius 14–16px, soft two-layer shadow (a hairline `0 1px 2px` plus a wide
  diffuse `0 8px 24px` at low alpha). No hard borders where a shadow can carry
  the edge; hairlines (`--ar-line`) only *inside* cards.
- **Type hierarchy.** Big friendly headings; small uppercase letter-spaced
  muted labels for sections and table headers; mono (`ar-mono`) reserved for
  instrument values (numbers, IDs, dataset names, code).
- **Pill segmented controls.** Mode/pane switches are rounded-full segmented
  pills with a filled active state (LUVAL's "All History | Purchase" pattern) —
  not underlined tabs, not icon-only rails.
- **Soft-tinted icon chips.** Leading icons sit in rounded squares/circles with
  a soft brand-tinted fill (Piduiteun/TWISTY project icons) — arframe already
  has the type-chip vocabulary (`#`, `A`, calendar…); this pass standardises
  the treatment (one size, one fill scale) across pickers, role rows, and
  accordion headers.
- **Status = dot + word.** Colored 6px dot plus a plain word (Complete /
  In progress / Canceled), never a heavy filled badge.
- **Controls.** Rounded-lg inputs with quiet borders that sharpen on focus;
  one prominent rounded primary button per surface (Run); everything else is
  ghost/quiet. Generous padding — reference surfaces are never cramped.

Implementation notes: everything expressible as tokens goes in
`inst/www/tokens.css` (radius, shadow pair, chip fills); components consume
tokens only. The `frontend-design` skill is loaded at implementation time and
each surface is screenshot-eyeballed against the reference shots before
sign-off (standing rule).

## 1. Inspector frame (`mod_card.R`)

- Delete the far-right `.ar-insp-tabs` icon rail and its collapsed-strip
  behaviour.
- New **top strip** inside the panel: a segmented pill control
  `Roles · Options · Filters` (labels, not icon-only). Help lives on the
  sections (§4), not the strip. Same mechanism as today: pure CSS class flip
  (`ar-insp-tab-*` on the card root) via one custom message; panes stay
  always-mounted; no re-render on switch.
- `ranks` leaves `.INSP_TABS`; `mod_card_ranks.R` is deleted (its controls
  relocate, §2). `.card_region_group()` unchanged (no region ever mapped to
  ranks).
- Collapse behaviour: the inspector still folds when nothing is selected; the
  re-open affordance becomes an explicit panel-toggle button at the right end
  of the canvas toolbar (no rail remains to click). Resize handle stays.

## 2. Ranks → Options ORDER section (`mod_card_options.R`)

One accordion section, labelled **ORDER**, rendered per generator:

| Generator | Control | Commit |
|---|---|---|
| summary / crosstab | row-block drag list (existing rows) | `.reorder_slot()` (unchanged shared helper) |
| occurrence | incidence order: `freq` / `alpha` as a two-way pill | `options$hier_sort`, default elided |
| line / box | x-level drag list | `options$x_order` |
| km | section not rendered | — |

Row style follows the datasetviewer sort-key rows (mono name, control on the
right); the listing generator's existing SORT/stack editors already match and
stay as-is. Empty states keep the "assign variables in Roles first" directive
sentence — relocated into the section body, not deleted.

## 3. Accordions (`utils_atoms.R`)

- New `.accordion_section(id, label, icon, body, open = TRUE, help = NULL)`
  atom: native `<details>/<summary>` — no JS library. Summary row = soft-tinted
  icon chip + uppercase label + optional muted count/summary + `?` icon
  (stopPropagation) + rotating chevron.
- Every section in Roles, Options, and Filters renders through it,
  default-open. Open/closed state is client-side only (native DOM), survives
  server re-renders of *sibling* sections but not its own re-render — accepted
  (matches current section behaviour).
- Setup section cards keep their card layout (they are page-level cards, not
  accordions) and gain only the `?` icon in the card header.

## 4. Help system (`R/utils_help.R` — new)

- `.help_icon(ns, topic)` atom: small circled `?` button, right end of section
  headers / card headers; posts `{topic, nonce}` to ONE shared observer per
  module scope (the `cell_edit` idiom — no per-section observers).
- One shared `.show_help(topic)` opens `shiny::modalDialog` with content from
  the **help registry**: a named list `topic -> shiny tagList` in
  `utils_help.R`.
- **Content bar (user call): in-depth, any-level-of-user, never two words.**
  Every entry is a self-contained tutorial in the datasetviewer Filter-help
  format: bold heading; plain-language prose explaining what the section does
  and *why it exists in a submission workflow*; inline code chips; 2–4 worked
  examples with real ADaM columns (`AGE >= 18`, `SAFFL = "Y"`,
  `AEBODSYS is not na`, `TRT01A` vs `TRT01P`); a "what happens to the rendered
  table" consequence line per choice where the topic is an Options section.
- Modal styling matches the reference: rounded card, sectioned prose, bordered
  code chips/blocks; ⎋/X to close; no nested scrolling traps.
- All inline gray `.ar-opt-hint` / helper paragraphs are removed; their content
  is absorbed (and expanded) into the registry. Directive empty-state sentences
  (§2) are the one exception — they are calls to action, not help.
- Registry coverage test: every accordion section id and Setup card id has a
  registry entry (test fails on a new section without help).

## 5. Slug filenames everywhere (arpillar + arframe)

- `.output_slug()` moves to arpillar (exported there; arframe drops its copy):
  `"<t|f|l>-<number>-<kebab-title>"`; empty slug falls back to `@id`; on
  collision within a report, append `-<id>`.
- `arpillar::report_to_folder()` writes `outputs/<slug>.json` and removes any
  stale `*.json` it did not emit this pass (the whole-folder rewrite already
  exists; deletion closes the rename gap). `folder_to_report()` already globs
  `outputs/*.json` — no read-side change. `@id` stays the stable identity
  inside the JSON.
- arframe `.emit_programs()` writes `programs/<slug>.R`, keeps `run-all.R`,
  and removes stale `.R` files it did not emit (never touching `run-all.R`).
- The async render path writes `output/<slug>.rtf` with the same stale-file
  cleanup; `manifest.csv` and the code-view/RTF download names pick the slug
  up automatically.
- Golden-gate sensitive: the RTF byte goldens must be re-pointed at the new
  paths, not regenerated.

## 6. Code view highlighting (`mod_paper.R`)

- Delete `.hl_r()`. Render
  `downlit::highlight(script, classes = downlit::classes_pandoc())`, falling
  back to escaped plain text when highlighting returns `NA`/errors.
- `downlit` (r-lib, 0.4.5 installed; the pkgdown mechanism) joins `Imports` —
  approved 2026-07-10.
- CSS maps downlit's pandoc classes (`fu`, `op`, `st`, `fl`, `co`, `va`, `kw`)
  onto the datasetviewer palette: purple keywords/functions, red operators and
  pipe, green strings, blue numbers, gray italic comments.
- downlit auto-links known functions to their reference docs — kept (links
  open in a new tab); textContent parity for the Copy button is preserved and
  pinned by a test.
- The code pane itself is restyled to the card language: bordered light block,
  mono at a comfortable size, filename header row with Copy / Download / Close
  as quiet buttons (already close; polish to token values).

## Testing

- Slug: rename deletes stale json/R/rtf triplet; collision suffix; empty-title
  fallback; round-trip load unchanged.
- Ranks relocation: each ORDER control commits from Options (row-block order,
  hier_sort elision, x_order) — regression tests moved, not dropped.
- Help: registry covers every section/card id; modal opens from a section
  header without toggling the accordion.
- Highlighting: downlit spans present; `textContent == script` byte parity;
  plain-text fallback on highlight failure.
- Standing screenshot-eyeball pass (real CDISC pilot data) across Report
  drill, Setup, and the code view against the reference shots.

## Out of scope

Dark theme; Data-mode help modals; Ranks-style ordering for future generators;
parsing external `.R` edits back into specs.
