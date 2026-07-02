# arframe — the Galley design system (binding)

- **Status:** approved 2026-07-02 (user picked Galley over Instrument / Registry /
  blend, from three from-scratch mockup directions; "redesign from scratch, no
  constraint").
- **Supersedes:** the visual-language and layout sections (§5 frame regions, §11
  visual language) of `2026-07-01-arframe-ui-redesign-design.md`, and the
  datasetviewer light-SAS-blue skin as the default. The PRODUCT paradigm of that
  spec stands unchanged: submission-native, report-as-project, fill-a-shell,
  screen==paper, JSON + reproducible-code persistence, store/module architecture
  (§5.1–§5.5).
- **Thesis:** the deliverable is the interface. arframe is the compositor's proof
  room: a typeset page on a desk, a table of contents beside it, proof stamps for
  status, and a galley card summoned from the page itself. No competitor centres
  the document; this design is unmistakable and subject-true.

## 1. Tokens (`--ar-*`, single source `inst/www/tokens.css`)

| Token | Value | Use |
|---|---|---|
| `--ar-desk` | `#E9EBEA` | The canvas everything sits on (cool lab gray) |
| `--ar-chrome` | `#FBFBFA` | App bar + status bar |
| `--ar-paper` | `#FFFFFF` | The page |
| `--ar-paper-edge` | `#C9CDCF` | Page + card border |
| `--ar-card` | `#FCFCFB` | Galley card / overlay surfaces |
| `--ar-rule` | `#D8DCDD` | Chrome hairlines |
| `--ar-rule-2` | `#E4E7E8` | Inner hairlines (paper running head, card dividers) |
| `--ar-rule-3` | `#E0E3E4` | Quietest hairline (card item borders) |
| `--ar-ink` | `#1B1F23` | Primary text; the solid Export button |
| `--ar-ink-2` | `#3A4046` | Secondary text (paper stat stubs) |
| `--ar-ink-3` | `#5C6670` | Tertiary (footnotes, quiet buttons) |
| `--ar-ink-4` | `#7A838C` | **Decoration only** (hairline-adjacent, at-rest chip borders) — NOT info-bearing text |
| `--ar-ink-5` | `#9AA3AB` | **Decoration only** (leader dots) — NOT text |
| `--ar-accent` | `#2D5FA8` | Ink-stamp blue: selection, active, links, focus-ring pair |
| `--ar-accent-weak` | `rgba(45,95,168,0.07)` | Active washes |
| `--ar-ready` | `#257045` | READY stamp |
| `--ar-draft` | `#7A5409` (text) / `#C9A227` (border) | DRAFT stamp |
| `--ar-error` | `#B3261E` | ERROR stamp, error summary, destructive |
| `--ar-focus` | `#FFDD00` | Keyboard focus (paired with `#0B0C0C` shadow — GOV.UK two-tone) |
| chips | cat `#0369A1`/`#E0F2FE` · meas `#6D28D9`/`#EDE9FE` · date `#92400E`/`#FEF3C7` | Variable type chips (18×18, unchanged) |
| `--ar-radius-sm` | `1px` | Stamps |
| `--ar-radius` | `2px` | Everything else (print-square, deliberately tighter than before) |
| `--ar-shadow-float` | `0 8px 24px rgba(20,28,40,0.14)` | ONLY on true overlays (Add-output dialog, unpinned galley card, palette) |
| spacing | `4 / 8 / 12 / 16 / 24 / 32 px` | The only spacing values |
| motion | `120ms / 180ms`, `cubic-bezier(0.2,0,0,1)`; `prefers-reduced-motion` collapses all | |

Colour semantics are LOCKED: blue = selection/active, red = error, yellow = focus,
green/amber = stamp states only. Never repurposed. Every colour signal pairs with
text or shape (stamps carry words; ghost slots carry dashes + words).

**Contrast floor (binding, gated by the token test).** Every info-bearing text
run — micro-labels, TOC numbers, group headers, the status bar, the source line,
stamp text — must clear **WCAG AA 4.5:1 against its background** (desk / paper /
chrome). `--ar-ink-3 #5C6670` is the muted-gray floor for small text on the desk
(4.88:1). `--ar-ink-4`/`--ar-ink-5` are for decoration (leader dots, hairlines)
only. The stamp text hexes in the token table are Task-4-reconciled against the
desk: `--ar-ready #257045` (5.04:1), `--ar-draft #7A5409` (5.66:1, text only --
the `#C9A227` border is non-text and only needs 3.0:1), `--ar-error #B3261E`
(5.46:1, unchanged). The earlier indicative mockup values (`#2E7D4F` 4.21:1,
`#9A6B0B` 3.91:1) both failed the floor and were replaced; the Task-4 contrast
test (`test-theme.R`) is the gate these hexes are pinned against.

## 2. Type

- **UI face:** IBM Plex Sans (400 / 500 / 600). **Data face:** IBM Plex Mono
  (400 / 500). Both OFL — self-host latin woff2 subsets in `inst/www/fonts/`
  with an `inst/COPYRIGHTS` entry; `@font-face` in `tokens.css` (linked, not
  inlined, so relative font URLs resolve). System fallbacks:
  `"IBM Plex Sans", system-ui, -apple-system, sans-serif` /
  `"IBM Plex Mono", ui-monospace, SFMono-Regular, Menlo, monospace`.
- **Mono owns:** TLF numbers, stamps, micro-labels, option values, the status
  bar, the ⌘K hint, the source line, and THE ENTIRE PAPER (the RTF deliverable
  is Courier; the screen preview renders its tabular content in Plex Mono —
  paper honesty is the identity).
- Scale: body 13px · secondary 12px · micro-label 11px caps `letter-spacing:0.12em`
  500 · paper table 11px mono (screen) · report title 12.5–13px 500 · stamps 11px
  mono caps `letter-spacing:0.08em`.
- Micro-labels and stamps are the ONLY uppercase; everything else sentence case.

## 3. Anatomy (one 100vh frame, no side icon rails)

```
┌──────────────────────────────────────────────────────────────────┐
│ 42px bar: arframe · | · <report title ✎> ······ Data  QC  ⌘K  [Export package] │
├──────────────┬───────────────────────────────────────────────────┤
│ CONTENTS     │                    desk                            │
│ (280px col,  │        ┌───────────────────────┐   ┌────────────┐ │
│  on the desk,│        │   the typeset page    │   │ galley card│ │
│  no panel    │        │   (paper, mono)       │   │ (floating; │ │
│  chrome)     │        │                       │   │  pinnable) │ │
│              │        └───────────────────────┘   └────────────┘ │
├──────────────┴───────────────────────────────────────────────────┤
│ 26px status: 4 outputs · 2 ready ·············· adsl 254×48 · saved 14:02 │
└──────────────────────────────────────────────────────────────────┘
```

- **App bar (42px, `--ar-chrome`):** mono wordmark `arframe` · hairline divider ·
  report title (click-to-edit inline, pencil affordance) · spacer · quiet text
  buttons `Data` and `QC` (mode switches; active = accent text + underline) ·
  `⌘K` mono hint · `Export package` (solid `--ar-ink` button, the ONE filled
  control in the chrome).
- **Contents column (280px, set directly on the desk — no panel background):**
  `CONTENTS` micro-label; groups `TABLES / FIGURES / LISTINGS` (micro-labels,
  faint); one entry per output: mono number + sans title, a dotted leader
  (`border-bottom: 1px dotted`) filling to a right-aligned **stamp**. Active
  entry = 2px accent left bar + `--ar-accent-weak` wash. Hover reveals a grip
  (drag-reorder, SortableJS) and a kebab (rename / duplicate / remove).
  Bottom: `+ Add output` accent text action. The TOC **is** the output
  switcher — there are NO canvas tabs; one page at a time.
- **Desk/canvas:** the page centred, true page proportions; a quiet mono
  fit ⇄ page toggle at the desk's top right. The paper carries: running head
  (protocol left, `Page n of n` right, hairline under), centred title block
  (`Table 14.1.1` / title / population — all mono), the table (mono, single
  top/bottom ink rules — no vertical rules, no zebra), footnote block above a
  closing rule, and the **source line** (`Source: adsl · arframe 0.1 · <date>`,
  faint mono — provenance is part of the design).
- **Galley card (the inspector):** summoned by clicking a page REGION or a TOC
  entry's configure action. Floats right of the page (`--ar-card`, paper edge
  border, float shadow), 320px. Header: region micro-label + **pin** + close.
  Pinned = docks as a flush right panel (shadow drops, becomes part of the
  frame) and persists across output switches. Esc closes when unpinned.
- **Status bar (26px):** mono, faint: `4 outputs · 2 ready` · spacer · active
  dataset `adsl 254×48 · lazy` · `saved 14:02`.

## 4. Page regions and direct manipulation (the primary navigation)

The page is divided into click regions; hovering shows a faint accent outline,
clicking selects (1.5px accent outline, `outline-offset: 1px`) and opens the
galley card routed to that region:

| Region (`data-ar-region`) | Card contents |
|---|---|
| `title` | Number, title, population line, page-fit options |
| `columns` (header band) | Treatment-arms role slot (+ eligible picker) |
| `rows` (stub/body per summarize block) | Summarize slot: chips + grip reorder + remove; add-variable dashed row; per-region stat options (decimals) |
| `footnotes` | Footnote lines (one per row, add/remove) |
| `source` | Read-only provenance + "View code" action (emit_code preview) |
| figure regions: `axes`, `series`, `legend` | x/y/group (or time/censor/group) slots; figure options (palette, legend, intervals, at-risk) |
| `filters` (a small `Population: Safety` tag under the title when filters exist) | Presets + filter builder + live `matched of total` count |

**Ghost shell:** an unfilled region renders ON THE PAPER as a dashed ghost block
in the exact position the real content will occupy — dashed `--ar-paper-edge`
border + mono hint (`assign treatment arms`) + a `+` glyph. The page is always a
complete shell from the first second. An empty report shows a blank sheet with a
ghost title block and one CTA (`Add output`).

**Add output:** a centred overlay card (float shadow): template list (kind glyph +
label + description) · dataset picker (empty by default) · a `Recommended for
your data` section (from `detect_structure` over the catalog — the Suggest
surface lives here, not in a rail). Confirm = adds, selects, closes.

**QC (app-bar button):** swaps the desk for the proof-check sheet — a paper-styled
summary listing every output with its stamp, each `validate_output` problem as a
jump link, and the run log (mono). This is the Review surface.

**Data (app-bar button):** same frame, data furniture: contents column lists
datasets (name + rows×cols + structure tag + loaded state), desk shows the
datasetviewer grid for the selected dataset, and a pinnable galley card shows the
variable profile (type, distinct, missing, top values with count bars) + the
`Use in a new output →` bridge (pre-seeds Add-output's dataset).

## 5. Stamps (the status vocabulary)

Mono caps, 11px, `letter-spacing:0.08em`, 1px colored border + colored text,
`--ar-radius-sm`, transparent fill (letterpress, never a filled pill):

| Stamp | Colour | Oracle source |
|---|---|---|
| `READY` | `--ar-ready` | `output_status == "ready"` |
| `DRAFT` | `--ar-draft` | `"draft"` |
| `NO DATA` | `--ar-ink-4` | `"needs_data"` |
| `ERROR` | `--ar-error` | app-side broken flag (render failed) |

Each stamp carries `aria-label` with the full sentence ("Ready to render").
Colour never alone: the word IS the signal.

## 6. Interaction + accessibility (carries over, binding)

- GOV.UK two-tone keyboard focus (`--ar-focus` + near-black), focus ≠ selection
  (accent reserved for selection). `fieldset` + `legend` (the micro-label is the
  legend) for every grouped control in the card. Error summary (`role="alert"`,
  "There is a problem", focus moved, jump links) renders at the top of the PAPER
  when a render fails; inline card messages word-identical (one source:
  `validate_output`).
- Keyboard: ↑/↓ move TOC selection; Enter opens the card on the selected output's
  first incomplete region; Esc closes unpinned card; ⌘K palette is a v1.1 task.
- Radios for small sets, checkboxes for multi; numeric inputs `type="text"` +
  `inputmode`; visible labels always (never placeholder-as-label).
- Responsive: below 1100px the contents column collapses to a slim numbered strip
  (numbers + stamps only, flyout on hover/focus); the galley card becomes a
  bottom sheet. Verify reflow at 1000 / 1440 / 1920 via shinytest2 viewports.
- Reduced motion collapses the card float-in and ghost-fill transitions.

## 7. What died with the rails (deliberate deletions)

- No left/right icon strips, no five-activity rail, no canvas output tabs, no
  page-tab strip: TOC = navigation, page regions = configuration, app bar =
  modes. Fewer chrome layers than Viya/teal/rapido is the point.
- Outputs rail → the Add-output overlay. Suggest rail → the overlay's
  Recommended section. Review rail → the QC sheet. Options/Filters/Ranks panes →
  galley-card regions. Ranks remains a disabled "coming" row in the relevant
  card region.

## 8. Future skins

Instrument (the dark direction) is preserved as a later theme toggle: the frame
is token-driven, so dark = a token swap (`--ar-desk → #101215` etc.). Not v1.
