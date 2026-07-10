
## 2026-07-10 — restyling Shiny/bslib controls: verify computed styles, not your CSS

- **Pattern:** restyled `radioButtons` pills looked broken three ways before
  the real causes surfaced: (1) the browser served a CACHED arframe.css —
  hard-reload (or cache-bust the `<link>`) before eyeballing any CSS change;
  (2) bslib/shiny-sass injects `column-gap: 13px` on `.shiny-options-group`
  and `padding-left: ~23px` on `.radio-inline` (radio-dot space) that outrank
  a 2-class selector — zero them explicitly with a 3-class selector; (3) two
  exact 50% flex bases + borders can overflow and stack full-width — use a
  smaller basis (40%) and let grow fill.
- **Rule:** when a restyle "doesn't apply", read `getComputedStyle()` in the
  live page FIRST (gap, padding, flex, wrap) instead of iterating blind CSS.
- Also: `frame-mode` posts without `priority: "event"` and the store is
  shared across sessions — a click that races the page load leaves mode
  desynced (nav stuck on Setup after reload). Pre-existing bug, not yet fixed.
