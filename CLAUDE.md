# arframe

Submission-native clinical report builder — the **Galley** Shiny UI on the
**arpillar** engine (thin bslib shell; all engine logic lives in arpillar,
`Remotes: vthanik/arpillar`).

- `arframe()` is the only export; everything else is `@noRd` internal.
- One injected structured store is the sole inter-module channel; modules
  only wire UI ↔ store. No DBI/cards/ggplot2/tabular calls inside a
  `render*`/`observe`/`reactive`.
- Dev loop: `devtools::document()` → `test()` → `check()` (0/0/0) +
  `air format R/ tests/`; rebuild JS with `Rscript tools/build.R` when
  `srcjs/` changes.

**All design decisions, locked user calls, working conventions, and the
live backlog are in `CLAUDE.local.md` (gitignored, local). Read it at
session start — do not re-derive or re-litigate what it records.**
