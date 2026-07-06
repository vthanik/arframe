# Dev launcher: load_all() the arframe worktree, mount the CDISC pilot
# folders, run the app on a fixed port so Claude's preview tool can attach.
options(shiny.port = 4321, shiny.host = "127.0.0.1", shiny.autoreload = TRUE)
devtools::load_all("/Users/vignesh/projects/r/arframe-setup-redesign")
arframe(folders = c(
  ADaM = "/Users/vignesh/projects/data/cdisc-adam-pilot",
  SDTM = "/Users/vignesh/projects/data/cdisc-sdtm-pilot"
))
