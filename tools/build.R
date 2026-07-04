# Rebuild the committed JS bundle from srcjs/. Run from the package root:
#   (cd srcjs && npm ci)   # one-time (or npm install on first run)
#   Rscript tools/build.R
# The bundle is committed (inst/www/arframe.bundle.js) so R CMD INSTALL
# never needs Node; srcjs/ is .Rbuildignore'd out of the tarball.

if (!file.exists("DESCRIPTION") || !dir.exists("srcjs")) {
  stop("Run from the package root: Rscript tools/build.R")
}

status <- system2(
  "npx",
  c(
    "--prefix", "srcjs",
    "esbuild", "srcjs/main.js",
    "--bundle",
    "--format=iife",
    "--target=es2018",
    "--outfile=inst/www/arframe.bundle.js"
  )
)
if (status != 0) stop("esbuild failed with status ", status)
cat("Wrote inst/www/arframe.bundle.js\n")
