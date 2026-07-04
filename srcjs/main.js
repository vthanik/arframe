// Entry for the committed bundle (inst/www/arframe.bundle.js). The bridge is
// the v1 Galley client logic, moved verbatim from inst/www/arframe.js; the
// toolbar is the first Preact component (canvas top toolbar). Rebuild with
// Rscript tools/build.R -- the bundle is committed so the package installs
// without Node.
import "./bridge.js";
import "./toolbar.js";
