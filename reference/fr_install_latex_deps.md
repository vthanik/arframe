# Install Required LaTeX Packages

Proactively installs all LaTeX packages needed by arframe's PDF backend.
Checks which packages are missing, then installs them using a three-tier
strategy:

## Usage

``` r
fr_install_latex_deps(repository = NULL)
```

## Arguments

- repository:

  Character or `NULL`. CTAN mirror URL for the system `tlmgr` fallback
  (Tier 2). Default `NULL` uses
  `"https://mirror.ctan.org/systems/texlive/tlnet"`. Set this for
  corporate networks that require a specific mirror.

## Value

Invisible `NULL`. Called for its side effect of installing packages.

## Details

1.  **TinyTeX** — if
    [`tinytex::is_tinytex()`](https://rdrr.io/pkg/tinytex/man/is_tinytex.html)
    is `TRUE`, uses
    [`tinytex::tlmgr_install()`](https://rdrr.io/pkg/tinytex/man/tlmgr.html)
    (user-writable, auto-updates).

2.  **System tlmgr** — if TinyTeX fails or is absent but system `tlmgr`
    is on `PATH`, calls `tlmgr --repository <mirror> install` directly.
    This handles corporate RHEL/Ubuntu systems with system TeX Live.

3.  **Error** — if no TeX distribution is found, provides install
    instructions.

Missing packages are detected via
[`tinytex::check_installed()`](https://rdrr.io/pkg/tinytex/man/check_installed.html)
when available, falling back to `kpsewhich` on the command line.

## See also

[`fr_latex_deps()`](https://vthanik.github.io/arframe/reference/fr_latex_deps.md)
for the package list,
[`fr_render()`](https://vthanik.github.io/arframe/reference/fr_render.md)
for PDF output.

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard usage
fr_install_latex_deps()

# Corporate network with specific mirror
fr_install_latex_deps(
  repository = "https://ctan.mirror.garr.it/mirrors/ctan/systems/texlive/tlnet"
)
} # }
```
