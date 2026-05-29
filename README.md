# `dda` — Distributional Data Analysis

<!-- badges: start -->
<!-- badges: end -->

R package for working with probability densities as first-class
objects in the [Bayes-Hilbert space](https://en.wikipedia.org/wiki/Bayes_space)
of functional data. Each density is represented by its centred log-ratio
(`clr`) transform stored as a B-spline `fd` object, so the operations
that make sense for densities — geometric mean, perturbation,
scalar powering, distance in the Aitchison-Bayes geometry — reduce to
ordinary functional-data operations on the `clr`.

Density estimation is done by penalised log-spline maximum likelihood
(Silverman 1982; Ramsay & Silverman 2005). The inner Fisher-scoring
loop is reimplemented in Rust and exposed via [savvy](https://yutannihilation.github.io/savvy/guide/);
typical wall-clock speedups vs the original `fda::density.fd` are
**10–60×**, and the port fixes two real bugs in the R reference
(see the vignette).

## Installation

From GitHub:

```r
# install.packages("devtools")
devtools::install_github("camillemndn/dda")
```

A Rust toolchain (`cargo` + `rustc` ≥ 1.78) is required at install
time. Cargo dependencies are pre-vendored under `src/rust/vendor/`,
so no network access is needed.

If you use Nix, `nix develop` (or `nix-shell`) sets up everything
including the dev tools (`cargo`, `rustfmt`, `clippy`, `savvy-cli`).

## Quick example

```r
library(dda)

# Fit a density to a sample
set.seed(1)
x <- c(rnorm(300, -1, 0.7), rnorm(200, 1.5, 0.5))
p <- dd(x, rangeval = c(-4, 4))

# Plot
plot(p)

# Bayes-space operations
q <- dd(rnorm(500, 0, 1.5), rangeval = c(-4, 4))
p_plus_q  <- p + q           # density "addition" (perturbation)
p_minus_q <- p - q           # density "subtraction"
two_p     <- 2 * p           # scalar power: p^2 / Z

# Geometric mean of a list of densities
samples  <- lapply(1:10, \(.) rnorm(200))
ddl      <- as_dd(samples)
mu       <- gmean(ddl)
```

## Backend selection

`density_mpl()` is the unified entry point with two backends:

```r
b <- fda::create.bspline.basis(c(-4, 4), nbasis = 13)
Wp <- fda::fdPar(fda::fd(matrix(0, 13, 1), b))

r1 <- density_mpl(x, Wp)                       # default: rust backend
r2 <- density_mpl(x, Wp, backend = "legacy")   # original R algorithm
```

The legacy backend is a verbatim port of `fda::density.fd` and is kept
only for bit-for-bit compatibility with prior results. The Rust backend
is the default; it is **faster and corrects two bugs** in the legacy
code path:

1. Incorrect Fisher-information scaling for frequency-weighted input
   (off by a factor of `N`).
2. The `stepit` line search exits at non-stationary points whenever it
   cannot make further progress, often with gradient norm ~10⁻¹ at
   termination.

Both are documented with derivations in `vignettes/density-mpl.qmd`.

## What's in the box

- `dd()`, `as_dd()` — constructors for distributional data objects.
- `density_mpl()` — penalised log-spline MLE with Rust / legacy R
  backends.
- `+.dd`, `-.dd`, `*.dd`, `gmean()`, `var()`, `cov()`, `center()`,
  `relative()` — Bayes-space arithmetic and summaries.
- `plot.dd()`, `plot.ddl()`, `plot_funs()` — `ggplot2`-based
  visualisation.
- `dd_eval()` — evaluate a `dd` density at arbitrary points.

## Status

**Alpha / research code.** The Rust kernel is well-tested
(end-to-end equivalence + closed-form moment-matching tests against
the MLE first-order condition). The surrounding R API is still
settling — expect signature changes and gaps in documentation
for non-`density_mpl_*` functions. Use at your own risk.

## Citation

If you use the Rust density estimator or the bug analysis in published
work, please cite the vignette at `vignettes/density-mpl.qmd` until
a paper is available.

## License

MIT. See `LICENSE`.
