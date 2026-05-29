# Migration plan: from fda-subclass to self-contained `dd` class

## Goal

Reposition `dda` from "wrapper around `fda::density.fd`" to "typed
algebra for densities-as-data with multiple geometries and pluggable
ecosystem conversions". Concretely:

- `dd` becomes its own canonical class with its own storage layout.
- `fda` and `tidyfun` become *conversion targets*, not parents.
- The Rust kernel stays as the inner numerical loop, fed a flat
  representation that `dd` natively stores.
- New geometric machinery (Wasserstein distances, optimal-transport
  barycentres, density-on-density regression) gets added on top of
  the typed core — this is the contribution that justifies the
  package's existence.

## Why

The current `dd` is `class = c("dd", "fd")`. Most operations are
thin wrappers around `fda::*.fd`. The package's value is reduced to
"`fda::density.fd` plus a few bug fixes and a Rust speedup." That
positioning makes the package hard to defend as a research artifact
beyond the bug story.

Owning the type lets us:

1. Enforce density-specific invariants in the constructor.
2. Implement multiple geometries (Bayes / Wasserstein / L²) with
   correct operator semantics per geometry, rather than overloading
   one ambiguous set of arithmetic methods.
3. Decouple from `fda`'s release cadence and API.
4. Let the Rust kernel speak the canonical `dd` representation
   directly, removing the `.pack_basis()` translation layer.
5. Open the door to Wasserstein and regression methods that don't
   fit fda's `fd` model at all (e.g., quantile-function storage).

## Non-goals

- Becoming a fork of `fda`. We keep `as_fd()` so downstream tooling
  works.
- Forcing users into a tidyverse workflow. `as_tfb()` is optional.
- Reimplementing every `fda` algorithm. We provide density-specific
  primitives and let users coerce out for everything else.

---

## Target architecture

### Canonical storage

A `dd` object holds:

```
$kind         : character   # "bspline" | "fourier" | "polygonal" | "wasserstein" | ...
$nbasis       : integer
$range        : numeric(2)
$knots        : numeric     # bspline / polygonal
$norder       : integer     # bspline
$params       : numeric     # fourier (period), power, expon, …
$coefs        : matrix      # nbasis × n_realisations
$constant     : numeric     # length n_realisations
$sample       : list / NULL # raw observations (optional)
$geometry     : character   # "bayes" | "wasserstein" | "l2" — declares which algebra applies
```

The geometry tag is important: it determines which `Ops.dd` branch
fires, which distance is the default, and prevents silent
geometry-mismatch bugs (a `dd` in Bayes representation should not be
silently averaged in Wasserstein space).

### Constructors

```
dd_from_sample(x, basis_kind = "bspline", ...)   # current dd()
dd_from_clr(fd_obj_or_coefs, basis_kind, ...)    # already-clr-transformed input
dd_from_quantile(q_fn, ...)                      # Wasserstein-native: store inverse CDF
dd_from_density(p_fn, ...)                       # arbitrary density function
dd_from_grid(grid, values)                       # raw evaluations
```

### Conversions out

```
as_fd(x)       # → fda::fd        (clr fd; loses constant + sample)
as_tfb(x)      # → tidyfun::tfb   (basis-expansion functional data)
as_tfd(x)      # → tidyfun::tfd   (dense functional data)
as.numeric(x)  # evaluate on a grid (delegates to dd_eval)
```

### Conversions in

```
as_dd.fd(x, geometry = "bayes")
as_dd.tfb(x, geometry = "bayes")
as_dd.tfd(x)
as_dd.numeric(x)         # already exists
```

### Algebraic surface

`Ops.dd` dispatches by `$geometry`:

```
Ops.dd <- function(e1, e2) {
  geom <- get_common_geometry(e1, e2)
  switch(geom,
    "bayes"       = bayes_op(.Generic, e1, e2),
    "wasserstein" = wasserstein_op(.Generic, e1, e2),
    "l2"          = l2_op(.Generic, e1, e2),
    stop("Unknown geometry: ", geom)
  )
}
```

Distances are explicit verbs (not operators):

```
bayes_distance(p, q)
wasserstein_distance(p, q, order = 2)
hellinger_distance(p, q)
kl_divergence(p, q)
```

Means / barycentres:

```
bayes_mean(ddl)        # current gmean
wasserstein_mean(ddl)  # OT barycentre — new
l2_mean(ddl)           # straightforward; new
```

### Rust kernel boundary

The kernel today consumes `(kind, nbasis, range, knots, norder,
params, coefs, weights, Z, K, ...)` as flat vectors. If `dd` stores
exactly that layout, the `.pack_basis()` translation disappears: the
R wrapper passes `dd` fields straight to `density_mpl_rust_raw()`.
Adding new operations (Wasserstein quantile-function inversion, fast
KL, …) becomes a matter of new `#[savvy]` entry points reading the
same flat shape.

---

## Phased plan

### Phase 1 — Architectural pivot (1–2 weeks)

Goal: ship a `dda` 0.2.0 that has the new type internally, the same
public surface for `density_mpl()` users, and `as_fd()` /
`as_tfb()` / `as_dd()` round-trippable conversions.

Steps:

1. **Define `new_dd()` constructor** in `R/dd-class.R` returning the
   target storage layout. Add a `validate_dd()` companion.
2. **Reimplement `dd()` from sample** to produce the new structure
   (estimate via `density_mpl()`, store `coefs`, derive
   `constant`, set `geometry = "bayes"`).
3. **Write conversions**:
   - `as_fd.dd()` — strip `constant`/`sample`/`geometry`, build an
     `fda::fd` from `coefs` + reconstructed `basisfd`.
   - `as_tfb.dd()` — build a `tidyfun::tfb` from the same.
   - `as_dd.fd()` and `as_dd.tfb()` — inverse direction.
4. **Replace `R/density-mpl-rust.R::.pack_basis()`** with a function
   that reads from the new `dd` fields directly. If `dd` already
   stores the Rust-friendly layout, this collapses to ~10 lines.
5. **Move the penalty matrix `K` and the identifiability projector
   `Z` out of fda** (or keep fda for them as a Suggests until Phase
   3). Decision: probably reimplement both natively — both are
   short and remove a fda dependency for normal use.
6. **Update operators and methods** to consult `$geometry`:
   - `Ops.dd` group method replacing `+.dd` / `-.dd` / `*.dd`.
   - `mean.dd` / `var.dd` / `cov.dd` dispatch on geometry.
   - Delete `gmean()` as a separate function; it's now
     `bayes_mean()` and the default for `mean.dd` when geometry is
     `"bayes"`.
7. **Rewrite `R/density-mpl-legacy.R` to also produce a new-style
   `dd`** (or drop it entirely — Phase 1 is a natural moment to
   delete the legacy backend; the migration is the breaking change
   that justifies removing it).
8. **Tests**: round-trip tests `dd → fd → dd` and `dd → tfb → dd`
   must be identity. Existing moment-matching tests should pass
   unchanged because they don't touch the storage.
9. **Update vignette** to lead with the type story; demote the bug
   discoveries to a section.
10. **Bump version to 0.2.0**, note breaking changes in `NEWS.md`.

Risk: this is a breaking change for any external user. We have
none confirmed. The internal user (`R/dd.R:74` calling
`density_mpl()`) only needs the constructor to keep working.

### Phase 2 — Wasserstein geometry (3–4 weeks)

Goal: add a second geometry that fda cannot represent. This is the
first time `dd` does something `fda::fd` cannot.

Steps:

1. **Add `dd_from_quantile()`** — store the density via its inverse
   CDF on a monotone B-spline basis. New `geometry = "wasserstein"`
   tag.
2. **Conversions between geometries**: `as_bayes(dd_wass)` and
   `as_wasserstein(dd_bayes)`. These are not free — they require
   numerical integration of the CDF and inverse interpolation. Add
   to Rust.
3. **`wasserstein_distance(p, q, order = 2)`**: in 1D this is just
   the L^p norm between the two quantile functions. Implement
   natively in R using existing fd machinery; benchmark; port to
   Rust if it's a bottleneck.
4. **`wasserstein_mean(ddl)`**: the OT barycentre in 1D is the
   pointwise mean of the quantile functions. Trivial once
   `dd_from_quantile()` exists.
5. **`Ops.dd` Wasserstein branch**: addition and scalar
   multiplication in Wasserstein geometry are operations on the
   quantile representation. Document the semantics (perturbation
   ↔ displacement interpolation).
6. **Wasserstein FPCA**: Petersen–Müller (2016) — log-map to the
   tangent space at the barycentre, then standard FPCA. Implement
   on top of the above.
7. **Vignette `vignettes/wasserstein.qmd`** showing density-data
   analysis with Wasserstein geometry, contrasted with Bayes.

This is the phase that makes `dda` a paper.

### Phase 3 — Density-on-density regression (2–3 weeks)

Goal: implement Petersen–Müller / Chen–Zhou / similar
distribution-on-distribution regression models. Currently no
R package has this in a usable form.

Steps:

1. **`density_regression(y ~ x, data, geometry = "wasserstein")`**
   for the simplest case (1D scalar-on-distribution or
   distribution-on-distribution).
2. **Prediction methods**: `predict.density_regression`.
3. **Diagnostic plots**: residual densities in the chosen geometry.
4. **Vignette**: a worked example with the Vietnam temperature
   data (which is already shipped).

This is the phase that gives `dda` a clear novelty pitch beyond
software engineering.

---

## Detailed checklist for Phase 1

The intent of this checklist is to make Phase 1 a series of small,
reviewable commits — not a big-bang rewrite.

- [ ] `R/dd-class.R`: `new_dd()`, `validate_dd()`, `is_dd()`.
- [ ] `R/dd-class.R`: `print.dd()` (one-line summary including
      geometry and `constant`).
- [ ] `R/dd-class.R`: `format.dd()` (for tibble pretty-printing).
- [ ] `R/dd-conversion.R`: `as_fd.dd`, `as_tfb.dd`, `as_tfd.dd`.
- [ ] `R/dd-conversion.R`: `as_dd.fd`, `as_dd.tfb`, `as_dd.tfd`,
      `as_dd.numeric` (the last is already there).
- [ ] `R/dd-eval.R`: rewrite `dd_eval()` to read from new storage.
- [ ] `R/density-mpl.R`: dispatcher unchanged; backend wrappers
      take new `dd` instead of `WfdParobj`.
- [ ] `R/density-mpl-rust.R`: collapse `.pack_basis()` into direct
      field access.
- [ ] `R/penalty.R`: native implementation of `getbasispenalty()`
      equivalent for B-spline (and any other basis types we keep).
- [ ] `R/zerobasis.R`: native implementation (5 lines).
- [ ] `R/operations.R`: replace per-operator methods with `Ops.dd`
      consulting `$geometry`.
- [ ] `R/density-mpl-legacy.R`: decision — keep as deprecated
      backend that accepts the new `dd` type, or delete. Recommend
      delete; the bug discoveries are documented in the vignette.
- [ ] `tests/testthat/test-dd-conversion.R`: round-trip identity.
- [ ] `tests/testthat/test-dd-class.R`: validator behaviour, geometry
      tag preservation under operations.
- [ ] `tests/testthat/test-density-mpl-rust.R`: unchanged (covers
      the math, not the storage).
- [ ] DESCRIPTION: drop `fda` from `Depends`, add to `Suggests`.
- [ ] DESCRIPTION: bump to 0.2.0.
- [ ] `NEWS.md`: write the breaking-change announcement.
- [ ] `README.md`: update the pitch.

## Open decisions (resolve during Phase 1)

1. **Multiple representations or one canonical?** The cleanest
   design has one canonical layout per geometry. A Bayes `dd` is
   B-spline-on-clr; a Wasserstein `dd` is B-spline-on-quantile.
   Decided: yes, one canonical per geometry, with intra-geometry
   conversion via `as_bayes()` / `as_wasserstein()`. Avoids storing
   redundant representations.
2. **Drop `fda` from Imports entirely?** Decision: yes by end of
   Phase 1. Move to Suggests so the comparison tests against
   `density.fd` still run when fda is installed.
3. **Keep `density_mpl_legacy`?** Decision: delete. The vignette
   documents the bugs. Reproducing old results requires installing
   `fda` directly, which is the honest dependency.
4. **`tidyfun` in Imports or Suggests?** Decision: Suggests. Users
   who want `as_tfb()` will already have `tidyfun` installed; we
   don't pull it in for everyone.
5. **Versioning of the savvy FFI**: the Rust kernel's signature is
   stable as long as we keep feeding it the same flat representation.
   If we add Wasserstein-specific Rust entries in Phase 2, the
   existing entry point doesn't change — we add new ones.

## Rollback

If Phase 1 lands but Phase 2/3 stall, the package is still
defensible: a cleanly-typed Bayes-Hilbert density library with
conversions to the standard FDA tools. That's a meaningful position
on its own, even without Wasserstein.

If Phase 1 itself proves too disruptive (e.g., we discover a
downstream user we didn't know about), the rollback is to keep `dd`
as a class-extension of `fd` (current state) and only do the
operator cleanup (`Ops.dd` group generic, delete the buggy `*.dd`).
That's a 0.1.x patch release rather than a 0.2 reset.
