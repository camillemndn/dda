# Validate density_fd_rust against the reference R implementation density.fd.
#
# Note on tolerance: density.fd's line search (fda::stepit) exits when it
# cannot make further progress, which is *not* the same as a true stationary
# point — gradients of size ~1e-1 to 1e-4 are common at its termination.
# The Rust implementation uses Armijo backtracking on the Newton direction
# and typically converges further. So strict coefficient equality is not the
# right test; we instead check (a) the density itself agrees on a grid, and
# (b) Rust's objective is no worse than R's.

skip_if_no_rust <- function() {
  if (!nzchar(Sys.getenv("DDA_RUST_INSTALLED", ""))) {
    testthat::skip("Set DDA_RUST_INSTALLED=1 once the Rust backend is built.")
  }
}

run_both <- function(x, basis, lambda = 0, conv = 1e-8, iterlim = 100) {
  Wfd0 <- fda::fd(matrix(0, basis$nbasis, 1), basis)
  WfdPar <- fda::fdPar(Wfd0, lambda = lambda)
  r_ref  <- dda:::density.fd(x, WfdPar, conv = conv, iterlim = iterlim)
  r_rust <- dda::density_fd_rust(x, WfdPar, conv = conv, iterlim = iterlim)
  list(ref = r_ref, rust = r_rust)
}

density_on_grid <- function(res, grid) {
  W <- as.numeric(fda::eval.fd(grid, res$Wfdobj))
  exp(W) / res$C
}

expect_densities_close <- function(res, grid, tol = 1e-2) {
  # Compare only where the reference density carries non-trivial mass: the
  # B-spline boundary coefficients are weakly identified when no data falls
  # near the basis range endpoints, so the un-penalized problem has
  # multiple near-optimal solutions that disagree wildly outside the data
  # region. The density inside the data region is well-determined.
  p_ref  <- density_on_grid(res$ref,  grid)
  p_rust <- density_on_grid(res$rust, grid)
  bulk <- p_ref > 0.05 * max(p_ref)
  rel  <- abs(p_rust[bulk] - p_ref[bulk]) / p_ref[bulk]
  expect_lt(max(rel), tol)
}

expect_rust_at_least_as_good <- function(res) {
  # Rust's objective should be ≤ R's, plus a tiny slack for line-search
  # path differences. Gradient norm should be small in absolute terms.
  expect_lte(res$rust$Flist$f, res$ref$Flist$f + 1e-3)
  expect_lt(res$rust$Flist$norm, 1e-2)
}

test_that("B-spline basis on a Gaussian sample: densities match", {
  skip_if_no_rust()
  set.seed(1); x <- rnorm(500)
  basis <- fda::create.bspline.basis(c(-4, 4), nbasis = 13, norder = 4)
  res <- run_both(x, basis)
  expect_densities_close(res, seq(-4, 4, length.out = 401))
  expect_rust_at_least_as_good(res)
})

test_that("B-spline with lambda > 0: penalized density matches", {
  skip_if_no_rust()
  set.seed(2)
  x <- c(rnorm(300, -1, 0.7), rnorm(200, 1.5, 0.5))
  basis <- fda::create.bspline.basis(c(-4, 4), nbasis = 21, norder = 4)
  res <- run_both(x, basis, lambda = 1e-2)
  expect_densities_close(res, seq(-4, 4, length.out = 401))
  expect_rust_at_least_as_good(res)
})

test_that("Fourier basis on a periodic-like sample", {
  skip_if_no_rust()
  set.seed(3); x <- runif(500, 0, 2 * pi)
  basis <- fda::create.fourier.basis(c(0, 2 * pi), nbasis = 9)
  res <- run_both(x, basis)
  expect_densities_close(res, seq(0, 2 * pi, length.out = 401))
  expect_rust_at_least_as_good(res)
})

test_that("Two-column input with frequencies is handled identically", {
  skip_if_no_rust()
  xs <- seq(-3, 3, length.out = 50)
  fs <- round(100 * dnorm(xs, 0, 1))
  xf <- cbind(xs, fs)
  basis <- fda::create.bspline.basis(c(-4, 4), nbasis = 13, norder = 4)
  res <- run_both(xf, basis)
  expect_densities_close(res, seq(-4, 4, length.out = 401), tol = 5e-3)
  expect_rust_at_least_as_good(res)
})
