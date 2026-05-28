# Validate density_fd_rust against the reference R implementation density.fd.
# Tolerance is relaxed (1e-3 on coefs, 1e-5 on C) because the line-search
# strategy differs (Armijo vs. fda's stepit) — they converge to the same
# minimizer but trace slightly different paths.

skip_if_no_rust <- function() {
  if (!nzchar(Sys.getenv("DDA_RUST_INSTALLED", ""))) {
    testthat::skip("Set DDA_RUST_INSTALLED=1 once the Rust backend is built.")
  }
}

run_both <- function(x, basis, lambda = 0, conv = 1e-6, iterlim = 50) {
  Wfd0 <- fda::fd(matrix(0, basis$nbasis, 1), basis)
  WfdPar <- fda::fdPar(Wfd0, lambda = lambda)
  r_ref  <- density.fd(x, WfdPar, conv = conv, iterlim = iterlim)
  r_rust <- dda::density_fd_rust(x, WfdPar, conv = conv, iterlim = iterlim)
  list(ref = r_ref, rust = r_rust)
}

test_that("B-spline basis: agrees with density.fd on a Gaussian sample", {
  skip_if_no_rust()
  set.seed(1)
  x <- rnorm(500)
  basis <- fda::create.bspline.basis(c(-4, 4), nbasis = 13, norder = 4)
  res <- run_both(x, basis)
  expect_equal(as.numeric(res$rust$Wfdobj$coefs),
               as.numeric(res$ref$Wfdobj$coefs), tolerance = 1e-3)
  expect_equal(res$rust$C, res$ref$C, tolerance = 1e-5)
})

test_that("B-spline + lambda > 0: penalized fit matches", {
  skip_if_no_rust()
  set.seed(2)
  x <- c(rnorm(300, -1, 0.7), rnorm(200, 1.5, 0.5))
  basis <- fda::create.bspline.basis(c(-4, 4), nbasis = 21, norder = 4)
  res <- run_both(x, basis, lambda = 1e-2)
  expect_equal(as.numeric(res$rust$Wfdobj$coefs),
               as.numeric(res$ref$Wfdobj$coefs), tolerance = 1e-3)
  expect_equal(res$rust$C, res$ref$C, tolerance = 1e-5)
})

test_that("Fourier basis: density matches on a periodic-like sample", {
  skip_if_no_rust()
  set.seed(3)
  x <- runif(500, 0, 2 * pi)
  basis <- fda::create.fourier.basis(c(0, 2 * pi), nbasis = 9)
  res <- run_both(x, basis)
  expect_equal(res$rust$C, res$ref$C, tolerance = 1e-4)
})

test_that("two-column input with frequencies is handled identically", {
  skip_if_no_rust()
  set.seed(4)
  xs <- seq(-3, 3, length.out = 50)
  fs <- round(100 * dnorm(xs, 0, 1))
  xf <- cbind(xs, fs)
  basis <- fda::create.bspline.basis(c(-4, 4), nbasis = 13, norder = 4)
  res <- run_both(xf, basis)
  expect_equal(as.numeric(res$rust$Wfdobj$coefs),
               as.numeric(res$ref$Wfdobj$coefs), tolerance = 1e-3)
})
