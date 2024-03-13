#' @export
density.dd <- function(sample, rangeval = c(-5, 45), nbasis = 10, lambda = 0) {
  # Set up basis for W(x)
  basisobj <- fda::create.bspline.basis(range(rangeval), nbasis)
  # Set up initial value for wfdobj
  wfd0 <- fda::fd(matrix(0, nbasis, 1), basisobj)
  wfdparobj <- fda::fdPar(wfd0, lambda = lambda)
  # Estimate density
  fda::density.fd(sample, wfdparobj)
}

#' @export
eval.dd <- function(xval, denslist) {
  wval <- fda::eval.fd(xval, denslist$wfdobj)
  exp(wval) / denslist$C
}

#' @export
plot.dd <- function(
    wobj, rangeval = wobj$wfdobj$basis$rangeval,
    h = 0.01,
    ...) {
  argvals <- seq(min(rangeval), max(rangeval), h)
  plot(argvals, eval.dd(argvals, wobj), type = "l", ...)
}

#' @export
lines.dd <- function(
    wobj,
    rangeval = wobj$wfdobj$basis$rangeval,
    h = 0.01,
    ...) {
  argvals <- seq(min(rangeval), max(rangeval), h)
  lines(argvals, eval.dd(argvals, wobj), type = "l", ...)
}
