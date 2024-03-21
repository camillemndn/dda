density_kernel <- density.default
#' @description
#' Create a new distributional data object.
#' @param sample An `fd` object representating the clr transform.
#' @param rangeval An `fd` object representating the clr transform.
#' @param nbasis An `fd` object representating the clr transform.
#' @param lambda An `fd` object representating the clr transform.
#' @param clr An `fd` object representating the clr transform.
#' @param constant Normalizing constant.
#' @param basis The bspline basis.
#' @return A new `Distributional Data` object.
#' @export
density.default <- function(sample = NULL, method = c("MPL", "kernel"),
                            basis = fda::create.bspline.basis(rangeval, nbasis),
                            rangeval = NULL,
                            nbasis = 10,
                            lambda = 0,
                            clr = fda::fd(sample, basisobj = basis),
                            constant = NULL, ...) {
  if (match.arg(method) == "kernel") {
    return(density_kernel(sample, ...))
  }

  if (is.null(rangeval)) {
    rangeval <- if (!is.null(sample)) range(sample) else c(0, 1)
  }
  if (!is.null(sample)) {
    # Set up basis for W(x)
    basisobj <- fda::create.bspline.basis(rangeval, nbasis)
    # Set up initial value for wfdobj
    wfd0 <- fda::fd(matrix(0, nbasis, 1), basisobj)
    wfdparobj <- fda::fdPar(wfd0, lambda = lambda)
    # Estimate density
    wfdobj <- fda::density.fd(sample, wfdparobj)$Wfdobj
    wint <- integrate(
      \(t) fda::eval.fd(wfdobj, t),
      min(rangeval), max(rangeval),
      rel.tol = .Machine$double.eps^0.5
    )$value
    ddobj <- wfdobj - wint / diff(rangeval)
  } else {
    ddobj <- clr
  }
  ddobj$constant <- constant
  ddobj$constant <- normalize(ddobj)
  structure(ddobj, class = c("dd", "fd"))
}

#' @export
eval.default <- eval
#' @export
eval <- function(...) {
  UseMethod("eval")
}
#' @description
#' Evaluates a distributional data object at a point `t`.
#' @param ddobj A `dd` object.
#' @param t A real number `t`.
#' @return A real number.
#' @export
eval.dd <- function(ddobj, t) {
  class(ddobj) <- "fd"
  exp(fda::eval.fd(ddobj, t)) / as.numeric(ddobj$constant)
}

#' @description
#' Computes the normalizing constant of distributional data object.
#' @return Nothing.
#' @export
normalize <- function(self) {
  if (!(is.numeric(self$constant))) {
    constant <- rep(NA, ncol(self$coefs))
    for (i in seq_len(ncol(self$coefs))) {
      rangemin <- min(self$basis$rangeval)
      rangemax <- max(self$basis$rangeval)
      dens <- \(t) exp(fda::eval.fd(self[i], t))
      constant[i] <- format(
        integrate(dens, rangemin, rangemax,
          rel.tol = .Machine$double.eps^0.5
        )$value,
        scientific = TRUE
      )
    }
    constant
  }
}

#' @description
#' Plots a distributional data object.
#' @param ddobj A `dd` object.
#' @param rangeval The interval on which to plot.
#' @param h The step between each point.
#' @param ... The rest.
#' @return Nothing.
#' @export
plot.dd <- function(ddobj, rangeval = ddobj$basis$rangeval,
                    h = 0.01,
                    ...) {
  argvals <- seq(min(rangeval), max(rangeval), h)
  plot(rep(argvals, ncol(ddobj$coefs)), eval(ddobj, argvals), type = "l", ...)
}

#' @title Aitchison mean
#' @description
#' Computes the mean of a vector of densities.
#' @param ... The vector of densities.
#' @return A density object which is the mean.
#' @export
mean.dd <- function(...) density(clr = fda::mean.fd(...))

#' @export
`+.dd` <- function(...) density(clr = fda:::`+.fd`(...))
#' @export
`-.dd` <- function(...) density(clr = fda:::`-.fd`(...))
#' @export
`*.dd` <- function(...) density(clr = fda:::`*.fd`(...))
#' @export
`[.dd` <- function(...) density(clr = fda:::`[.fd`(...))

#' @export
var.default <- var
var <- function(...) UseMethod("var")
#' @export
var.dd <- function(...) structure(fda::var.fd(...), class = "bidd")
#' @export
cov.default <- cov
#' @export
cov <- function(...) UseMethod("cov")
#' @export
cov.dd <- var.dd

#' @export
merge.dd <- function(...) {
  fdlist <- as.list(...)
  coefs <- as.matrix(data.frame(lapply(fdlist, \(x) x$coefs)))
  reps <- unlist(lapply(fdlist, \(x) x$fdnames$reps))
  colnames(coefs) <- reps
  fdnames <- list(
    time = setNames(lapply(fdlist, \(x) x$fdnames$time), reps),
    reps = reps,
    value = fdlist[[1]]$fdnames$values
  )
  fdobj <- fdlist[[1]]
  fdobj$coefs <- coefs
  fdobj$fdnames <- fdnames
  structure(fdobj, class = c("dd", "fd"))
}

#' @export
ICS.default <- ICS::ICS

#' @export
ICS <- function(...) UseMethod("ICS")

#' @export
ICS.fd <- function(fdobj, ...) {
  changemat <- zbsplines(basis = fdobj$basis, inv = TRUE)
  gram <- t(changemat) %*% fda::inprod(fdobj$basis, fdobj$basis) %*% changemat
  icsobj <- ICS::ICS(crossprod(zbsplines(fdobj), gram), ...)
  icsobj$W <- fda::fd(
    zbsplines(coefs = icsobj$W, basis = fdobj$basis, inv = TRUE),
    fdobj$basis
  )
  icsobj
}

#' @export
ICS.dd <- function(...) {
  icsobj <- ICS.fd(...)
  icsobj$W <- density(clr = icsobj$W)
  icsobj
}

#' @export
zbsplines <- function(
    fdobj = NULL,
    coefs = fdobj$coefs, basis = fdobj$basis, inv = FALSE) {
  rangeval <- basis$rangeval
  knots <- basis$params
  k <- length(knots)
  p <- basis$nbasis
  d <- p - k
  a <- min(rangeval)
  b <- max(rangeval)
  extknots <- c(rep(a, d), knots, rep(b, d))
  dinvmat <- diag(diff(extknots, lag = d))
  kmat <- diag(p)[, -p]
  kmat[cbind(2:p, 1:(p - 1))] <- -1
  kinvmat <- MASS::ginv(kmat)
  changemat <- if (inv) d * solve(dinvmat) %*% kmat else kinvmat %*% dinvmat / d
  if (is.null(coefs)) {
    changemat
  } else {
    changemat %*% coefs
  }
}
