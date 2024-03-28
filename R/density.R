#' @noRd
#' @importFrom stats density
density_kernel <- density.default

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sample PARAM_DESCRIPTION, Default: NULL
#' @param method PARAM_DESCRIPTION, Default: c("MPL", "kernel")
#' @param basis PARAM_DESCRIPTION, Default: fda::create.bspline.basis(rangeval, nbasis)
#' @param rangeval PARAM_DESCRIPTION, Default: NULL
#' @param nbasis PARAM_DESCRIPTION, Default: 10
#' @param lambda PARAM_DESCRIPTION, Default: 0
#' @param clr PARAM_DESCRIPTION, Default: fda::fd(sample, basisobj = basis)
#' @param constant PARAM_DESCRIPTION, Default: NULL
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[fda]{create.bspline.basis}}, \code{\link[fda]{fd}}, \code{\link[fda]{fdPar}}, \code{\link[fda]{density.fd}}, \code{\link[fda]{eval.fd}}
#' @rdname density.default
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
    wint <- stats::integrate(
      \(t) fda::eval.fd(wfdobj, t),
      min(rangeval), max(rangeval),
      rel.tol = .Machine$double.eps^0.5
    )$value
    ddobj <- wfdobj - wint / diff(rangeval)
  } else {
    ddobj <- clr
  }
  ddobj$constant <- if (is.null(constant)) normalize(ddobj) else constant
  structure(ddobj, class = c("dd", "fd"))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname merge.dd
#' @export
merge.dd <- function(...) {
  fdlist <- as.list(...)
  coefs <- as.matrix(data.frame(lapply(fdlist, \(x) x$coefs)))
  reps <- unlist(lapply(fdlist, \(x) x$fdnames$reps))
  colnames(coefs) <- reps
  fdnames <- list(
    time = stats::setNames(lapply(fdlist, \(x) x$fdnames$time), reps),
    reps = reps,
    value = fdlist[[1]]$fdnames$values
  )
  fdobj <- fdlist[[1]]
  fdobj$coefs <- coefs
  fdobj$fdnames <- fdnames
  density(clr = fdobj, constant = unlist(lapply(fdlist, \(x) x$constant)))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fdobj PARAM_DESCRIPTION, Default: NULL
#' @param coefs PARAM_DESCRIPTION, Default: fdobj$coefs
#' @param basis PARAM_DESCRIPTION, Default: fdobj$basis
#' @param inv PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[MASS]{ginv}}
#' @rdname zbsplines
#' @export
#' @importFrom MASS ginv
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
