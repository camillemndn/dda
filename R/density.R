#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sample PARAM_DESCRIPTION, Default: NULL
#' @param method PARAM_DESCRIPTION, Default: c("MPL", "kernel")
#' @param basis PARAM_DESCRIPTION, Default: fda::create.bspline.basis(rangeval, nbasis)
#' @param rangeval PARAM_DESCRIPTION, Default: NULL
#' @param nbasis PARAM_DESCRIPTION, Default: NULL
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
#' @rdname dd
#' @export
#' @importFrom stats quantile
dd <- function(
    sample = NULL,
    basis = fda::create.bspline.basis(
      rangeval = rangeval,
      nbasis = nbasis, norder = norder, breaks = breaks
    ),
    breaks = if (!is.null(sample)) {
      quantile(unlist(sample), seq(0, 1, length.out = nbasis - norder + 2))
    } else {
      NULL
    },
    rangeval = if (!is.null(sample)) range(breaks) else c(0, 1),
    nbasis = 12,
    norder = 5,
    lambda = 0,
    clr = fda::fd(unlist(sample), basisobj = basis),
    constant = NULL,
    normalize = TRUE,
    ...) {
  return_list <- FALSE
  if (inherits(sample, "list")) {
    return(lapply(sample, \(x) dd(unlist(x),
      basis = basis, lambda = lambda,
      clr = clr, constant = constant, normalize = normalize
    )))
  }
  if (is.numeric(sample)) {
    cat("\r", "Sample size: ", length(sample))
    rangeval <- basis$rangeval
    # Set up initial value for wfdobj
    wfd0 <- fda::fd(matrix(0, basis$nbasis, 1), basis)
    wfdparobj <- fda::fdPar(wfd0, lambda = lambda)
    # Estimate density
    wfdobj <- fda::density.fd(sample, wfdparobj)$Wfdobj
    wint <- stats::integrate(
      \(t) fda::eval.fd(wfdobj, t),
      min(rangeval), max(rangeval),
      rel.tol = .Machine$double.eps^0.5,
      stop.on.error = FALSE
    )$value
    ddobj <- wfdobj - wint / diff(rangeval)
  } else {
    ddobj <- clr
  }
  if (normalize) {
    ddobj$constant <- if (is.null(constant)) normalize(ddobj) else constant
  }
  ddobj$sample <- sample
  class(ddobj) <- c("dd", "fd")
  if (return_list) list(ddobj) else ddobj
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
    args = stats::setNames(lapply(fdlist, \(x) x$fdnames$time), reps),
    reps = reps,
    funs = fdlist[[1]]$fdnames$values
  )
  fdobj <- fdlist[[1]]
  fdobj$coefs <- coefs
  fdobj$fdnames <- fdnames
  dd(clr = fdobj, constant = unlist(lapply(fdlist, \(x) x$constant)))
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
#' @rdname to_zbsplines
#' @export
to_zbsplines <- function(
    fdobj = NULL,
    coefs = fdobj$coefs, basis = fdobj$basis, inv = FALSE) {
  rangeval <- basis$rangeval
  knots <- basis$params
  g <- length(knots)
  p <- basis$nbasis
  d <- p - g
  a <- min(rangeval)
  b <- max(rangeval)
  extknots <- c(rep(a, d), knots, rep(b, d))

  dinvmat <- diag(diff(extknots, lag = d)) / d
  dmat <- solve(dinvmat)

  kinvmat <- diag(p)[-p, ]
  kinvmat[lower.tri(kinvmat)] <- 1
  kmat <- diag(p)[, -p]
  kmat[cbind(2:p, 1:(p - 1))] <- -1

  changemat <- if (inv) dmat %*% kmat else kinvmat %*% dinvmat
  if (is.null(coefs)) changemat else changemat %*% coefs
}
