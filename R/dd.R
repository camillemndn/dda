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
    full_sample = sample,
    basis = fda::create.bspline.basis(
      rangeval = rangeval,
      nbasis = nbasis, norder = norder, breaks = breaks
    ),
    breaks = if (is.numeric(sample)) {
      quantile(full_sample, seq(0, 1, length.out = nbasis - norder + 2))
    } else {
      NULL
    },
    rangeval = if (is.numeric(sample)) range(breaks) else c(0, 1),
    nbasis = 12,
    norder = 5,
    lambda = 0,
    clr = fda::fd(basisobj = basis),
    constant = NULL,
    normalize = TRUE,
    ...) {
  if (is.numeric(sample)) {
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
  ddobj$basis$call <- NULL
  ddobj$sample <- sample
  class(ddobj) <- c("dd", "fd")
  ddobj
}

#' @export
as_dd <- function(...) UseMethod("as_dd")

#' @export
#' @importFrom parallel mclapply
as_dd.list <- function(l, mc.cores = NULL, ...) {
  if (is.null(mc.cores)) {
    ddlist <- lapply(l, \(x) as_dd(x, full_sample = unlist(l), ...))
  } else {
    ddlist <- parallel::mclapply(l, \(x) as_dd(x, full_sample = unlist(l), ...),
      mc.cores = mc.cores
    )
  }
  structure(ddlist, class = c("ddl", "fdl", "list"))
}

#' @export
c.dd <- function(...) {
  ddlist <- list(...)
  ddlist <- lapply(ddlist, \(x) {
    x$basis$call <- NULL
    x
  })
  fdobj <- do.call(fda:::c.fd, ddlist)
  ddobj <- dd(clr = fdobj, constant = unlist(lapply(ddlist, \(x) x$constant)))
  ddobj$sample <- lapply(ddlist, \(x) x$sample)
  if (all(sapply(ddobj$sample, is.null))) ddobj$sample <- NULL
  ddobj
}

#' @export
c.ddl <- function(l) {
  do.call(c.dd, l)
}

#' @export
as_dd.xts <- function(sample, ...) {
  dd(sample, ...)
}
#' @export
as_dd.dd <- function(ddobj) ddobj

#' @export
as_dd.fd <- function(fdobj, ...) {
  dd(clr = fdobj, basisobj = fdobj$basis, ...)
}
#' @export
as_dd.numeric <- function(sample, ...) {
  dd(sample, ...)
}

#' @export
as.list.dd <- function(ddobj) {
  ddlist <- lapply(seq_len(ncol(ddobj$coefs)), \(i) {
    di <- ddobj[i]
    di$sample <- ddobj$sample[[i]]
    di
  })
  structure(ddlist, class = c("ddl", "fdl", "list"))
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
