#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sample PARAM_DESCRIPTION, Default: NULL
#' @param full_sample PARAM_DESCRIPTION, Default: sample
#' @param basis PARAM_DESCRIPTION, Default: fda::create.bspline.basis(rangeval = rangeval, nbasis = nbasis,
#'    norder = norder, breaks = breaks)
#' @param knots_pos PARAM_DESCRIPTION, Default: 'quantiles'
#' @param breaks PARAM_DESCRIPTION, Default: if (is.numeric(sample)) {
#'    if (knots_pos == "quantiles") {
#'        quantile(full_sample, seq(0, 1, length.out = nbasis -
#'            norder + 2))
#'    }
#'    else {
#'        quantile(range(full_sample), seq(0, 1, length.out = nbasis -
#'            norder + 2))
#'    }
#' } else {
#'    NULL
#' }
#' @param rangeval PARAM_DESCRIPTION, Default: if (is.numeric(sample)) range(breaks) else c(0, 1)
#' @param nbasis PARAM_DESCRIPTION, Default: 12
#' @param norder PARAM_DESCRIPTION, Default: 5
#' @param lambda PARAM_DESCRIPTION, Default: 0
#' @param clr PARAM_DESCRIPTION, Default: fda::fd(basisobj = basis)
#' @param constant PARAM_DESCRIPTION, Default: NULL
#' @param normalize PARAM_DESCRIPTION, Default: TRUE
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
#'  \code{\link[stats]{integrate}}
#' @rdname dd
#' @export
#' @importFrom fda create.bspline.basis fd fdPar density.fd eval.fd
#' @importFrom stats integrate
dd <- function(
    sample = NULL,
    full_sample = sample,
    basis = fda::create.bspline.basis(
      rangeval = rangeval,
      nbasis = nbasis, norder = norder, breaks = breaks
    ),
    knots_pos = "quantiles",
    breaks = if (is.numeric(sample)) {
      if (knots_pos == "quantiles") {
        quantile(full_sample, seq(0, 1, length.out = nbasis - norder + 2))
      } else {
        quantile(range(full_sample), seq(0, 1, length.out = nbasis - norder + 2))
      }
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
#' @rdname as_dd
#' @export
as_dd <- function(...) UseMethod("as_dd")

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param l PARAM_DESCRIPTION
#' @param full_sample PARAM_DESCRIPTION, Default: unlist(l)
#' @param mc.cores PARAM_DESCRIPTION, Default: NULL
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
#'  \code{\link[parallel]{mclapply}}
#' @rdname as_dd
#' @export
#' @importFrom parallel mclapply
as_dd.list <- function(l, full_sample = unlist(l), mc.cores = NULL, ...) {
  if (is.null(mc.cores)) {
    ddlist <- lapply(l, \(x) as_dd(x, full_sample = unlist(l), ...))
  } else {
    ddlist <- parallel::mclapply(l, \(x) as_dd(x, full_sample = full_sample, ...),
      mc.cores = mc.cores
    )
  }
  structure(ddlist, class = c("ddl", "fdl", "list"))
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
#' @seealso
#'  \code{\link[fda]{character(0)}}
#' @rdname c
#' @export
c.dd <- function(...) {
  ddlist <- list(...)
  ddlist <- lapply(ddlist, \(x) {
    x$basis$call <- NULL
    x
  })
  ddobj <- do.call(fda:::c.fd, ddlist)
  class(ddobj) <- c("dd", "fd")
  ddobj$constant <- unlist(lapply(ddlist, \(x) x$constant))
  ddobj$sample <- lapply(ddlist, \(x) x$sample)
  if (all(sapply(ddobj$sample, is.null))) ddobj$sample <- NULL
  ddobj
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param l PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname c
#' @export
c.ddl <- function(...) {
  l <- list(...)
  lddobj <- lapply(l, function(ddlobj) do.call(c.dd, ddlobj))
  ddobj <- do.call(c.dd, lddobj)
  if (length(l) > 1) {
    return(as.list(ddobj))
  } else {
    return(ddobj)
  }
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sample PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname as_dd
#' @export
as_dd.xts <- function(sample, ...) {
  dd(sample, ...)
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ddobj PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname as_dd
#' @export
as_dd.dd <- function(ddobj, ...) ddobj

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fdobj PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname as_dd
#' @export
as_dd.fd <- function(fdobj, ...) {
  dd(clr = fdobj, basisobj = fdobj$basis, ...)
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sample PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname as_dd
#' @export
as_dd.numeric <- function(sample, ...) {
  dd(sample, ...)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ddobj PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname as.list
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
