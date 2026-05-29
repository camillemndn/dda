#' Build a distributional-data (`dd`) object
#'
#' Constructor for a `dd` object: a B-spline `fd` representation of a
#' log-density together with its normalizing constant (so the density is
#' `exp(W(x)) / constant`).
#'
#' @param sample Optional numeric sample used to estimate the density.
#' @param full_sample Pooled reference sample for knot placement. Defaults
#'   to `sample`.
#' @param basis A `basisfd` object. Defaults to a B-spline basis built from
#'   `rangeval`, `nbasis`, `norder`, `breaks`.
#' @param knots_pos How to derive knots from `full_sample` (`"quantiles"`
#'   by default).
#' @param breaks Numeric vector of knot positions. By default chosen from
#'   `full_sample` according to `knots_pos`; `NULL` for non-numeric input.
#' @param rangeval Numeric pair giving the range over which the density
#'   lives. Defaults to `range(breaks)` for numeric input, `c(0, 1)`
#'   otherwise.
#' @param nbasis Number of basis functions. Default `12`.
#' @param norder B-spline order. Default `5`.
#' @param lambda Penalty parameter forwarded to [density_mpl()].
#' @param clr Pre-computed clr-transformed `fd` (skip density estimation if
#'   supplied).
#' @param constant Optional pre-computed normalizing constant.
#' @param normalize Logical; if `TRUE` (default) compute the normalizing
#'   constant via [normalize()].
#' @param ... Further arguments forwarded to [density_mpl()].
#' @return A `dd` object: an `fd` carrying `constant` and (optionally)
#'   `sample`.
#' @rdname dd
#' @export
#' @importFrom fda create.bspline.basis fd fdPar eval.fd
#' @importFrom stats integrate quantile
dd <- function(
	sample = NULL,
	full_sample = sample,
	basis = fda::create.bspline.basis(
		rangeval = rangeval,
		nbasis = nbasis,
		norder = norder,
		breaks = breaks
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
	...
) {
	if (is.numeric(sample)) {
		rangeval <- basis$rangeval
		# Set up initial value for wfdobj
		wfd0 <- fda::fd(matrix(0, basis$nbasis, 1), basis)
		wfdparobj <- fda::fdPar(wfd0, lambda = lambda)
		# Estimate density
		wfdobj <- density_mpl(sample, wfdparobj)$Wfdobj
		wint <- stats::integrate(
			\(t) fda::eval.fd(wfdobj, t),
			min(rangeval),
			max(rangeval),
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

#' Coerce to a distributional-data object
#'
#' @param x Object to coerce: a numeric sample, an `fd`, `dd`, list, or
#'   `xts` time series.
#' @param full_sample Pooled reference sample used to set common breakpoints
#'   across list elements. Defaults to `unlist(x)`.
#' @param mc.cores Optional integer; if non-`NULL`, list elements are
#'   processed in parallel via [parallel::mclapply()].
#' @param ... Further arguments forwarded to [dd()].
#' @return A `dd` object, or a list of them with class `c("ddl", "fdl",
#'   "list")` for list / xts input.
#' @rdname as_dd
#' @export
as_dd <- function(x, ...) UseMethod("as_dd")

#' @rdname as_dd
#' @export
#' @importFrom parallel mclapply
as_dd.list <- function(x, full_sample = unlist(x), mc.cores = NULL, ...) {
	if (is.null(mc.cores)) {
		ddlist <- lapply(x, \(xi) as_dd(xi, full_sample = unlist(x), ...))
	} else {
		ddlist <- parallel::mclapply(
			x,
			\(xi) as_dd(xi, full_sample = full_sample, ...),
			mc.cores = mc.cores
		)
	}
	structure(ddlist, class = c("ddl", "fdl", "list"))
}

#' Concatenate distributional / functional objects
#'
#' @param ... Objects to concatenate (each `dd`, `ddl`, or `fdl`).
#' @return A single combined object of the same class as the first input.
#' @rdname c
#' @export
c.dd <- function(...) {
	ddlist <- list(...)
	ddlist <- lapply(ddlist, \(x) {
		x$basis$call <- NULL
		x
	})
	ddobj <- do.call(utils::getFromNamespace("c.fd", "fda"), ddlist)
	class(ddobj) <- c("dd", "fd")
	ddobj$constant <- unlist(lapply(ddlist, \(x) x$constant))
	ddobj$sample <- lapply(ddlist, \(x) x$sample)
	if (all(sapply(ddobj$sample, is.null))) {
		ddobj$sample <- NULL
	}
	ddobj
}

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

#' @rdname as_dd
#' @export
as_dd.xts <- function(x, ...) {
	dd(x, ...)
}
#' @rdname as_dd
#' @export
as_dd.dd <- function(x, ...) x

#' @rdname as_dd
#' @export
as_dd.fd <- function(x, ...) {
	dd(clr = x, basisobj = x$basis, ...)
}

#' @rdname as_dd
#' @export
as_dd.numeric <- function(x, ...) {
	dd(x, ...)
}

#' Coerce a `dd` object to a list
#'
#' Splits a multi-realization `dd` object into a list (one element per
#' column of `x$coefs`).
#'
#' @param x A `dd` object.
#' @param ... Currently ignored.
#' @return A `ddl` list (also of class `fdl` / `list`).
#' @export
as.list.dd <- function(x, ...) {
	ddlist <- lapply(seq_len(ncol(x$coefs)), \(i) {
		di <- x[i]
		di$sample <- x$sample[[i]]
		di
	})
	structure(ddlist, class = c("ddl", "fdl", "list"))
}

#' Change of basis to / from ZB-splines
#'
#' Converts between a B-spline `fd` representation and its ZB-spline
#' (zero-integral) equivalent.
#'
#' @param fdobj An `fd` object (used only if `coefs` / `basis` are absent).
#' @param coefs Coefficient matrix. Defaults to `fdobj$coefs`.
#' @param basis The originating B-spline basis. Defaults to `fdobj$basis`.
#' @param inv If `TRUE`, apply the inverse transformation (ZB → B).
#' @return A matrix of transformed coefficients (or `fd` when `fdobj` is
#'   passed).
#' @keywords internal
#' @export
to_zbsplines <- function(
	fdobj = NULL,
	coefs = fdobj$coefs,
	basis = fdobj$basis,
	inv = FALSE
) {
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
