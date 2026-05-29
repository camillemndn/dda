#' Mean of distributional data
#'
#' @param x A `dd` object (or list of them) / a `ddl` list.
#' @param ... Further arguments forwarded to [dd()].
#' @return A `dd` object.
#' @rdname mean
#' @export
#' @importFrom fda mean.fd
mean.dd <- function(x, ...) {
  return_list <- FALSE
  if (inherits(x, "list")) {
    x <- do.call(c, x)
    return_list <- TRUE
  }
  meandd <- dd(clr = fda::mean.fd(x), ...)
  if (return_list) list(meandd) else meandd
}

#' @rdname mean
#' @export
mean.ddl <- function(x, ...) as.list(mean(c(x), ...))

#' Center a functional/distributional object
#'
#' Subtract the mean (in the appropriate Bayes space) from each realization.
#'
#' @param x A `dd` object or list (`ddl` / `fdl`).
#' @param ... Further arguments forwarded to [dd()].
#' @return A `dd` object (or list, for list input).
#' @rdname center
#' @export
center <- function(x, ...) UseMethod("center")

#' @rdname center
#' @export
#' @importFrom fda center.fd
center.dd <- function(x, ...) {
  dd(clr = fda::center.fd(x), ...)
}


#' @rdname center
#' @export
center.ddl <- function(x, ...) as.list(center(c(x), ...))

#' Geometric mean in Bayes space
#'
#' Bayes-space mean of a `dd` object or a list (`ddl`) of them.
#'
#' @param x A `dd` object or a `ddl` list.
#' @param ... Further arguments forwarded to the underlying [mean.dd()].
#' @return A `dd` object (or list for `ddl` input).
#' @rdname gmean
#' @export
gmean <- function(x, ...) UseMethod("gmean")

#' @rdname gmean
#' @export
gmean.ddl <- function(x, ...) as.list(gmean(c(x), ...))

#' @rdname gmean
#' @export
gmean.dd <- function(x, ...) mean.dd(x, normalize = FALSE)

#' Bayes-space arithmetic on `dd` / `ddl` objects
#'
#' Bayes-Hilbert space operations on distributional data:
#' \itemize{
#'   \item `+.dd(p, q)`: density "addition" \eqn{p \oplus q = pq / \int pq};
#'         clr is `clr(p) + clr(q)`.
#'   \item `-.dd(p, q)`: density "subtraction" \eqn{p \ominus q = (p/q) / Z};
#'         clr is `clr(p) - clr(q)`.
#'   \item `*.dd(alpha, p)`: scalar multiplication
#'         \eqn{\alpha \odot p = p^{\alpha} / Z}; clr is `alpha * clr(p)`.
#'         One operand must be a numeric scalar; `dd * dd` is not defined
#'         in Bayes space and errors.
#'   \item `[.dd(x, i)`: select realizations from a multi-column `dd`.
#' }
#'
#' @param e1,e2 `dd` (or `ddl`) operands. For `*` exactly one of them must
#'   be a length-1 numeric scalar.
#' @return A `dd` object (or `ddl` list for `ddl` operands).
#' @rdname operations
#' @export
`+.dd` <- function(e1, e2) dd(clr = fda::plus.fd(e1, e2))

#' @rdname operations
#' @export
`-.dd` <- function(e1, e2) dd(clr = fda::minus.fd(e1, e2))

#' @rdname operations
#' @export
`*.dd` <- function(e1, e2) {
  if (is.numeric(e1) && length(e1) == 1L) {
    alpha <- e1; p <- e2
  } else if (is.numeric(e2) && length(e2) == 1L) {
    alpha <- e2; p <- e1
  } else {
    stop("`*` on `dd` is Bayes-space scalar multiplication; ",
         "one operand must be a length-1 numeric scalar.")
  }
  fd_p <- as.fd(p)
  fd_p$coefs <- alpha * fd_p$coefs
  dd(clr = fd_p)
}

#' @rdname operations
#' @param x A `dd` object to subset.
#' @param i Index (vector, logical, or name) selecting realizations.
#' @export
`[.dd` <- function(x, i) dd(clr = fda::`[.fd`(x, i))
#' Bayes-space relative density
#'
#' Generic `relative(e1, e2)` returns the Bayes-space ratio of two
#' densities. The default method dispatches to [fda::minus.fd()] on the
#' clr-transformed log-densities.
#'
#' @param e1,e2 `dd` objects (or lists thereof) to compare.
#' @param ... Further arguments forwarded to the underlying method.
#' @return A `dd` object.
#' @rdname relative
#' @export
relative <- function(e1, e2, ...) UseMethod("relative")

#' @rdname relative
#' @export
#' @importFrom fda minus.fd
relative.dd <- function(e1, e2, ...) {
  dd(clr = fda::minus.fd(e1, e2), normalize = FALSE)
}

#' @rdname operations
#' @export
`+.ddl` <- function(e1, e2) as.list(`+.dd`(c(e1), c(e2)))

#' @rdname operations
#' @export
`-.ddl` <- function(e1, e2) as.list(`-.dd`(c(e1), c(e2)))

#' @rdname operations
#' @export
`*.ddl` <- function(e1, e2) {
  if (is.numeric(e1) && length(e1) == 1L && inherits(e2, "ddl")) {
    structure(lapply(e2, function(p) e1 * p),
              class = c("ddl", "fdl", "list"))
  } else if (inherits(e1, "ddl") && is.numeric(e2) && length(e2) == 1L) {
    structure(lapply(e1, function(p) p * e2),
              class = c("ddl", "fdl", "list"))
  } else {
    stop("`*` on `ddl` is Bayes-space scalar multiplication; ",
         "one operand must be a length-1 numeric scalar.")
  }
}

#' @rdname relative
#' @export
relative.ddl <- function(e1, e2, ...) {
  as.list(relative.dd(c(e1, normalize = FALSE), c(e2, normalize = FALSE), ...))
}


#' Variance
#'
#' Generic dispatching to [stats::var()] by default and to [fda::var.fd()]
#' for `dd` input.
#'
#' @param x A numeric vector or a `dd` object.
#' @param ... Further arguments forwarded to the underlying method.
#' @return A variance estimate; for `dd` input, an object of class `bidd`.
#' @rdname var
#' @export
var <- function(x, ...) UseMethod("var")

#' @rdname var
#' @export
var.default <- function(x, ...) stats::var(x, ...)

#' @rdname var
#' @export
#' @importFrom fda var.fd
var.dd <- function(x, ...) structure(fda::var.fd(x, ...), class = "bidd")

#' Covariance
#'
#' Generic dispatching to [stats::cov()] by default and to [fda::var.fd()]
#' for `dd` input.
#'
#' @param x A numeric vector / matrix or a `dd` object.
#' @param ... Further arguments forwarded to the underlying method.
#' @return A covariance estimate.
#' @rdname cov
#' @export
cov <- function(x, ...) UseMethod("cov")

#' @rdname cov
#' @export
cov.default <- function(x, ...) stats::cov(x, ...)

#' @rdname cov
#' @export
cov.dd <- var.dd
