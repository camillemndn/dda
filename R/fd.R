#' Coerce to / from an `fd` object
#'
#' @param x Object to coerce.
#' @param merge Pass-through to [fda::as.fd()].
#' @param ... Further arguments forwarded to the underlying method.
#' @rdname as.fd
#' @export
#' @importFrom fda as.fd
#' @import fda
as.fd.list <- function(x, merge = FALSE, ...) {
  fdlist <- lapply(x, \(xi) fda::as.fd(xi, ...))
  structure(fdlist, class = c("fdl", "list"))
}

#' @rdname as.fd
#' @export
as.list.fd <- function(x, ...) {
  fdlist <- lapply(seq_len(ncol(x$coefs)), \(i) x[i])
  structure(fdlist, class = c("fdl", "list"))
}

#' @rdname as.fd
#' @export
as.fd.fd <- function(x, ...) x

#' @rdname as.fd
#' @export
as.fd.dd <- function(x, ...) {
  x$sample <- NULL
  x$constant <- NULL
  structure(x, class = "fd")
}

#' @rdname c
#' @export
c.fdl <- function(...) {
  do.call(getS3method("c", "fd"), unlist(list(...), recursive = FALSE))
}

#' @export
center.fdl <- function(x, ...) as.list(center(c(x), ...))

#' @rdname plot
#' @export
#' @method plot fdl
plot.fdl <- function(x, ...) {
  plot_funs(data.frame(fun = I(x)), fun, ...)
}
