#' @export
#' @importFrom fda as.fd
#' @import fda
as.fd.list <- function(l, merge = FALSE, ...) {
  fdlist <- lapply(l, \(x) fda::as.fd(x, ...))
  structure(fdlist, class = c("fdl", "list"))
}

#' @export
as.list.fd <- function(fdobj) {
  fdlist <- lapply(seq_len(ncol(fdobj$coefs)), \(i) fdobj[i])
  structure(fdlist, class = c("fdl", "list"))
}

#' As fd
#' @rdname as.fd
#' @export
as.fd.fd <- function(fdobj) fdobj

#' @rdname as.fd
#' @export
as.fd.dd <- function(ddobj, ...) {
  ddobj$sample <- NULL
  ddobj$constant <- NULL
  structure(ddobj, class = "fd")
}

#' @rdname c
#' @export
c.fdl <- function(l) {
  do.call(fda:::c.fd, l)
}

#' @export
center.fdl <- function(fdlist, ...) as.list(center(c(fdlist), ...))

#' @rdname plot
#' @export
#' @method plot fdl
plot.fdl <- function(fdlist, ...) {
  plot_funs(data.frame(fun = I(fdlist)), fun, ...)
}
