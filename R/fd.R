#' @export
#' @importFrom fda as.fd
#' @import fda
as.fd.list <- function(fdlist, merge = FALSE, ...) {
  # A list of fd objects is merged by default.
  if (merge || all(sapply(fdlist, \(x) inherits(x, "fd")))) {
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
    fdobj
  } else {
    lapply(fdlist, \(x) fda::as.fd(x, ...))
  }
}

#' @export
as.fd.fd <- function(fdobj, ...) {
  lapply(seq_len(ncol(fdobj$coefs)), \(i) fdobj[i])
}

#' @export
as.fd.dd <- function(ddobj, ...) {
  ddobj$sample <- NULL
  ddobj$constant <- NULL
  structure(ddobj, class = "fd")
}
