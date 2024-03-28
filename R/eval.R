#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param expr PARAM_DESCRIPTION
#' @param envir PARAM_DESCRIPTION, Default: parent.frame()
#' @param enclos PARAM_DESCRIPTION, Default: if (is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv()
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname eval.default
#' @export
eval.default <- eval

eval <- function(...) UseMethod("eval")
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ddobj PARAM_DESCRIPTION
#' @param t PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[fda]{eval.fd}}
#' @rdname eval.dd
#' @export
#' @importFrom fda eval.fd
eval.dd <- function(ddobj, t) {
  class(ddobj) <- "fd"
  sweep(exp(fda::eval.fd(ddobj, t)), 2, as.numeric(ddobj$constant), "/")
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param self PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[fda]{eval.fd}}
#' @rdname normalize
#' @export
#' @importFrom fda eval.fd
normalize <- function(self) {
  if (!(is.numeric(self$constant))) {
    constant <- rep(NA, ncol(self$coefs))
    for (i in seq_len(ncol(self$coefs))) {
      rangemin <- min(self$basis$rangeval)
      rangemax <- max(self$basis$rangeval)
      dens <- \(t) exp(fda::eval.fd(self[i], t))
      constant[i] <- format(
        stats::integrate(dens, rangemin, rangemax,
          rel.tol = .Machine$double.eps^0.5
        )$value,
        scientific = TRUE
      )
    }
    constant
  }
}
