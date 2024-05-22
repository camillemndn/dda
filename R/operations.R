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
#'  \code{\link[fda]{mean.fd}}
#' @rdname mean.dd
#' @export
#' @importFrom fda mean.fd
mean.dd <- function(ddobj, ...) {
  return_list <- FALSE
  if (inherits(ddobj, "list")) {
    ddobj <- merge.dd(ddobj)
    return_list <- TRUE
  }
  meandd <- dd(clr = fda::mean.fd(ddobj), ...)
  if (return_list) list(meandd) else meandd
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
#'  \code{\link[fda]{mean.fd}}
#' @rdname gmean.dd
#' @export
#' @importFrom fda mean.fd
gmean.dd <- function(ddobj, ...) mean.dd(ddobj, normalize = FALSE)

#' @export
`+.dd` <- function(...) dd(clr = fda::plus.fd(...))
#' @export
`-.dd` <- function(...) dd(clr = fda::minus.fd(...))
#' @export
`*.dd` <- function(...) dd(clr = fda::times.fd(...))
#' @export
`[.dd` <- function(...) dd(clr = fda::`[.fd`(...))

#' @noRd
#' @export
var.default <- var

#' @noRd
#' @export
var <- function(...) UseMethod("var")

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
#'  \code{\link[fda]{var.fd}}
#' @rdname var.dd
#' @export
#' @importFrom fda var.fd
var.dd <- function(...) structure(fda::var.fd(...), class = "bidd")

#' @noRd
#' @export
cov.default <- cov

#' @noRd
#' @export
cov <- function(...) UseMethod("cov")

#' @rdname var.dd
#' @export
#' @importFrom fda var.fd
cov.dd <- var.dd
