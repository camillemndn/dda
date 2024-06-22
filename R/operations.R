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

#' @export
mean.ddl <- function(ddlist, ...) as.list(mean(c(ddlist), ...))

#' @export
center <- function(...) UseMethod("center")

#' @export
center.dd <- function(ddobj, ...) {
  dd(clr = fda::center.fd(ddobj), ...)
}


#' @export
center.ddl <- function(ddlist, ...) as.list(center(c(ddlist), ...))

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
gmean.ddl <- function(ddlist, ...) as.list(gmean(c(ddlist), ...))

#' @export
gmean.dd <- function(ddlist, ...) mean.dd(ddobj, normalize = FALSE)

#' @export
`+.dd` <- function(...) dd(clr = fda::plus.fd(...))
#' @export
`-.dd` <- function(...) dd(clr = fda::minus.fd(...))
#' @export
`*.dd` <- function(...) dd(clr = fda::times.fd(...))
#' @export
`[.dd` <- function(...) dd(clr = fda::`[.fd`(...))
#' @export
relative <- function(...) UseMethod("relative")
#' @export
relative.dd <- function(...) dd(clr = fda::minus.fd(...), normalize = FALSE)

#' @export
`+.ddl` <- function(ddl1, ddl2, ...) as.list(`+.dd`(c(ddl1), c(ddl2), ...))
#' @export
`-.ddl` <- function(ddl1, ddl2, ...) as.list(`-.dd`(c(ddl1), c(ddl2), ...))
#' @export
`*.ddl` <- function(ddl1, ddl2, ...) as.list(`*.dd`(c(ddl1), c(ddl2), ...))
#' @export
relative.ddl <- function(ddl1, ddl2, ...) as.list(relative.dd(c(ddl1, normalize = FALSE), c(ddl2, normalize = FALSE), ...))


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
