#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ddobj PARAM_DESCRIPTION
#' @param rangeval PARAM_DESCRIPTION, Default: ddobj$basis$rangeval
#' @param h PARAM_DESCRIPTION, Default: 0.001
#' @param show.legend PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname plot
#' @export
#' @method plot dd
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @importFrom fda plot.fd
plot.dd <- function(ddobj, ...) {
  plot_funs(data.frame(fun = I(as.list(ddobj))), fun, ...)
}

#' @rdname plot
#' @export
#' @method plot ddl
plot.ddl <- function(ddlist, ...) {
  plot_funs(data.frame(fun = I(ddlist)), fun, ...)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param .data PARAM_DESCRIPTION
#' @param funs PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @param n PARAM_DESCRIPTION, Default: 401
#' @param rangeval PARAM_DESCRIPTION, Default: range(lapply(dplyr::pull(.data, {
#'    {
#'        funs
#'    }
#' }), function(fun) fun$basis$rangeval))
#' @param x PARAM_DESCRIPTION, Default: seq(rangeval[1], rangeval[2], length.out = n)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[dplyr]{pull}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{geom_path}}
#' @rdname plot_funs
#' @export
#' @importFrom dplyr pull
#' @importFrom ggplot2 ggplot aes geom_line
plot_funs <- function(.data, funs, ..., n = 401,
                      rangeval = range(lapply(
                        dplyr::pull(.data, {{ funs }}),
                        \(fun) fun$basis$rangeval
                      )),
                      x = seq(rangeval[1], rangeval[2], length.out = n)) {
  eval_funs(.data, {{ funs }}, n, rangeval, x) |>
    ggplot2::ggplot(ggplot2::aes(x, y, group = id, ...)) +
    ggplot2::geom_line()
}
