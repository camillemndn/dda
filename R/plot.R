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
#' @rdname plot.dd
#' @export
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @importFrom fda plot.fd
plot.dd <- function(ddobj, ..., n = 401,
                    rangeval = range(lapply(
                      dplyr::pull(.data, {{ funs }}),
                      \(fun) fun$basis$rangeval
                    )),
                    x = seq(rangeval[1], rangeval[2], length.out = n)) {
  plot_funs(data.frame(fun = I(as_dd(ddobj))), fun, ...)
}

#' @export
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
