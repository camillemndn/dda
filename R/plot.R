#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ddobj PARAM_DESCRIPTION
#' @param rangeval PARAM_DESCRIPTION, Default: ddobj$basis$rangeval
#' @param h PARAM_DESCRIPTION, Default: 0.001
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
plot.dd <- function(ddobj, rangeval = ddobj$basis$rangeval,
                    h = 0.001,
                    ...) {
  argvals <- seq(min(rangeval), max(rangeval), h)
  df <- data.frame(cbind(argvals, eval.dd(ddobj, argvals)))
  df_long <- reshape(df,
    varying = list(names(df)[-1]),
    v.names = "y",
    times = names(df)[-1],
    timevar = "obs",
    direction = "long"
  )
  ggplot(df_long, aes(x = argvals, y = y, color = obs)) +
    geom_line() +
    labs(x = "x", y = "y", title = "Multiple Curves")
}
