#' Plot a distributional-data object
#'
#' @param x A `dd` object or list (`ddl` / `fdl`).
#' @param ... Aesthetic arguments forwarded to ggplot layers.
#' @return A `ggplot` object.
#' @rdname plot
#' @export
#' @method plot dd
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @importFrom fda plot.fd
plot.dd <- function(x, ...) {
  plot_funs(data.frame(fun = I(as.list(x))), fun, ...)
}

#' @rdname plot
#' @export
#' @method plot ddl
plot.ddl <- function(x, ...) {
  plot_funs(data.frame(fun = I(x)), fun, ...)
}

#' Plot a column of functional objects
#'
#' Takes a data frame with a list column of `fd` objects, evaluates each on
#' a common grid, and draws them as overlaid line plots.
#'
#' @param .data A data frame with a list column of `fd` objects.
#' @param funs Tidy-selection for that column.
#' @param ... Additional aesthetic arguments forwarded to [ggplot2::aes()].
#' @param n Number of evaluation points. Default `401`.
#' @param rangeval Numeric pair; abscissa range. Defaults to the union of
#'   the basis ranges of the functions in `funs`.
#' @param x Evaluation grid. Defaults to `seq(rangeval[1], rangeval[2],
#'   length.out = n)`.
#' @return A `ggplot` object.
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
    ggplot2::ggplot(ggplot2::aes(.data$x, .data$y, group = .data$id, ...)) +
    ggplot2::geom_line()
}

#' Faceted scatter-matrix plot
#'
#' Internal helper used to render an unordered grid of pairwise scatterplots
#' over the non-`ddl` columns of `data`, faceted by variable pair.
#'
#' @param data A data frame with named columns.
#' @param subset_vars Character vector of columns to plot pairwise.
#' @param ... Additional aesthetic arguments forwarded to ggplot.
#' @return A `ggplot` object.
#' @keywords internal
#' @export
#' @importFrom ggplot2 ggplot aes geom_text facet_grid vars coord_fixed
#'   scale_x_continuous scale_y_continuous
plot_matrix <- function(data, subset_vars, ...) {
  data <- data |> dplyr::select(dplyr::where(\(x) !inherits(x, "ddl")))
  if (!all(subset_vars %in% names(data))) {
    stop("Some variables in subset_vars are not present in the data.")
  }

  pairs <- tidyr::crossing(var1 = subset_vars, var2 = subset_vars) |>
    dplyr::filter(.data$var1 != .data$var2)

  long_data <- pairs |>
    dplyr::rowwise() |>
    dplyr::mutate(
      x = list(data[[.data$var1]]),
      y = list(data[[.data$var2]]),
      other = list(data |> dplyr::select(!dplyr::all_of(subset_vars)))
    ) |>
    tidyr::unnest(c(.data$x, .data$y, .data$other))

  break_by_x <- function(step) {
    function(limits) {
      seq(ceiling(limits[1] / step) * step, floor(limits[2] / step) * step, step)
    }
  }

  ggplot2::ggplot(long_data, ggplot2::aes(x = .data$x, y = .data$y, ...)) +
    ggplot2::geom_text() +
    ggplot2::facet_grid(ggplot2::vars(.data$var2), ggplot2::vars(.data$var1)) +
    ggplot2::coord_fixed() +
    ggplot2::scale_x_continuous(breaks = break_by_x(2)) +
    ggplot2::scale_y_continuous(breaks = break_by_x(2))
}

