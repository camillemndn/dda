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

#' @export
plot_matrix <- function(data, subset_vars, ...) {
  data <- data |> select(where(\(x) !inherits(x, "ddl")))
  # Check if subset_vars are in the data
  if (!all(subset_vars %in% names(data))) {
    stop("Some variables in subset_vars are not present in the data.")
  }

  # Get all possible pairs of the subset variables
  pairs <- crossing(var1 = subset_vars, var2 = subset_vars) %>%
    filter(var1 != var2)

  # Create the long format data frame
  long_data <- pairs %>%
    rowwise() %>%
    mutate(x = list(data[[var1]]), y = list(data[[var2]]), other = list(data |> dplyr::select(!subset_vars))) %>%
    unnest(c(x, y, other))

  break_by_x <- function(x) {
    function(limits) {
      seq(ceiling(limits[1] / x) * x, floor(limits[2] / x) * x, x)
    }
  }

  long_data %>%
    ggplot(aes(x = x, y = y, ...)) +
    geom_text() +
    facet_grid(vars(var2), vars(var1)) +
    coord_fixed() +
    scale_x_continuous(breaks = break_by_x(2)) +
    scale_y_continuous(breaks = break_by_x(2))
}

#' @export
#' @method plot ICS_Out_fd
plot.ICS_Out_fd <- function(object, ...) {
  X <- object$X
  IC_distances <- object$ics_distances
  n <- nrow(object$scores)
  p <- ncol(object$scores)

  screeplot_dat <- data.frame(
    IC = 1:p,
    gen_kurtosis = object$gen_kurtosis,
    selected = as.factor(ifelse(1:p %in% object$index, "selected", "not selected")),
    eigenfun = I(as.list(object$H_dual))
  )
  g1 <- ggplot(screeplot_dat, aes(IC, gen_kurtosis)) +
    geom_line(alpha = 0.5) +
    geom_point(aes(color = selected, size = selected))
  plot(g1)
  outlier_fact <- as.factor(ifelse(object$outliers == 1, "outlier", "not outlier"))
  distances_dat <- data.frame(
    Index = 1:n, IC_distances,
    outlier = outlier_fact
  )
  screeplot_dat$IC <- as.factor(screeplot_dat$IC)
  g2 <- screeplot_dat |>
    filter(selected == "selected") |>
    plot_funs(eigenfun, color = IC) +
    if (inherits(X, "dd")) geom_hline(yintercept = 1 / diff(X$basis$rangeval))
  plot(g2)

  g3 <- ggplot(distances_dat, aes(Index, IC_distances, color = outlier)) +
    geom_point() +
    geom_hline(yintercept = object$ics_dist_cutoff)

  pairs_dat <- data.frame(object$scores, outlier = outlier_fact)

  g3b <- GGally::ggpairs(pairs_dat,
    diag = "blank",
    columns = 1:max(max(object$index), 2),
    aes(label = 1:n, shape = NA, color = outlier)
  ) + geom_text()
  print(g3b)

  if (!inherits(X, "ICS")) {
    f_dat <- data.frame(
      X = I(as.list(X)),
      outlier = outlier_fact
    )
    g4 <- plot_funs(f_dat, X, color = outlier, alpha = outlier)
    plot(g4)
  }

  g <- gridExtra::grid.arrange(ggplotGrob(g1), ggplotGrob(g2),
    if (exists("g3b")) ggmatrix_gtable(g3b) else ggplotGrob(g3),
    if (exists("g4")) ggplotGrob(g4) else NULL,
    ncol = 2
  )
  plot(g)
}
