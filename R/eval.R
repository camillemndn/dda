#' Evaluate a `dd` object at given points
#'
#' Returns the density values \eqn{p(t) = \exp(W(t)) / C} where W is the
#' log-density carried by the `dd` object and C its normalizing constant.
#'
#' @param ddobj A `dd` object.
#' @param t Numeric vector of points at which to evaluate the density.
#' @return Matrix of density values, one column per realization in `ddobj`.
#' @seealso [fda::eval.fd()]
#' @export
#' @importFrom fda eval.fd
dd_eval <- function(ddobj, t) {
  class(ddobj) <- "fd"
  if (is.null(ddobj$constant)) {
    print("The normalizing constant is set as 1")
    ddobj$constant <- 1
  }
  sweep(exp(fda::eval.fd(ddobj, t)), 2, as.numeric(ddobj$constant), "/")
}

#' Evaluate a column of functional objects on a shared grid
#'
#' Tidyverse helper used by [plot_funs()]: takes a data frame containing a
#' list column of `fd` objects, evaluates each on the grid, and returns a
#' long data frame with `id`, `x`, `y` columns.
#'
#' @param .data A data frame with a list column of `fd` objects.
#' @param funs Tidy-selection for that column.
#' @param n,rangeval,x Grid configuration.
#' @return A long data frame.
#' @keywords internal
#' @export
eval_funs <- function(.data, funs, n = 101,
                      rangeval = range(lapply(
                        dplyr::pull(.data, {{ funs }}),
                        \(fun) fun$basis$rangeval
                      )),
                      x = seq(rangeval[1], rangeval[2], length.out = n)) {
  .data |>
    dplyr::ungroup() |>
    dplyr::mutate("{{funs}}_eval" := lapply(
      {{ funs }},
      function(fobj) {
        rangeval <- fobj$basis$rangeval
        x <- x[x >= rangeval[1] & x <= rangeval[2]]
        data.frame(x = x, y = c(fda::eval.fd(fobj, x)))
      }
    )) |>
    dplyr::mutate(id = seq_len(dplyr::n())) |>
    tidyr::unnest(rlang::englue("{{funs}}_eval"))
}

#' Compute the normalizing constant of a log-density carrier
#'
#' Numerically integrates `exp(W(t))` over the basis range and returns the
#' normalizing constants per realization, formatted as character strings in
#' scientific notation (one per column of `self$coefs`).
#'
#' @param self An object carrying `coefs`, `basis`, and (optionally)
#'   `constant`.
#' @return Character vector of normalizing constants, one per realization;
#'   `NULL` if `self$constant` is already numeric.
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
