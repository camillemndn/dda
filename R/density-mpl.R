#' Maximum penalized likelihood density estimation
#'
#' Fits a density of the form \eqn{p(x) = \exp(\phi(x)^\top c)/C} on a
#' bounded interval, with optional smoothness penalty.
#' Dispatches between the Rust backend (default, ~10–60x faster) and the
#' original R algorithm (preserved as [density_mpl_legacy()]).
#'
#' @param x Observation vector or two-column (value, frequency) matrix.
#' @param WfdParobj An `fdPar` object (or `fd` / `basisfd`).
#' @param conv Convergence tolerance on the objective. Default `1e-4`.
#' @param iterlim Maximum Fisher-scoring iterations. Default `20`.
#' @param backend Which implementation to use. `"rust"` (default) calls the
#'   compiled kernel via [density_mpl_rust()]; `"legacy"` calls the original
#'   R code [density_mpl_legacy()] for bit-for-bit compatibility.
#' @param ... Further arguments forwarded to the selected backend.
#'
#' @return A list with `Wfdobj`, `C`, `Flist`, `iternum`, `iterhist`.
#' @export
density_mpl <- function(x, WfdParobj, conv = 1e-4, iterlim = 20,
                        backend = c("rust", "legacy"), ...) {
  backend <- match.arg(backend)
  switch(backend,
    rust   = density_mpl_rust  (x, WfdParobj, conv = conv, iterlim = iterlim, ...),
    legacy = density_mpl_legacy(x, WfdParobj, conv = conv, iterlim = iterlim, ...)
  )
}
