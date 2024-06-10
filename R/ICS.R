#' @export
ICS.default <- ICS::ICS

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
#' @rdname ICS
#' @export
ICS <- function(...) UseMethod("ICS")

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fdobj PARAM_DESCRIPTION
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
#'  \code{\link[fda]{inprod}}, \code{\link[fda]{fd}}
#'  \code{\link[ICS]{ICS-S3}}, \code{\link[ICS]{ics}}, \code{\link[ICS]{ICS}}
#' @rdname ICS.fd
#' @export
#' @importFrom fda inprod fd
#' @importFrom ICS ICS
#' @import ICS
ICS.fd <- function(fdobj, ...) {
  changemat <- to_zbsplines(basis = fdobj$basis, inv = TRUE)
  gram <- t(changemat) %*% gram(fdobj$basis) %*% changemat
  icsobj <- ICS::ICS(crossprod(to_zbsplines(fdobj), gram), ...)
  W <- icsobj$W
  icsobj$W <- fda::fd(
    to_zbsplines(coefs = t(W), basis = fdobj$basis, inv = TRUE),
    fdobj$basis
  )
  class(icsobj) <- c("ICS", "fd")
  icsobj
}

gram <- memoise::memoise(function(bobj) {
  rval <- bobj$rangeval
  p <- bobj$nbasis
  f <- function(t) t(fda::eval.basis(t, bobj)) %*% fda::eval.basis(t, bobj)

  result_matrix <- matrix(0, nrow = p, ncol = p)

  for (i in 1:p) {
    for (j in 1:p) {
      element_function <- Vectorize(function(x) {
        f(x)[i, j]
      })
      result_matrix[i, j] <- stats::integrate(
        element_function,
        rval[1], rval[2]
      )$value
    }
  }
  result_matrix
})

# Example usage with a basis object (replace with your actual basis object)
# basis_obj <- create.bspline.basis(rangeval = c(0, 1), nbasis = 4)
# result <- gram(basis_obj)
# print(result)

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
#' @rdname ICS.dd
#' @export
ICS.dd <- function(...) {
  icsobj <- ICS.fd(...)
  icsobj$W <- dd(clr = icsobj$W)
  class(icsobj) <- c("ICS", "dd")
  icsobj
}

#' @export
ICS.fdl <- function(fdlist, ...) ICS(c(fdlist), ...)

#' @export
mdist_simu_test <- memoise::memoise(ICSOutlier::dist_simu_test)

#' @rdname ICS_outlier
#' @export
ICS_outlier.default <- ICSOutlier::ICS_outlier

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param X PARAM_DESCRIPTION
#' @param S1 PARAM_DESCRIPTION, Default: ICS_cov
#' @param S2 PARAM_DESCRIPTION, Default: ICS_cov4
#' @param S1_args PARAM_DESCRIPTION, Default: list()
#' @param S2_args PARAM_DESCRIPTION, Default: list()
#' @param ICS_algorithm PARAM_DESCRIPTION, Default: c("whiten", "standard", "QR")
#' @param method PARAM_DESCRIPTION, Default: 'norm_test'
#' @param test PARAM_DESCRIPTION, Default: 'agostino.test'
#' @param n_eig PARAM_DESCRIPTION, Default: 10000
#' @param level_test PARAM_DESCRIPTION, Default: 0.05
#' @param adjust PARAM_DESCRIPTION, Default: TRUE
#' @param level_dist PARAM_DESCRIPTION, Default: 0.025
#' @param n_dist PARAM_DESCRIPTION, Default: 10000
#' @param type PARAM_DESCRIPTION, Default: 'smallprop'
#' @param n_cores PARAM_DESCRIPTION, Default: NULL
#' @param iseed PARAM_DESCRIPTION, Default: NULL
#' @param pkg PARAM_DESCRIPTION, Default: 'ICSOutlier'
#' @param q_type PARAM_DESCRIPTION, Default: 7
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname ICS_outlier
#' @export
ICS_outlier <- function(...) UseMethod("ICS_outlier")

#' @rdname ICS_outlier
#' @export
#' @importFrom ggplot2 ggplot geom_point aes geom_line geom_hline ggplotGrob guides
#' @importFrom GGally ggpairs ggmatrix_gtable
ICS_outlier.fd <- function(
    X, S1 = ICS::ICS_cov, S2 = ICS::ICS_cov4, S1_args = list(), S2_args = list(),
    ICS_algorithm = c("whiten", "standard", "QR"), index = NULL, method = "norm_test",
    test = "agostino.test", n_eig = 10000, level_test = 0.05,
    adjust = TRUE, level_dist = 0.025, n_dist = 10000, type = "smallprop",
    n_cores = NULL, iseed = NULL, pkg = "ICSOutlier", q_type = 7,
    ...) {
  # Step 1: Checks and apply ICS if necessary
  algorithm <- match.arg(ICS_algorithm)
  method <- match.arg(method, c("norm_test", "simulation"))
  if (!(inherits(X, "fd"))) {
    stop("'X' must be of class 'fd'")
  }
  if (!is.function(S1)) {
    stop(paste("S1 must be specified as a function"))
  }
  if (!is.function(S2)) {
    stop(paste("S2 must be specified as a function"))
  }
  if (inherits(X, "ICS")) {
    warning(paste("'X' already has class 'ICS', not applying ICS"))
    object <- X
  } else {
    object <- tryCatch(
      {
        ICS(X,
          S1 = S1, S2 = S2, S1_args = S1_args, S2_args = S2_args,
          algorithm = algorithm, center = TRUE, fix_signs = "scores"
        )
      },
      warning = function(w) stop(w),
      error = function(e) stop(e)
    )
  }
  rownames(object$scores) <- rownames(X)
  row_names <- rownames(object$scores)

  # Step 2: Select the components
  n <- nrow(object$scores)
  p <- ncol(object$scores)
  type <- match.arg(type, c("smallprop"))
  if (is.null(index)) {
    res_method <- switch(method,
      norm_test = {
        ICSOutlier::comp_norm_test(object,
          test = test, level = level_test,
          adjust = adjust, type = type
        )
      },
      simulation = {
        ICSOutlier::comp_simu_test(object,
          S1 = S1, S2 = S2, S1_args = S1_args,
          S2_args = S2_args, m = n_eig, level = level_test,
          adjust = adjust, type = type, n_cores = n_cores,
          iseed = iseed, pkg = pkg, q_type = q_type, ...
        )
      }
    )
  } else {
    res_method <- list(index = index)
  }

  # Step 3: Detecting the outliers
  if (sum(res_method$index < 0.5)) {
    outliers <- rep(0L, n)
    names(outliers) <- row_names
    IC_distances <- rep(0L, n)
    names(IC_distances) <- row_names
    IC_distances_quantile <- rep(0, n)
  } else {
    empty_object <- list(scores = matrix(nrow = n, ncol = p))
    class(empty_object) <- "ICS"
    IC_distances_quantile <- mdist_simu_test(empty_object,
      S1 = S1,
      S2 = S2, S1_args = S1_args, S2_args = S2_args, m = n_dist,
      index = res_method$index, level = level_dist, n_cores = n_cores,
      iseed = iseed, pkg = pkg, q_type = q_type, ...
    )
    IC_distances <- ICSOutlier::ics_distances(object, index = res_method$index)
    outliers <- as.integer(IC_distances > IC_distances_quantile)
    names(outliers) <- row_names
  }
  res <- list(
    X = X,
    scores = object$scores,
    W = object$W,
    gen_kurtosis = object$gen_kurtosis,
    outliers = outliers, ics_distances = IC_distances,
    ics_dist_cutoff = IC_distances_quantile, level_dist = level_dist,
    level_test = level_test, method = method, index = res_method$index,
    test = res_method$test, criterion = res_method$criterion,
    adjust = res_method$adjust, type = res_method$type, n_dist = as.integer(n_dist),
    n_eig = as.integer(n_eig), S1_label = object$S1_label,
    S2_label = object$S2_label
  )
  class(res) <- "ICS_Out_fd"
  res
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
    eigenfun = I(if (inherits(X, "dd")) as.list(object$W) else as.list(object$W))
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
    plot_funs(eigenfun, color = IC)
  plot(g2)

  g3 <- ggplot(distances_dat, aes(Index, IC_distances, color = outlier)) +
    geom_point() +
    geom_hline(yintercept = object$ics_dist_cutoff)

  pairs_dat <- data.frame(object$scores, outlier = outlier_fact)

  if (length(object$index) > 1) {
    g3b <- GGally::ggpairs(pairs_dat,
      diag = "blank",
      columns = object$index,
      aes(label = 1:n, shape = NA, color = outlier)
    ) + geom_text()
    print(g3b)
  } else {
    plot(g3)
  }

  if (!inherits(X, "ICS")) {
    f_dat <- data.frame(
      X = I(if (inherits(X, "dd")) as.list(X) else as.list(X)),
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

#' @export
ICS_outlier.fdl <- function(fdlist, ...) ICS_outlier(c(fdlist), ...)
