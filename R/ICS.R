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
ICS.fd <- function(fdobj, ...) {
  changemat <- to_zbsplines(basis = fdobj$basis, inv = TRUE)
  gram <- t(changemat) %*% fda::inprod(fdobj$basis, fdobj$basis) %*% changemat
  icsobj <- ICS::ICS(crossprod(to_zbsplines(fdobj), gram), ...)
  icsobj$W <- fda::fd(
    to_zbsplines(coefs = t(icsobj$W), basis = fdobj$basis, inv = TRUE),
    fdobj$basis
  )
  icsobj
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
#' @rdname ICS.dd
#' @export
ICS.dd <- function(...) {
  icsobj <- ICS.fd(...)
  icsobj$W <- density(clr = icsobj$W)
  icsobj
}

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
ICS_outlier.dd <- function(
    X, S1 = ICS::ICS_cov, S2 = ICS::ICS_cov4, S1_args = list(), S2_args = list(),
    ICS_algorithm = c("whiten", "standard", "QR"), method = "norm_test",
    test = "agostino.test", n_eig = 10000, level_test = 0.05,
    adjust = TRUE, level_dist = 0.025, n_dist = 10000, type = "smallprop",
    n_cores = NULL, iseed = NULL, pkg = "ICSOutlier", q_type = 7,
    ...) {
  algorithm <- match.arg(ICS_algorithm)
  method <- match.arg(method, c("norm_test", "simulation"))
  if (!(inherits(X, "dd") | inherits(X, "fd"))) {
    stop("'X' must be of class 'data.frame' or 'matrix'")
  }
  if (!is.function(S1)) {
    stop(paste("S1 must be specified as a function"))
  }
  if (!is.function(S2)) {
    stop(paste("S2 must be specified as a function"))
  }
  object <- tryCatch(
    {
      ICS.dd(X,
        S1 = S1, S2 = S2, S1_args = S1_args, S2_args = S2_args,
        algorithm = algorithm, center = TRUE, fix_signs = "scores"
      )
    },
    warning = function(w) stop(w),
    error = function(e) stop(e)
  )
  rownames(object$scores) <- rownames(X)
  row_names <- rownames(object$scores)
  type <- match.arg(type, c("smallprop"))
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
  n <- nrow(object$scores)
  p <- ncol(object$scores)
  if (sum(res_method$index < 0.5)) {
    outliers <- rep(0L, n)
    names(outliers) <- row_names
    IC_distances <- rep(0L, n)
    names(IC_distances) <- row_names
    IC_distances_quantile <- rep(0, n)
  } else {
    IC_distances_quantile <- ICSOutlier::dist_simu_test(object,
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
    outliers = outliers, ics_distances = IC_distances,
    ics_dist_cutoff = IC_distances_quantile, level_dist = level_dist,
    level_test = level_test, method = method, index = res_method$index,
    test = res_method$test, criterion = res_method$criterion,
    adjust = res_method$adjust, type = res_method$type, n_dist = as.integer(n_dist),
    n_eig = as.integer(n_eig), S1_label = object$S1_label,
    S2_label = object$S2_label
  )
  class(res) <- "ICS_Out"
  res
}

#' @rdname ICS_outlier
#' @export
ICS_outlier.fd <- function(
    X, S1 = ICS::ICS_cov, S2 = ICS::ICS_cov4, S1_args = list(), S2_args = list(),
    ICS_algorithm = c("whiten", "standard", "QR"), method = "norm_test",
    test = "agostino.test", n_eig = 10000, level_test = 0.05,
    adjust = TRUE, level_dist = 0.025, n_dist = 10000, type = "smallprop",
    n_cores = NULL, iseed = NULL, pkg = "ICSOutlier", q_type = 7,
    ...) {
  algorithm <- match.arg(ICS_algorithm)
  method <- match.arg(method, c("norm_test", "simulation"))
  if (!(inherits(X, "dd") | inherits(X, "fd"))) {
    stop("'X' must be of class 'data.frame' or 'matrix'")
  }
  if (!is.function(S1)) {
    stop(paste("S1 must be specified as a function"))
  }
  if (!is.function(S2)) {
    stop(paste("S2 must be specified as a function"))
  }
  object <- tryCatch(
    {
      ICS.fd(X,
        S1 = S1, S2 = S2, S1_args = S1_args, S2_args = S2_args,
        algorithm = algorithm, center = TRUE, fix_signs = "scores"
      )
    },
    warning = function(w) stop(w),
    error = function(e) stop(e)
  )
  rownames(object$scores) <- rownames(X)
  row_names <- rownames(object$scores)
  type <- match.arg(type, c("smallprop"))
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
  n <- nrow(object$scores)
  p <- ncol(object$scores)
  if (sum(res_method$index < 0.5)) {
    outliers <- rep(0L, n)
    names(outliers) <- row_names
    IC_distances <- rep(0L, n)
    names(IC_distances) <- row_names
    IC_distances_quantile <- rep(0, n)
  } else {
    IC_distances_quantile <- ICSOutlier::dist_simu_test(object,
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
    outliers = outliers, ics_distances = IC_distances,
    ics_dist_cutoff = IC_distances_quantile, level_dist = level_dist,
    level_test = level_test, method = method, index = res_method$index,
    test = res_method$test, criterion = res_method$criterion,
    adjust = res_method$adjust, type = res_method$type, n_dist = as.integer(n_dist),
    n_eig = as.integer(n_eig), S1_label = object$S1_label,
    S2_label = object$S2_label
  )
  class(res) <- "ICS_Out"
  res
}
