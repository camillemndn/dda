#' R6 Class Representing Distributional Data
#'
#' @description
#' Using Bayes Spaces.
#'
#' @details
#' More details here.
#' @export

dd <- R6::R6Class("dd",
  public = list(

    #' @field clr An `fd` object containing representation of clr on a
    #' ZB-spline basis.
    clr = NULL,

    #' @field constant Normalizing constant.
    constant = NULL,

    #' @description
    #' Create a new distributional data object.
    #' @param sample An `fd` object representating the clr transform.
    #' @param rangeval An `fd` object representating the clr transform.
    #' @param nbasis An `fd` object representating the clr transform.
    #' @param lambda An `fd` object representating the clr transform.
    #' @param clr An `fd` object representating the clr transform.
    #' @param constant Normalizing constant.
    #' @param basis The bspline basis.
    #' @return A new `Distributional Data` object.
    initialize = function(sample = NULL,
                          basis = fda::create.bspline.basis(rangeval, nbasis),
                          rangeval = NULL,
                          nbasis = 10,
                          lambda = 0,
                          clr = fda::fd(sample, basisobj = basis),
                          constant = NULL) {
      if (is.null(rangeval)) {
        rangeval <- if (!is.null(sample)) range(sample) else c(0, 1)
      }
      if (!is.null(sample)) {
        # Set up basis for W(x)
        basisobj <- fda::create.bspline.basis(rangeval, nbasis)
        # Set up initial value for wfdobj
        wfd0 <- fda::fd(matrix(0, nbasis, 1), basisobj)
        wfdparobj <- fda::fdPar(wfd0, lambda = lambda)
        # Estimate density
        wfdobj <- fda::density.fd(sample, wfdparobj)$Wfdobj
        wint <- integrate(
          \(t) fda::eval.fd(wfdobj, t),
          min(rangeval), max(rangeval),
          rel.tol = .Machine$double.eps^0.5
        )$value
        self$clr <- wfdobj - wint / diff(rangeval)
      } else {
        self$clr <- clr
      }
      self$constant <- constant
    },
    #' @description
    #' Add two distributional data objects together.
    #' @param dd2 Another `dd` object.
    #' @return A new `dd` object.
    `+` = function(dd2) {
      return(dd$new(
        clr = self$clr + dd2$clr
      ))
    },
    #' @description
    #' Substract two distributional data objects together.
    #' @param dd2 Another `dd` object.
    #' @return A new `dd` object.
    `-` = function(dd2 = NULL) {
      if (is.null(dd2)) {
        return(dd$new(
          clr = -self$clr
        ))
      }
      return(dd$new(
        clr = self$clr - dd2$clr
      ))
    },
    #' @description
    #' Multiply a distributional data object with a scalar.
    #' @param alpha A numerical value.
    #' @return A new `dd` object.
    `*` = function(alpha) {
      return(dd$new(
        clr = alpha * self$clr
      ))
    },
    #' @description
    #' Evaluates a distributional data object at a point `t`.
    #' @param t A real number `t`.
    #' @return A real number.
    eval = function(t) {
      self$normalize()
      exp(fda::eval.fd(self$clr, t)) / as.numeric(self$constant)
    },
    #' @description
    #' Computes the normalizing constant of distributional data object.
    #' @return Nothing.
    normalize = function() {
      if (!(is.numeric(self$constant))) {
        rangemin <- min(self$clr$basis$rangeval)
        rangemax <- max(self$clr$basis$rangeval)
        dens <- \(t) exp(fda::eval.fd(self$clr, t))
        self$constant <- format(
          integrate(dens, rangemin, rangemax,
            rel.tol = .Machine$double.eps^0.5
          )$value,
          scientific = TRUE
        )
      }
    },
    #' @description
    #' Plots a distributional data object.
    #' @param rangeval The interval on which to plot.
    #' @param h The step between each point.
    #' @param ... The rest.
    #' @return Nothing.
    plot = function(rangeval = self$clr$basis$rangeval,
                    h = 0.01,
                    ...) {
      argvals <- seq(min(rangeval), max(rangeval), h)
      plot(argvals, self$eval(argvals), type = "l", ...)
    },
    #' @description
    #' Computes the Aitchison inner product.
    #' @param dd2 The other density to compute inner product.
    #' @return The inner product.
    inprod = function(dd2 = self) {
      fda::inprod(self$clr, dd2$clr)
    }
  )
)

#' @description
#' Computes the Aitchison perturbation.
#' @export
#' @param dd First density to sum.
`+.dd` <- function(dd, ...) {
  dd$`+`(...)
}
#' @description
#' Computes the Aitchison negative perturbation.
#' @export
#' @param dd First density to substract.
`-.dd` <- function(dd1, ...) {
  dd1$`-`(...)
}
#' @description
#' Computes the Aitchison powering.
#' @export
#' @param dd First density to sum.
#' @param alpha Scalar.
`*.dd` <- function(alpha, dd) {
  if (is.numeric(dd)) {
    alpha$`*`(dd)
  } else {
    dd$`*`(alpha)
  }
}

#' @title Plot
#' @description
#' Plots a distributional data object.
#' @param dd Density to plot.
#' @export
plot.dd <- function(dd, ...) {
  dd$plot(...)
}
#' @export
eval.default <- eval
#' @export
eval <- function(...) {
  UseMethod("eval")
}

#' @title Evaluation
#' @description
#' Evaluates a distributional data object at a point `t`.
#' @param dd Density to eval.
#' @param t A real number `t`.
#' @return A real number.
#' @export
eval.dd <- function(dd, t) {
  dd$eval(t)
}
#' @export
inprod <- function(...) {
  UseMethod("inprod")
}

#' @title Inner product
#' @description
#' Computes the Aitchison inner product.
#' @param ... The other density to compute inner product.
#' @return The inner product.
#' @export
inprod.dd <- function(dd1, ...) {
  dd1$inprod(...)
}

#' @title Aitchison mean
#' @description
#' Computes the mean of a vector of densities.
#' @param ... The vector of densities.
#' @return A density object which is the mean.
#' @export
mean_dd <- function(...) {
  args <- list(...)
  vec_dd <- if (length(args) == 1 && is.list(args[[1]])) args[[1]] else args
  sum_dd <- Reduce(`+.dd`, vec_dd)
  n <- length(vec_dd)
  1 / n * sum_dd
}
