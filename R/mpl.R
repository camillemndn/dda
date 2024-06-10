#' @export
density_nlm <- function(x, WfdParobj, conv = 1e-04, iterlim = 200, active = NULL, dbglev = 0, ...) {
  # Ensure WfdParobj is a fdPar object
  if (!inherits(WfdParobj, "fdPar")) {
    if (inherits(WfdParobj, "fd") || inherits(WfdParobj, "basisfd")) {
      WfdParobj <- fdPar(WfdParobj)
    } else {
      stop("WfdParobj is not a fdPar object")
    }
  }

  Wfdobj <- WfdParobj$fd
  Lfdobj <- WfdParobj$Lfd
  Lfdobj <- int2Lfd(Lfdobj)
  basisobj <- Wfdobj$basis
  nbasis <- basisobj$nbasis
  rangex <- basisobj$rangeval

  x <- as.matrix(x)
  xdim <- dim(x)
  N <- xdim[1]
  m <- xdim[2]

  if (m > 2 && N > 2) stop("Argument x must have either one or two columns.")
  if ((N == 1 | N == 2) & m > 1) {
    x <- t(x)
    n <- N
    N <- m
    m <- n
  }

  if (m == 1) {
    f <- rep(1, N)
  } else {
    f <- x[, 2]
    fsum <- sum(f)
    f <- f / fsum
    x <- x[, 1]
  }

  f <- as.matrix(f)
  inrng <- (1:N)[x >= rangex[1] & x <= rangex[2]]
  if (length(inrng) != N) {
    warning("Some values in x out of range and not used.")
  }

  x <- x[inrng]
  f <- f[inrng]
  nobs <- length(x)

  climit <- c(rep(-50, nbasis), rep(400, nbasis))
  cvec0 <- Wfdobj$coefs
  lambda <- WfdParobj$lambda

  if (lambda > 0) {
    Kmat <- lambda * fda::getbasispenalty(basisobj, Lfdobj)
  }

  # Objective function
  objective_function <- function(cvec) {
    result <- fda:::loglfnden(x, f, basisobj, cvec)
    logl <- result[[1]]
    Dlogl <- result[[2]]
    hmat <- fda:::Varfnden(x, basisobj, cvec)
    if (lambda > 0) {
      penalty <- sum(cvec * (Kmat %*% cvec))
      logl <- logl - penalty
      Dlogl <- Dlogl - 2 * Kmat %*% cvec
      hmat <- hmat + 2 * Kmat
    }
    attr(logl, "gradient") <- -Dlogl
    attr(logl, "hessian") <- hmat
    return(-logl)
  }

  # Optimize using nlm
  result <- nlm(f = objective_function, p = cvec0, gradtol = conv, iterlim = iterlim, fscale = 1, print.level = dbglev)
  print(result$iterations)
  Wfdobj$coefs <- result$estimate
  C <- normalize(dd(clr = fda::fd(result$estimate, basisobj)))

  return(list(Wfdobj = Wfdobj, C = C, Flist = list(f = result$minimum), iternum = result$iterations, iterhist = result))
}

# Helper functions used in the code above need to be defined, such as loglfnden, Varfnden, normden.phi, etc.


#' @import torch
DensityFunction <- torch::nn_module(
  initialize = function(basisobj, Wfdobj, lambda, Kmat) {
    self$basisobj <- basisobj
    self$Wfdobj <- Wfdobj
    self$cvec <- nn_parameter(torch_tensor(Wfdobj$coefs, dtype = torch_float()))
    self$lambda <- lambda
    self$Kmat <- Kmat
  },
  forward = function(x, f) {
    cvec <- self$cvec
    self$Wfdobj$coefs <- as.numeric(cvec)
    result <- fda:::loglfnden(x, f, self$basisobj, as.numeric(cvec))
    logl <- result[[1]]
    penalty <- 0
    if (self$lambda > 0) {
      penalty <- cvec %>%
        t() %>%
        matmul(self$Kmat) %>%
        matmul(cvec)
    }
    return(-logl + penalty)
  }
)

optimize_density <- function(x, WfdParobj, conv = 1e-04, iterlim = 20, dbglev = 0) {
  if (!inherits(WfdParobj, "fdPar")) {
    if (inherits(WfdParobj, "fd") || inherits(WfdParobj, "basisfd")) {
      WfdParobj <- fdPar(WfdParobj)
    } else {
      stop("WFDPAROBJ is not a fdPar object")
    }
  }

  Wfdobj <- WfdParobj$fd
  basisobj <- Wfdobj$basis
  nbasis <- basisobj$nbasis
  rangex <- basisobj$rangeval
  lambda <- WfdParobj$lambda

  x <- as.matrix(x)
  if (ncol(x) == 2) {
    f <- x[, 2]
    f <- f / sum(f)
    x <- x[, 1]
  } else {
    f <- rep(1, nrow(x))
  }

  inrng <- which(x >= rangex[1] & x <= rangex[2])
  if (length(inrng) != length(x)) {
    warning("Some values in X out of range and not used.")
    x <- x[inrng]
    f <- f[inrng]
  }

  x <- torch_tensor(x, dtype = torch_float())
  f <- torch_tensor(f, dtype = torch_float())

  Kmat <- if (lambda > 0) lambda * getbasispenalty(basisobj, WfdParobj$Lfd) else torch_tensor(0, dtype = torch_float())

  model <- DensityFunction(basisobj, Wfdobj, lambda, Kmat)
  optimizer <- optim_lbfgs(model$parameters, lr = 0.1, max_iter = iterlim)

  loss_closure <- function() {
    optimizer$zero_grad()
    loss <- model(x, f)
    loss$backward()
    if (dbglev > 0) {
      cat("Loss:", as.numeric(loss), "\n")
    }
    return(loss)
  }

  optimizer$step(loss_closure)

  Wfdobj$coefs <- as.numeric(model$cvec)
  C <- normden.phi(basisobj, Wfdobj$coefs)

  list(Wfdobj = Wfdobj, C = C, Flist = list(f = as.numeric(loss_closure())), iternum = optimizer$state$iters, iterhist = optimizer$state$history)
}
