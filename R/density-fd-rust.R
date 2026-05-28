#' Penalized log-spline density estimation (Rust backend)
#'
#' Reimplementation of [density.fd] with the Fisher-scoring loop and Romberg
#' integrators executed in Rust. Same statistical model, same output shape;
#' typically 10–50x faster on B-spline bases with `nbasis` in the usual range.
#'
#' @param x         Observation vector (length n) or two-column matrix (x, f).
#' @param WfdParobj An `fdPar` object (or `fd` / `basisfd`, in which case the
#'   penalty is zero).
#' @param conv      Convergence tolerance on the objective. Default `1e-4`.
#' @param iterlim   Maximum Fisher-scoring iterations. Default `20`.
#'
#' @return A list with the same fields as [density.fd]:
#'   `Wfdobj`, `C`, `Flist`, `iternum`, `iterhist`.
#' @export
density_fd_rust <- function(x, WfdParobj, conv = 1e-4, iterlim = 20) {

  if (!inherits(WfdParobj, "fdPar")) {
    if (inherits(WfdParobj, "fd") || inherits(WfdParobj, "basisfd")) {
      WfdParobj <- fda::fdPar(WfdParobj)
    } else {
      stop("WfdParobj is not an fdPar object")
    }
  }

  Wfdobj   <- WfdParobj$fd
  basisobj <- Wfdobj$basis
  nbasis   <- basisobj$nbasis
  rangex   <- basisobj$rangeval
  lambda   <- WfdParobj$lambda

  # --- normalize x / f exactly as density.fd does --------------------------
  x <- as.matrix(x)
  xdim <- dim(x); N <- xdim[1]; m <- xdim[2]
  if (m > 2 && N > 2) stop("Argument x must have one or two columns.")
  if ((N == 1 || N == 2) && m > 1) { x <- t(x); n <- N; N <- m; m <- n }
  if (m == 1) {
    f <- rep(1, N)
  } else {
    f <- x[, 2]; f <- f / sum(f); x <- x[, 1]
  }
  in_rng <- x >= rangex[1] & x <= rangex[2]
  if (!all(in_rng)) {
    warning("Some values in x out of range and not used.")
    x <- x[in_rng]; f <- f[in_rng]
  }

  # --- pack basis into the flat shape expected by Rust ---------------------
  basis_pack <- .pack_basis(basisobj)

  # --- precompute K = lambda * getbasispenalty(Lfd) and Z = zerobasis() ----
  if (lambda > 0) {
    Lfd  <- fda::int2Lfd(WfdParobj$Lfd)
    Kmat <- lambda * fda::getbasispenalty(basisobj, Lfd)
  } else {
    Kmat <- numeric(0)
  }

  Z <- fda::zerobasis(nbasis)  # nbasis × (nbasis - 1)

  # --- call into Rust ------------------------------------------------------
  cvec0 <- as.numeric(Wfdobj$coefs)
  res <- density_fd_rust_raw(
    x          = as.numeric(x),
    f          = as.numeric(f),
    basis_kind = as.integer(basis_pack$kind),
    nbasis     = as.integer(nbasis),
    range      = as.numeric(rangex),
    knots      = as.numeric(basis_pack$knots),
    norder     = as.integer(basis_pack$norder),
    params     = as.numeric(basis_pack$params),
    cvec0      = cvec0,
    zmat       = as.numeric(Z),
    zmat_rows  = as.integer(nrow(Z)),
    kmat       = as.numeric(Kmat),
    conv       = as.numeric(conv),
    iterlim    = as.integer(iterlim),
    climit_lo  = as.numeric(-50),
    climit_hi  = as.numeric(400)
  )

  Wfdobj$coefs <- matrix(res$coefs, ncol = 1)
  Flist <- list(f = res$neg_log_l, norm = res$grad_norm)
  iterhist <- matrix(res$iterhist, ncol = 4)
  colnames(iterhist) <- c("iter", "F", "neg_logL", "grad_norm")

  list(
    Wfdobj   = Wfdobj,
    C        = res$C,
    Flist    = Flist,
    iternum  = res$iternum,
    iterhist = iterhist
  )
}

# Internal: translate an fda basisfd into the (kind, knots, norder, params)
# quadruple consumed by basis.rs.
.pack_basis <- function(basisobj) {
  type   <- basisobj$type
  nbasis <- basisobj$nbasis
  rng    <- basisobj$rangeval

  if (type == "bspline") {
    norder <- basisobj$nbasis - length(basisobj$params)
    breaks <- c(rng[1], basisobj$params, rng[2])
    knots  <- c(rep(rng[1], norder - 1), breaks, rep(rng[2], norder - 1))
    list(kind = 0L, knots = knots, norder = norder, params = numeric(0))
  } else if (type == "fourier") {
    list(kind = 1L, knots = numeric(0), norder = 0L,
         params = as.numeric(basisobj$params[1]))
  } else if (type == "const") {
    list(kind = 2L, knots = numeric(0), norder = 0L, params = numeric(0))
  } else if (type == "monom") {
    list(kind = 3L, knots = numeric(0), norder = 0L, params = numeric(0))
  } else if (type == "polygonal" || type == "polyg") {
    list(kind = 4L, knots = as.numeric(basisobj$params), norder = 0L,
         params = numeric(0))
  } else if (type == "power") {
    list(kind = 5L, knots = numeric(0), norder = 0L,
         params = as.numeric(basisobj$params))
  } else if (type == "expon" || type == "exponential") {
    list(kind = 6L, knots = numeric(0), norder = 0L,
         params = as.numeric(basisobj$params))
  } else {
    stop("Basis type '", type, "' is not supported by density_fd_rust.")
  }
}
