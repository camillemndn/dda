# Wall-clock benchmark: density.fd (R) vs density_fd_rust (Rust).
#
# Both implementations are called with their package-default convergence
# settings (conv = 1e-4, iterlim = 20), so this is the realistic comparison
# a user would see — not "time to reach a fixed |g|".

suppressMessages(library(dda))
library(microbenchmark)
ref  <- dda:::density.fd
rust <- dda::density_fd_rust

make_basis <- function(type, K, rng = c(-4, 4)) {
  switch(type,
    bspline = fda::create.bspline.basis(rng, nbasis = K, norder = 4),
    fourier = fda::create.fourier.basis(rng, nbasis = K)
  )
}
make_sample <- function(N) rnorm(N)

bench_one <- function(N, K, type, times = 10) {
  set.seed(N * 7 + K)
  x      <- make_sample(N)
  basis  <- make_basis(type, K)
  Wfd0   <- fda::fd(matrix(0, basis$nbasis, 1), basis)
  WfdPar <- fda::fdPar(Wfd0)
  mb <- microbenchmark(
    R    = ref (x, WfdPar),
    Rust = rust(x, WfdPar),
    times = times
  )
  summ <- aggregate(time ~ expr, data = mb, FUN = median)
  t_r    <- summ$time[summ$expr == "R"]    / 1e6  # ns → ms
  t_rust <- summ$time[summ$expr == "Rust"] / 1e6
  data.frame(
    type   = type, N = N, K = K,
    R_ms     = round(t_r,    2),
    Rust_ms  = round(t_rust, 2),
    speedup  = round(t_r / t_rust, 1)
  )
}

grid <- expand.grid(
  type = c("bspline", "fourier"),
  N    = c(100, 500, 2000, 10000),
  K    = c(7, 13, 21),
  stringsAsFactors = FALSE
)
# Fourier needs odd K; trim.
grid <- subset(grid, !(type == "fourier" & K %% 2 == 0))

cat("Benchmarking", nrow(grid), "configurations...\n")
res <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  cat(sprintf("  %s  N=%5d  K=%2d ... ", grid$type[i], grid$N[i], grid$K[i]))
  out <- bench_one(grid$N[i], grid$K[i], grid$type[i],
                   times = if (grid$N[i] >= 2000) 5 else 10)
  cat(sprintf("R=%.1fms  Rust=%.1fms  speedup=%.1fx\n",
              out$R_ms, out$Rust_ms, out$speedup))
  out
}))

cat("\n=== Summary ===\n")
print(res, row.names = FALSE)
