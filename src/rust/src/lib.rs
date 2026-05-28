//! dda: Rust kernel for distributional data analysis.
//!
//! Currently exposes:
//!   - `density_fd_rust(...)`: penalized log-spline density estimation.
//!
//! Module layout:
//!   mat     — row-major dense matrix + small LA primitives (Cholesky, etc.)
//!   basis   — point evaluation of fda basis families
//!   romberg — generic Romberg integration over [a, b]
//!   density — Fisher-scoring loop with line search
//!   ffi     — savvy bindings exposed to R

mod basis;
mod density;
mod ffi;
mod mat;
mod romberg;
