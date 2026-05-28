//! savvy bindings: the single `density_mpl_rust` entry point seen by R.
//!
//! The R-side wrapper is responsible for:
//!   * extracting basis params from a `basisfd` object into the flat vectors
//!     we accept here,
//!   * precomputing the penalty matrix `K = λ · getbasispenalty(Lfd)` (or
//!     passing an empty vector when λ = 0),
//!   * precomputing `Z = zerobasis(nbasis)`.
//!
//! Doing the unpacking R-side keeps the Rust code free of any callback into R.

use savvy::{savvy, OwnedIntegerSexp, OwnedListSexp, OwnedRealSexp, RealSexp, Sexp};

use crate::basis::{Basis, BasisKind};
use crate::density::{density_fd, DensityOpts};
use crate::mat::Matrix;

#[allow(clippy::too_many_arguments)]
#[savvy]
fn density_mpl_rust_raw(
    x: RealSexp,
    f: RealSexp,
    basis_kind: i32,
    nbasis: i32,
    range: RealSexp,
    knots: RealSexp,
    norder: i32,
    params: RealSexp,
    cvec0: RealSexp,
    zmat: RealSexp,
    zmat_rows: i32,
    kmat: RealSexp,
    conv: f64,
    iterlim: i32,
    climit_lo: f64,
    climit_hi: f64,
) -> savvy::Result<Sexp> {
    let nbasis = nbasis as usize;
    let norder = norder as usize;
    let iterlim = iterlim as usize;

    let range_slice = range.as_slice();
    let basis = Basis {
        kind: BasisKind::from_tag(basis_kind as u8),
        nbasis,
        range: (range_slice[0], range_slice[1]),
        knots: knots.as_slice().to_vec(),
        norder,
        params: params.as_slice().to_vec(),
    };

    let x_vec: Vec<f64> = x.as_slice().to_vec();
    let f_vec: Vec<f64> = f.as_slice().to_vec();
    let cvec0_vec: Vec<f64> = cvec0.as_slice().to_vec();

    let zmat_rows = zmat_rows as usize;
    let zmat_data: Vec<f64> = zmat.as_slice().to_vec();
    let zmat_cols = if zmat_rows == 0 { 0 } else { zmat_data.len() / zmat_rows };
    // R passes column-major; convert to our row-major layout.
    let zmat_matrix = column_major_to_row_major(&zmat_data, zmat_rows, zmat_cols);

    let kmat_data: Vec<f64> = kmat.as_slice().to_vec();
    let kmat_option = if kmat_data.is_empty() {
        None
    } else {
        Some(column_major_to_row_major(&kmat_data, nbasis, nbasis))
    };

    let opts = DensityOpts {
        conv,
        iterlim,
        kmat: kmat_option,
        zmat: zmat_matrix,
        climit_lo,
        climit_hi,
    };

    let result = density_fd(&basis, &x_vec, &f_vec, &cvec0_vec, &opts);

    // Build a named list: list(coefs, C, neg_log_l, grad_norm, iternum, iterhist).
    let mut out = OwnedListSexp::new(6, true)?;

    let mut coefs = OwnedRealSexp::new(result.cvec.len())?;
    for (i, v) in result.cvec.iter().enumerate() {
        coefs.set_elt(i, *v)?;
    }
    out.set_name_and_value(0, "coefs", coefs)?;

    out.set_name_and_value(1, "C", scalar_real(result.c_norm)?)?;
    out.set_name_and_value(2, "neg_log_l", scalar_real(result.neg_log_l)?)?;
    out.set_name_and_value(3, "grad_norm", scalar_real(result.grad_norm)?)?;
    out.set_name_and_value(4, "iternum", scalar_int(result.iternum as i32)?)?;

    let nrows = result.iterhist.len();
    let mut hist = OwnedRealSexp::new(nrows * 4)?;
    // Column-major: column k stored as hist[k * nrows + r].
    for r in 0..nrows {
        for c in 0..4 {
            hist.set_elt(c * nrows + r, result.iterhist[r][c])?;
        }
    }
    out.set_name_and_value(5, "iterhist", hist)?;

    Ok(out.into())
}

fn scalar_real(v: f64) -> savvy::Result<OwnedRealSexp> {
    let mut s = OwnedRealSexp::new(1)?;
    s.set_elt(0, v)?;
    Ok(s)
}

fn scalar_int(v: i32) -> savvy::Result<OwnedIntegerSexp> {
    let mut s = OwnedIntegerSexp::new(1)?;
    s.set_elt(0, v)?;
    Ok(s)
}

fn column_major_to_row_major(data: &[f64], rows: usize, cols: usize) -> Matrix {
    let mut m = Matrix::zeros(rows, cols);
    for j in 0..cols {
        for i in 0..rows {
            *m.at_mut(i, j) = data[j * rows + i];
        }
    }
    m
}
