//! Tiny row-major dense matrix and the linear-algebra primitives needed by
//! the Fisher-scoring loop. Everything is small (K ≤ ~30), so naive
//! triple-loops are fine.

#[derive(Clone)]
pub struct Matrix {
    pub data: Vec<f64>,
    pub rows: usize,
    pub cols: usize,
}

impl Matrix {
    pub fn zeros(rows: usize, cols: usize) -> Self {
        Self {
            data: vec![0.0; rows * cols],
            rows,
            cols,
        }
    }

    #[inline(always)]
    pub fn at(&self, i: usize, j: usize) -> f64 {
        self.data[i * self.cols + j]
    }

    #[inline(always)]
    pub fn at_mut(&mut self, i: usize, j: usize) -> &mut f64 {
        &mut self.data[i * self.cols + j]
    }

    /// Row slice (read-only).
    pub fn row(&self, i: usize) -> &[f64] {
        &self.data[i * self.cols..(i + 1) * self.cols]
    }
}

/// y = A^T b where A is (n × k), b is length n. Result has length k.
pub fn at_mul_vec(a: &Matrix, b: &[f64]) -> Vec<f64> {
    let mut y = vec![0.0; a.cols];
    for i in 0..a.rows {
        let bi = b[i];
        if bi == 0.0 {
            continue;
        }
        let row = a.row(i);
        for j in 0..a.cols {
            y[j] += row[j] * bi;
        }
    }
    y
}

/// y = A x where A is (m × n), x is length n. Result has length m.
pub fn mul_vec(a: &Matrix, x: &[f64]) -> Vec<f64> {
    let mut y = vec![0.0; a.rows];
    for i in 0..a.rows {
        let row = a.row(i);
        let mut s = 0.0;
        for j in 0..a.cols {
            s += row[j] * x[j];
        }
        y[i] = s;
    }
    y
}

/// y = Z^T x for an arbitrary dense Z. Length-cols(Z) result.
pub fn zt_vec(z: &Matrix, x: &[f64]) -> Vec<f64> {
    at_mul_vec(z, x)
}

/// C = Z^T H Z for symmetric H. Z is (n × m), H is (n × n), C is (m × m).
pub fn zt_h_z(z: &Matrix, h: &Matrix) -> Matrix {
    let n = z.rows;
    let m = z.cols;
    debug_assert_eq!(h.rows, n);
    debug_assert_eq!(h.cols, n);
    // tmp = H Z  (n × m)
    let mut tmp = Matrix::zeros(n, m);
    for i in 0..n {
        for j in 0..m {
            let mut s = 0.0;
            for r in 0..n {
                s += h.at(i, r) * z.at(r, j);
            }
            *tmp.at_mut(i, j) = s;
        }
    }
    // out = Z^T tmp  (m × m)
    let mut out = Matrix::zeros(m, m);
    for i in 0..m {
        for j in 0..m {
            let mut s = 0.0;
            for r in 0..n {
                s += z.at(r, i) * tmp.at(r, j);
            }
            *out.at_mut(i, j) = s;
        }
    }
    out
}

/// In-place Cholesky factorization A = L Lᵀ for symmetric PD A (m × m).
/// Writes L into the lower triangle of `a`; upper triangle is untouched.
/// Returns `Err(())` on non-positive diagonal (matrix not PD).
pub fn cholesky(a: &mut Matrix) -> Result<(), ()> {
    let m = a.rows;
    debug_assert_eq!(a.cols, m);
    for i in 0..m {
        for j in 0..=i {
            let mut s = a.at(i, j);
            for r in 0..j {
                s -= a.at(i, r) * a.at(j, r);
            }
            if i == j {
                if s <= 0.0 {
                    return Err(());
                }
                *a.at_mut(i, j) = s.sqrt();
            } else {
                *a.at_mut(i, j) = s / a.at(j, j);
            }
        }
    }
    Ok(())
}

/// Solve A x = b for symmetric PD A, given its Cholesky factor L (lower
/// triangle of `l`). Returns x in place of b.
pub fn cholesky_solve(l: &Matrix, b: &mut [f64]) {
    let m = l.rows;
    // Forward solve L y = b.
    for i in 0..m {
        let mut s = b[i];
        for r in 0..i {
            s -= l.at(i, r) * b[r];
        }
        b[i] = s / l.at(i, i);
    }
    // Back solve Lᵀ x = y.
    for i in (0..m).rev() {
        let mut s = b[i];
        for r in (i + 1)..m {
            s -= l.at(r, i) * b[r];
        }
        b[i] = s / l.at(i, i);
    }
}

pub fn dot(a: &[f64], b: &[f64]) -> f64 {
    a.iter().zip(b).map(|(x, y)| x * y).sum()
}

pub fn nrm2(a: &[f64]) -> f64 {
    dot(a, a).sqrt()
}
