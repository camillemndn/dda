//! Point evaluation of the fda basis families on a row of observations.
//!
//! Only nderiv = 0 is exposed — the penalty matrix K is precomputed by R
//! before the Rust call, so the hot loop never needs derivatives.

use crate::mat::Matrix;

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum BasisKind {
    Bspline = 0,
    Fourier = 1,
    Const = 2,
    Monom = 3,
    Polygonal = 4,
    Power = 5,
    Expon = 6,
}

impl BasisKind {
    pub fn from_tag(t: u8) -> Self {
        match t {
            0 => Self::Bspline,
            1 => Self::Fourier,
            2 => Self::Const,
            3 => Self::Monom,
            4 => Self::Polygonal,
            5 => Self::Power,
            6 => Self::Expon,
            _ => panic!("unknown basis kind tag {t}"),
        }
    }
}

pub struct Basis {
    pub kind: BasisKind,
    pub nbasis: usize,
    pub range: (f64, f64),
    /// B-spline: augmented knot vector, length `nbasis + norder`.
    /// Polygonal: argvals (length `nbasis`).
    pub knots: Vec<f64>,
    /// B-spline order (= degree + 1). Unused otherwise.
    pub norder: usize,
    /// Fourier: `params[0]` is the period.
    /// Power / Expon: per-basis exponents / rates (length `nbasis`).
    pub params: Vec<f64>,
}

impl Basis {
    /// Build the design matrix Φ(x) ∈ ℝ^{n × nbasis} (row-major).
    pub fn eval(&self, x: &[f64]) -> Matrix {
        let n = x.len();
        let k = self.nbasis;
        let mut phi = Matrix::zeros(n, k);
        match self.kind {
            BasisKind::Bspline => bspline_eval(self, x, &mut phi),
            BasisKind::Fourier => fourier_eval(self, x, &mut phi),
            BasisKind::Const => {
                for i in 0..n {
                    *phi.at_mut(i, 0) = 1.0;
                }
            }
            BasisKind::Monom => {
                for i in 0..n {
                    let mut p = 1.0;
                    for j in 0..k {
                        *phi.at_mut(i, j) = p;
                        p *= x[i];
                    }
                }
            }
            BasisKind::Polygonal => polygonal_eval(self, x, &mut phi),
            BasisKind::Power => {
                for i in 0..n {
                    for j in 0..k {
                        *phi.at_mut(i, j) = x[i].powf(self.params[j]);
                    }
                }
            }
            BasisKind::Expon => {
                for i in 0..n {
                    for j in 0..k {
                        *phi.at_mut(i, j) = (self.params[j] * x[i]).exp();
                    }
                }
            }
        }
        phi
    }
}

/// Cox–de Boor recursion. `b.knots` is the full augmented knot vector
/// t[0..nbasis+norder] (fda's convention from `create.bspline.basis()`).
///
/// At x in span [t[l], t[l+1]) with k-1 ≤ l ≤ n-1, exactly `norder` basis
/// functions B_{l-k+1, k}, ..., B_{l, k} are nonzero.
fn bspline_eval(b: &Basis, x: &[f64], out: &mut Matrix) {
    let k = b.norder;
    let n = b.nbasis;
    let t = &b.knots;
    debug_assert_eq!(t.len(), n + k);

    // Working buffer holds the k nonzero basis values, plus a sentinel at
    // index k used as a right-edge zero during the recurrence.
    let mut work = vec![0.0_f64; k + 1];

    for (i, &xi_raw) in x.iter().enumerate() {
        let xi = xi_raw.clamp(b.range.0, b.range.1);

        // Find span l with t[l] ≤ xi < t[l+1], constrained to k-1 ≤ l ≤ n-1.
        let mut l = k - 1;
        while l < n - 1 && t[l + 1] <= xi {
            l += 1;
        }

        // Reset working buffer; degree 0 has B_l = 1 at slot k-1.
        for v in work.iter_mut() {
            *v = 0.0;
        }
        work[k - 1] = 1.0;

        // Slot `pos` represents basis index l - k + 1 + pos.
        // Iterate degrees d = 1..k-1; after step d, slots [k-1-d ..= k-1]
        // hold the d+1 nonzero basis values at degree d.
        for d in 1..k {
            for pos in (k - 1 - d)..=(k - 1) {
                let i_prime = l + 1 + pos - k; // basis index B_{i', d+1}
                let t_i = t[i_prime];
                let t_i_d = t[i_prime + d];
                let t_i_1 = t[i_prime + 1];
                let t_i_d1 = t[i_prime + d + 1];

                let left = if t_i_d > t_i {
                    (xi - t_i) / (t_i_d - t_i) * work[pos]
                } else {
                    0.0
                };
                let right = if t_i_d1 > t_i_1 {
                    (t_i_d1 - xi) / (t_i_d1 - t_i_1) * work[pos + 1]
                } else {
                    0.0
                };
                work[pos] = left + right;
            }
        }

        // Write into row i, columns l-k+1 ..= l.
        let col0 = l + 1 - k;
        for r in 0..k {
            *out.at_mut(i, col0 + r) = work[r];
        }
    }
}

/// fda Fourier basis on [a, b] with period P = params[0]. nbasis must be odd:
///     φ_1(x) = 1/√P
///     φ_{2m}(x)   = √(2/P) sin(2π m (x − a)/P)
///     φ_{2m+1}(x) = √(2/P) cos(2π m (x − a)/P)
fn fourier_eval(b: &Basis, x: &[f64], out: &mut Matrix) {
    let period = b.params[0];
    let a = b.range.0;
    let omega = 2.0 * std::f64::consts::PI / period;
    let c0 = 1.0 / period.sqrt();
    let cm = (2.0 / period).sqrt();
    let n_pairs = (b.nbasis - 1) / 2;
    for (i, &xi) in x.iter().enumerate() {
        *out.at_mut(i, 0) = c0;
        let theta = omega * (xi - a);
        for m in 1..=n_pairs {
            let mt = (m as f64) * theta;
            *out.at_mut(i, 2 * m - 1) = cm * mt.sin();
            *out.at_mut(i, 2 * m) = cm * mt.cos();
        }
    }
}

/// Tent functions at the argvals stored in `b.knots` (length `nbasis`).
fn polygonal_eval(b: &Basis, x: &[f64], out: &mut Matrix) {
    let nodes = &b.knots;
    let k = b.nbasis;
    for (i, &xi_raw) in x.iter().enumerate() {
        let xi = xi_raw.clamp(b.range.0, b.range.1);
        if xi <= nodes[0] {
            *out.at_mut(i, 0) = 1.0;
            continue;
        }
        if xi >= nodes[k - 1] {
            *out.at_mut(i, k - 1) = 1.0;
            continue;
        }
        let mut j = 0;
        while j + 1 < k && nodes[j + 1] < xi {
            j += 1;
        }
        let w = (xi - nodes[j]) / (nodes[j + 1] - nodes[j]);
        *out.at_mut(i, j) = 1.0 - w;
        *out.at_mut(i, j + 1) = w;
    }
}
