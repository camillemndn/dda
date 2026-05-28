//! Penalized log-spline density estimation by Fisher scoring with line search.
//!
//! Mirrors the structure of `density.fd` in R; differences are local:
//!   * Line search is a simple Armijo backtrack (5 tries) on the unnormalized
//!     Newton direction, instead of the quadratic-interpolating `stepit`.
//!     Fisher scoring + PD reduced Hessian make this sufficient for descent.
//!   * Cholesky solve of the (K-1)×(K-1) reduced Hessian is hand-rolled
//!     (cf. `mat::cholesky`).
//!
//! All integrals (C, E[φ], E[φφᵀ]) go through `romberg::romberg`.

use crate::basis::Basis;
use crate::mat::{
    cholesky, cholesky_solve, dot, mul_vec, nrm2, zt_h_z, zt_vec, Matrix,
};
use crate::romberg::romberg;

pub struct DensityResult {
    pub cvec: Vec<f64>,
    pub c_norm: f64,
    pub neg_log_l: f64,
    pub grad_norm: f64,
    pub iternum: usize,
    pub iterhist: Vec<[f64; 4]>, // (iter, F, -logl, grad_norm)
}

pub struct DensityOpts {
    pub conv: f64,
    pub iterlim: usize,
    /// Precomputed by R as λ · getbasispenalty(Lfd). May be empty if λ = 0.
    pub kmat: Option<Matrix>,
    /// Z ∈ ℝ^{K × (K-1)} spanning the identifiable subspace (precomputed by R
    /// as `zerobasis(nbasis)`).
    pub zmat: Matrix,
    pub climit_lo: f64,
    pub climit_hi: f64,
}

/// Per-observation contributions used by both the log-likelihood and gradient.
fn loglfnden(
    basis: &Basis,
    x: &[f64],
    f: &[f64],
    cvec: &[f64],
    cval: f64,
) -> (f64, Vec<f64>) {
    let n = x.len();
    let nb = basis.nbasis;
    let phi = basis.eval(x);
    let fsum: f64 = f.iter().sum();

    let mut logl = 0.0;
    for i in 0..n {
        let wxi = dot(phi.row(i), cvec);
        logl += wxi * f[i] - fsum * cval.ln() / n as f64;
    }

    let e_phi = integrate_expect(basis, cvec, cval);
    let mut dlogl = vec![0.0; nb];
    for i in 0..n {
        let row = phi.row(i);
        for j in 0..nb {
            dlogl[j] += (row[j] - e_phi[j]) * f[i];
        }
    }
    (logl, dlogl)
}

fn varfnden(basis: &Basis, x: &[f64], cvec: &[f64], cval: f64) -> Matrix {
    let nb = basis.nbasis;
    let e_phi = integrate_expect(basis, cvec, cval);
    let e_phi_outer = integrate_expect_outer(basis, cvec, cval);
    let nobs = x.len() as f64;
    let mut h = Matrix::zeros(nb, nb);
    for i in 0..nb {
        for j in 0..nb {
            *h.at_mut(i, j) = nobs * (e_phi_outer.at(i, j) - e_phi[i] * e_phi[j]);
        }
    }
    h
}

fn normden(basis: &Basis, cvec: &[f64]) -> f64 {
    romberg(basis.range, 1, |pts| {
        let phi = basis.eval(pts);
        let mut t = 0.0;
        for i in 0..phi.rows {
            let w = dot(phi.row(i), cvec).max(-50.0);
            t += w.exp();
        }
        vec![t]
    })[0]
}

fn integrate_expect(basis: &Basis, cvec: &[f64], cval: f64) -> Vec<f64> {
    let nb = basis.nbasis;
    romberg(basis.range, nb, |pts| {
        let phi = basis.eval(pts);
        let mut out = vec![0.0; nb];
        for i in 0..phi.rows {
            let w = dot(phi.row(i), cvec).max(-50.0);
            let px = w.exp() / cval;
            let row = phi.row(i);
            for j in 0..nb {
                out[j] += row[j] * px;
            }
        }
        out
    })
}

fn integrate_expect_outer(basis: &Basis, cvec: &[f64], cval: f64) -> Matrix {
    let nb = basis.nbasis;
    let flat = romberg(basis.range, nb * nb, |pts| {
        let phi = basis.eval(pts);
        let mut out = vec![0.0; nb * nb];
        for i in 0..phi.rows {
            let w = dot(phi.row(i), cvec).max(-50.0);
            let px = w.exp() / cval;
            let row = phi.row(i);
            for a in 0..nb {
                let ra = row[a] * px;
                for b in 0..nb {
                    out[a * nb + b] += ra * row[b];
                }
            }
        }
        out
    });
    Matrix {
        data: flat,
        rows: nb,
        cols: nb,
    }
}

/// Objective F(c) = -ℓ(c) + cᵀ K c (K already contains the λ factor).
fn objective(neg_logl: f64, cvec: &[f64], kmat: Option<&Matrix>) -> f64 {
    let mut f = neg_logl;
    if let Some(k) = kmat {
        let kc = mul_vec(k, cvec);
        f += dot(cvec, &kc);
    }
    f
}

/// Add penalty contributions 2 K c to a gradient vector in place.
fn add_penalty_grad(grad: &mut [f64], cvec: &[f64], kmat: Option<&Matrix>) {
    if let Some(k) = kmat {
        let kc = mul_vec(k, cvec);
        for i in 0..grad.len() {
            grad[i] += 2.0 * kc[i];
        }
    }
}

fn add_penalty_hessian(h: &mut Matrix, kmat: Option<&Matrix>) {
    if let Some(k) = kmat {
        for i in 0..h.rows {
            for j in 0..h.cols {
                *h.at_mut(i, j) += 2.0 * k.at(i, j);
            }
        }
    }
}

pub fn density_fd(basis: &Basis, x: &[f64], f: &[f64], cvec0: &[f64], opts: &DensityOpts) -> DensityResult {
    let nb = basis.nbasis;
    let kmat = opts.kmat.as_ref();
    let z = &opts.zmat;

    let mut cvec = cvec0.to_vec();

    let mut cval = normden(basis, &cvec);
    let (mut logl, dlogl) = loglfnden(basis, x, f, &cvec, cval);
    let mut neg_logl = -logl;
    let mut grad: Vec<f64> = dlogl.iter().map(|v| -v).collect();
    add_penalty_grad(&mut grad, &cvec, kmat);
    let mut f_old = objective(neg_logl, &cvec, kmat);

    // Reduced gradient and Newton direction.
    let mut grad_r = zt_vec(z, &grad);

    let mut hmat = varfnden(basis, x, &cvec, cval);
    add_penalty_hessian(&mut hmat, kmat);
    let mut h_r = zt_h_z(z, &hmat);

    let mut deltac = solve_newton_step(&mut h_r, &grad_r, z);

    let mut iterhist = Vec::with_capacity(opts.iterlim + 1);
    iterhist.push([0.0, f_old, neg_logl, (nrm2(&grad_r).powi(2) / grad_r.len() as f64).sqrt()]);

    let mut iternum = 0;
    let mut trial = 1.0_f64;

    for _ in 0..opts.iterlim {
        iternum += 1;

        // Armijo backtracking line search on F(c + α Δc).
        let slope0 = dot(&deltac, &grad); // directional derivative of F at α = 0.
        if slope0 >= -1e-12 {
            break;
        }

        let mut alpha = trial;
        let mut accepted = false;
        let mut cvec_new = cvec.clone();
        let mut f_new = f_old;
        let mut logl_new = logl;
        let mut grad_new = grad.clone();
        let mut cval_new = cval;

        for _bt in 0..6 {
            // Box-constraint clip on the step length.
            let mut alpha_cap = alpha;
            for i in 0..nb {
                if deltac[i] > 0.0 {
                    let cap = (opts.climit_hi - cvec[i]) / deltac[i];
                    if cap < alpha_cap {
                        alpha_cap = cap;
                    }
                } else if deltac[i] < 0.0 {
                    let cap = (opts.climit_lo - cvec[i]) / deltac[i];
                    if cap < alpha_cap {
                        alpha_cap = cap;
                    }
                }
            }
            alpha = alpha_cap.max(0.0);
            if alpha < 1e-12 {
                break;
            }

            for i in 0..nb {
                cvec_new[i] = cvec[i] + alpha * deltac[i];
            }
            cval_new = normden(basis, &cvec_new);
            let (l, dl) = loglfnden(basis, x, f, &cvec_new, cval_new);
            logl_new = l;
            f_new = objective(-l, &cvec_new, kmat);
            grad_new = dl.iter().map(|v| -v).collect();
            add_penalty_grad(&mut grad_new, &cvec_new, kmat);

            if f_new <= f_old + 1e-4 * alpha * slope0 {
                accepted = true;
                break;
            }
            alpha *= 0.5;
        }

        if !accepted {
            break;
        }

        cvec = cvec_new;
        cval = cval_new;
        logl = logl_new;
        neg_logl = -logl;
        grad = grad_new;
        let grad_r_new = zt_vec(z, &grad);

        iterhist.push([iternum as f64, f_new, neg_logl, (nrm2(&grad_r_new).powi(2) / grad_r_new.len() as f64).sqrt()]);

        let converged = (f_new - f_old).abs() < opts.conv;
        f_old = f_new;
        grad_r = grad_r_new;
        if converged {
            break;
        }

        // Refresh Hessian and Newton step.
        let mut h = varfnden(basis, x, &cvec, cval);
        add_penalty_hessian(&mut h, kmat);
        h_r = zt_h_z(z, &h);
        deltac = solve_newton_step(&mut h_r, &grad_r, z);

        // Keep next trial = 1 (Fisher step length is well scaled).
        trial = 1.0;
    }

    let grad_norm = (nrm2(&grad_r).powi(2) / grad_r.len() as f64).sqrt();
    DensityResult {
        cvec,
        c_norm: cval,
        neg_log_l: -logl,
        grad_norm,
        iternum,
        iterhist,
    }
}

/// Compute Δc = Z (Hᵣ⁻¹ (−gᵣ)) using a Cholesky factorization of Hᵣ.
/// Falls back to steepest descent on the reduced gradient if Hᵣ is not PD.
fn solve_newton_step(h_r: &mut Matrix, grad_r: &[f64], z: &Matrix) -> Vec<f64> {
    let mut rhs: Vec<f64> = grad_r.iter().map(|v| -v).collect();
    let mut chol = h_r.clone();
    match cholesky(&mut chol) {
        Ok(()) => {
            cholesky_solve(&chol, &mut rhs);
            mul_vec(z, &rhs)
        }
        Err(()) => {
            // Steepest descent in the reduced space as a safe fallback.
            mul_vec(z, &grad_r.iter().map(|v| -v).collect::<Vec<f64>>())
        }
    }
}
