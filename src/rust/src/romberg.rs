//! Generic Romberg integration on [a, b] with Neville extrapolation.
//!
//! Ports `polintarray` / the trapezoid+extrapolation ladder used by the three
//! integrators in `density.fd.R` (normden.phi, expectden.phi, expectden.phiphit).
//! The integrand is supplied as a closure taking a slice of evaluation points
//! and returning the *sum* over those points of the per-point output vector
//! (length `out_len`). The Romberg ladder converts these trapezoid-style
//! partial sums into the converged integral.

const EPS: f64 = 1e-7;
const JMAX: usize = 15;

pub fn romberg<F>(range: (f64, f64), out_len: usize, integrand_sum: F) -> Vec<f64>
where
    F: Fn(&[f64]) -> Vec<f64>,
{
    let (a, b) = range;
    let width = b - a;

    // 1-based: slot 0 unused. Length JMAX + 2.
    let mut h = vec![1.0_f64; JMAX + 2];
    h[2] = 0.25;
    let mut smat: Vec<Vec<f64>> = (0..=JMAX + 1).map(|_| vec![0.0; out_len]).collect();

    // j = 1: trapezoid with one interval (just endpoints).
    let s1 = integrand_sum(&[a, b]);
    for k in 0..out_len {
        smat[1][k] = width * s1[k] / 2.0;
    }

    let mut tnm = 0.5_f64;
    let mut ss = smat[1].clone();

    for j in 2..=JMAX {
        tnm *= 2.0;
        let del = width / tnm;
        let pts: Vec<f64> = if j == 2 {
            vec![0.5 * (a + b)]
        } else {
            let n_pts = tnm as usize;
            (0..n_pts).map(|i| a + del * (0.5 + i as f64)).collect()
        };
        let s = integrand_sum(&pts);
        for k in 0..out_len {
            smat[j][k] = 0.5 * (smat[j - 1][k] + width * s[k] / tnm);
        }

        if j >= 5 {
            let lo = j - 4;
            let (s_new, d_new) = polint(&h[lo..=j], &smat[lo..=j]);
            let max_ss = s_new.iter().map(|v| v.abs()).fold(0.0_f64, f64::max);
            let max_ds = d_new.iter().map(|v| v.abs()).fold(0.0_f64, f64::max);
            ss = s_new;
            if max_ds < EPS * max_ss {
                return ss;
            }
        }

        smat[j + 1] = smat[j].clone();
        h[j + 1] = 0.25 * h[j];
    }

    // No convergence inside JMAX steps — return the best extrapolation we
    // computed. (Matches the warn-and-return behavior of the R code.)
    ss
}

/// Neville polynomial extrapolation of vector-valued samples to x0 = 0.
/// Returns (estimate, last correction). Operates component-wise.
fn polint(xa: &[f64], ya: &[Vec<f64>]) -> (Vec<f64>, Vec<f64>) {
    let n = xa.len();
    let m_out = ya[0].len();
    debug_assert_eq!(ya.len(), n);

    // Closest abscissa to x0 = 0.
    let mut ns: usize = 0;
    let mut best = xa[0].abs();
    for i in 1..n {
        let d = xa[i].abs();
        if d < best {
            best = d;
            ns = i;
        }
    }

    let mut c: Vec<Vec<f64>> = ya.to_vec();
    let mut d: Vec<Vec<f64>> = ya.to_vec();
    let mut y: Vec<f64> = ya[ns].clone();
    let mut dy: Vec<f64> = vec![0.0; m_out];

    // Track ns in R's 1-based convention so the index formulas match the
    // reference `polintarray()` implementation; we subtract 1 on access.
    let mut r_ns: isize = ns as isize; // R's `ns` after `ns <- ns - 1`

    for m in 1..n {
        for i in 0..(n - m) {
            let ho = xa[i];
            let hp = xa[i + m];
            let denom = ho - hp;
            for k in 0..m_out {
                let w = (c[i + 1][k] - d[i][k]) / denom;
                d[i][k] = hp * w;
                c[i][k] = ho * w;
            }
        }
        let cond = 2 * r_ns < (n as isize - m as isize);
        if cond {
            // dy = cs[ns + 1] in R's 1-based ⇒ c[r_ns] in 0-based
            dy = c[r_ns as usize].clone();
        } else {
            // dy = ds[ns] in R's 1-based ⇒ d[r_ns - 1] in 0-based
            dy = d[(r_ns - 1) as usize].clone();
            r_ns -= 1;
        }
        for k in 0..m_out {
            y[k] += dy[k];
        }
    }
    (y, dy)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integrates_polynomial_exactly() {
        // ∫_0^1 (1 + 2x + 3x^2) dx = 1 + 1 + 1 = 3
        let s = romberg((0.0, 1.0), 1, |pts| {
            let mut t = 0.0;
            for &x in pts {
                t += 1.0 + 2.0 * x + 3.0 * x * x;
            }
            vec![t]
        });
        assert!((s[0] - 3.0).abs() < 1e-10);
    }

    #[test]
    fn integrates_gaussian() {
        // ∫_-5^5 exp(-x^2/2)/sqrt(2π) dx ≈ 1 to ~1e-7 tolerance
        let s = romberg((-5.0, 5.0), 1, |pts| {
            let mut t = 0.0;
            for &x in pts {
                t += (-0.5 * x * x).exp() / (2.0 * std::f64::consts::PI).sqrt();
            }
            vec![t]
        });
        assert!((s[0] - 1.0).abs() < 1e-5);
    }
}
