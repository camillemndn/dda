use argmin::{
    core::{observers::ObserverMode, Executor, Gradient, Hessian, State},
    solver::newton::Newton,
};
use argmin_observer_slog::SlogLogger;
use argmin_testfunctions::{rosenbrock_derivative, rosenbrock_hessian};
use extendr_api::prelude::*;
use ndarray::{Array, Array1, Array2};

struct Rosenbrock {}

impl Gradient for Rosenbrock {
    type Param = Array1<f64>;
    type Gradient = Array1<f64>;

    fn gradient(
        &self,
        p: &Self::Param,
    ) -> std::result::Result<Self::Gradient, argmin::core::Error> {
        Ok(Array1::from(rosenbrock_derivative(p.as_slice().unwrap())))
    }
}

impl Hessian for Rosenbrock {
    type Param = Array1<f64>;
    type Hessian = Array2<f64>;

    fn hessian(&self, p: &Self::Param) -> std::result::Result<Self::Hessian, argmin::core::Error> {
        let h = rosenbrock_hessian(p.as_slice().unwrap())
            .into_iter()
            .flatten()
            .collect();
        Ok(Array::from_shape_vec((p.len(), p.len()), h)?)
    }
}

/// Return string `"Hello world!"` to R.
/// @export
#[extendr]
fn density_mpl() -> Robj {
    // Define cost function
    let cost = Rosenbrock {};

    // Define initial parameter vector
    let init_param: Array1<f64> = Array1::from(vec![1.2, 1.2]);

    // Set up solver
    let solver: Newton<f64> = Newton::new();

    // Run solver
    let res = Executor::new(cost, solver)
        .configure(|state| state.param(init_param).max_iters(10))
        .add_observer(SlogLogger::term(), ObserverMode::Always)
        .run();

    // Print result
    res.unwrap()
        .state()
        .get_best_param()
        .unwrap()
        .clone()
        .try_into()
        .unwrap()
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod dda;
    fn density_mpl;
}
