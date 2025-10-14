# smartcurvefit

An R package for **robust nonlinear curve fitting** with **cross-validated loss selection**.  
It supports **power-law**, **exponential**, and **logarithmic** regression models, implemented with an efficient **C++ backend** and an intuitive **S3 interface** (`print`, `summary`, `plot`, `predict`).  

The package automatically **estimates model parameters** (coefficients `a` and `b`) from the data, selects the best loss function (L1, L2, or Huber) via cross-validation, and provides fitted values, residuals, and diagnostics.

---

## 📦 Package Components

### Core Structure
- **DESCRIPTION / NAMESPACE**: Standard package metadata, dependencies, and exported functions.
- **R/**: Contains R interface functions and S3 methods.
- **src/**: Contains C++ backend (`fit.cpp`, `loss.cpp`, `models.cpp`) compiled via Rcpp.
- **tests/testthat/**: Unit tests for all major functions.
- **vignettes/**: Example usage and design notes (detailed vignette under construction).

### R Files
- **fit_model.R**  
  - Main entry point: `fit_model()`  
  - Performs input validation, calls the C++ backend, and constructs the `"smartFit"` S3 object.  
  - Exports S3 methods (OOP):
    - `print.smartFit`: concise textual summary.  
    - `summary.smartFit`: extended diagnostics and CV results.  
    - `plot.smartFit`: base R plotting of fitted curve + optional residuals.  
    - `predict.smartFit`: predictions for new data.  

- **utils-check.R**  
  - Internal helper functions for input validation:  
    - `.check_input_size_fit()`: warns if input vectors are too large for CV.  
    - `.check_input_size_predict()`: warns if prediction dataset is very large.  

### C++ Files (src/)
- **fit.cpp**  
  - Implements `fit_model_cv_cpp()` (exported to R).  
  - Orchestrates:
    - Starting parameter estimation for each model.  
    - Fitting under multiple loss functions (L1, L2, Huber).  
    - k-fold cross-validation (default k=5).  
    - Selection of the best loss function based on RMSE/MAE.  
    - Returning coefficients, fitted values, residuals, CV diagnostics.  

- **loss.cpp**  
  - Implements an **OOP hierarchy for loss functions**:  
    - `Loss` (abstract base class).  
    - `L2Loss` → Mean Squared Error (inherits from `Loss`).  
    - `L1Loss` → Mean Absolute Error (inherits from `Loss`).  
    - `HuberLoss` → Adaptive robust loss (inherits from `Loss`, uses median/MAD-based δ).  
  - Each class overrides a common interface (e.g., `compute_loss()`, `gradient()`),  
    allowing polymorphic behavior when swapping loss functions.

- **models.cpp**  
  - Implements an **OOP hierarchy for regression models**:  
    - `Model` (abstract base class).  
    - `PowerLawModel` → \(y = a \cdot x^b\) (inherits from `Model`).  
    - `ExponentialModel` → \(y = a \cdot e^{bx}\) (inherits from `Model`).  
    - `LogModel` → \(y = a + b \cdot \log(x)\) (inherits from `Model`).  
  - Each model subclass defines its own `predict()` and `fit()` methods,  
    while sharing the abstract interface from the base class.  


### Testing (tests/testthat/)
- **test-fit_model.R**  
  - Comprehensive test suite covering both model fitting and S3 methods.  
  - **Model fitting tests**:  
    - Correct behavior of `fit_model()` under *power-law*, *exponential*, and *logarithmic* models.  
    - Validation of cross-validation loss selection (L1, L2, Huber).  
    - Edge cases: non-numeric input, length mismatch, NA/NaN/Inf values, constant vectors, and too few points.  
  - **S3 method tests**:  
    - `print.smartFit` and `summary.smartFit`: check structure, output, and unused argument warnings.  
    - `plot.smartFit`: verifies fitted curves and residual plots; ensures warnings for invalid graphical args.  
    - `predict.smartFit`: tests predictions on training data and new data, and error handling for invalid inputs (non-numeric, empty, NA/NaN/Inf, domain violations).  

### Vignettes -- ***Under Development***
- **Generic vignette available** (shows example fits, summary, and plots).  
- **Detailed vignettes are under construction**, to cover:
  - Model theory and derivations.  
  - Cross-validation strategy.  
  - Comparing robust loss functions (L1/L2/Huber).  
  - Extended examples with real-world data.

---

## 🚀 Installation

Make sure you have R (≥ 4.0) and the **devtools/ remotes** package:

- Quick Installation with vignettes:
```r
install.packages("devtools")
devtools::install_github("SwatiNeha/smartcurvefit", build_vignettes = TRUE)
```
or

```r
install.packages("remotes")
remotes::install_github("SwatiNeha/smartcurvefit", build_vignettes = TRUE)
```

- If you want to install testthat files also:

```r
remotes::install_github(
  "SwatiNeha/smartcurvefit",
  build_vignettes = TRUE,
  INSTALL_opts = c("--install-tests")
)

library(testthat)
test_package("smartcurvefit")
```

Then load:

```r
library(smartcurvefit)
```
---


## 📂Sample Dataset

The package demonstrates usage with a **sample dataset** that can be easily generated in R:

- Power Law
```r
set.seed(42)
x <- 1:30
y <- 2 * x^1.2 + rnorm(length(x))

fit <- fit_model(x, y, "power_law")
print(fit)
summary(fit)
plot(fit, show_residuals = TRUE)
predict(fit, newdata = c(5, 10, 20))
```
- Exponential

```r
set.seed(789)
x <- 1:30
y <- 3 * exp(0.2 * x) + rnorm(length(x), sd = 10)
y[c(7, 15, 23)] <- y[c(7, 15, 23)] + 50  
fit_exp <- fit_model(x, y, model_type = "exponential", metric = "rmse")
print(fit_exp)
```

- Logarithmic

```r
set.seed(101)
x <- 1:50
y <- 3 + 2*log(x) + rnorm(50, sd=0.3)  # Gaussian noise, symmetric
fit_log <- fit_model(x, y, model_type="logarithmic", metric="rmse")
print(fit_log)
```

These example acts as the default sample data for illustrating package functionality.

---

## 🔧 Usage

### Fit a model

```r
set.seed(42)
x <- 1:30
y <- 2 * x^1.2 + rnorm(length(x))

fit <- fit_model(x, y, model_type = "power_law")
print(fit)
summary(fit)
plot(fit, show_residuals = TRUE)
```

This will fit a **power-law model** of the form:  y≈a⋅x^b

with estimated coefficients (example run) and plot of the fitted curve over the data:

```
smartCurveFit object
  Model type   : power_law
  Chosen loss  : L2 (selection by CV-RMSE, score = 3.91)
  Coefficients : a = 1.95, b = 1.19
  Formula      : y ~ 1.953 * x^1.189
  RMSE = 3.91 | MAE = 3.12 | MSE = 15.3
```

---

### Predict on new data

```r
predict(fit, newdata = c(5, 10, 20))
```

Example output:

```
       5       10       20 
  11.426   22.431   46.128 
```

So for input values `x = 5, 10, 20`, the fitted model predicts `y ≈ 11.4, 22.4, 46.1`.

This will be the case with all the three laws: power_law, exponential and logarithmic.

## 📘 Documentation

- Function manuals: `?fit_model`, `?plot.smartFit`, `?print.smartfit`, `?summary.smartFit`, `?predict.smartFit`, .  
- Vignettes: available via  

```r
browseVignettes("smartcurvefit")
```
---

## 🧪 Test Plan

The **testing strategy** for all exported and internal functions in the `smartcurvefit` package are given below  
All tests are written using the **testthat** framework.  
The goal is to ensure **correctness, robustness, reproducibility, and clarity of error handling**.

## 📌 Core Principles

- **Required arguments** must be validated and covered in unit tests.  
- **Optional arguments** must be tested for defaults, warnings, and ignored behavior.  
- **Edge cases** must trigger clear, informative errors or warnings.  
- **Return objects** must always match the documented class/structure.  
- **Consistency** across functions is essential (S3 generics must behave as expected).


## 🔧 Function-by-Function Test Plan

### 1. `fit_model(x, y, model_type = NULL, metric = "rmse", ...)`

**Arguments**
- `x`, `y` → required; numeric vectors of equal length, ≥ 3 points.  
- `model_type` → optional; one of `"power_law"`, `"exponential"`, `"logarithmic"` (default `"power_law"`).  
- `metric` → optional; `"rmse"` (default) or `"mae"`.  
- `...` → unused; might trigger warning.  

**Tests**
-  Successful fits for all three models.  
-  Default to `"power_law"` when `model_type = NULL`.  
-  Errors for:
  - Non-numeric `x` or `y`.  
  - Mismatched lengths.  
  - Fewer than 3 points.  
  - NA/NaN/Inf values.  
  - Unsupported `model_type`.  
-  Warnings for:
  - `x < 1` in `"power_law"`.  
  - Extremely large/small magnitudes (overflow/underflow).  
  - Large dataset (via `.check_input_size_fit()`).  
  - Unused arguments.  
- Return object with required fields.
  - Confirms returned object is of class `smartFit`.
  - Confirms contents: coefficients, fitted values, residuals, CV results, all-results.


### 2. `print.smartFit(x, ...)`

**Arguments**
- `x` → required; must be `"smartFit"`.  
- `...` → unused; may trigger warning.  

**Tests**
- Displays model type, loss, coefficients, metrics, and compact CV results.  
- Errors if object is not `"smartFit"` or missing fields.  
- Warnings for unused arguments.  
- Output includes model formula (`y ~ a * x^b`, etc.).


### 3. `summary.smartFit(object, ...)`

**Arguments**
- `object` → required; `"smartFit"`.  
- `...` → unused; may trigger warning.  

**Tests**
- Shows extended diagnostics: coefficients, fit metrics, residual summary, CV results, all-loss fits.  
- Errors for invalid object or missing fields.  
- Warnings for unused arguments.  


### 4. `plot.smartFit(x, y, ..., show_residuals = FALSE, col_points = "steelblue", pch_points = 19, col_line = "red", lwd_line = 2)`

**Arguments**
- `x` → required; `"smartFit"`.  
- `y` → ignored.  
- `show_residuals` → optional; toggles residual plot.  
- Graphical args (`col_points`, `pch_points`, etc.).  
- `...` → optional; invalid args trigger warning.  

**Tests**
- Scatter plot + fitted curve.  
- Residual plot when `show_residuals = TRUE`.  
- Custom styling applied.  
- Errors for invalid `"smartFit"` object.  
- Warnings for invalid graphical args.  

---

### 5. `predict.smartFit(object, newdata = NULL, ...)`

**Arguments**
- `object` → required; `"smartFit"`.  
- `newdata` → optional; numeric vector, defaults to training `x`.  
- `...` → unused; may trigger warning.  

**Tests**
- Predictions consistent with model formulas.  
- Works on training data and newdata.  
- Errors for:
  - Invalid `"smartFit"` object.  
  - Non-numeric, empty, NA/NaN, Inf `newdata`.  
  - Domain errors (`x <= 0` for power-law/logarithmic).  
- Warnings for:
  - Very large `newdata` (via `.check_input_size_predict()`).  
  - Unused arguments.  


### 6. `.check_input_size_fit(x, y, n_threshold = 5000)` *(internal)*

**Tests**
- Warns if dataset exceeds threshold.  
- Silent otherwise.  
- Indirectly covered through `fit_model()`.  

---

### 7. `.check_input_size_predict(newdata, n_threshold = 2e8)` *(internal)*

**Tests**
- Warns if newdata exceeds threshold.  
- Silent otherwise.  
- Indirectly covered through `predict.smartFit()`.  

---

## ⚡ Edge Cases (Cross-Function)

- Extremely large (`1e120`) or small (`1e-120`) numbers.  
- Constant vectors (`sd(x) = 0` or `sd(y) = 0`).  
- High-precision decimals.  
- Missing `model_type` → defaults to `"power_law"`.  
- Incorrect `"smartFit"` structure (missing fields).  
- Tie-breaking in CV loss selection (priority: L2 → Huber → L1).  


## Expected Coverage

- **Unit tests**: each function tested independently.  
- **Integration tests**: full pipeline (fit → print → summary → plot → predict).  
- **Validation tests**: input checking and error/warning handling.  
- **Edge tests**: large datasets, boundary cases, malformed inputs.  


## 📋 Test Checklist (at a glance)

| Function              | Valid Inputs | Invalid Inputs | Warnings | Edge Cases | Output Structure |
|-----------------------|--------------|----------------|----------|------------|------------------|
| `fit_model()`         | ✅            | ❌              | ⚠️        | ✅          | `"smartFit"` obj |
| `print.smartFit()`    | ✅            | ❌              | ⚠️        | –          | Console text     |
| `summary.smartFit()`  | ✅            | ❌              | ⚠️        | –          | Console text     |
| `plot.smartFit()`     | ✅            | ❌              | ⚠️        | ✅          | Base R plot      |
| `predict.smartFit()`  | ✅            | ❌              | ⚠️        | ✅          | Numeric vector   |
| `.check_input_size_*` | ✅            | –              | ⚠️        | ✅          | Invisible TRUE   |


### Example: Correct Input

```r
fit <- fit_model(1:20, 3 + 2*log(1:20) + rnorm(20), "logarithmic")
summary(fit)
plot(fit, show_residuals = TRUE)
```

### Example: Incorrect Inputs

```r
fit_model(c(1,2,3), c(2,3), "power_law")      # mismatched lengths
fit_model(c(-1,0,2), c(1,2,3), "power_law")  # invalid x values
fit_model(1:10, rep(5,10), "exponential")    # constant y
```

---

The test suite covers the **full lifecycle** of the package — from fitting and validation, through S3 method behavior, to edge cases and utility checks.


## 👩‍🔬 Development Notes

- The C++ backend uses **OOP design** (`Model` + `Loss` classes).  
- No Placeholder functions. 

