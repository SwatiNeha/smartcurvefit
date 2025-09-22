#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// ---- Base Model ----
class Model {
public:
  virtual ~Model() {}
  virtual NumericVector predict(const NumericVector& x,
                                const NumericVector& params) const = 0;
};

// ---- Power law: y = a * x^b ----
class PowerLawModel : public Model {
public:
  NumericVector predict(const NumericVector& x,
                        const NumericVector& p) const override {
    NumericVector yhat(x.size());
    const double a = p[0], b = p[1];
    for (int i = 0; i < x.size(); ++i)
      yhat[i] = a * std::pow(x[i], b);
    return yhat;
  }
};

// ---- Exponential: y = a * exp(b * x) ----
class ExponentialModel : public Model {
public:
  NumericVector predict(const NumericVector& x,
                        const NumericVector& p) const override {
    NumericVector yhat(x.size());
    const double a = p[0], b = p[1];
    for (int i = 0; i < x.size(); ++i)
      yhat[i] = a * std::exp(b * x[i]);
    return yhat;
  }
};

// ---- Logarithmic Model ----
class LogModel : public Model {
public:
  NumericVector predict(const NumericVector& x, const NumericVector& params) const override {
    double a = params[0];
    double b = params[1];
    int n = x.size();
    NumericVector yhat(n);
    for (int i=0; i<n; ++i) {
      if (x[i] <= 0.0) {
        yhat[i] = NA_REAL; // log undefined for non-positive x
      } else {
        yhat[i] = a + b * std::log(x[i]);
      }
    }
    return yhat;
  }
};
