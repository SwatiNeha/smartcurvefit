#include <Rcpp.h>
using namespace Rcpp;

// ---- Base Loss ----
class Loss {
public:
  virtual ~Loss() {}
  virtual double compute(const NumericVector& y,
                         const NumericVector& yhat) const = 0;
};

// ---- L2 (MSE) ----
class L2Loss : public Loss {
public:
  double compute(const NumericVector& y,
                 const NumericVector& yhat) const override {
    const int n = y.size();
    double s = 0.0;
    for (int i = 0; i < n; ++i) {
      const double r = y[i] - yhat[i];
      s += r * r;
    }
    return s / n; // MSE
  }
};

// ---- L1 (MAE) ----
class L1Loss : public Loss {
public:
  double compute(const NumericVector& y,
                 const NumericVector& yhat) const override {
    const int n = y.size();
    double s = 0.0;
    for (int i = 0; i < n; ++i) {
      s += std::fabs(y[i] - yhat[i]);
    }
    return s / n; // MAE
  }
};
