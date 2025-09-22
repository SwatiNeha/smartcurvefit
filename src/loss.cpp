#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <algorithm>
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

// ---- Huber (Adaptive δ) ----
class HuberLoss : public Loss {
public:
  double compute(const NumericVector& y,
                 const NumericVector& yhat) const override {
                   const int n = y.size();
                   std::vector<double> r(n);
                   for (int i = 0; i < n; ++i) r[i] = y[i] - yhat[i];

                   // median
                   std::nth_element(r.begin(), r.begin() + n/2, r.end());
                   double med = r[n/2];

                   // absolute deviations
                   for (int i = 0; i < n; ++i) r[i] = std::fabs(r[i] - med);
                   std::nth_element(r.begin(), r.begin() + n/2, r.end());
                   double mad = r[n/2];

                   double delta = 1.345 * mad;
                   if (!R_finite(delta) || delta <= 1e-8) delta = 1.0; // fallback

                   // standard Huber loss
                   double s = 0.0;
                   for (int i = 0; i < n; ++i) {
                     double ri = y[i] - yhat[i];
                     if (std::fabs(ri) <= delta) {
                       s += 0.5 * ri * ri;  // quadratic region
                     } else {
                       s += delta * (std::fabs(ri) - 0.5 * delta);  // linear region
                     }
                   }
                   return s / n;
                 }
};
