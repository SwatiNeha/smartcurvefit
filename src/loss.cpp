#include <Rcpp.h>
using namespace Rcpp;

// ---- Base Loss ----
class Loss {
public:
  virtual double compute(NumericVector y, NumericVector yhat) = 0;
  virtual ~Loss() {}
};

// ---- L2 Loss ----
class L2Loss : public Loss {
public:
  double compute(NumericVector y, NumericVector yhat) {
    double sum = 0.0;
    for (int i = 0; i < y.size(); ++i) {
      double diff = y[i] - yhat[i];
      sum += diff * diff;
    }
    return sum / y.size();
  }
};
