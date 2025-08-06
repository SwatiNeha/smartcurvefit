#include <Rcpp.h>
using namespace Rcpp;

// ---- Base Model ----
class Model {
public:
  virtual NumericVector predict(NumericVector x, NumericVector params) = 0;
  virtual int n_params() = 0;
  virtual ~Model() {}
};

// ---- Power Law Model ----
class PowerLawModel : public Model {
public:
  NumericVector predict(NumericVector x, NumericVector params) {
    double a = params[0];
    double b = params[1];
    NumericVector yhat(x.size());
    for (int i = 0; i < x.size(); ++i) {
      yhat[i] = a * pow(x[i], b);
    }
    return yhat;
  }
  int n_params() { return 2; }
};
