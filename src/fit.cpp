#include <Rcpp.h>
#include "models.cpp"
#include "loss.cpp"
using namespace Rcpp;

// [[Rcpp::export]]
List fit_model_cpp(NumericVector x, NumericVector y, std::string model_type,
                   NumericVector start_params,
                   double a_min = 0.5, double a_max = 2.0, double b_min = 0.5, double b_max = 2.0, double step = 0.1) {
  // 1. Choose model
  Model* model = NULL;
  if (model_type == "power_law") {
    model = new PowerLawModel();
  } else {
    stop("Model type not supported");
  }
  // 2. Always use L2Loss for now
  Loss* loss = new L2Loss();

  // 3. Grid search for best parameters
  double best_loss = 1e20;
  double best_a = start_params[0], best_b = start_params[1];
  NumericVector params(2);
  for (double a = a_min; a <= a_max; a += step) {
    for (double b = b_min; b <= b_max; b += step) {
      params[0] = a; params[1] = b;
      NumericVector yhat = model->predict(x, params);
      double l = loss->compute(y, yhat);
      if (l < best_loss) {
        best_loss = l;
        best_a = a; best_b = b;
      }
    }
  }
  NumericVector best_params = NumericVector::create(best_a, best_b);
  NumericVector fitted = model->predict(x, best_params);

  delete model;
  delete loss;
  return List::create(
    Named("coefficients") = best_params,
    Named("fitted.values") = fitted,
    Named("loss") = best_loss
  );
}
