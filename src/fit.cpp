// fit.cpp
#include <Rcpp.h>
#include <cmath>
#include <string>
#include <memory>
#include <vector>
#include <limits>
#include <algorithm>
#include <numeric>
#include "models.cpp"
#include "loss.cpp"

using namespace Rcpp;

// metric helper functions
static inline double mse_of(const NumericVector& y, const NumericVector& yhat) {
  const int n = y.size();
  double s = 0.0;
  for (int i = 0; i < n; ++i) {
    const double r = y[i] - yhat[i];
    s += r * r;
  }
  return s / n;
}
static inline double rmse_of(const NumericVector& y, const NumericVector& yhat) {
  return std::sqrt(mse_of(y, yhat));
}
static inline double mae_of(const NumericVector& y, const NumericVector& yhat) {
  const int n = y.size();
  double s = 0.0;
  for (int i = 0; i < n; ++i) s += std::fabs(y[i] - yhat[i]);
  return s / n;
}

// starting guess for power law using MLE of residuals and solving
static NumericVector start_guess_power(const NumericVector& x, const NumericVector& y) {
  int n = x.size();
  for (int i=0;i<n;++i) if (x[i] <= 0.0 || y[i] <= 0.0) return NumericVector::create(1.0, 1.0);
  if (n < 2) return NumericVector::create(1.0, 1.0);

  double Sx=0, Sy=0, Sxx=0, Sxy=0;
  for (int i=0;i<n;++i) {
    double lx = std::log(x[i]);
    double ly = std::log(y[i]);
    Sx += lx; Sy += ly; Sxx += lx*lx; Sxy += lx*ly;
  }
  double denom = n*Sxx - Sx*Sx;
  if (std::fabs(denom) < 1e-12) return NumericVector::create(1.0, 1.0);

  double b = (n*Sxy - Sx*Sy) / denom;
  double a = std::exp((Sy - b*Sx)/n);
  return NumericVector::create(a, b);
}

// starting guess for exponential using MLE of residuals and solving
static NumericVector start_guess_exp(const NumericVector& x, const NumericVector& y) {
  int n = x.size();
  for (int i=0;i<n;++i) if (y[i] <= 0.0) return NumericVector::create(1.0, 0.1);
  if (n < 2) return NumericVector::create(1.0, 0.1);

  double Sx=0, Sy=0, Sxx=0, Sxy=0;
  for (int i=0;i<n;++i) {
    double lx = x[i];
    double ly = std::log(y[i]);
    Sx += lx; Sy += ly; Sxx += lx*lx; Sxy += lx*ly;
  }
  double denom = n*Sxx - Sx*Sx;
  if (std::fabs(denom) < 1e-12) return NumericVector::create(1.0, 0.1);

  double b = (n*Sxy - Sx*Sy) / denom;
  double a = std::exp((Sy - b*Sx)/n);
  return NumericVector::create(a, b);
}

// starting guess for logarithmic  using MLE of residuals and solving
static NumericVector start_guess_log(const NumericVector& x, const NumericVector& y) {
  int n = x.size();
  for (int i=0;i<n;++i) if (x[i] <= 0.0) return NumericVector::create(0.0, 1.0);
  if (n < 2) return NumericVector::create(0.0, 1.0);

  double Sx=0, Sy=0, Sxx=0, Sxy=0;
  for (int i=0;i<n;++i) {
    double lx = std::log(x[i]);
    double ly = y[i];
    Sx += lx; Sy += ly; Sxx += lx*lx; Sxy += lx*ly;
  }
  double denom = n*Sxx - Sx*Sx;
  if (std::fabs(denom) < 1e-12) return NumericVector::create(0.0, 1.0);

  double b = (n*Sxy - Sx*Sy) / denom;
  double a = (Sy - b*Sx) / n;
  return NumericVector::create(a, b);
}

//  local structure for storing the fit
struct FitResult {
  std::string loss_name;
  double a, b;
  NumericVector fitted;
  double mse, rmse, mae;
};

// function to run losses
static FitResult fit_one_loss_local(const NumericVector& x, const NumericVector& y,
                                    const std::string& model_name,
                                    const std::string& loss_name,
                                    const NumericVector& start_params,
                                    bool enforce_a_positive = true) {
  std::unique_ptr<Model> model;
  if (model_name == "power_law")        model.reset(new PowerLawModel());
  else if (model_name == "exponential") model.reset(new ExponentialModel());
  else if (model_name == "logarithmic") model.reset(new LogModel());
  else Rcpp::stop("Error: Unknown model_name.");

  std::unique_ptr<Loss> loss;
  if      (loss_name == "L2")    loss.reset(new L2Loss());
  else if (loss_name == "L1")    loss.reset(new L1Loss());
  else if (loss_name == "Huber") loss.reset(new HuberLoss());
  else Rcpp::stop("Error: Unknown loss_name.");

  const double a0 = start_params[0];
  const double b0 = start_params[1];

// window size and step size to search for parameters range

  double widen_factor = (loss_name == "L1") ? 1.5 : 1.0;
  double wa = widen_factor * std::max(0.5 * std::fabs(a0), 0.25);
  double wb = widen_factor * std::max(0.5 * std::fabs(b0), 0.25);
  double step = std::max(wa, wb) / 20.0;
  int refine_iters = 2;

  // lambda function - inline
  auto eval_grid = [&](double a_min, double a_max, double b_min, double b_max,
                       double stp, double &best_a, double &best_b, double &best_obj) {
    NumericVector params(2), yhat;
    const double eps = 1e-12;
    for (double a = a_min; a <= a_max + eps; a += stp) {
      double a_test = enforce_a_positive ? std::max(a, 1e-12) : a;
      params[0] = a_test;
      for (double b = b_min; b <= b_max + eps; b += stp) {
        params[1] = b;
        yhat = model->predict(x, params);
        double obj = loss->compute(y, yhat);
        if (obj < best_obj) {
          best_obj = obj; best_a = a_test; best_b = b;
        }
      }
    }
  };

  double a_min = a0 - wa, a_max = a0 + wa;
  double b_min = b0 - wb, b_max = b0 + wb;
  if (enforce_a_positive && model_name == "power_law") a_min = std::max(a_min, 1e-12);

  double best_a = enforce_a_positive && model_name == "power_law" ? std::max(a0, 1e-12) : a0;
  double best_b = b0;
  NumericVector yhat0 = model->predict(x, NumericVector::create(best_a, best_b));
  double best_obj = loss->compute(y, yhat0);

  eval_grid(a_min, a_max, b_min, b_max, step, best_a, best_b, best_obj);
  for (int it=0; it<refine_iters; ++it) {
    step *= 0.5; wa *= 0.5; wb *= 0.5;
    double na_min = best_a - wa, na_max = best_a + wa;
    double nb_min = best_b - wb, nb_max = best_b + wb;
    if (enforce_a_positive && model_name == "power_law") na_min = std::max(na_min, 1e-12);
    eval_grid(na_min, na_max, nb_min, nb_max, step, best_a, best_b, best_obj);
  }

  NumericVector best_params = NumericVector::create(best_a, best_b);
  NumericVector fitted      = model->predict(x, best_params);
  return FitResult{loss_name, best_a, best_b, fitted,
                   mse_of(y, fitted), rmse_of(y, fitted), mae_of(y, fitted)};
}

// build deterministic k folds
static std::vector<int> make_fold_ids(int n, int k) {
  std::vector<int> fid(n);
  for (int i=0;i<n;++i) fid[i] = i % k;
  return fid;
}

// [[Rcpp::export]]
List fit_model_cv_cpp(NumericVector x, NumericVector y,
                      std::string model_type,
                      std::string select_metric) {
  int n = x.size();
  if (n != y.size()) Rcpp::stop("Error: x and y must have the same length.");
  if (n < 3)         Rcpp::stop("Error: Need at least three points for CV.");
  for (int i=0;i<n;++i)
    if (!R_finite(x[i]) || !R_finite(y[i])) Rcpp::stop("Error: Non-finite values detected.");

  if (model_type == "" || model_type == "auto") model_type = "power_law";
  if (model_type != "power_law" && model_type != "exponential" && model_type != "logarithmic")
    Rcpp::stop("Error: Unsupported model type.");
  if ((model_type == "power_law" || model_type == "logarithmic")) {
    for (int i=0;i<n;++i) if (x[i] <= 0.0)
      Rcpp::stop("Error: For power_law/logarithmic, all x must be > 0.");
    }

    // metric conversion
    std::string metric = select_metric;
    std::transform(metric.begin(), metric.end(), metric.begin(), ::tolower);
    if (metric != "rmse" && metric != "mae")
      Rcpp::stop("Error: metric must be 'rmse' or 'mae'.");

    int k = std::min(5, n); if (k < 2) k = 2;
    std::vector<int> fid = make_fold_ids(n, k);

    std::vector<double> rmse_L1(k), mae_L1(k),
    rmse_L2(k), mae_L2(k),
    rmse_H(k),  mae_H(k);

    // training validation split
    for (int f=0; f<k; ++f) {
      std::vector<int> trn_idx, val_idx;
      for (int i=0;i<n;++i) {
        if (fid[i] == f) val_idx.push_back(i);
        else trn_idx.push_back(i);
      }
      NumericVector xtr(trn_idx.size()), ytr(trn_idx.size());
      NumericVector xva(val_idx.size()), yva(val_idx.size());
      for (size_t j=0;j<trn_idx.size();++j){ xtr[j]=x[trn_idx[j]]; ytr[j]=y[trn_idx[j]]; }
      for (size_t j=0;j<val_idx.size();++j){ xva[j]=x[val_idx[j]]; yva[j]=y[val_idx[j]]; }

      NumericVector start = (model_type == "power_law")     ? start_guess_power(xtr, ytr) :
        (model_type == "exponential")   ? start_guess_exp(xtr, ytr) :
        start_guess_log(xtr, ytr);

      bool enforce_pos = (model_type == "power_law");
      FitResult trn_L1 = fit_one_loss_local(xtr, ytr, model_type, "L1", start, enforce_pos);
      FitResult trn_L2 = fit_one_loss_local(xtr, ytr, model_type, "L2", start, enforce_pos);
      FitResult trn_H  = fit_one_loss_local(xtr, ytr, model_type, "Huber", start, enforce_pos);

      std::unique_ptr<Model> model;
      if (model_type == "power_law") model.reset(new PowerLawModel());
      else if (model_type == "exponential") model.reset(new ExponentialModel());
      else model.reset(new LogModel());

      NumericVector yhat1 = model->predict(xva, NumericVector::create(trn_L1.a, trn_L1.b));
      NumericVector yhat2 = model->predict(xva, NumericVector::create(trn_L2.a, trn_L2.b));
      NumericVector yhatH = model->predict(xva, NumericVector::create(trn_H.a, trn_H.b));

      rmse_L1[f] = rmse_of(yva, yhat1); mae_L1[f] = mae_of(yva, yhat1);
      rmse_L2[f] = rmse_of(yva, yhat2); mae_L2[f] = mae_of(yva, yhat2);
      rmse_H[f]  = rmse_of(yva, yhatH); mae_H[f]  = mae_of(yva, yhatH);
    }

    auto mean_of = [](const std::vector<double>& v){ double s=0; for(double z: v) s+=z; return s/v.size(); };
    auto sd_of   = [](const std::vector<double>& v){ double m=0; for(double z: v) m+=z; m/=v.size(); double s=0; for(double z: v){ double d=z-m; s+=d*d;} return std::sqrt(s/(v.size()>1? v.size()-1 : 1)); };

    double m_rmse_L1 = mean_of(rmse_L1), m_rmse_L2 = mean_of(rmse_L2), m_rmse_H = mean_of(rmse_H);
    double m_mae_L1  = mean_of(mae_L1),  m_mae_L2  = mean_of(mae_L2),  m_mae_H  = mean_of(mae_H);
    double s_rmse_L1 = sd_of(rmse_L1),   s_rmse_L2 = sd_of(rmse_L2),   s_rmse_H = sd_of(rmse_H);
    double s_mae_L1  = sd_of(mae_L1),    s_mae_L2 = sd_of(mae_L2),     s_mae_H  = sd_of(mae_H);

    std::string winner;
    double tol = 1e-8;  // tolerance for near-equality

    auto is_close = [&](double a, double b) {
      return std::fabs(a - b) < tol;
    };

    if (metric == "rmse") {
      double best = std::min({m_rmse_L1, m_rmse_L2, m_rmse_H});

      if (is_close(m_rmse_L2, best)) {
        winner = "L2";  // prefer L2 if it's tied for best
      } else if (is_close(m_rmse_H, best)) {
        winner = "Huber";
      } else {
        winner = "L1";
      }

    } else { // metric == "mae"
      double best = std::min({m_mae_L1, m_mae_L2, m_mae_H});

      if (is_close(m_mae_L2, best)) {
        winner = "L2";  // prefer L2 if it's tied for best
      } else if (is_close(m_mae_H, best)) {
        winner = "Huber";
      } else {
        winner = "L1";
      }
    }


    double cv_score = (metric == "rmse") ?
    ((winner=="L1")? m_rmse_L1 : (winner=="L2")? m_rmse_L2 : m_rmse_H) :
      ((winner=="L1")? m_mae_L1  : (winner=="L2")? m_mae_L2  : m_mae_H);

    NumericVector start_full = (model_type == "power_law")     ? start_guess_power(x, y) :
      (model_type == "exponential")   ? start_guess_exp(x, y) :
      start_guess_log(x, y);
    bool enforce_pos_full = (model_type == "power_law");

    FitResult full_L1 = fit_one_loss_local(x, y, model_type, "L1", start_full, enforce_pos_full);
    FitResult full_L2 = fit_one_loss_local(x, y, model_type, "L2", start_full, enforce_pos_full);
    FitResult full_H  = fit_one_loss_local(x, y, model_type, "Huber", start_full, enforce_pos_full);

    FitResult full_win = (winner=="L1")? full_L1 : (winner=="L2")? full_L2 : full_H;

    DataFrame cvdf = DataFrame::create(
      Named("loss")     = CharacterVector::create("L1","L2","Huber"),
      Named("mean_rmse")= NumericVector::create(m_rmse_L1, m_rmse_L2, m_rmse_H),
      Named("sd_rmse")  = NumericVector::create(s_rmse_L1, s_rmse_L2, s_rmse_H),
      Named("mean_mae") = NumericVector::create(m_mae_L1,  m_mae_L2,  m_mae_H),
      Named("sd_mae")   = NumericVector::create(s_mae_L1,  s_mae_L2,  s_mae_H)
    );

    DataFrame fulldf = DataFrame::create(
      Named("loss") = CharacterVector::create("L1","L2","Huber"),
      Named("a")    = NumericVector::create(full_L1.a, full_L2.a, full_H.a),
      Named("b")    = NumericVector::create(full_L1.b, full_L2.b, full_H.b),
      Named("mse")  = NumericVector::create(full_L1.mse, full_L2.mse, full_H.mse),
      Named("rmse") = NumericVector::create(full_L1.rmse, full_L2.rmse, full_H.rmse),
      Named("mae")  = NumericVector::create(full_L1.mae, full_L2.mae, full_H.mae)
    );

    return List::create(
      Named("model")            = model_type,
      Named("chosen_loss")      = winner,
      Named("coefficients")     = NumericVector::create(_["a"]=full_win.a, _["b"]=full_win.b),
      Named("fitted.values")    = full_win.fitted,
      Named("rmse")             = full_win.rmse,
      Named("mae")              = full_win.mae,
      Named("mse")              = full_win.mse,
      Named("cv_results")       = cvdf,
      Named("all_results")      = fulldf,
      Named("selection_metric") = std::string("CV-") + (metric=="rmse"?"RMSE":"MAE") + " (k=5)",
      Named("selection_score")  = cv_score
    );
}
