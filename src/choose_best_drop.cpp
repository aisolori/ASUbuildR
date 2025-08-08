// file: src/choose_best_drop_candidate_xgb2.cpp
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List choose_best_drop_candidate_xgb2(
    IntegerVector drop_candidates,   // 1-based
    NumericVector unemp_vec,
    NumericVector emp_vec,
    NumericVector prob_vec,          // xgb prob per tract (same length)
    NumericVector border_cost,       // optional per-tract cost (same length) or length 0
    double remaining_unemp,
    double remaining_emp,
    double total_new_unemp,
    double total_new_emp,
    double unemp_buffer, /* weights / shape */

    double alpha = 1.0,
    double beta  = 0.15,
    double gamma = 0.05,
    double delta = 0.0,
    double p     = 5.0
) {
  double best_score = R_NegInf;
  int best_index = NA_INTEGER;

  const bool use_border = (border_cost.size() == unemp_vec.size());

  for (int i = 0; i < drop_candidates.size(); ++i) {
    int one_based = drop_candidates[i];
    int idx = one_based - 1;
    if (idx < 0 || idx >= unemp_vec.size()) continue;

    // buffer: don't drop more U than you're adding
    if (unemp_vec[idx] >= unemp_buffer) continue;

    double cand_unemp = remaining_unemp - unemp_vec[idx] + total_new_unemp;
    double cand_emp   = remaining_emp   - emp_vec[idx]   + total_new_emp;
    double denom      = cand_unemp + cand_emp;
    if (denom <= 0) continue;

    double cand_ur = cand_unemp / denom;

    // two equivalent styles; pick ONE:
    // 1) curved-UR base:
    double base = cand_unemp * std::pow(1.0 + cand_ur, p);

    // composite penalty components
    double pen_u   = (unemp_buffer > 0.0 ? unemp_vec[idx] / unemp_buffer : 0.0);
    double pen_ml  = (prob_vec.size() == unemp_vec.size() && R_finite(prob_vec[idx]))
      ? prob_vec[idx] * unemp_vec[idx] : 0.0;
    double pen_shp = (use_border && R_finite(border_cost[idx])) ? border_cost[idx] : 0.0;

    // final score
    double score = alpha * base - beta * pen_u - gamma * pen_ml - delta * pen_shp;

    if (score > best_score) {
      best_score = score;
      best_index = one_based; // return R index
    }
  }

  return List::create(_["best_index"] = best_index,
                      _["best_score"] = best_score);
}

