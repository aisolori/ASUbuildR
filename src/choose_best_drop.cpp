#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List choose_best_drop_candidate(IntegerVector drop_candidates,
                                NumericVector unemp_vec,
                                NumericVector emp_vec,
                                double rem_unemp,
                                double rem_emp,
                                double new_unemp,
                                double new_emp,
                                double unemp_buffer) {

  double base_u   = rem_unemp + new_unemp;        // u
  double base_e   = rem_emp   + new_emp;          // e
  double base_u19 = std::pow(base_u, 1.9);        // u^1.9
  double best_U   = R_NegInf;
  int    best_idx = NA_INTEGER;

  for (int j = 0; j < drop_candidates.size(); ++j) {
    int idx = drop_candidates[j] - 1;             // 1-based → 0-based
    if (idx < 0 || idx >= unemp_vec.size()) continue;
    if (unemp_vec[idx] >= unemp_buffer) continue; // honour buffer

    double cand_u = base_u - unemp_vec[idx];
    double cand_e = base_e - emp_vec[idx];
    if (cand_u + cand_e <= 0) continue;

    // optimistic bound: emp → 0  (skip if impossible to beat)
    double max_possible = std::pow(cand_u, 0.9);
    if (max_possible <= best_U) continue;

    double cand_U = std::pow(cand_u, 1.9) / (cand_u + cand_e);
    if (cand_U > best_U) {
      best_U   = cand_U;
      best_idx = drop_candidates[j];              // keep 1-based
    }
  }

  return List::create(_["best_index"] = best_idx,
                      _["best_U"]     = best_U);
}
