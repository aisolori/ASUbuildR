#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List choose_best_drop_candidate(IntegerVector drop_candidates,
                                NumericVector unemp_vec,
                                NumericVector emp_vec,
                                double remaining_unemp,
                                double remaining_emp,
                                double total_new_unemp,
                                double total_new_emp,
                                double unemp_buffer) {
  double best_ur = R_NegInf;
  int best_index = NA_INTEGER;

  int n = drop_candidates.size();
  for (int i = 0; i < n; ++i) {
    int idx = drop_candidates[i] - 1; // 1-based to 0-based
    if (idx < 0 || idx >= unemp_vec.size()) continue;

    if (unemp_vec[idx] >= unemp_buffer) continue;

    double cand_unemp = remaining_unemp - unemp_vec[idx] + total_new_unemp;
    double cand_emp   = remaining_emp   - emp_vec[idx]   + total_new_emp;
    double cand_ur    = cand_unemp / (cand_unemp + cand_emp);

    if (cand_ur > best_ur) {
      best_ur = cand_ur;
      best_index = drop_candidates[i];
    }
  }

  return List::create(_["best_index"] = best_index,
                      _["best_ur"] = best_ur);
}

