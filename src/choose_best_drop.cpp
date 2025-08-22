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
                                double unemp_buffer,
                                double ur_thresh = 0.0645) {

  double base_u = rem_unemp + new_unemp;
  double base_e = rem_emp   + new_emp;

  double best_score = R_NegInf;
  int best_idx = NA_INTEGER;

  for (int j = 0; j < drop_candidates.size(); ++j) {
    int idx = drop_candidates[j] - 1;  // R is 1-based, C++ is 0-based
    if (idx < 0 || idx >= unemp_vec.size()) continue;
    if (unemp_vec[idx] >= unemp_buffer) continue;

    double cand_u = base_u - unemp_vec[idx];
    double cand_e = base_e - emp_vec[idx];
    if (cand_u + cand_e <= 0) continue;

    double cand_ur = cand_u / (cand_u + cand_e);
    double ur_diff = cand_ur - ur_thresh;

    // Scale the candidate unemployment count by how far the UR is above or below the threshold
    double score = cand_u * pow((1 + ur_diff*50),2);

    if (score > best_score) {
      best_score = score;
      best_idx = drop_candidates[j];  // return in R's 1-based index
    }
  }

  return List::create(_["best_index"] = best_idx,
                      _["best_score"] = best_score);
}
