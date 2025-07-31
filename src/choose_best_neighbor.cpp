#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List choose_best_neighbor(IntegerVector boundary_tracts,
                          NumericVector emp_vec,
                          NumericVector unemp_vec,
                          NumericVector pop_vec,
                          double asu_emp,
                          double asu_unemp,
                          double asu_pop,
                          double ur_thresh = 0.0645) {
  double best_improvement = R_NegInf;
  int best_index = NA_INTEGER;
  double best_ur = NA_REAL, best_emp = NA_REAL,
         best_unemp = NA_REAL, best_pop = NA_REAL;

  int n = boundary_tracts.size();
  for (int i = 0; i < n; ++i) {
    int idx = boundary_tracts[i] - 1; // convert from 1-based to 0-based
    if (idx < 0 || idx >= emp_vec.size()) continue;

    double total_emp   = asu_emp   + emp_vec[idx];
    double total_unemp = asu_unemp + unemp_vec[idx];
    double total_pop   = asu_pop   + pop_vec[idx];
    double total_ur    = total_unemp / (total_emp + total_unemp);

    if (total_ur >= ur_thresh) {
      double improvement = pow(total_unemp, 0.9) * total_ur;
      if (Rcpp::NumericVector::is_na(improvement)) improvement = 0.0;
      if (improvement > best_improvement) {
        best_improvement = improvement;
        best_index       = boundary_tracts[i];
        best_ur          = total_ur;
        best_emp         = total_emp;
        best_unemp       = total_unemp;
        best_pop         = total_pop;
      }
    }
  }

  return List::create(
      _["best_index"] = best_index,
      _["best_ur"] = best_ur,
      _["best_emp"] = best_emp,
      _["best_unemp"] = best_unemp,
      _["best_pop"] = best_pop
  );
}
