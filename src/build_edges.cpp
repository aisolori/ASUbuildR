#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix build_asu_edges(List nb, IntegerVector asu_vec) {
  int n = nb.size();
  std::vector<std::pair<int,int>> edges;
  edges.reserve(n);

  for (int i = 0; i < n; ++i) {
    int asu_i = asu_vec[i];
    if (asu_i == NA_INTEGER) continue;
    IntegerVector neighbs = nb[i];
    for (int j = 0; j < neighbs.size(); ++j) {
      int idx = neighbs[j] - 1;
      if (idx < 0 || idx >= n) continue;
      int asu_j = asu_vec[idx];
      if (asu_j == NA_INTEGER || asu_j == asu_i) continue;
      int a = std::min(asu_i, asu_j);
      int b = std::max(asu_i, asu_j);
      edges.emplace_back(a,b);
    }
  }

  if (edges.empty()) {
    return IntegerMatrix(0,2);
  }

  std::sort(edges.begin(), edges.end());
  edges.erase(std::unique(edges.begin(), edges.end()), edges.end());

  IntegerMatrix res(edges.size(), 2);
  for (size_t i = 0; i < edges.size(); ++i) {
    res(i,0) = edges[i].first;
    res(i,1) = edges[i].second;
  }

  return res;
}

