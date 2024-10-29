#include <Rcpp.h>
using namespace Rcpp;

inline int mySample(sugar::probs_t prob, int n) {
  NumericVector p = clone(prob.get());
  int out = sugar::SampleNoReplace(p, n, 1, false)[0];
  return out;
}

// [[Rcpp::export]]
int sim1(IntegerVector x, List probs, int nSubjects, int startWeek = 1L) {
  int n = 0;
  int i = startWeek - 1L;
  int ss = x.size();
  while (n <= nSubjects) {
    int idx = i % 52;
    sugar::probs_t prob = probs[idx];
    NumericVector p = clone(prob.get());
    int id = sugar::SampleNoReplace(p, ss, 1, false)[0];
    n += x[id];
    i++;
  }
  return i;
}


