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

// [[Rcpp::export]]
IntegerVector getPredVec(IntegerVector x, List probs) {
  int ss = x.size();
  IntegerVector pred(52);
  for (int i = 0; i < 52; i++) {
    sugar::probs_t prob = probs[i];
    NumericVector p = clone(prob.get());
    int id = sugar::SampleNoReplace(p, ss, 1, false)[0];
    pred[i] = x[id];
  }
  return pred;
}

// [[Rcpp::export]]
double getDistance(IntegerVector x, IntegerVector y, List probs) {
  IntegerVector pred = cumsum(getPredVec(x, probs));
  IntegerVector pred1 = pred - y; 
  NumericVector pred2 = pow(pred1, 2);
  double pred3 = sum(pred2);
  return sqrt(pred3);
}


// [[Rcpp::export]]
List PredCIbyWk(IntegerVector x, List probs, int nSim, NumericVector pq) {
  IntegerVector y(nSim);
  List out(52);
  Environment stats("package:stats");
  Function quantile = stats["quantile"];
  int npr = pq.size();
  NumericVector q(npr);
  for (int i = 0; i < 52; i++) {
    sugar::probs_t prob = probs[i];
    NumericVector p = clone(prob.get());
    IntegerVector idx = sugar::SampleReplace(p, 52, nSim, false);
    IntegerVector y1 = x[idx];
    y = y + y1;
    q = quantile(y, pq);
    out[i] = q;
  }
  return out;
}




