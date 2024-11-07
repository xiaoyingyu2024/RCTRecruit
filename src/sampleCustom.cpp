#include <Rcpp.h>
using namespace Rcpp;


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

using namespace sugar;

class rct {
public:
  
  List probs;
  List binomWt;
  List cauchyWt;
  IntegerVector train;
  IntegerVector target;
  Environment e;
  
  rct(Environment e) {
    this->e = e;
    init();
  }
  rct() { }
  rct(IntegerVector train, List probs) {
    this->train = train;
    setProbs(probs);
  }
  
  void setEnv(Environment e) {
    this->e = e;
  }
  
  void init() {
    train = e["train"];
    List binom = e["binomWt"];
    List cauchy = e["cauchyWt"];
    int s = binom.size();
    for (int i = 0; i < s; i++) {
      binom[i] = as<probs_t>(binom[i]);
      cauchy[i] = as<probs_t>(cauchy[i]);
    }
    this->binomWt = binom;
    this->cauchyWt = cauchy;
    this->probs = binom;
  }
  
  void useCauchy(bool b) {
    probs = b ? cauchyWt : binomWt;
  }
  
  Environment getEnv() {return e;}
  
  void setProbs(List &probs) {
    int s = probs.size();
    for (int i = 0; i < s; i++) {
      probs[i] = as<probs_t>(probs[i]);
    }
    this->probs = probs;
  }

  void setTarget(IntegerVector TargetVec) {
    this->target = TargetVec;
    IntegerVector cumulativeTarget = cumsum(TargetVec);
    cumTarget = cumulativeTarget;
  }
  
  IntegerVector getCumTarget() {return cumTarget;}
  
  NumericVector quantile(IntegerVector x, NumericVector pq) {
    Environment stats("package:stats");
    Function qntl = stats["quantile"];
    int npr = pq.size();
    NumericVector q(npr);
    q = qntl(x, pq);
    return q;
  }

  int weeks2NsubjectsUnit(int nSubjects) {
    int n = 0, i = 0;
    while (n <= nSubjects) {
      NumericVector p = clone(probs(i % 52).get());
      n += SampleNoReplace(p, 1, train)(0);
      i++;
    }
    return i;
  }
  
  IntegerVector weeks2Nsubjects(int nSim, int nSubjects) {
    IntegerVector y(nSim);
    for (int i = 0; i < nSim; i++) {
      y(i) = weeks2NsubjectsUnit(nSubjects);
    }
    return y;
  }

  NumericMatrix PredCIbyWk(int nSim) {
    IntegerVector y(nSim);
    NumericVector pq = {.025, .5, .975};
    NumericMatrix out(52, 3);
    colnames(out) = as<CharacterVector>(quantile(y, pq).names());
    for (int i = 0; i < 52; i++) {
      NumericVector p = clone(probs(i).get());
      y = y + SampleReplace(p, nSim, train);
      out.row(i) = quantile(y, pq);
    }
    return out;
  }
  
  IntegerVector getPredVec() {
    IntegerVector pred(52);
    for (int i = 0; i < 52; i++) {
      NumericVector p = clone(probs(i).get());
      pred(i) = SampleNoReplace(p, 1, train)(0);
    }
    return pred;
  }
  
  double getDistanceUnit() {
    IntegerVector pred(52);
    NumericVector p = clone(probs(0).get());
    pred(0) = SampleNoReplace(p, 1, train)(0);
    for (int i = 1; i < 52; i++) {
      p = clone(probs(i).get());
      pred(i) = pred(i - 1) + SampleNoReplace(p, 1, train)(0);
    }
    return sqrt(sum(pow(pred - cumTarget, 2)));
  }
  
  NumericVector getDistance(int nSim) {
    NumericVector out(nSim);
    for (int i = 0; i < nSim; i++) {
      out(i) = getDistanceUnit();
    }
    return out;
  }
  
private:
  IntegerVector cumTarget;
};


RCPP_MODULE(mod) {
  class_<rct>("rct")
  .default_constructor()
  .constructor<Environment>()
  .constructor<IntegerVector, List>()
  .method("setProbs", &rct::setProbs)
  .method("useCauchy", &rct::useCauchy)
  .method("setTarget", &rct::setTarget)
  .method("getCumTarget", &rct::getCumTarget)
  .method("quantile", &rct::quantile)
  .method("PredCIbyWk", &rct::PredCIbyWk)
  .method("getDistanceUnit", &rct::getDistanceUnit)
  .method("getDistance", &rct::getDistance)
  .method("weeks2NsubjectsUnit", &rct::weeks2NsubjectsUnit)
  .method("weeks2Nsubjects", &rct::weeks2Nsubjects)
  .field("e", &rct::e)
  .field_readonly("probs", &rct::probs)
  .field("train", &rct::train, "The train vector")
  .field_readonly("target", &rct::target)
  ;
}






