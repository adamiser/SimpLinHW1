#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
List SimpLinCpp(NumericVector x, NumericVector y) {

  // Useful values and memory allocation
  int n = x.size();
  arma::vec yvec = y;
  arma::vec cib0(2);
  arma::vec cib1(2);

  // Set up X matrix
  arma::vec xvec = x;
  arma::mat xmat(n,2);
  xmat.col(0) = arma::ones(n);
  xmat.col(1) = xvec;

  // Calculate beta
  arma::vec beta = (xmat.t() * xmat).i() * xmat.t() * yvec;

  // Calculate predicted values
  arma::vec predvals = beta(0) + beta(1)*xvec;

  // Calculate residuals
  arma::vec resids = yvec - predvals;

  // Calculate SE
  double sig2 = sum(pow(resids,2)) / (n-2);
  arma::vec se = pow(diagvec(sig2 * (xmat.t() * xmat).i()),0.5);

  // Calculate 95% CI

  double lowerb0 = beta(0) - R::qt(0.975, n-2, true, false) * se(0);
  double lowerb1 = beta(1) - R::qt(0.975, n-2, true, false) * se(1);
  double upperb0 = beta(0) + R::qt(0.975, n-2, true, false) * se(0);
  double upperb1 = beta(1) + R::qt(0.975, n-2, true, false) * se(1);

  cib0(0) = lowerb0;
  cib0(1) = upperb0;
  cib1(0) = lowerb1;
  cib1(1) = upperb1;

  return List::create(_["beta"] = beta,
                      _["SE"] = se,
                      _["95%_CI_b0"] = cib0,
                      _["95%_CI_b1"] = cib1,
                      _["Residuals"] = resids,
                      _["Fitted Values"] = predvals);

}
