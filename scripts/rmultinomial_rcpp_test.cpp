#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector callRMultinom(NumericVector x) {
  int n = x.size();
  IntegerVector d(n);
  R::rmultinom(1, x.begin(), n, d.begin());
  return d;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

callRMultinom(rep(0.25,4))
*/
