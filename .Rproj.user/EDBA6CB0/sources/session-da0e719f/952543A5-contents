
### Wrapper function for SimpLinCpp

#' SimpLinR: Wrapper function for SimpLinCpp
#'
#' Under the hood, SimpLinCpp performs simple linear regression in C++. This function acts as a wrapper
#' to catch errors.
#'
#' @param x numeric vector; predictor variable
#' @param y numeric vector; response variable
#' @import Rcpp
#' @import RcppArmadillo
#' @export SimpLinR

SimpLinR <- function(x, y) {

  if (!is.numeric(x) || !is.numeric(y)) {
    stop("One of x or y is non-numeric.")
  }

  if (!is.vector(x) || !is.vector(y)) {
    stop("One of x or y is not a vector.")
  }

  if (length(x) != length(y)) {
    stop("x and y have differing lengths.")
  }

  SimpLinCpp(x,y)

}

