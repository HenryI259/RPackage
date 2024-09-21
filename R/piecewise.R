#' @title Piecewise linear function
#'
#' @param x X variable
#' @param xk X coordinate of intersection
#' @param coef Coefficients of the function as a vector
#'
#' @return A piecewise linear function with respect to X
#' @export
#'
#' @examples
#' piecewise(5, 8, c(0.5, 0.2, -0.3))
piecewise <- function(x, xk, coef) {
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}
