#' @title Confidence Intervals
#'
#' @param x vector containing a sample
#'
#' @return A list containing the 95% confidence interval for 𝜇
#' @importFrom stats qt
#' @importFrom stats sd
#' @export
#'
#' @examples
#' x=c(4.9769, 4.9722, 4.999, 4.9925, 4.9686, 5.0662, 4.9239, 4.9781)
#' myci(x)
myci = function(x) {
  n=length(x)

  t=qt(0.975,n-1)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(n)
  ci[2]=mean(x)+t*sd(x)/sqrt(n)
  ci
}
