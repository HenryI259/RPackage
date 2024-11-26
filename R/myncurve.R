#' @title Normal Distribution Plot
#'
#' @param a variable to find the probability that Y<= a
#' @param mu mean of the distribution
#' @param sigma standard deviation of the distribution
#'
#' @return A plot of a normal distribution with mean mu and sd sigma along with
#' the lower tail probability of a
#' @importFrom graphics curve
#' @importFrom stats dnorm
#' @importFrom graphics polygon
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' myncurve(4, 3, 2)

#globalVariables("x")

myncurve = function(a, mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma),
        xlab="Y", ylab="Normal Density")

  xcurve1=seq(mu-3*sigma,a,length=1000)
  ycurve1=dnorm(xcurve1, mean=mu, sd=sigma)

  polygon(c(mu-3*sigma, xcurve1, a), c(0,ycurve1,0), col="Blue")
  area = pnorm(a,mean=mu, sd=sigma)
  list(mu=mu, sigma=sigma, area = round(area, 4))
}
