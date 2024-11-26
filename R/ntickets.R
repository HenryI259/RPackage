#' @title Optimal Ticket Finding
#'
#' @param N Variable for the amount of seats on the place
#' @param gamma Variable for the probability the plane is overbooked
#' @param p Variable for the probability that a passenger shows
#'
#' @return
#' Creates two plots representing the Objective Vs n to find the optimal tickets
#' sold using the binomial distribution and normal approximation. Then creates a named
#' list containing the optimal tickets sold using both distributions along with N,
#' gamma, and p.
#' @export
#' @importFrom stats pbinom
#' @importFrom stats uniroot
#' @importFrom graphics points
#' @importFrom graphics segments
#' @importFrom graphics abline
#'
#' @examples
#' ntickets(200, 0.02, 0.95)
#' ntickets(500, 0.01, 0.9)
#'
ntickets=function(N, gamma, p) {
  npoints=c(0:(1.1*N))

  probsB=1-gamma-pbinom(N,npoints,p)
  xB=which.min(abs(probsB))
  nd=xB-1

  nc=uniroot(function(n)1-gamma-pnorm(N, n*p, sqrt(n*p*(1-p))),
          c(0,1.1*N))

  plot(x=1,
       xlim = c(N, 1.1*N), ylim =c(0, 1),
       xlab="n", ylab="Objective",
       main = paste("Objective Vs n to find optimal tickets sold\n (", nd, ") gamma=", gamma, " N=", N, " discrete", sep = ""))
  points(npoints, probsB, pch=16, col = "Blue")
  segments(c(N:(1.1*N-1)), 1-gamma-pbinom(N,c(N:(1.1*N-1)),p),
           c(N:(1.1*N-1))+1, 1-gamma-pbinom(N,c(N:(1.1*N-1))+1,p))
  abline(v = nd, h = 0, col = "Red", lwd=2)

  curve(1-gamma-pnorm(N, x*p, sqrt(x*p*(1-p))),
        xlim = c(N, 1.1*N), ylim =c(0, 1),
        xlab="n", ylab="Objective",
        main = paste("Objective Vs n to find optimal tickets sold\n (", nc$root, ") gamma=", gamma, " N=", N, " continuous", sep = ""))
  abline(v = nc$root, h = 0, col = "Blue")

  list(nd=nd, nc=nc$root, N=N, gamma=gamma, p=p)
}

