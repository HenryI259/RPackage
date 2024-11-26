#' @title Binomial experiment
#'
#' @param iter number of iterations of the experiment
#' @param n number of trails
#' @param p probability of a success
#'
#' @return a barplot showing the distribution of successes after n trials
#' @importFrom graphics barplot
#' @importFrom grDevices rainbow
#' @export
#'
#' @examples
#' mybin(iter = 10000, n = 10, p = 0.7)

mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)

  #Make a vector to hold the number of successes in each trial
  succ=c()

  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))

    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }

  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  iter.lab = paste0("iter = ", iter)
  n.lab = paste0("n = ", n)
  p.lab = paste0("p = ", p)
  lab = paste(iter.lab, n.lab, p.lab, sep = ", ")
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation",     xlab="Number of successes")
  succ.tab/iter
}
