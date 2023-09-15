#' Histogram of Sample Mean from Poisson Distribution
#'
#' This function samples from a Poisson distribution, computes the sample means,
#' and produces a histogram of these sample means. Additionally, it overlays a
#' normal distribution curve on the histogram based on the properties of the Poisson distribution.
#'
#' @param n A integer of the sample size for each iteration.
#' @param iter A integer of the number of iterations or samples to be taken.
#' @param lambda A numeric value of the lambda parameter for the Poisson distribution, default is 10.
#' @param ... Additional graphical parameters to be passed to the hist() function.
#'
#' @return
#' This function returns a layout of 3 plots:
#' 1. A histogram of sample means with an overlaid normal distribution curve.
#' 2. A barplot of relative frequencies of sampled y values.
#' 3. A plot representing the probability function for the Poisson distribution.
#' No return value to the console.
#'
#' @export
#'
#' @examples
#' mycltp(n=2, iter=10000, lambda = 4)
mycltp=function(n,iter,lambda=10,...){
  y=rpois(n*iter,lambda=lambda)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)
  ymax=max(param$density)
  ymax=1.1*ymax
  layout(matrix(c(1,1,2,3),nrow=2,ncol=2, byrow=TRUE))
  hist(w,freq=FALSE,  ylim=c(0,ymax), col=rainbow(max(w)),
       main=paste("Histogram of sample mean","\n", "sample size= ",n," iter=",iter," lambda=",lambda,sep=""),
       xlab="Sample mean",...)
  curve(dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="Red",lty=2,lwd=3)
  barplot(table(y)/(n*iter),col=rainbow(max(y)), main="Barplot of sampled y", ylab ="Rel. Freq",xlab="y" )
  x=0:max(y)
  plot(x,dpois(x,lambda=lambda),type="h",lwd=5,col=rainbow(max(y)),
       main="Probability function for Poisson", ylab="Probability",xlab="y")
}
