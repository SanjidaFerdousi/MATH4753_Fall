#' Bootstrapping estimation
#'
#'
#'
#' @param iter An integer which is the number of bootstrap iterations and default is 10,000.
#' @param x A numeric vector.
#' @param fun A character string.
#' @param alpha A numeric value of the significance level for the confidence interval.
#' @param cx A numeric value of character expansion for text annotations on the histogram.
#' @param ... Additional parameters to be passed to the histogram function.

#' @return A list containing:
#'   \item{ci}{A numeric vector of length 2 indicating the lower and upper bounds of the confidence interval.}
#'   \item{fun}{The function used for the statistic.}
#'   \item{x}{The original sample.}
#'   \item{xstat}{A numeric vector of bootstrap statistics.}
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' sample_data <- rnorm(100)
#' results <- myboot2(x=sample_data, fun="mean")
#' print(results$ci)
#'
myboot2 <- function(iter=10000, x, fun="mean", alpha=0.05, cx=1.5, ...){
  n = length(x) # sample size

  y = sample(x, n*iter, replace=TRUE)
  rs.mat = matrix(y, nrow =n, ncol=iter, byrow=TRUE)
  xstat = apply(rs.mat, 2, fun)
  ci = quantile(xstat, c(alpha/2, 1-alpha/2))

  para = hist(xstat, freq=FALSE, las=1,
              main=paste("Histogram of Bootstrap sample statistics","\n","alpha=", alpha, " iter=", iter, sep=""),
              ...)

  mat = matrix(x, nrow=length(x), ncol=1, byrow=TRUE)
  pte = apply(mat, 2, fun)
  abline(v=pte, lwd=3, col="Black")
  segments(ci[1], 0, ci[2], 0, lwd=4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep=""), col="Red", cex=cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep=""), col="Red", cex=cx)
  text(pte, max(para$density)/2, round(pte, 2), cex=cx)

  invisible(return(list(ci=ci, fun=fun, x=x, xstat=xstat)) )
}
