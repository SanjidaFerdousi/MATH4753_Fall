#' Plot of density
#'
#' @param mu is the mean value.
#' @param sigma is the standard deviation.
#' @param a is the upper limit for shading and annotation.
#'
#' @return plot the graph with shaded area.
#' @export
#' @import stats
#'
#' @examples
#' myncurve(mu = 10, sigma = 4, a = 8)
#'
myncurve = function(mu, sigma, a){
  x <- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve <- seq(mu - 3 * sigma, a, length = 1000)
  ycurve <- dnorm(xcurve, mean = mu, sd = sigma)
  area <- round(pnorm(a, mean = mu, sd = sigma), 4)
  polygon(c(mu - 3 * sigma, xcurve, a), c(0,ycurve, 0), col = "lightblue")
  text(a, 0.5*dnorm(a-1, mean = mu, sd = sigma), paste0("P(X<= ", a, ") =", area))
}


