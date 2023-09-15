#' Calculate Confidence Interval for Mean
#'
#' @param x A numeric vector for which the confidence interval of the mean is to be calculated.
#' @param alpha The significance level used to calculate the confidence interval.
#'
#' @return A numeric vector with the lower and upper bounds of the confidence interval.
#' @export
#'
#' @examples
#' sample_data <- c(10, 12, 15, 14, 13, 16, 18)
#' myci(sample_data, 0.05)
myci <- function(x, alpha) {

  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }

  n <- length(x)
  mean_x <- mean(x)
  se <- sd(x) / sqrt(n)

  t_lower <- qt(alpha / 2, df = n - 1, lower.tail = TRUE)
  t_upper <- qt(alpha / 2, df = n - 1, lower.tail = FALSE)

  ci_lower <- mean_x + t_lower * se
  ci_upper <- mean_x + t_upper * se

  return(c(lower = ci_lower, upper = ci_upper))
}
