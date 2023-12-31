% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myci.R
\name{myci}
\alias{myci}
\title{Calculate Confidence Interval for Mean}
\usage{
myci(x, alpha)
}
\arguments{
\item{x}{A numeric vector for which the confidence interval of the mean is to be calculated.}

\item{alpha}{The significance level used to calculate the confidence interval.}
}
\value{
A numeric vector with the lower and upper bounds of the confidence interval.
}
\description{
Calculate Confidence Interval for Mean
}
\examples{
sample_data <- c(10, 12, 15, 14, 13, 16, 18)
myci(sample_data, 0.05)
}
