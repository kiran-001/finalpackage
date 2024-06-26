% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/minimumN.R
\name{minimumN}
\alias{minimumN}
\title{Calculate minimum sample size needed for significant t-test results}
\usage{
minimumN(x1, x2 = NULL)
}
\arguments{
\item{x1}{A numeric vector containing the first sample.}

\item{x2}{A numeric vector containing the second sample (optional).}
}
\value{
Minimum sample size needed.
}
\description{
This function calculates the minimum sample size required to achieve 80\% power for a t-test at a significance level of 0.05.
It can handle both one-sample and two-sample t-tests.
}
\examples{
sample1 = rnorm(10, mean = 5)
sample2 = rnorm(10, mean = 5.5)
minimumN(sample1)        # One-sample t-test
minimumN(sample1, sample2)  # Two-sample t-test
}
