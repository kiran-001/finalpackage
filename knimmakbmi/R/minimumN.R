#' Calculate minimum sample size needed for significant t-test results
#'
#' This function calculates the minimum sample size required to achieve 80% power for a t-test at a significance level of 0.05.
#' It can handle both one-sample and two-sample t-tests.
#'
#' @param x1 A numeric vector containing the first sample.
#' @param x2 A numeric vector containing the second sample (optional).
#' @return Minimum sample size needed.
#' @importFrom stats sd
#' @importFrom pwr pwr.t.test
#' @examples
#' sample1 = rnorm(10, mean = 5)
#' sample2 = rnorm(10, mean = 5.5)
#' minimumN(sample1)        # One-sample t-test
#' minimumN(sample1, sample2)  # Two-sample t-test
#' @export
minimumN = function(x1, x2 = NULL) {
  if (!is.numeric(x1) || (!is.null(x2) && !is.numeric(x2))) {
    stop("Both inputs must be numeric vectors.")
  }

  if (!requireNamespace("pwr", quietly = TRUE)) {
    stop("Please install the 'pwr' package.")
  }

  # Calculate effect size
  if (is.null(x2)) {
    # One-sample t-test scenario
    d = (mean(x1) - 0) / sd(x1)
  } else {
    # Two-sample t-test scenario
    mean1 = mean(x1)
    mean2 = mean(x2)
    sd1 = sd(x1)
    sd2 = sd(x2)
    pooled_sd = sqrt(((length(x1) - 1) * sd1^2 + (length(x2) - 1) * sd2^2) / (length(x1) + length(x2) - 2))
    d = abs(mean1 - mean2) / pooled_sd
  }

  # Calculate required sample size using pooled two-sample t-test formula
  result = pwr::pwr.t.test(d = d, power = 0.8, sig.level = 0.05, type = if (is.null(x2)) "one.sample" else "two.sample")

  return(ceiling(result$n))
}
