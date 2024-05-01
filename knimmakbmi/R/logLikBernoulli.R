#' Calculate the maximum likelihood estimate of parameter p for Bernoulli distribution
#'
#' @param data A numeric vector of binary data (0s and 1s).
#' @return A list containing the maximum log-likelihood and the corresponding parameter p.
#' @export
#' @examples
#' data = c(1, 0, 0, 0, 1, 1, 1)
#' logLikBernoulli(data)
logLikBernoulli = function(data) {
  if (length(data) == 0) {
    stop("Input data vector is empty")
  }
  best_p = NULL
  max_logLik = -Inf
  for (p in seq(0, 1, by = 0.001)) {
    logLik = sum(data * log(p + 1e-10) + (1 - data) * log(1 - p + 1e-10))
    if (logLik > max_logLik) {
      max_logLik = logLik
      best_p = p
    }
  }
  return(list(max_log_likelihood = max_logLik, p = best_p))
}
