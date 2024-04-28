#' Reverse the effects of scaling on a vector
#'
#' This function takes a scaled vector `x` and reverses the scaling process. It assumes that `x`
#' has been scaled using the base R `scale()` function, which stores the center (mean) and scale (standard deviation)
#' as attributes of the returned object.
#'
#' @param x A numeric vector that has been scaled.
#' @return A numeric vector with the scaling reversed.
#' @examples
#' original = 1:10
#' scaled = scale(original)
#' unscaled = unscale(scaled)
#' all.equal(unscaled, original) # Should return TRUE
#' @export
unscale = function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }

  # Check if 'x' has the necessary attributes
  if (!is.null(attr(x, "scaled:center")) && !is.null(attr(x, "scaled:scale"))) {
    center = attr(x, "scaled:center")
    scale = attr(x, "scaled:scale")
    unscaled = x * scale + center
  } else {
    stop("The input vector does not have scaling attributes.")
  }

  return(unscaled)
}
