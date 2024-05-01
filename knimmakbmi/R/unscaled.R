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

  # Retrieve the center and scale attributes
  center = attr(x, "scaled:center")
  scale = attr(x, "scaled:scale")

  # Check if attributes exist
  if (!is.null(center) && !is.null(scale)) {
    if (any(is.nan(scale))) {  # Check for NaN scale
      # Return a vector of the center value repeated
      unscaled = rep(center, length(x))
    } else {
      # Perform the normal unscaling operation
      unscaled = x * scale + center
      # Convert matrix to vector if needed
      if (is.matrix(unscaled) && ncol(unscaled) == 1) {
        unscaled = as.vector(unscaled)
      }
    }
  } else {
    stop("The input vector does not have scaling attributes.")
  }

  return(unscaled)
}
