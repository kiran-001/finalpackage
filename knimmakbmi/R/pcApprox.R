#' Approximate data matrix using a specified number of principal components
#'
#' This function performs PCA on a data matrix `x` and uses `npc` principal components to
#' approximate and then rescale and recenter the data to match the original data's statistics.
#'
#' @param x A numeric matrix or data frame of data to approximate.
#' @param npc The number of principal components to use for the approximation.
#' @return A numeric matrix approximating the original data, rescaled and recentered.
#' @importFrom stats prcomp
#' @examples
#' data_matrix = matrix(rnorm(100), nrow=10)
#' approx_data = pcApprox(data_matrix, 2)
#' @export
pcApprox = function(x, npc) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("Input x must be a matrix or data frame.")
  }
  if (!is.numeric(npc) || npc <= 0 || npc > ncol(x)) {
    stop("Parameter npc must be a positive integer less than or equal to the number of columns in x.")
  }

  # Perform PCA
  pca = prcomp(x, scale. = TRUE, center = TRUE)

  # Reconstruction using the selected number of PCs
  x_rotated = pca$x[, 1:npc, drop = FALSE] %*% t(pca$rotation[, 1:npc, drop = FALSE])

  # Correctly reverse the scaling and centering
  approx_data = x_rotated
  approx_data = sweep(approx_data, 2, pca$scale, FUN="*")
  approx_data = sweep(approx_data, 2, pca$center, FUN="+")

  return(approx_data)
}
