#' Principal Components Approximation
#'
#' This function performs a PCA on the given dataset and returns an approximation
#' of the original data using a specified number of principal components.
#'
#' @param x A numeric matrix or data frame.
#' @param npc Number of principal components to retain.
#' @return A matrix or data frame of the approximated data.
#' @export
#' @examples
#' set.seed(123)
#' x <- matrix(rnorm(100), nrow = 10)
#' approximation <- pcApprox(x, npc = 2)
#' print(approximation)
pcApprox <- function(x, npc) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("x must be a numeric matrix or data frame")
  }

  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  scaled_x <- scale(x, center = TRUE, scale = TRUE)
  pca <- prcomp(scaled_x, center = FALSE, scale. = FALSE)
  pc_scores <- pca$x[, 1:npc]

  loadings <- pca$rotation[, 1:npc]
  approximation <- pc_scores %*% t(loadings)

  # Rescale and center the approximation to match the original data
  approximation <- scale(approximation, center = FALSE, scale = FALSE)
  approximation <- sweep(approximation, 2, attr(scaled_x, "scaled:scale"), FUN = "*")
  approximation <- sweep(approximation, 2, attr(scaled_x, "scaled:center"), FUN = "+")
  return(approximation)
}
