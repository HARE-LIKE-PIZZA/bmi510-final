#' Unscale a Scaled Vector
#'
#' This function reverses the scaling transformation applied to a numeric vector.
#' It restores the vector to its original scale if it has been centered and/or scaled.
#'
#' @param x A scaled numeric vector.
#' @return Numeric vector with the scaling reversed.
#' @export
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' scaled_x <- scale(x)
#' unscaled_x <- unscale(scaled_x)
#' print(unscaled_x)
unscale <- function(x) {
  if (!is.null(attr(x, "scaled:center")) && !is.null(attr(x, "scaled:scale"))) {
    center <- attr(x, "scaled:center")
    scale <- attr(x, "scaled:scale")

    unscaled_x <- x * scale + center
    attr(unscaled_x, "scaled:center") <- NULL
    attr(unscaled_x, "scaled:scale") <- NULL

    return(unscaled_x)
  } else {
    # If the vector has not been scaled, return it as is
    return(x)
  }
}
