#' Normalise data
#' @param x a vector of numeric values
#' @return a vector of normalised values
#' @examples
#' normalise(c(1:5))
normalise <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
