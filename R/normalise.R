#' Normalise data
#'
#' The function is built to normalise a vector of numbers.
#' @param x a vector of numeric values
#' @return a vector of normalised values
#' @examples
#' normalise(c(1:5))
#' @export
normalise <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
