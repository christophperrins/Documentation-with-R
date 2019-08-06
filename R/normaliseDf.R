#' Normalise dataframe
#'
#' This function will normalise the dataframe entered.
#' If only certain columns need to be normalised, these can be specified.
#' The order of the columns is kept the same
#' @param dataframe a dataframe to be normalised
#' @param applicableColumns a vector of character of dataframe names to be normalised in the dataframe
#' @return a dataframe with normalised values
#' @examples
#' dataframe <- data.frame(first = c(1:3), second = c(4:6), third = c(7:9))
#' normaliseDf(dataframe)
#' normaliseDf(dataframe, c("first", "third"))
#' @export
normaliseDf <- function(dataframe, applicableColumns=c()) {
  if (length(applicableColumns)==0) {
    numericData <- lapply(dataframe, as.numeric)
    normalisedData <- data.frame(lapply(numericData, normalise))
  } else {
    columnNames <- names(dataframe)
    numericData <- lapply(dataframe[, applicableColumns], as.numeric)
    normalisedPartialData <- lapply(numericData, normalise)
    normalisedData <- cbind(normalisedPartialData, dataframe[, setdiff(names(dataframe), applicableColumns), drop = FALSE])
    normalisedData <- normalisedData[, columnNames]
  }
  normalisedData
}
