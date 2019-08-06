#' @title Remove rows with Na Values from dataframe
#'
#' @description
#' The function will remove all rows which contain na values.
#' Specific columns can be cleaned whilst ignoring the other columns
#' @usage
#' removeNaRows(data)
#' removeNaRows(data, c("A", "B", "C"))
#' @param dataframe
#' a dataframe
#' @param applicableColumns
#' a vector of dataframe names which should be checked, ignore others
#' @return The dataframe with rows which have NA values removed
#' @examples
#' removeNaRows(data)
#' removeNaRows(read.csv("train.csv"))
#' removeNaRows(data, c("A", "B", "C"))
removeNaRows <- function(dataframe, applicableColumns=c()) {
  if (length(applicableColumns)==0) {
    isNa <- is.na(dataframe)
  } else {
    isNa <- is.na(dataframe[, applicableColumns])
  }
  rowsWithoutNa <- rowSums(isNa) == 0
  dataframe[rowsWithoutNa, ]
}
