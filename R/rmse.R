#' @title Root Mean square Error
#' @name rmse
#'
#' @description Calculate the root mean square error
#' @param true true values
#' @param estimated estimated values
#'
#' @return A numeric vector
#'
#' @author Alexandre Jaloto
#'
rmse <- function(true, estimated)
{
  result <- sqrt(mean((true-estimated)^2))
  result
}
