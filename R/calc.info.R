#' @title Compute item information
#' @name calc.info
#'
#' @description Calculate information of each item in the bank for a theta
#' @param bank matrix with item parameters (a, b, c)
#' @param theta current theta
#'
#' @details
#'
#' @return A vector with the information of each item
#'
#' @author Alexandre Jaloto
#'
#' @export

calc.info <- function(bank, theta)
{
  # parameters
  a <- bank[,1]
  b <- bank[,2]
  c <- bank[,3]

  # probability of answering correctly
  p <- c + (1 - c)*(exp(a*(theta-b)))/(1 + exp(a*(theta-b)))

  # information
  info <- a^2*((p - c)^2/(1 - c)^2)*((1 - p)/p)

  return(info)
}
