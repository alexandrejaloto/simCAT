#' @title Compute probability
#' @name calc.prob
#'
#' @description Calculate probability of observing certain answer to a dichotomous item, given a theta
#' @param bank matrix with item parameters (a, b, c)
#' @param theta theta
#' @param u `1` for correct, `0` for wrong
#'
#' @return A vector with the probability of seeing determined response in each item
#'
#' @author Alexandre Jaloto
#'
#' @export

calc.prob <- function(theta, bank, u = 1)
{
  a <- bank[,1]
  b <- bank[,2]
  c <- bank[,3]
  p <- c + (1 - c)*(exp(a*(theta-b)))/(1 + exp(a*(theta-b)))

  u <- as.numeric(u)

  p <- p^u*(1-p)^(1-u)

  return(p)
}
