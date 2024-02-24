#' @title Generate response pattern
#' @name gen.resp
#'
#' @description Generate response pattern based on probability of answering correct a dichotomous item, given a theta and an item bank
#' @param bank matrix with item parameters (a, b, c)
#' @param theta theta
#'
#' @return A vector with the probability of seeing determined response in each item
#'
#' @author Alexandre Jaloto
#'
#' @export

gen.resp <- function(theta, bank)
{
  p <- lapply(theta, calc.prob, bank = bank)

  p <- do.call(rbind, p)

  random <- stats::runif(nrow(p)*ncol(p))

  resp <- apply(p > random, 2, as.numeric)

  return(resp)

}
