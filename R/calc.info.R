#' @title Compute item information
#' @name calc.info
#'
#' @description Calculate information of each item in the bank for a theta
#' @param bank matrix with item parameters (a, b, c)
#' @param theta current theta
#' @param model may be `3PL` or `graded`
#'
#' @return A vector with the information of each item
#'
#' @author Alexandre Jaloto
#'
#' @export

calc.info <- function(bank, theta, model = '3PL')
{
  if (model == '3PL')
  {
    # parameters
    a <- bank[,1]
    b <- bank[,2]
    c <- bank[,3]

    # probability of answering correctly
    p <- c + (1 - c)*(exp(a*(theta-b)))/(1 + exp(a*(theta-b)))

    # information
    info <- a^2*((p - c)^2/(1 - c)^2)*((1 - p)/p)
  }

  if (model == 'graded')
  {
    # parameters
    a <- bank[,1]
    bj <- bank[,2:ncol(bank)]

    # probability of answering correctly
    # p <- c(1, as.numeric(exp(a*(theta-bj))/(1 + exp(a*(theta-bj)))), 0)
    p <- data.frame(1, exp(a*(theta-bj))/(1 + exp(a*(theta-bj))), 0)

    # derivada da probability
    dp <- a * p * (1 - p)

    p <- p[1:(length(p) - 1)] - p[2:length(p)]
    dp <- dp[1:(length(dp) - 1)] - dp[2:length(dp)]

    # information
    info <- rowSums(dp^2/p, na.rm = TRUE)
  }

  return(info)

}
