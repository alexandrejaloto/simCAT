#' @title EAP estimation
#' @name eap
#'
#' @description Estimates theta with Expected a Posteriori
#'
#' @param pattern response pattern (0 and 1) with the number of columns
#'  corresponding to the number of items
#' @param bank data.frame with item parameters (a, b, c)
#'
#' @details 40 quadrature points, ranging from -4 to 4. Priori with normal distribution (mean = 0, sd = 1).
#'
#' @return data.frame with estimated `theta` and `SE`.
#'
#' @author Alexandre Jaloto
#'
#' @export

eap <- function(pattern, bank)
{
  # data(tcals)
  # # Item bank creation with 'tcals' item parameters
  # prov <- breakBank(tcals)
  # bank <- data.frame(prov$itemPar)
  # bank <- bank[1:3,]
  # pattern <- c(0,1)
  # pattern <- c(0,1,0)
  # quadrature points
  qdpts <- seq(-4, 4, length.out = 40)

  # priori
  prior <- stats::dnorm(qdpts, mean=0, sd=1, log = FALSE)/sum(stats::dnorm(qdpts, mean=0, sd=1, log = FALSE))

  bank <- data.frame(bank)
  names(bank) <- c('a', 'b', 'c')

  # probability for the observed response in each quadrature point
  # p <- t(data.frame(sapply(qdpts, calc.prob, bank$a, bank$b, bank$c, pattern)))

  p <- matrix(NA,length(qdpts),nrow(bank))
  for(j in 1:length(qdpts)){
    for(i in 1:nrow(bank)){
      # p[j,i] <- calc.prob(theta=qdpts[j], a=bank$a[i], b=bank$b[i], c=bank$c[i], u=pattern[i])
      p[j,i] <- calc.prob(theta=qdpts[j], bank = bank[i,], u=pattern[i])
    }
  }

  # likelihood for each qdpt
  # lkl <- data.frame(apply(p, 1, prod))

  lkl <- matrix(NA,length(qdpts),1)
  for(j in 1:length(qdpts)){
    lkl[j] <- prod(p[j,])
  }

  theta <- sum(qdpts*prior*lkl)/sum(prior*lkl)
  SE <- sqrt(sum((qdpts-theta)^2*lkl*prior)/sum(lkl*prior))

  eap <- data.frame(theta, SE)

  eap

  # mod <- bank %>%
  #   data.frame() %>%
  #   rename(a1 = a, d = b, g = c) %>%
  #   # rename(a1 = X1, d = X2, g = X3) %>%
  #   mutate(d = -d*a1) %>%
  #   select(a1, d, g) %>%
  #   mirtCAT::generate.mirt_object('3PL')
  #
  # mirt::fscores(object = mod, method = 'EAP', response.pattern = pattern,
  #               quadpts = 40,
  #               theta_lim = c(-4,4),
  #               max_theta = 4)
  #


  return(eap)
}
