#' @title CAT Evaluation
#' @name cat.evaluation
#'
#' @description Evaluate a CAT simulation
#' @param results list with restults of a CAT simulation from `simCAT`
#' @param true.scores true scores
#' @param item.name vector with the name of all items in the bank
#' @param rmax item maximum exposure rate
#'
#' @details
#'
#' @return a list with two elements. `evaluate` is a `data.frame`
#' with the following variables:
#' \itemize
#' {
#' \item `rmse` root mean square error between true and estimated score
#' \item `correlation` correlation between true and estimated score
#' \item `bias` bias between true and estimated score
#' \item `overlap` overlap rate
#' \item `min_exp` minimum exposure rate
#' \item `max_exp` maximum exposure rate
#' \item `n_exp0` number of items not administered
#' \item `n_exp_rmax` number of items with exposure rate higher than rmax
#' \item `length_mean` average mean of test length
#' \item `length_median` average median of test length
#' \item `min_length` minimum test length
#' \item `max_length` maximum test length
#' }
#' `conditional` is a data.frame with the same variables
#' conditioned to the true scores. The `colnames` are the thetas
#' in each decil, that is,
#' `quantile(true.scores, probs = seq(.1, 1, length.out = 10))`.
#'
#' @author Alexandre Jaloto
#'
cat.evaluation <- function(results, true.scores, item.name, rmax)
{

  exposure <- exposure.rate(results$prev.resps, item.name)

  # test length
  teste.length <- c()
  for(person in 1:length(results$convergence))
  {
    teste.length <- c(teste.length, length(results$prev.resps[[person]]))
  }

  eval <- list(
    evaluate = data.frame(
      # eval <- data.frame(
      rmse = rmse(results$score$theta, true.scores),
      # se médio
      correlation = cor(results$score$theta, true.scores),
      bias = mean(true.scores - results$score$theta),
      overlap = sum(exposure$Freq^2)/sum(exposure$Freq),
      min_exp = min(exposure$Freq),
      max_exp = max(exposure$Freq),
      n_exp0 = sum(exposure$Freq == 0),
      n_exp_rmax = sum(exposure$Freq > rmax),
      length_mean = mean(teste.length),
      # sd length
      length_median = median(teste.length),
      min_length = min(teste.length),
      max_length = max(teste.length)
    )
  )

  # conditional evaluation ----

  thetas <- quantile(true.scores, probs = seq(.1,1,length.out = 10))

  levels <- cut(x = true.scores, breaks = c(-Inf,thetas), labels = 1:10)

  conditional.exp <- list()
  for (i in 1:10)
    conditional.exp[[i]] <- exposure.rate(
      previous = subset(results$prev.resps, levels == i),
      item.name = item.name
    )


  conditional.length <- list()
  for(i in 1:10)
    conditional.length[[i]] <- subset(teste.length, levels == i)


  conditional <- data.frame(matrix(ncol = 11))

  names(conditional) <- c('var', paste0('Q', 1:10))

  for(i in 1:10)
  {
    conditional[1,(i+1)] <- rmse(subset(results$score$theta, levels == i), subset(true.scores, levels == i))
    conditional[2,(i+1)] <- cor(subset(results$score$theta, levels == i), subset(true.scores, levels == i))
    conditional[3,(i+1)] <- mean(subset(true.scores, levels == i) - subset(results$score$theta, levels == i))
    conditional[4,(i+1)] <- sum(conditional.exp[[i]]$Freq^2)/sum(conditional.exp[[i]]$Freq)
    conditional[5,(i+1)] <- min(conditional.exp[[i]]$Freq)
    conditional[6,(i+1)] <- max(conditional.exp[[i]]$Freq)
    conditional[7,(i+1)] <- sum(conditional.exp[[i]]$Freq == 0)
    conditional[8,(i+1)] <- sum(conditional.exp[[i]]$Freq > rmax)
    conditional[9,(i+1)] <- mean(conditional.length[[i]])
    conditional[10,(i+1)] <- median(conditional.length[[i]])
    conditional[11,(i+1)] <- min(conditional.length[[i]])
    conditional[12,(i+1)] <- max(conditional.length[[i]])
  }

  conditional$var <- c(
    'rmse',
    'correlation',
    'bias',
    'overlap',
    'min_exp',
    'max_exp',
    'n_exp0',
    'n_exp_rmax',
    'length_mean',
    'length_median',
    'min_length',
    'max_length'
  )

  names(conditional) <- c('var', thetas)

  eval$conditional <- conditional

  return(eval)
}
