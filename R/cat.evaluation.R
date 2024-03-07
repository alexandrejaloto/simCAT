#' @title CAT Evaluation
#' @name cat.evaluation
#'
#' @description Evaluate a CAT simulation
#' @param results list with results of a CAT simulation from `simCAT`
#' @param true.scores true scores
#' @param item.name vector with the name of all items in the bank
#' @param rmax item maximum exposure rate
#'
#' @return a list with two elements.
#'
#' `evaluate` is a `data.frame`. Each line corresponds to a replication,
#' and the columns are the following variables:
#' \itemize{
#' \item `rmse` root mean square error between true and estimated score
#' \item `se` standard error of measurement
#' \item `correlation` correlation between true and estimated score
#' \item `bias` bias between true and estimated score
#' \item `overlap` overlap rate
#' \item `min_exp` minimum exposure rate
#' \item `max_exp` maximum exposure rate
#' \item `n_exp0` number of items not administered
#' \item `n_exp_rmax` number of items with exposure rate higher than rmax
#' \item `length_mean` average mean of test length
#' \item `length_sd` standard deviation of test length
#' \item `length_median` average median of test length
#' \item `min_length` minimum test length
#' \item `max_length` maximum test length
#' }
#' `conditional` is a data.frame with the same variables (except
#' for `length_sd` and `length_median`)
#' conditioned to the true scores. The `colnames` are the thetas
#' in each decile, that is,
#' `quantile(true.scores, probs = seq(.1, 1, length.out = 10))`. Each
#' line corresponds to the mean of the investigated variables for each
#' decile. If there are replications, values are the replication means
#' for each decile.
#'
#' @examples
#'
#' \donttest{
#' set.seed(1)
#' n.items <- 50
#' pars <- data.frame(
#'  a = rlnorm(n.items),
#'  b = rnorm(n.items),
#'  c = rbeta(n.items, 5, 17),
#'  d = 1)
#'
#' # thetas
#' theta <- rnorm(100)
#'
#' # simulate responses
#' resps <- gen.resp(theta, pars[,1:3])
#'
#' results <- simCAT(resps = resps,
#'  bank = pars[,1:3],
#'  start.theta = 0,
#'  sel.method = 'MFI',
#'  cat.type = 'variable',
#'  threshold = .3,
#'  stop = list(se = .3, max.items = 10))
#'
#' eval <- cat.evaluation(
#'  results = results,
#'  true.scores = theta,
#'  item.name = paste0('I', 1:nrow(pars)),
#'  rmax = 1)
#'
#' #### 3 replications
#' replications <- 3
#'
#' # simulate responses
#' set.seed(1)
#' resps <- list()
#' for(i in 1:replications)
#'  resps[[i]] <- gen.resp(theta, pars[,1:3])
#'
#' # CAT
#' results <- list()
#' for (rep in 1:replications)
#' {
#'  print(paste0('replication: ', rep, '/', replications))
#'  results[[rep]] <- simCAT(
#'   resps = resps[[rep]],
#'   bank = pars[,1:3],
#'   start.theta = 0,
#'   sel.method = 'MFI',
#'   cat.type = 'variable',
#'   threshold = .3,
#'   stop = list(se = .5, max.items = 10))
#' }
#'
#' eval <- cat.evaluation(
#'  results = results,
#'  true.scores = theta,
#'  item.name = paste0('I', 1:nrow(pars)),
#'  rmax = 1)
#' }
#'
#' @author Alexandre Jaloto
#'
#' @export
#'
cat.evaluation <- function(results, true.scores, item.name, rmax)
{



  # true scores deciles
  thetas <- stats::quantile(true.scores, probs = seq(.1,1,length.out = 10))
  levels <- cut(x = true.scores, breaks = c(-Inf,thetas), labels = 1:10)

  # object for average evaluation
  evaluation <- list()

  # dependent variables
  rmse <- data.frame()
  se <- data.frame()
  cor <- data.frame()
  bias <- data.frame()
  overlap <- data.frame()
  min_exp <- data.frame()
  max_exp <- data.frame()
  n_exp0 <- data.frame()
  n_exp_rmax <- data.frame()
  length_mean <- data.frame()
  length_sd <- data.frame()
  length_median <- data.frame()
  min_length <- data.frame()
  max_length <- data.frame()

  # if there is no replication
  if(!is.null(names(results)))
  # {replications <- length(results)} else {replications <- 1}
  results <- list(results)

  for (i in 1:length(results))
  {
    # exposure rate
    exposure <- exposure.rate(results[[i]]$prev.resps, item.name)

    # test length
    teste.length <- c()
    for(person in 1:length(results[[i]]$convergence))
    {
      teste.length <- c(teste.length, length(results[[i]]$prev.resps[[person]]))
    }

    evaluation[[i]] <- data.frame(
      rmse = rmse(results[[i]]$score$theta, true.scores),
      se = mean(results[[i]]$score$SE),
      correlation = cor(results[[i]]$score$theta, true.scores),
      bias = mean(true.scores - results[[i]]$score$theta),
      overlap = (mean((exposure$Freq - mean(exposure$Freq))^2) + mean(exposure$Freq)^2)/mean(exposure$Freq),
      min_exp = min(exposure$Freq),
      max_exp = max(exposure$Freq),
      n_exp0 = sum(exposure$Freq == 0),
      n_exp_rmax = sum(exposure$Freq > rmax),
      length_mean = mean(teste.length),
      length_sd = stats::sd(teste.length),
      length_median = stats::median(teste.length),
      min_length = min(teste.length),
      max_length = max(teste.length)
    )

    # conditional evaluation ----

    conditional.exp <- list()
    for (q in 1:10)
      conditional.exp[[q]] <- exposure.rate(
        previous = subset(results[[i]]$prev.resps, levels == q),
        item.name = item.name
      )

    conditional.length <- list()
    for(q in 1:10)
      conditional.length[[q]] <- subset(teste.length, levels == q)

    for(q in 1:10)
    {
      rmse[i,q] <- rmse(subset(results[[i]]$score$theta, levels == q), subset(true.scores, levels == q))
      se[i,q] <- mean(subset(results[[i]]$score$SE, levels == q))
      cor[i,q] <- cor(subset(results[[i]]$score$theta, levels == q), subset(true.scores, levels == q))
      bias[i,q] <- mean(subset(true.scores, levels == q) - subset(results[[i]]$score$theta, levels == q))
      overlap[i,q] <- (mean((conditional.exp[[q]]$Freq - mean(conditional.exp[[q]]$Freq))^2) + mean(conditional.exp[[q]]$Freq)^2)/mean(conditional.exp[[q]]$Freq)
      min_exp[i,q] <- min(conditional.exp[[q]]$Freq)
      max_exp[i,q] <- max(conditional.exp[[q]]$Freq)
      n_exp0[i,q] <- sum(conditional.exp[[q]]$Freq == 0)
      n_exp_rmax[i,q] <- sum(conditional.exp[[q]]$Freq > rmax)
      length_mean[i,q] <- mean(conditional.length[[q]])
      length_sd[i,q] <- stats::sd(conditional.length[[q]])
      length_median[i,q] <- stats::median(conditional.length[[q]])
      min_length[i,q] <- min(conditional.length[[q]])
      max_length[i,q] <- max(conditional.length[[q]])
    }
  }

  # final object
  eval <- list()

  # final general evaluation
  eval$evaluation <- do.call(rbind, evaluation)
  # eval$evaluation <- colMeans(eval$evaluation)

  # final conditional evaluation
  rmse <- colMeans(rmse)
  se <- colMeans(se)
  cor <- colMeans(cor)
  bias <- colMeans(bias)
  overlap <- colMeans(overlap)
  min_exp <- colMeans(min_exp)
  max_exp <- colMeans(max_exp)
  n_exp0 <- colMeans(n_exp0)
  n_exp_rmax <- colMeans(n_exp_rmax)
  length_mean <- colMeans(length_mean)
  min_length <- colMeans(min_length)
  max_length <- colMeans(max_length)

  eval$conditional <- data.frame(
    rbind(
      rmse,
      se,
      cor,
      bias,
      overlap,
      min_exp,
      max_exp,
      n_exp0,
      n_exp_rmax,
      length_mean,
      min_length,
      max_length
    )
  )

  names(eval$conditional) <- c(thetas)

  return(eval)
}
