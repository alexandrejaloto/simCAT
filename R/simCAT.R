#' @title CAT simulation
#' @name simCAT
#'
#' @description A CAT simulation with dichotomous items.
#'
#' @param resps a matrix with responses (0 and 1).
#' The number of columns
#'  corresponds to the number of items
#' @param bank matrix with item parameters (a, b, c)
#' @param model may be `3PL` or `graded`
#' @param start.theta first theta
#' @param sel.method item selection method: may be `MFI`, `progressive`
#'  or `random`
#' @param cat.type CAT with `variable` or `fixed` length
#' Necessary only for progressive method.
#' @param acceleration acceleration parameter.
#' Necessary only for progressive method.
#' @param met.weight the procedure to calculate the `progressive`'s weight in variable-length
#' CAT. It can be `"magis"` or `"mcclarty"` (default). See details.
#' @param threshold threshold for `cat.type`.
#' Necessary only for progressive method.
#' @param rmax item maximum exposure rate
#' @param content.names vector with the contents of the test
#' @param content.props desirable proportion of each content in test, in
#' the same order of `content.names`
#' @param content.items vector indicating the content of each item
#' @param met.content content balancing method: `MCCAT` (default), `CCAT`
#' #' or `MMM`. See `content.balancing` for more information.
#' @param stop list with stopping rule and thresholds
#' \itemize{
#' \item `se` minimum standard error
#' \item `delta.theta` minimum absolute difference between current and previous theta
#' \item `hypo` minimum standard error reduction
#' \item `hyper` minimum standard error reduction after achieving `se`
#' \item `info` maximum information of an available item
#' \item `max.items` maximum number of items
#' \item `min.items` maximum number of items
#' \item `fixed` fixed number of items
#' }
#' @param progress shows progress bar
#'
#' @details
#'
#' For details about formula of selection methods, see \code{\link[simCAT]{select.item}}.
#'
#' @return a list with five elements
#' \itemize{
#' \item `score` estimated theta
#' \item `convergence` `TRUE` if the application ended before reaching the maximum test length
#' \item `theta.history` estimated theta after each item administration
#' \item `se.history` standard error after each item administration
#' \item `prev.resps` previous responses (administered items)
#' }
#'
#' @references
#'
#' Barrada, J. R., Olea, J., Ponsoda, V., & Abad, F. J. (2008). \emph{Incorporating randomness in the Fisher information for improving item-exposure control in CATs}. British Journal of Mathematical and Statistical Psychology, 61(2), 493–513. 10.1348/000711007X230937
#'
#' Leroux, A. J., & Dodd, B. G. (2016). \emph{A comparison of exposure control procedures in CATs using the GPC model}. The Journal of Experimental Education, 84(4), 666–685. 10.1080/00220973.2015.1099511
#'
#' Magis, D., & Barrada, J. R. (2017). \emph{Computerized adaptive testing with R: recent updates of the package catR}. Journal of Statistical Software, 76(Code Snippet 1). 10.18637/jss.v076.c01
#'
#' McClarty, K. L., Sperling, R. A., & Dodd, B. G. (2006). \emph{A variant of the progressive-restricted item exposure control procedure in computerized adaptive testing}. Annual Meeting of the American Educational Research Association, San Francisco
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

simCAT <- function(resps, bank, model = '3PL', start.theta = 0, sel.method = 'MFI',
                   cat.type = 'variable', acceleration = 1,
                   met.weight = 'mcclarty', threshold = .30, rmax = 1,
                   content.names = NULL, content.props = NULL,
                   content.items = NULL, met.content = 'MCCAT',
                   stop = list(se = .3, hypo = .015, hyper = Inf), progress = TRUE)
{

  # preparation ----
  bank <- data.frame(bank)
  rownames(bank) <- paste0('I', 1:nrow(bank))

  if (model == '3PL')
  {
    mod <- bank
    names(mod) <- c('a1', 'd', 'g')
    mod$d <- -mod$a1*mod$d

    mod <- mirtCAT::generate.mirt_object(mod, '3PL')
  }

  if (model == 'graded')
  {
    mod <- bank
    names(mod) <- c('a1', paste0('d', 1:(ncol(bank)-1)))
    mod[,2:ncol(mod)] <- -mod$a1*mod[,2:ncol(mod)]

    mod <- mirtCAT::generate.mirt_object(mod, 'graded')
  }

  # if(cat.type == 'variable' & is.null(stop$max.items))
  # {
  #   warning('The maximum number of items was set to be nrow(bank)')
  #   max.items <- nrow(bank)
  # }

  if(!is.null(stop$max.items))
    max.items <- stop$max.items

  if(is.null(stop$max.items))
  {
    max.items <- nrow(bank)
    if(cat.type == 'variable')
    {
      warning('The maximum number of items was set to be nrow(bank)')
      stop$max.items <- max.items
    }
  }

  results <- list()

  # objects -----------------------------------------------

  # theta and se
  score <- data.frame(matrix(ncol = 2))
  # did the CAT converge?  (for whole application)
  convergence <- c()
  # theta history (for whole application)
  theta.history <- list()
  # se history (for whole application)
  se.history <- list()
  # previous responses
  prev.resps <- list()

  # progress bar ----
  if(progress)
    bar <- utils::txtProgressBar(min = 0, max = nrow(resps), char = "|", style = 3)

  # simulation ----

  for (person in 1:nrow(resps))
    # for (person in 1:10)
  {

    # person <- 1

    ## verify item exposure -----------------------------------------

    # if it is the first person
    if (person == 1)
    {
      number_items_available <- 1:nrow(bank)
      bank_available <- bank

    } else {

      exposure <- exposure.rate(prev.resps, rownames(bank))

      # select available items
      number_items_available <- which (exposure$Freq <= rmax)
      bank_available <- bank[number_items_available,]
    }

    # simulation ----

    pattern <- rep(NA, nrow(bank))
    end <- list(stop = FALSE)
    administered <- NULL
    theta.cat <- theta.hist <- start.theta
    SE <- se.hist <- 1

    while(!end$stop)
    {

      # select item ----

      item_select <- select.item(
        bank = bank_available,
        model = model,
        theta = theta.cat,
        administered = administered,
        sel.method = sel.method,
        cat.type = cat.type,
        threshold = threshold,
        SE = SE,
        acceleration = acceleration,
        met.weight = met.weight,
        max.items = max.items,
        content.names = content.names,
        content.props = content.props,
        content.items = content.items[number_items_available],
        met.content = met.content
      )

      # item_select <- which(rownames(items_available) == item_select$name)

      # update administered items
      administered <- c(administered, item_select$item)

      # estimate theta
      # pattern: select from resps only the available items, and from them, the administered ones (and the person)
      # theta <- eap(
      #   pattern = resps[,number_items_available][person,administered],
      #   bank = bank_available[administered,]
      # )

      # pattern: select from resps only the available items, and from them, the administered ones (and the person)
      pattern[number_items_available][administered] <- resps[,number_items_available][person,administered]

      # estimate theta
      theta <- data.frame(
        mirt::fscores(
          object = mod,
          response.pattern = pattern,
          quadpts = 40,
          theta_lim = c(-4, 4)
        )
      )

      # update theta
      theta.cat <- theta$F1

      # delta theta
      delta.theta <- abs(theta.cat - theta.hist[length(theta.hist)])

      # update theta history
      theta.hist <- c(theta.hist, theta.cat)

      # update SE
      SE <- theta$SE

      # delta SE
      delta.se <- se.hist[length(se.hist)] - SE

      # update SE history
      se.hist <- c(se.hist, SE)

      # compute information for theta.cat
      info <- calc.info(bank = bank_available, theta = theta.cat, model = model)
      info[administered] <- 0
      info <- max(info)

      # stop the CAT? ----

      end <- stop.cat(
        rule = stop,
        current = list(
          se = SE,
          delta.theta = delta.theta,
          info = info,
          applied = length(administered),
          delta.se = delta.se
        )
      )

    }

    # store results
    score[person,] <- c(theta.cat, SE)
    convergence[person] <- end$convergence
    theta.history[[person]] <- theta.hist
    se.history[[person]] <- se.hist
    prev.resps[[person]] <- rownames(bank_available)[administered]

    # progress bar
    if(progress)
      utils::setTxtProgressBar(bar, person)

  }

  names (score) <- c('theta', 'SE')

  results <- list(
    score = score,
    convergence = convergence,
    theta.history = theta.history,
    se.history = se.history,
    prev.resps = prev.resps
  )
  return(results)
}

