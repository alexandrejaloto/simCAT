#' @title CAT simulation
#' @name simCAT
#'
#' @description A CAT simulation with dicotomic items.
#'
#' @param resps a matrix with responses (0 and 1).
#' The number of columns
#'  corresponds to the number of items
#' @param bank matrix with item parameters (a, b, c)
#' @param start.theta first theta
#' @param sel.method item selection method
#' @param cat.type CAT with `variable` or `fixed` length
#' Necessary only for progressive method.
#' @param acceleration acceleration parameter.
#' Necessary only for progressive method.
#' @param threshold threshold for `cat.type`.
#' Necessary only for progressive method.
#' @param rmax item maximum exposure rate
#' @param content.names vector with the contents of the test
#' @param content.props desirable proportion of each content in test, in
#' the same order of `content.names`
#' @param content.items vector indicating the content of each item
#' @param met.content content balancing method: `MCCAT` (default) or `CCAT`
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
#'
#' @details
#'
#' @return
#'
#' @author Alexandre Jaloto
#'
#' @export

simCAT <- function(resps, bank, start.theta = 0, sel.method = 'MFI',
                   cat.type = 'variable', acceleration = 1,
                   threshold = .30, rmax = 1,
                   content.names = NULL, content.props = NULL,
                   content.items = NULL, met.content = 'MCCAT',
                   stop = list(se = .3, hypo = .015, hyper = Inf))
{

  # preparation ----

  bank <- data.frame(bank)
  rownames(bank) <- paste0('I', 1:nrow(bank))

  if(!is.null(stop$max.items))
    max.items <- stop$max.items

  results <- list()

  # objects -----------------------------------------------

  # theta and se
  score <- data.frame(matrix(ncol = 2))
  # did the CAT congerge?  (for whole application)
  convergence <- list()
  # theta history (for whole application)
  theta.history <- list()
  # se history (for whole application)
  se.history <- list()
  # previous responses
  prev.resps <- list()

  # progress bar ----
  bar <- txtProgressBar(min = 0, max = nrow(resps), char = "|", style = 3)

  # simulation ----

  for (person in 1:nrow(resps))
    # for (person in 1:10)
  {

    # person <- 1

    ## verificar a exposição dos itens -----------------------------------------

    # if it is the first person
    if (person == 1)
    {
      number_items_available <- 1:nrow(bank)
      items_available <- bank

    } else {

      exposure <- exposure.rate(prev.resps, rownames(bank))

      # select available items
      number_items_available <- which (exposure$Freq <= rmax)
      items_available <- bank[number_items_available,]
    }

    # simulation ----

    end <- list(stop = FALSE)
    administered <- NULL
    theta.cat <- theta.hist <- start.theta
    SE <- se.hist <- 1

    while(!end$stop)
    {

      # select item ----

      item_select <- select.item(
        bank = items_available,
        theta = theta.cat,
        administered = administered,
        sel.method = sel.method,
        cat.type = cat.type,
        threshold = threshold,
        SE = SE,
        acceleration = acceleration,
        max.items = max.items,
        content.names = content.names,
        content.props = content.props,
        content.items = content.items,
        met.content = met.content
      )

      # item_select <- which(rownames(items_available) == item_select$name)

      # update administered items
      administered <- c(administered, item_select$item)

      # estimate theta
      # pattern: select from resps only the available items, and from them, the administered ones (and the person)
      theta <- eap(
        pattern = resps[,number_items_available][person,administered],
        bank = items_available[administered,]
      )

      # update theta
      theta.cat <- theta$theta

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
      info <- calc.info(bank = items_available, theta = theta.cat)
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
    convergence[[person]] <- end$convergence
    theta.history[[person]] <- theta.hist
    se.history[[person]] <- se.hist
    prev.resps[[person]] <- rownames(items_available)[administered]

    # progress bar
    setTxtProgressBar(bar, person)

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

