#' @title Compute exposure rates
#' @name exposure.rate
#'
#' @description Calculate exposure rate of items in a bank
#' @param previous list with previous responses. Each element corresponds to a person and has the names of the applied items.
#' @param item.name vector with the name of all items in the bank
#'
#' @return `data.frame` with
#' \itemize{
#' \item `items` name of the items
#' \item `Freq` exposure rate
#' }
#' @author Alexandre Jaloto
#'
exposure.rate <- function(previous, item.name)
{

  # vector with all items administered in all previous CATs
  items_administered <- do.call('c', previous)

  # number of previous CATs
  number.cat <- length(previous)

  # exposure rate of each item
  rate <- data.frame(table(items_administered) / number.cat)

  # data.frame with items and join
  exposure <- data.frame(
    items = item.name
  )

  exposure <- dplyr::left_join(
    exposure,
    rate,
    by = c('items' = 'items_administered')
  )

  # change NA (not administered) with 0
  exposure[is.na(exposure$Freq), 'Freq'] <- 0

  return(exposure)

}
