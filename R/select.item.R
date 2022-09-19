#' @title Select next item
#' @name select.item
#'
#' @description Select next item to be administered
#' @param bank matrix with item parameters (a, b, c)
#' @param theta current theta
#' @param administered vector with administered items, `NULL` if it is the first
#' item
#' @param sel.method selection method
#' @param stop.type type of stopping rule (`precision` or `length`).
#' Necessary only for progressive method.
#' @param threshold threshold for `stop.type`
#' Necessary only for progressive method.
#' @param SE current standard error
#' Necessary only for progressive method, with `stop.type = "precision"`
#' @param acceleration acceleration parameter
#' #' Necessary only for progressive method.
#' @param max.items maximum number of items to be administered
#' Necessary only for progressive method, with `stop.type = "precision"`
#' @param content.names vector with the contents of the test
#' @param content.props desirable proportion of each content in test, in
#' the same order of `content.names`
#' @param content.items vector indicating the content of each item
#' @param met.content content balancing method: `MCCAT` (default) or `CCAT`
#'
#' @details
#'
#' @return A list with two elements
#'
#' `item` the number o the selected item in item bank
#' `name` name of the selected item (row name)
#'
#' @author Alexandre Jaloto
#'
#' @export

select.item <- function(bank, theta, administered = NULL,
                       sel.method = 'MFI', stop.type = 'precision',
                       threshold = .30, SE, acceleration = 1,
                       max.items = 45, content.names = NULL,
                       content.props = NULL, content.items = NULL,
                       met.content = 'MCCAT')
{

  # balanceamento de conteúdo ----

  # no content balancing
  if (is.null(content.props))
  {
    OUT <- administered

    # content balancing
  } else {

    # vector with items that will be unavailable due to content balancing
    OUT <- NULL
    OUT <- content.balancing(bank = bank,
                             administered = administered,
                             met.content = met.content,
                             content.names = content.names,
                             content.props = content.props,
                             content.items = content.items)
  }

  OUT <- unique(c(administered, OUT))
  items_available <- which (!((1:nrow(bank)) %in% OUT))

  # MFI ----
  if (sel.method == "MFI") {

    info <- calc.info(bank, theta)

    # select item
    select <- which(info == max(info[items_available]))

  }

  # random ----
  if (sel.method == "random") {

    select <- sample((1:nrow(bank))[items_available], 1)

  }

  #  progressive ----
  if (sel.method == "progressive") {

    n.administered <- length(administered)

    W <- 0

    if(n.administered > 0)
    {
      if(stop.type == 'length')
        W <- sum((1:n.administered)^acceleration)/sum((1:(threshold-1))^acceleration)

      if(stop.type == 'precision')
      {

        # this package uses 'se <- 1/(sqrt(1 + info))'
        info.theta <- 1/SE^2 - 1
        info.threshold <- 1/threshold^2 - 1

        # if you want to use 'se <- 1/(sqrt(info))', use the two following lines
        # info.theta <- 1/SE^2
        # info.threshold <- 1/threshold^2

        W <- max(info.theta/info.threshold, n.administered/(max.items-1))^acceleration
      }
    }

    info <- calc.info(bank, theta)

    # random values
    r <- runif(nrow(bank), 0, max(info))

    # progressive
    select <- (1 - W)*r + W*info

    # exclude unavailable items
    select[OUT] <- 0

    # select the highest 'select'
    select <- which(select == max(select))

    # if there is a tie, pick one randomly
    select <- select[sample(length(select), 1)]
  }

  # end of function ----

  it.select <- list(
    item = select,
    name = rownames(bank)[select]
  )

  return(it.select)

}
