#' @title Select next item
#' @name select.item
#'
#' @description Select next item to be administered
#'
#' @param bank matrix with item parameters (a, b, c)
#' @param model may be `3PL` or `graded`
#' @param theta current theta
#' @param administered vector with administered items, `NULL` if it is the first
#' item
#' @param sel.method item selection method: may be `MFI`, `progressive`
#'  or `random`
#' @param cat.type CAT with `variable` or `fixed` length.
#' Necessary only for progressive method.
#' @param threshold threshold for `cat.type`.
#' Necessary only for progressive method.
#' @param SE current standard error.
#' Necessary only for progressive method, with `cat.type = "variable"`
#' @param acceleration acceleration parameter.
#' Necessary only for progressive method.
#' @param met.weight the procedure to calculate the `progressive`'s weight in variable-length
#' CAT. It can be `"magis"` or `"mcclarty"` (default). See details.
#' @param max.items maximum number of items to be administered.
#' Necessary only for progressive method, with `cat.type = "variable"`
#' @param content.names vector with the contents of the test
#' @param content.props desirable proportion of each content in test, in
#' the same order of `content.names`
#' @param content.items vector indicating the content of each item
#' @param met.content content balancing method: `MCCAT` (default), `CCAT`
#' or `MMM`. See `content.balancing` for more information.
#'
#' @details
#'
#' In the progressive (Revuelta & Ponsoda, 1998), the administered item is the one that has the highest weight. The weight of the
#' item `i` is calculated as following:
#'
#' \deqn{W_i = (1-s)R_i+sI_i}
#'
#' where `R` is a random number between zero and the maximum information of an
#' item in the bank
#' for the current theta, `I` is the item information and `s` is the importance
#' of the component. As
#' the application progresses, the random component loses importance. There are some
#' ways to calculate `s`.
#' For fixed-length CAT, Barrada et al. (2008) uses
#'
#' \deqn{s = 0}
#'
#' if it is the first item of the test. For the other administering items,
#'
#' \deqn{s = \frac{\sum_{f=1}^{q}{(f-1)^k}}{\sum_{f=1}^{Q}{(f-1)^k}}}
#'
#' where `q` is the number of the item position in the test, `Q` is the
#' test length and `k` is the acceleration parameter. `simCAT` package uses these two
#' equations for fixed-length CAT. For variable-length, `simCAT` package can
#' use `"magis"` (Magis & Barrada, 2017):
#'
#' \deqn{s = max [ \frac{I(\theta)}{I_{stop}},\frac{q}{M-1}]^k}
#'
#' where \eqn{I(\theta)} is the item information for the current theta,
#'  \eqn{I_{stop}} is the information corresponding to the stopping error
#'  value, and `M` is the maximum length of the test. `simCAT` package uses as
#'  default `"mcclarty"` (adapted from McClarty et al., 2006):
#'
#' \deqn{s = (\frac{SE_{stop}}{SE})^k}
#'
#' where `SE` is the standard error for the current theta, \eqn{SE_{stop}} is
#' the stopping error value.
#'
#' @references
#' Barrada, J. R., Olea, J., Ponsoda, V., & Abad, F. J. (2008). \emph{Incorporating randomness in the Fisher information for improving item-exposure control in CATs}. British Journal of Mathematical and Statistical Psychology, 61(2), 493–513. 10.1348/000711007X230937
#'
#' Leroux, A. J., & Dodd, B. G. (2016). \emph{A comparison of exposure control procedures in CATs using the GPC model}. The Journal of Experimental Education, 84(4), 666–685. 10.1080/00220973.2015.1099511
#'
#' Magis, D., & Barrada, J. R. (2017). \emph{Computerized adaptive testing with R: recent updates of the package catR}. Journal of Statistical Software, 76(Code Snippet 1). 10.18637/jss.v076.c01
#'
#' McClarty, K. L., Sperling, R. A., & Dodd, B. G. (2006). \emph{A variant of the progressive-restricted item exposure control procedure in computerized adaptive testing}. Annual Meeting of the American Educational Research Association, San Francisco
#'
#' Revuelta, J., & Ponsoda, V. (1998). \emph{A comparison of item exposure control methods in computerized adaptive testing}. Journal of Educational Measurement, 35(4), 311–327. http://www.jstor.org/stable/1435308
#'
#' @return A list with two elements
#' \itemize{
#' \item `item` the number o the selected item in item bank
#' \item `name` name of the selected item (row name)
#' }
#' @author Alexandre Jaloto
#'
#' @export

select.item <- function(bank, model = '3PL', theta, administered = NULL,
                        sel.method = 'MFI', cat.type = 'variable',
                        threshold = .30, SE,
                        acceleration = 1, met.weight = 'mcclarty',
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

    info <- calc.info(bank, theta, model)

    # select item
    select <- which(info == max(info[items_available]))

    # if there is a tie, pick one randomly
    select <- select[sample(length(select), 1)]
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
      if(cat.type == 'fixed')
        W <- (sum(((2:(n.administered+1))-1)^acceleration))/(sum(((2:threshold)-1)^acceleration))

      if(cat.type == 'variable')
      {
        # this package uses 'se <- 1/(sqrt(1 + info))'
        info.theta <- 1/SE^2 - 1
        info.threshold <- 1/threshold^2 - 1

        # if you want to use 'se <- 1/(sqrt(info))', use the two following lines
        # info.theta <- 1/SE^2
        # info.threshold <- 1/threshold^2

        if (met.weight == 'mcclarty')
          W <- (threshold/SE)^acceleration

        if (met.weight == 'magis')
        W <- max(info.theta/info.threshold, n.administered/(max.items-1))^acceleration

        }
    }

    info <- calc.info(bank, theta)

    # random values
    r <- stats::runif(nrow(bank), 0, max(info[items_available]))

    # progressive
    select <- (1 - W)*r + W*info

    # exclude unavailable items
    select[OUT] <- NA

    # select the highest 'select'
    select <- which(select == max(select, na.rm = TRUE))

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
