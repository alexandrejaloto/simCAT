#' @title Check if the CAT ended
#' @name stop.cat
#' @description Check if any stopping rule has been achieved
#'
#' @param rule list with stopping rules
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
#' @param current list with current values
#' \itemize{
#' \item `se` current standard error
#' \item `delta.theta` absolute difference between current and previous theta
#' \item `info` maximum information of an available item for current theta
#' \item `applied` quantitative of applied items
#' \item `delta.se` standard error reduction
#' }
#'
#' @return A list with two elements:
#' \itemize{
#' \item `stop` `TRUE` if any stopping rule has been achieved
#' \item `convergence` logical. `FALSE` if the CAT stopped because it
#' achieved the maximum number of items. `TRUE` for any other case.
#' }
#' @author Alexandre Jaloto
#'
#' @export

stop.cat <- function(
  rule = list(
    se = NULL,
    delta.theta = NULL,
    hypo = NULL,
    hyper = NULL,
    info = NULL,
    max.items = NULL,
    min.items = NULL,
    fixed = NULL
  ),
  current = list(
    se = NULL,
    delta.theta = NULL,
    info = NULL,
    applied = NULL,
    delta.se = NULL
  )
){

  # if there is no item minimum, it will be 0
  if(is.null(rule$min.items))
    rule$min.items <- 0

  convergence <- FALSE

  if(!is.null(rule$se) & is.null(current$se))
    stop('Please inform currrent standard error')

  if(!is.null(rule$delta.theta) & is.null(current$delta.theta))
    stop('Please inform delta.theta')

  if(!(is.null(rule$hypo) | is.null(rule$hyper)) & is.null(current$delta.se))
    stop('Please inform delta.se')

  if((!is.null(rule$hypo) | !is.null(rule$hyper)) & is.null(rule$se))
    stop('Please inform current standard error')

  if(!is.null(rule$hypo) & is.null(rule$hyper))
    stop('Please inform hyper')

  if(is.null(rule$hypo) & !is.null(rule$hyper))
    stop('Please inform hypo')

  if(!is.null(rule$info) & is.null(current$info))
    stop('Please inform maximum information')

  # if(!is.null(current$applied) & (is.null(rule$max.items) & is.null(rule$fixed)))
  #   warning('You informed quantitative of applied items, but there is no rule related to it. Is it correct?')

  if(!is.null(rule$fixed) & (!is.null(rule$max.items) | !is.null(rule$delta.theta) | !is.null(rule$se) | !is.null(rule$delta.se) | !is.null(rule$info) | !is.null(rule$hypo)))
    stop('You chose fixed items and other variable-length rules. Choose one.')

  stop <- FALSE

  if(!is.null(rule$se))
    stop <- stop | current$se <= rule$se

  if(!is.null(rule$info))
    stop <- stop | current$info < rule$info

  if(!is.null(rule$delta.theta))
    stop <- stop | abs(current$delta.theta) <= rule$delta.theta

  if(!is.null(rule$hyper))
  {
    stop <- (current$delta.se <= rule$hyper) & current$se <= rule$se
    stop <- stop | current$delta.se <= rule$hypo
  }

  if (current$applied < rule$min.items)
    stop <- FALSE

  if (stop)
    convergence <- TRUE

  if (!is.null(rule$max.items)){
    if(is.null(current$applied))
      stop('Please inform quantitative of applied items')
    stop <- stop | current$applied >= rule$max.items
  }

  if (!is.null(rule$fixed)){
    if(is.null(current$applied))
      stop('Please inform quantitative of applied items')
    stop <- current$applied == rule$fixed
    if(stop)
      convergence <- TRUE
  }

  stop <- list(
    stop = stop,
    convergence = convergence
  )

  return(stop)

}
