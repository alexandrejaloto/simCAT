#' @title Content balancing
#' @name content.balancing
#'
#' @description Constricts the selection with content balancing (CCAT or MCCAT)
#'
#' @param bank matrix with item parameters (a, b, c)
#' @param administered vector with administered items, `NULL` if it is the first
#' item (default)
#' @param content.names vector with the contents of the test
#' @param content.props desirable proportion of each content in test, in
#' the same order of `content.names`
#' @param content.items vector indicating the content of each item
#' @param met.content content balancing method
#' \itemize{
#' \item `MCCAT` (default): the function picks all subgroups with
#' proportions most distant  from desirable.
#' \item `CCAT`: if there is any subgroup without
#' administered item, the function will randomly pick one.
#' If all subgroups has at least one applied item, the
#' function randomly picks one from those with the proportions most
#' distant from desirable.
#' \item `MMM`: based on the desired proportions of content, the algorithm
#' builds a sum-one cumulative distribution. Then, a random number with
#' uniform distribution between zero and one is drawn. This number
#' corresponds to an area in the cumulative distribution. It is from
#' the content located in this area that the content will be selected.
#' }
#'
#' @return A numeric vector with the items that will be excluded for
#' selection. That is, it returns the unavailable items. If all items
#' are available, it returns `NULL`.
#'
#' @author Alexandre Jaloto
#'
#' @export

content.balancing <- function(bank, administered = NULL, content.names,
                              content.props, content.items,
                              met.content = 'MCCAT')
{
  # preparação da função ----

  # verify if content information has been completely provided
  if(
    sum(
      is.null(
        content.names),
      is.null(
        content.props),
      is.null(
        content.items)
    ) %in% 1:2
  ) stop('You must inform content.names, content.props and content.items.')

  if(!(met.content %in% c('CCAT', 'MCCAT', 'MMM')))
    stop('met.content must be one of these: c("CCAT", "MCCAT", "MMM")')

  # how many contents?
  n.content <- length(content.names)

  # make the proportions sum 1
  if (sum(content.props) != 1)
    content.props <- content.props/sum(content.props)

  # again, how many contents?
  n.content <- length(content.names)

  # observed proportion of contents

  # if it is the first item
  if (is.null(administered)) {
    obs.prop <- rep(0, n.content)
    # if it is not the first item
  } else {
    obs.prop <- NULL
    for (i in 1:n.content)
      obs.prop[i] <- length(administered[content.items[administered] == content.names[i]])

    obs.prop <- obs.prop/sum(obs.prop)
  }

  # # desirable proportion
  # des.prop <- content.props
  #

  # CCAT ----
  if (met.content == 'CCAT')
  {
    # if there is still a subroup with unadministered item
    if (min(obs.prop) == 0) {
      # which content has 0?
      group.0 <- (1:n.content)[obs.prop == 0]
      # pick subgroup
      sel.group <- ifelse(
        length(group.0) == 1,
        group.0,
        sample(group.0, 1)
      )
    } else {
      # pick one from the most distant subgroups
      group.dist <- (1:n.content)[(content.props - obs.prop) == max(content.props - obs.prop)]
      sel.group <- ifelse(length(group.dist) == 1, group.dist,
                          sample(group.dist, 1))
    }
    OUT <- unique(c(administered, (1:length(content.items))[content.items != content.names[sel.group]]))
  }


  # MCCAT ----
  if (met.content == 'MCCAT')
  {
    # if there is still a subroup with unadministered item
    if (min(obs.prop) == 0) {
      # which content has 0?
      sel.group <- (1:n.content)[obs.prop == 0]
    } else {
      sel.group <- (1:n.content)[(content.props - obs.prop) == max(content.props - obs.prop)]
    }
    OUT <- unique(c(administered, (1:length(content.items))[!(content.items %in% content.names[sel.group])]))
  }

  # MMM ----
  if (met.content == 'MMM')
  {

    cuts <- 0

    for(i in 1:length(content.props))
      cuts <- c(cuts, cuts[i] + content.props[i])

    sel.group <- content.names[max(which (stats::runif(1) > cuts))]

    OUT <- unique(c(administered, (1:length(content.items))[!(content.items %in% content.names[sel.group])]))

  }
  # end of função ----

  # if all items are available
  if(length(OUT) == 0)
    OUT <- NULL

  return(OUT)

}
