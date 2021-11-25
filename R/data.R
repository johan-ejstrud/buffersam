#' Element
#'
#' Example of element dataset that is used by
#' \code{\link{buffered_random_sampling}}.
#'
#' @format A data frame with four or five variables:
#' \describe{
#' \item{\code{elementId}}{A unique string identifying the element.}
#' \item{\code{stratum}}{Name of the stratum that the element belongs to.}
#' \item{\code{latitude}}{Latitude in decimal degrees.}
#' \item{\code{longitude}}{Longitude in decimal degrees.}
#' \item{\code{preselect_probability}}{(Optional) used to sample proportionally
#' when preselecting elements that belong to multiple strata. See
#' \code{vignette("buffersam")}.}
#' }
"element"

#' Stratum
#'
#' Example of statum dataset that is used by
#' \code{\link{buffered_random_sampling}}.
#'
#' @format A data frame with three variables:
#' \describe{
#' \item{\code{stratum}}{A unique string identifying the stratum.}
#' \item{\code{area}}{Area of stratum in \eqn{m^2}.}
#' \item{\code{n_stations}}{Required number of stations in stratum.}
#' }
"stratum"
