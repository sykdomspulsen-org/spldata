#' All redistricting in Norway
#'
#' This function returns a dataset that is used to transfer "original" datasets
#' to the 2020 borders.
#'
#' @param border The year in which Norwegian geographical boundaries were designated.
#' @returns
#' \describe{
#' \item{location_code_current}{The location code per today.}
#' \item{location_code_original}{The location code as of 'year'.}
#' \item{calyear}{The year corresponding to 'county_code_original'.}
#' \item{weighting}{The weighting that needs to be applied.}
#' \item{granularity_geo}{nation/county/municip/wardbergen/wardoslo/wardstavanger/wardtrondheim/missingwardbergen/missingwardoslo/missingwardstavanger/missingwardtrondheim/notmainlandcounty/notmainlandmunicip/missingcounty}
#' }
#' @examples
#' spldata::nor_locations_redistricting()
#' @export
nor_locations_redistricting <- function(
    border = spldata::config$border
){

  stopifnot(border==2020)
  x <- get0("nor_locations_redistricting_b2020", envir = asNamespace("spldata"))
  d <- copy(x)
  return(d)
}

