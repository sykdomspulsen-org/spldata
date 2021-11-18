#' Redistricting in Norway (2020 borders).
#'
#' This dataset is used to transfer "original" datasets to the 2020 borders.
#'
#' Last updated 2021-11-15
#'
#' @format
#' \describe{
#' \item{location_code_current}{The location code per today.}
#' \item{location_code_original}{The location code as of 'year'.}
#' \item{calyear}{The year corresponding to 'county_code_original'.}
#' \item{weighting}{The weighting that needs to be applied.}
#' \item{granularity_geo}{nation/county/municip/wardbergen/wardoslo/wardstavanger/wardtrondheim/missingwardbergen/missingwardoslo/missingwardstavanger/missingwardtrondheim/notmainlandcounty/notmainlandmunicip/missingcounty}
#' }
"norway_locations_redistricting_b2020"

#' All redistricting in Norway (programable borders).
#'
#' @param include_year Do you want to include redistricting by year?
#' @param border The border year
#' @examples
#' norway_locations_redistricting()
#' @export
# f_redistricting <- function(
#   include_year = TRUE,
#   border = spldata::config$border
#   ){
#
#
#   require_namespace("tidyr")
#   stopifnot(border==2020)
#   d <- copy(spldata::norway_locations_redistricting_b2020)
#
#
#   if(include_year){
#     d[, original_calyear_max := max(calyear), by=.(location_code_original)]
#     d_original_max <- copy(d[calyear==original_calyear_max])
#     d_original_max[, uncount := 2030-1974]
#     d_original_max <- copy(tidyr::uncount(d_original_max, uncount))
#     d_original_max[, calyear := 1975 + 1:.N, by=.(location_code_original)]
#     d_original_max[, original_calyear_max := NULL]
#
#     d_original_max[
#       d,
#       on = c("location_code_original", "calyear"),
#       already_included := TRUE
#     ]
#     stats::xtabs(~d_original_max$already_included, addNA = T)
#     d_original_max <- d_original_max[is.na(already_included)]
#     d_original_max[, already_included := NULL]
#     d[, original_calyear_max := NULL]
#
#     d <- rbindlist(list(d, d_original_max))
#     setorder(d, location_code_current, location_code_original, calyear)
#   } else {
#     d <- unique(d[,.(location_code_current, location_code_original, granularity_geo)])
#   }
#   return(d)
# }





norway_locations_hierarchy_internal <- function(from, to, include_to_name = FALSE, border = fhidata::config$border){
  stopifnot(border==2020)
  stopifnot(from %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger",
    "municip",
    "baregion",
    "county",
    "region",
    "faregion",
    "notmainlandmunicip",
    "notmainlandcounty",
    "missingmunicip",
    "missingcounty"
  ))
  stopifnot(to %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger",
    "municip",
    "baregion",
    "county",
    "region",
    "faregion",
    "notmainlandmunicip",
    "notmainlandcounty",
    "missingmunicip",
    "missingcounty"
  ))
  if(border==2020){
    d <- fhidata::norway_locations_hierarchy_all_b2020
  }

  if(from %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger"
  )){
    col_from <- "ward_code"
  } else if(from %in% c(
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger"
  )){
    col_from <- "missingward_code"
  } else {
    col_from <- paste0(from,"_code")
  }

  if(to %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger"
  )){
    col_to <- "ward_code"
    col_to_name <- "ward_name"
  } else if(to %in% c(
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger"
  )){
    col_to <- "missingward_code"
    col_to_name <- "missingward_name"
  } else {
    col_to <- paste0(to,"_code")
    col_to_name <- paste0(to,"_name")
  }

  if(include_to_name){
    d <- d[, c(col_from, col_to, col_to_name), with=F]
  } else {
    d <- d[, c(col_from, col_to), with=F]
  }
  d <- stats::na.omit(d)

  if(from %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger"
  )){
    d <- d[grep(paste0("^",from), get(col_from))]
    setnames(d, col_from, paste0(from,"_code"))
  }

  if(to %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger"
  )){
    d <- d[grep(paste0("^",to), get(col_to))]
    setnames(d, col_to, paste0(to,"_code"))
  }
  d <- unique(d)

  if(ncol(d)==2){
    setnames(d, c("from_code","to_code"))
  } else {
    setnames(d, c("from_code","to_code", "to_name"))
  }

  return(d)
}



#' Hierarchies in Norway (programmable borders).
#'
#' @param from wardoslo, wardbergen, wardtrondheim, wardstavanger, municip, baregion, county, region, faregion, notmainlandmunicip, notmainlandcounty, missingmunicip, missingcounty
#' @param to wardoslo, wardbergen, wardtrondheim, wardstavanger, municip, baregion, county, region, faregion, notmainlandmunicip, notmainlandcounty, missingmunicip, missingcounty
#' @param include_to_name Do you want to include the name of the 'to' location?
#' @param border The border year
#' @examples
#' norway_locations_hierarchy(from="wardoslo", to="county")
#' norway_locations_hierarchy(from="municip", to="baregion")
#' @export
# norway_locations_hierarchy <- function(from, to, include_to_name = FALSE, border = fhidata::config$border){
#   plans <- expand.grid(
#     from = from,
#     to = to,
#     stringsAsFactors = FALSE
#   )
#   retval <- vector("list", length=nrow(plans))
#   for(i in seq_along(retval)){
#     retval[[i]] <- norway_locations_hierarchy_internal(from=plans$from[i], to=plans$to[i], include_to_name, border)
#   }
#   retval <- unique(rbindlist(retval))
#   retval
# }


