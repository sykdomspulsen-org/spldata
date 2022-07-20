location_code_to_granularity_geo.data.table <- function(x, location_reference = NULL){

  granularity_geo <- NULL

  if(is.null(location_reference)){
    retval <- stringr::str_extract(x[["location_code"]], "^[a-z]+")
    retval[retval=="norge"] <- "nation"
    return(retval)
  } else {
    return(location_reference[x[["location_code"]], on = "location_code", granularity_geo])
  }
}

location_code_to_granularity_geo.default <- function(x, location_reference = NULL){

  granularity_geo <- NULL

  if(is.null(location_reference)){
    retval <- stringr::str_extract(x, "^[a-z]+")
    retval[retval=="norge"] <- "nation"
    return(retval)
  } else {
    return(location_reference[data.table(location_code=x), on = "location_code", granularity_geo])
  }
}

#' Convert location_code to granularity_geo
#' @param x Either a vector, or a data.frame/data.table containing a column called "location_code"
#' @param location_reference A location reference data.table
#' @returns Character vector the same length as x, containing the corresponding granularity_geo
#' @examples
#' spldata::location_code_to_granularity_geo(c("norge", "county03"))
#' @export
location_code_to_granularity_geo <- function(x, location_reference = NULL){
  UseMethod("location_code_to_granularity_geo")
}

location_code_to_iso3.data.table <- function(x){
  return(rep("nor", nrow(x)))
}

location_code_to_iso3.default <- function(x){
  return(rep("nor", length(x)))
}

#' Convert location_code to iso3
#' @param x Either a vector, or a data.frame/data.table containing a column called "location_code"
#' @returns Character vector the same length as x, containing the corresponding iso3
#' @examples
#' spldata::location_code_to_iso3(c("norge", "county03"))
#' @export
location_code_to_iso3 <- function(x){
  UseMethod("location_code_to_iso3")
}

