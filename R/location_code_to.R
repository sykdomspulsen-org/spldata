#' Convert location_code to granularity_geo
#' @param x Datatable
#' @param location_reference A location reference data.table
#' @returns Character vector the same length as x, containing the corresponding granularity_geo
#' @examples
#' spldata::location_code_to_granularity_geo(c("norge", "county03"))
#' @export
location_code_to_granularity_geo <- function(x, location_reference = NULL){

  granularity_geo <- NULL

  if(is.null(location_reference)){
    retval <- stringr::str_extract(x, "^[a-z]+")
    retval[retval=="norge"] <- "nation"
    return(retval)
  } else {
    return(location_reference[data.table(location_code=x), on = "location_code", granularity_geo])
  }
}


#' Convert location_code to iso3
#' @param x datatable
#' @returns Character vector the same length as x, containing the corresponding iso3
#' @examples
#' spldata::location_code_to_iso3(c("norge", "county03"))
#' @export
location_code_to_iso3 <- function(x){
  return(rep("nor", length(x)))
}
