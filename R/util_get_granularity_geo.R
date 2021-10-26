#' get_granularity_geo
#' @param x Datatable
#' @param location_reference A location reference data.table
#' @export
get_granularity_geo <- function(x, location_reference = NULL){
  if(is.null(location_reference)){
    retval <- stringr::str_extract(x, "^[a-z]+")
    retval[retval=="norge"] <- "nation"
    return(retval)
  } else {
    return(location_reference[data.table(location_code=x), on = "location_code", granularity_geo])
  }
}
