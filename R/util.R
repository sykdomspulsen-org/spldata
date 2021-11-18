check_ref_to_new <- function(xref, xnew) {
  fail <- FALSE
  error_msg <- c()
  # reference files missing in new export
  trouble <- xref[!xref %in% xnew]
  if (length(trouble) > 0) {
    error_msg <- c(error_msg, crayon::red(glue::glue("\u2716 Missing in new: {trouble}\n\n")))
    fail <- TRUE
  }

  # new export files
  trouble <- xnew[!xnew %in% xref]
  if (length(trouble) > 0) {
    error_msg <- c(error_msg, crayon::red(glue::glue("\u2716 Something new: {trouble}\n\n")))
    fail <- TRUE
  }

  if (fail) {
    stop("\n", error_msg)
  }
}


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



zero_string <- function(n){
  x <- stringr::str_c(rep("0", n), collapse = "")
  x
}

