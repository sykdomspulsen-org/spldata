norway_locations_msis_to_spldata_internal <- function(x){
  x <- as.numeric(x)
  if(is.na(x)){
    return(NA_character_)
  } else if(x==0){
    return("norge")
  } else if(x %in% c(21:22)){
    return(paste0("notmainlandcounty",x))
  } else if(x == 99){
    return(paste0("missingcounty",formatC(x,width=2, flag="0", format="fg")))
  } else if(x <= 98){
    return(paste0("county",formatC(x,width=2, flag="0", format="fg")))
  } else if(x %in% c(2100:2299)){
    return(paste0("notmainlandmunicip",formatC(x,width=4, flag="0", format="fg")))
  } else if(x == 9999){
    return(paste0("missingmunicip",formatC(x,width=4, flag="0", format="fg")))
  } else if(x <= 9998){
    return(paste0("municip",formatC(x,width=4, flag="0", format="fg")))
  } else if(x %in% c(30101:30115)){
    return(paste0("wardoslo",formatC(x,width=6, flag="0", format="fg")))
  } else if(x %in% c(30116:30117)){
    return(paste0("extrawardoslo",formatC(x,width=6, flag="0", format="fg")))
  } else if(x %in% c(30199)){
    return(paste0("missingwardoslo",formatC(x,width=6, flag="0", format="fg")))
  } else if(x %in% c(460101:460108)){
    return(paste0("wardbergen",formatC(x,width=6, flag="0", format="fg")))
  } else if(x %in% c(460199)){
    return(paste0("missingwardbergen",formatC(x,width=6, flag="0", format="fg")))
  } else if(x %in% c(500101:500104)){
    return(paste0("wardtrondheim",formatC(x,width=6, flag="0", format="fg")))
  } else if(x %in% c(500199)){
    return(paste0("missingwardtrondheim",formatC(x,width=6, flag="0", format="fg")))
  } else if(x %in% c(110301:110309)){
    return(paste0("wardstavanger",formatC(x,width=6, flag="0", format="fg")))
  } else if(x %in% c(110399)){
    return(paste0("missingwardstavanger",formatC(x,width=6, flag="0", format="fg")))
  } else {
    return(NA_character_)
  }
}


#' Converts MSIS' location values to spldata location_codes
#'
#' @param x A vector of numbers that correspond to MSIS' location values
#' @examples
#' norway_locations_msis_to_spldata(c(0, 3, 21, 50, 99, 9999, 301, 5001, 9999, 30101, 30116, 30199, NA))
#' @export
norway_locations_msis_to_spldata <- Vectorize(norway_locations_msis_to_spldata_internal, USE.NAMES=FALSE)
