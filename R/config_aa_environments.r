#' Norwegian characters in unicode
#' @export nb
nb <- list()
nb$AA <- "\u00C5"
nb$aa <- "\u00E5"
nb$OE <- "\u00D8"
nb$oe <- "\u00F8"
nb$AE <- "\u00C6"
nb$ae <- "\u00E6"

#' Swedish characters in unicode
#' @export se
se <- list()
se$OE <- "\u00D6"
se$oe <- "\u00F6"
se$AE <- "\u00C4"
se$ae <- "\u00E4"

#' Config
#' @export
config <- new.env()
config$border <- 2020
config$use_current_year_as_1900_pop <- FALSE

#' set_config
#' @param border The year
#' @param use_current_year_as_1900_pop Replaces the year 1900's population data with the current year's population data
#' @export
set_config <- function(border = 2020, use_current_year_as_1900_pop = FALSE){
  stopifnot(border %in% c(2020))
  stopifnot(use_current_year_as_1900_pop %in% c(T,F))
  config$border <- border
  config$use_current_year_as_1900_pop <- use_current_year_as_1900_pop

}
