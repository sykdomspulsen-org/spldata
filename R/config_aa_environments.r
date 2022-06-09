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

#' set_config
#' @param border The year
#' @export
set_config <- function(border = 2020){
  stopifnot(border %in% c(2020))
  config$border <- border
}
