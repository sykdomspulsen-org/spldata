#' Norwegian characters in unicode
#' @examples
#' print(spldata::nb)
#' @export nb
nb <- list()
nb$AA <- "\u00C5"
nb$aa <- "\u00E5"
nb$OE <- "\u00D8"
nb$oe <- "\u00F8"
nb$AE <- "\u00C6"
nb$ae <- "\u00E6"

#' Swedish characters in unicode
#' @examples
#' print(spldata::se)
#' @export se
se <- list()
se$OE <- "\u00D6"
se$oe <- "\u00F6"
se$AE <- "\u00C4"
se$ae <- "\u00E4"

#' An environment containing configuration variables
#'
#' Available configuration variables:
#' - border_nor (default 2020): The year in which Norwegian geographical boundaries were designated.
#' @examples
#' print(ls(spldata::config))
#' for(i in names(spldata::config)){
#'   cat(i, ":", spldata::config[[i]], "\n")
#' }
#' @export
config <- new.env()
config$border_nor <- 2020

#' Set options in the package config
#' @param border_nor The year in which Norwegian geographical boundaries were designated.
#' @returns Nothing. Side effect of setting the `config` environment.
#' @export
set_config <- function(border_nor = NULL){
  if(!is.null(border_nor)){
    stopifnot(border_nor %in% c(2020))
    config$border_nor <- border_nor
  }
}
