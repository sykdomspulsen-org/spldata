#' Norwegian characters in unicode
#' @examples
#' print(nb)
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
#' print(se)
#' @export se
se <- list()
se$OE <- "\u00D6"
se$oe <- "\u00F6"
se$AE <- "\u00C4"
se$ae <- "\u00E4"

#' Config
#' @examples
#' print(ls(config))
#' @export
config <- new.env()
config$border <- 2020

#' Set config
#' @param border The year
#' @returns Nothing. Side effect of setting the `config` environment.
#' @export
set_config <- function(border = 2020){
  stopifnot(border %in% c(2020))
  config$border <- border
}
