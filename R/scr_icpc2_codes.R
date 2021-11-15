#' ICPC-2: The International Classification of Primary Care (Norwegian, English)
#'
#' Norwegian data is from 2020, English data is from 2018. (Link to be included)
#' @format
#' \describe{
#' \item{Code}{ICPC2 code}
#' \item{title_nb}{Description of the code in Norwegian.}
#' \item{title_en}{Description of the code in English.}
#' }
#' @import data.table
#' @source \url{https://ehelse.no/kodeverk/icpc-2.den-internasjonale-klassifikasjonen-for-primaerhelsetjenesten}
"icpc2_codes"


gen_icpc2_codes <- function(description_only = T) {

  # load norwegian data
  d_nb <- readxl::read_excel(system.file("rawdata","icpc2_code", "icpc2_code_nb_2020.xlsx", package = "spldata"))
  setDT(d_nb)
  setnames(d_nb, c('code', 'short_title_nb', 'long_title_nb'))

  # load english data
  d_en <- readxl::read_excel(system.file("rawdata", "icpc2_code", "icpc2_code_en_2018.xlsx", package = "spldata"))
  setDT(d_en)
  setnames(d_en, 1:3, c('code', 'short_title_en', 'long_title_en'))

  # join these two
  d_wide <- merge(d_nb, d_en, by="code", all=T)

  if(description_only == F){
    d <- d_wide
  }

  d <- d_wide[, c('code', 'short_title_nb', 'short_title_en')]

  # rename
  setnames(d, c('code', 'title_nb', 'title_en'))

  return(d)
}

