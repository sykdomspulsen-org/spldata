#' Names of areas in Denmark that existed in 2020.
#'
#' @format
#' \describe{
#' \item{iso3}{ISO3 code.}
#' \item{granularity_geo}{Nation (NUTS1), region (NUTS2)}
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' }
#' @source \url{https://en.wikipedia.org/wiki/NUTS_statistical_regions_of_Denmark}
"denmark_locations_names_b2020"

gen_denmark_locations_names <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "locations", "locations_denmark_b2020.xlsx", package = "spldata"))
  setDT(d)
  return(d)
}


#' Names of areas in Sweden that existed in 2020.
#'
#' @format
#' \describe{
#' \item{iso3}{ISO3 code.}
#' \item{granularity_geo}{Nation (NUTS1), county (NUTS3)}
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' }
#' @source \url{https://en.wikipedia.org/wiki/NUTS_statistical_regions_of_Sweden}
"sweden_locations_names_b2020"

gen_sweden_locations_names <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "locations", "locations_sweden_b2020.xlsx", package = "spldata"))
  setDT(d)
  return(d)
}


#' Names of areas in Finland that existed in 2020.
#'
#' @format
#' \describe{
#' \item{iso3}{ISO3 code.}
#' \item{granularity_geo}{nation, hospital district}
#' \item{location_code}{Location code (hospital district.}
#' \item{location_name}{Location name (hospital district.}
#' }
#' @source \url{https://en.wikipedia.org/wiki/NUTS_statistical_regions_of_Finland}
"finland_locations_names_b2020"

gen_finland_locations_names <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "locations", "locations_finland_b2020.xlsx", package = "spldata"))
  setDT(d)
  return(d)
}



#' Names of areas in Iceland that existed in 2020.
#'
#' @format
#' \describe{
#' \item{iso3}{Location code.}
#' \item{granularity_geo}{nation}
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' }
#' @source \url{}
"iceland_locations_names_b2020"

gen_iceland_locations_names <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "locations", "locations_iceland_b2020.xlsx", package = "spldata"))
  setDT(d)
  return(d)
}




# population ---- #

# gen_denmark_population_by_age <- function(x_year_end) {
#   stopifnot(x_year_end == 2020)
#
#   d <- readxl::read_excel(system.file("rawdata", "population", "population_denmark_b2020.xlsx", package = "spldata"))
#   setDT(d)
#   d1 <- copy(d)
#   d1[, year := 2021]
#   d <- rbind(d,d1)
#   return(d)
# }
#
# gen_sweden_population_by_age <- function(x_year_end) {
#   stopifnot(x_year_end == 2020)
#
#   d <- readxl::read_excel(system.file("rawdata", "population", "population_sweden_b2020.xlsx", package = "spldata"))
#   setDT(d)
#   d1 <- copy(d)
#   d1[, year := 2021]
#   d <- rbind(d,d1)
#   return(d)
# }
#
# gen_finland_population_by_age <- function(x_year_end) {
#   stopifnot(x_year_end == 2020)
#
#   d <- readxl::read_excel(system.file("rawdata", "population", "population_finland_b2020.xlsx", package = "spldata"))
#   setDT(d)
#   d1 <- copy(d)
#   d1[, year := 2021]
#   d <- rbind(d,d1)
#   return(d)
# }
#
# gen_iceland_population_by_age <- function(x_year_end) {
#   stopifnot(x_year_end == 2020)
#
#   d <- readxl::read_excel(system.file("rawdata", "population", "population_iceland_b2020.xlsx", package = "spldata"))
#   setDT(d)
#   d1 <- copy(d)
#   d1[, year := 2021]
#   d <- rbind(d,d1)
#   return(d)
# }
#









