# location names ----

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
#' @import data.table
"denmark_locations_names_b2020"

dnk_loc_names <- function(x_year_end) {
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
#' @import data.table
"sweden_locations_names_b2020"

swe_loc_names <- function(x_year_end) {
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
#' @import data.table

"finland_locations_names_b2020"

fin_loc_names <- function(x_year_end) {
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
#' @source \url{https://en.wikipedia.org/wiki/NUTS_statistical_regions_of_Iceland}
#' @import data.table
"iceland_locations_names_b2020"

isl_loc_names <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "locations", "locations_iceland_b2020.xlsx", package = "spldata"))
  setDT(d)
  return(d)
}




# population ----

#' Population in Denmark
#'
"denmark_population_by_age_b2020"

dnk_population_by_age <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "population", "population_denmark_b2020.xlsx", package = "spldata"))
  setDT(d)
  d1 <- copy(d)
  d1[, year := 2021]
  d <- rbind(d,d1)
  return(d)
}

#' Population in Sweden
#'
"sweden_population_by_age_b2020"

swe_population_by_age <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "population", "population_sweden_b2020.xlsx", package = "spldata"))
  setDT(d)
  d1 <- copy(d)
  d1[, year := 2021]
  d <- rbind(d,d1)
  return(d)
}

#' Population in Finland
#'
"finland_population_by_age_b2020"

fin_population_by_age <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "population", "population_finland_b2020.xlsx", package = "spldata"))
  setDT(d)
  d1 <- copy(d)
  d1[, year := 2021]
  d <- rbind(d,d1)
  return(d)
}

#' Population in Iceland
#'
"iceland_population_by_age_b2020"

isl_population_by_age <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "population", "population_iceland_b2020.xlsx", package = "spldata"))
  setDT(d)
  d1 <- copy(d)
  d1[, year := 2021]
  d <- rbind(d,d1)
  return(d)
}










