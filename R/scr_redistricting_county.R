# county (require pop) ----



# Creates the norway_county_merging (fylkesammenslaaing) data.table
redistricting_county <- function(x_year_end = 2020, x_year_start = 2000) {
  # variables used in data.table functions in this function
  . <- NULL
  year_start <- NULL
  municip_code <- NULL
  municip_code_current <- NULL
  level <- NULL
  county_code <- NULL
  region_code <- NULL
  year_end <- NULL
  municip_name <- NULL
  municip_code_end <- NULL
  county_name <- NULL
  region_name <- NULL
  realEnd <- NULL
  weighting <- NULL
  imputed <- NULL
  pop <- NULL
  location_code <- NULL
  county_code_original <- NULL
  municip_code_original <- NULL
  county_code_current <- NULL
  weighting_denominator_from_original <- NULL
  border_end <- NULL
  border_start <- NULL
  municip_code_end_new <- NULL
  weighting_new <- NULL
  # end


  # x_year_end <- 2020
  # x_year_start <- 2000

  # generate municip
  municips <- redistricting_municip(x_year_end = x_year_end,x_year_start = x_year_start)

  # population ----

  pop_municip <- population_by_age(x_year_end = x_year_end, original = TRUE)
  pop_municip <- pop_municip[imputed == FALSE,
                             .(population = sum(population)),
                             keyby = .(municip_code, year)]

  # imputed
  pops1 <- population_by_age(x_year_end = x_year_end)
  pops1 <- pops1[imputed == TRUE & level == "municip",
                 .(population = sum(population)),
                 keyby = .(municip_code = location_code, year)]

  pops <- rbind(pop_municip, pops1)

  x <- merge(
    municips,
    pops,
    by.x = c("municip_code_original", "year"),
    by.y = c("municip_code", "year"),
  )
  x[, county_code_original := stringr::str_sub(municip_code_original, 1, 9)]
  x[, county_code_current := stringr::str_sub(municip_code_current, 1, 9)]

  x[, county_code_original := stringr::str_replace(county_code_original, "municip", "county")]
  x[, county_code_current := stringr::str_replace(county_code_current, "municip", "county")]

  x[, weighting := weighting * pop]
  x <- x[, .(
    weighting = sum(weighting)
  ), keyby = .(
    year,
    county_code_original,
    county_code_current
  )]
  x[, weighting_denominator_from_original := sum(weighting), by = .(county_code_original, year)]
  x[, weighting := weighting / weighting_denominator_from_original]
  x[, weighting_denominator_from_original := NULL]

  x

  for (i in 1:30) {
    temp <- x[year == min(year)]
    temp[, year := year - 1]
    x <- rbind(temp, x)
  }

  extra_years <- max(x$year) + c(1:10)
  for (i in extra_years) {
    temp <- x[year == max(year)]
    temp[, year := i]
    x <- rbind(x, temp)
  }

  setcolorder(
    x,
    c(
      "county_code_current",
      "county_code_original",
      "year",
      "weighting"
    )
  )
  setnames(x, c("location_code_current", "location_code_original", "year", "weighting"))
  x

  return(x)
}





# gen_norway_locations_redistricting_county <- function(x_year_end){
#   stopifnot(x_year_end==2020)
#
#   d <- gen_norway_locations_redistricting_county_internal(
#     x_year_end = x_year_end,
#     x_year_start = 1910
#   )
#   setcolorder(
#     d,
#     c(
#       "county_code_current",
#       "county_code_original",
#       "year",
#       "weighting"
#     )
#   )
#   setnames(d, c("location_code_current", "location_code_original", "year", "weighting"))
#
#   return(d)
# }


redistricting_missingcounty <- function(
  x_year_end,
  x_year_start = 1940){

  retval <- data.table(location_code_current = "missingcounty99",
                       location_code_original = "missingcounty99",
                       year = seq(x_year_start, x_year_end+10, by = 1),
                       weighting = 1)

  return(retval)

}


redistricting_notmainlandcounty<- function(
  x_year_end,
  x_year_start = 1940){

  sv <- data.table(location_code_current = "notmainlandcounty21",
                   location_code_original = "notmainlandcounty21",
                   year = seq(x_year_start, x_year_end, by = 1),
                   weighting = 1)
  jm <- data.table(location_code_current = "notmainlandcounty22",
                   location_code_original = "notmainlandcounty22",
                   year = seq(x_year_start, x_year_end, by = 1),
                   weighting = 1)

  retval <- rbind(sv, jm)

  return(invisible(retval))

}
