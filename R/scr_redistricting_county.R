# county (require pop) ----

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
