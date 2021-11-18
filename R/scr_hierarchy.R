#' Hierarchy of different levels in Norway (2020 borders).
#'#'
#' Last updated 2021-02-15
#'
#' @format
#' \describe{
#' \item{municip_code}{The location code per today.}
#' \item{municip_name}{The location code as of 'year'.}
#' \item{baregion_code}{The location code per today.}
#' \item{baregion_name}{The location code as of 'year'.}
#' \item{county_code}{The location code as of 'year'.}
#' \item{county_name}{The location code as of 'year'.}
#' \item{region_code}{The location code as of 'year'.}
#' \item{region_name}{The location code as of 'year'.}
#' \item{faregion_name}{The location code as of 'year'.}
#' \item{faregion_code}{The location code as of 'year'.}
#' \item{ward_code}{The location code as of 'year'.}
#' \item{ward_name}{The location code as of 'year'.}
#' \item{missingward_code}{The location code as of 'year'.}
#' \item{missingward_name}{The location code as of 'year'.}
#' \item{notmainlandmunicip_code}{The location code as of 'year'.}
#' \item{notmainlandmunicip_name}{The location code as of 'year'.}
#' \item{notmainlandcounty_code}{The location code as of 'year'.}
#' \item{notmainlandcounty_name}{The location code as of 'year'.}
#' \item{missingmunicip_code}{The location code as of 'year'.}
#' \item{missingmunicip_name}{The location code as of 'year'.}
#' \item{missingcounty_code}{The location code as of 'year'.}
#' \item{missingcounty_name}{The location code as of 'year'.}
#' }
"norway_locations_hierarchy_all_b2020"

# hierarchy_municip <- function(
#   x_year_end,
#   x_year_start = 1940
# ){
#
#   d <- gen_norway_locations_redistricting_municip_internal(
#     x_year_end = x_year_end,
#     x_year_start = 1940,
#     include_extra_vars = T
#   )[year==max(year)]
#   d[, year := NULL]
#   d[, weighting := NULL]
#
#
#   # ba ---- #
#   ba <- data.table(readxl::read_excel(system.file("rawdata", "locations", "baregioner_2020.xlsx", package = "fhidata")))
#   setnames(
#     ba,
#     1:2,
#     c(
#       "municip",
#       "ba"
#     )
#   )
#   ba[, municip_code := paste0(
#     "municip",
#     formatC(as.numeric(
#       stringr::str_extract(municip, "^[0-9]+")),
#       width=4,
#       flag=0
#     ))
#   ]
#   ba[, baregion_code := paste0(
#     "baregion",
#     formatC(as.numeric(
#       stringr::str_extract(ba, "^[0-9]+")),
#       width=3,
#       flag=0
#     ))
#   ]
#   ba[, baregion_name := stringr::str_remove_all(ba, "^[0-9]+ ")]
#   d[
#     ba,
#     on="municip_code_current==municip_code",
#     baregion_code := baregion_code
#   ]
#   d[
#     ba,
#     on="municip_code_current==municip_code",
#     baregion_name := baregion_name
#   ]
#
#   return(invisible(d))
# }

# hierarchy_ward <- function(
#   x_year_end = 2020,
#   x_year_start = 1940
# ){
#
#   d <- redistricting_ward(
#     x_year_end = x_year_end,
#     x_year_start = 1940,
#     include_extra_vars = T
#   )[year==max(year)]
#   d[, year := NULL]
#   d[, weighting := NULL]
#
#   return(d)
# }

# hierarchy_notmainland <- function(
#   x_year_end = 2020,
#   x_year_start = 1940
# ){
#
#   d <- redistricting_notmainlandmunicip(
#     x_year_end = x_year_end,
#     x_year_start = 1940,
#     include_extra_vars = T
#   )[year==max(year)]
#   d[, year := NULL]
#   d[, weighting := NULL]
#
#   return(invisible(d))
# }

# hierarchy_missingmunicip <- function(
#   x_year_end,
#   x_year_start = 1940
# ){
#
#   d <- redistricting_missingmunicip(
#     x_year_end = x_year_end,
#     x_year_start = 1940,
#     include_extra_vars = T
#   )[year==max(year)]
#   d[, year := NULL]
#   d[, weighting := NULL]
#
#   return(invisible(d))
# }

# hierarchy_missingward <- function(
#   x_year_end,
#   x_year_start = 1940
# ){
#
#   d <- redistricting_missingward(
#     x_year_end = x_year_end,
#     x_year_start = 1940,
#     include_extra_vars = T
#   )[year==max(year)]
#   d[, year := NULL]
#   d[, weighting := NULL]
#
#   return(invisible(d))
# }


gen_hierarchy_from_redistrict <- function(
  geo_granularity,
  x_year_end = 2020,
  x_year_start = 1940
){

  stopifnot(geo_granularity %in% c(
    "municip", "ward", "notmainlandmunicip", "missingmunicip", "missingward"
  ))

  # municip (incl. ba) ----#
  if(geo_granularity == "municip"){
    # geo_granularity <- "municip"


    d <- redistricting_municip(x_year_end = x_year_end,
                               x_year_start = x_year_start,
                               include_extra_vars = T)


    # ba ---- #
    ba <- gen_location_name_ba_wide()

    d[
      ba,
      on="municip_code_current==municip_code",
      baregion_code := baregion_code
    ]
    d[
      ba,
      on="municip_code_current==municip_code",
      baregion_name := baregion_name
    ]

    # ward ----#
  }else if(geo_granularity == "ward"){

    # geo_granularity <- "ward"

    d <- redistricting_ward(x_year_end = x_year_end,
                            x_year_start = x_year_start,
                            include_extra_vars = T)

    # notmainlandmunicip ----#
  }else if(geo_granularity == "notmainlandmunicip"){
    # geo_granularity <- 'notmainlandmunicip'

    d <- redistricting_notmainlandmunicip(x_year_end = x_year_end,
                                          x_year_start = x_year_start,
                                          include_extra_vars = T)


    # missingmunicip ----#
  }else if(geo_granularity == "missingmunicip"){

    d <- redistricting_missingmunicip(x_year_end = x_year_end,
                                          x_year_start = x_year_start,
                                          include_extra_vars = T)


    # missingward ---- #
  }else if(geo_granularity == "missingward"){
    d <- redistricting_missingward(x_year_end = x_year_end,
                                      x_year_start = x_year_start,
                                      include_extra_vars = T)

  }else{
    stop("specify geo_granularity")
  }

  d <- d[year==max(year)]
  d[, year := NULL]
  d[, weighting := NULL]

  d

  return(d)
}



hierarchy_all <- function(
  x_year_end = 2020,
  x_year_start = 1940
){


  # municip ----
  municip <- gen_hierarchy_from_redistrict(geo_granularity = "municip")
  municip[, municip_code_original := NULL]
  setnames(municip, "municip_code_current", "municip_code")



  # ward ----
  ward <- gen_hierarchy_from_redistrict(geo_granularity = "ward")
  ward[, municip_name := NULL]
  ward[, ward_code_original := NULL]
  setnames(ward, "ward_code_current", "ward_code")


  # notmainlandmunicip ----
  notmainlandmunicip <- gen_hierarchy_from_redistrict(geo_granularity = "notmainlandmunicip")
  notmainlandmunicip[, location_code_original := NULL]
  setnames(
    notmainlandmunicip,
    c("location_code_current", "municip_name", "county_code", "county_name"),
    c("notmainlandmunicip_code", "notmainlandmunicip_name", "notmainlandcounty_code", "notmainlandcounty_name"),
  )


  # missingmunicip ----
  missingmunicip <- gen_hierarchy_from_redistrict(geo_granularity = "missingmunicip")
  missingmunicip[, location_code_original := NULL]
  setnames(
    missingmunicip,
    c("location_code_current", "municip_name", "county_code", "county_name"),
    c("missingmunicip_code", "missingmunicip_name", "missingcounty_code", "missingcounty_name"),
  )


  # missingward ----
  missingward <- gen_hierarchy_from_redistrict(geo_granularity = "missingward")
  missingward[, municip_name := NULL]
  missingward[, location_code_original := NULL]
  setnames(missingward, c("location_code_current", "location_name"), c("missingward_code","missingward_name"))



  d_1 <- merge(
    municip,
    missingward,
    by="municip_code",
    all=T
  )

  d_2 <- merge(
    municip,
    ward,
    by="municip_code",
    all=T
  )

  d <- rbind(d_1, d_2, fill=T)

  d <- rbind(
    d,
    notmainlandmunicip,
    fill = TRUE
  )
  d <- rbind(
    d,
    missingmunicip,
    fill = TRUE
  )

  d

  return(d)
}
