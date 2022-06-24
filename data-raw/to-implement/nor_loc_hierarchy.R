hierarchy_from_redistrict <- function(
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


    d <- nor_loc_redistricting_municip(x_year_end = x_year_end,
                                       x_year_start = x_year_start,
                                       include_extra_vars = T)


    # ba ---- #
    ba <- nor_loc_name_ba_wide()

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

    d <- nor_loc_redistricting_ward(x_year_end = x_year_end,
                                    x_year_start = x_year_start,
                                    include_extra_vars = T)

    # notmainlandmunicip ----#
  }else if(geo_granularity == "notmainlandmunicip"){
    # geo_granularity <- 'notmainlandmunicip'

    d <- nor_loc_redistricting_notmainlandmunicip(x_year_end = x_year_end,
                                                  x_year_start = x_year_start,
                                                  include_extra_vars = T)


    # missingmunicip ----#
  }else if(geo_granularity == "missingmunicip"){

    d <- nor_loc_redistricting_missingmunicip(x_year_end = x_year_end,
                                              x_year_start = x_year_start,
                                              include_extra_vars = T)


    # missingward ---- #
  }else if(geo_granularity == "missingward"){
    d <- nor_loc_redistricting_missingward(x_year_end = x_year_end,
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
"norway_locations_hierarchy_b2020"


# nor_loc_hierarchy_all()
nor_loc_hierarchy_all <- function(
  x_year_end = 2020,
  x_year_start = 1940
){


  # municip ----
  municip <- hierarchy_from_redistrict(geo_granularity = "municip")
  municip[, municip_code_original := NULL]
  setnames(municip, "municip_code_current", "municip_code")



  # ward ----
  ward <- hierarchy_from_redistrict(geo_granularity = "ward")
  ward[, municip_name := NULL]
  ward[, ward_code_original := NULL]
  setnames(ward, "ward_code_current", "ward_code")


  # notmainlandmunicip ----
  notmainlandmunicip <- hierarchy_from_redistrict(geo_granularity = "notmainlandmunicip")
  notmainlandmunicip[, location_code_original := NULL]
  setnames(
    notmainlandmunicip,
    c("location_code_current", "municip_name", "county_code", "county_name"),
    c("notmainlandmunicip_code", "notmainlandmunicip_name", "notmainlandcounty_code", "notmainlandcounty_name"),
  )


  # missingmunicip ----
  missingmunicip <- hierarchy_from_redistrict(geo_granularity = "missingmunicip")
  missingmunicip[, location_code_original := NULL]
  setnames(
    missingmunicip,
    c("location_code_current", "municip_name", "county_code", "county_name"),
    c("missingmunicip_code", "missingmunicip_name", "missingcounty_code", "missingcounty_name"),
  )


  # missingward ----
  missingward <- hierarchy_from_redistrict(geo_granularity = "missingward")
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




# (export int) ----


# nor_loc_hierarchy_from_to(from = 'county', to = 'municip')
nor_loc_hierarchy_from_to <- function(
  from,
  to,
  include_to_name = FALSE,
  border = spldata::config$border){

  stopifnot(border == 2020)
  stopifnot(from %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger",
    "municip",
    "baregion",
    "county",
    "region",
    "faregion",
    "notmainlandmunicip",
    "notmainlandcounty",
    "missingmunicip",
    "missingcounty"
  ))
  stopifnot(to %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger",
    "municip",
    "baregion",
    "county",
    "region",
    "faregion",
    "notmainlandmunicip",
    "notmainlandcounty",
    "missingmunicip",
    "missingcounty"
  ))

  d <- spldata::norway_locations_hierarchy_b2020
  # d <- norway_locations_hierarchy_all_b2020


  if(from %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger"
  )){
    col_from <- "ward_code"
  } else if(from %in% c(
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger"
  )){
    col_from <- "missingward_code"
  } else {
    col_from <- paste0(from,"_code")
  }

  if(to %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger"
  )){
    col_to <- "ward_code"
    col_to_name <- "ward_name"
  } else if(to %in% c(
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger"
  )){
    col_to <- "missingward_code"
    col_to_name <- "missingward_name"
  } else {
    col_to <- paste0(to,"_code")
    col_to_name <- paste0(to,"_name")
  }

  if(include_to_name){
    d <- d[, c(col_from, col_to, col_to_name), with=F]
  } else {
    d <- d[, c(col_from, col_to), with=F]
  }
  d <- stats::na.omit(d)

  if(from %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger"
  )){
    d <- d[grep(paste0("^",from), get(col_from))]
    setnames(d, col_from, paste0(from,"_code"))
  }

  if(to %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger"
  )){
    d <- d[grep(paste0("^",to), get(col_to))]
    setnames(d, col_to, paste0(to,"_code"))
  }
  d <- unique(d)

  if(ncol(d)==2){
    setnames(d, c("from_code","to_code"))
  } else {
    setnames(d, c("from_code","to_code", "to_name"))
  }

  return(d)
}


# (export) ----

#' Hierarchies in Norway (programmable borders).
#'
#' @param from wardoslo, wardbergen, wardtrondheim, wardstavanger, municip, baregion, county, region, faregion, notmainlandmunicip, notmainlandcounty, missingmunicip, missingcounty
#' @param to wardoslo, wardbergen, wardtrondheim, wardstavanger, municip, baregion, county, region, faregion, notmainlandmunicip, notmainlandcounty, missingmunicip, missingcounty
#' @param include_to_name Do you want to include the name of the 'to' location?
#' @param border The border year
#' @examples
#' nor_locations_hierarchy_from_to(from="wardoslo", to="county")
#' nor_locations_hierarchy_from_to(from="municip", to="baregion")
#' @export
nor_locations_hierarchy_from_to <- function(from, to, include_to_name = FALSE, border = spldata::config$border){
  plans <- expand.grid(
    from = from,
    to = to,
    stringsAsFactors = FALSE
  )
  retval <- vector("list", length=nrow(plans))
  for(i in seq_along(retval)){
    retval[[i]] <- nor_loc_hierarchy_from_to(from=plans$from[i], to=plans$to[i], include_to_name, border)
  }
  retval <- unique(rbindlist(retval))
  retval
}


