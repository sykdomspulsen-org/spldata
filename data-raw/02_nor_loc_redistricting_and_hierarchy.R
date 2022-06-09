# check the old and new merging

# old (gen_norway_municip_merging) ---- #
# 1. rename the function, it is the same as redistricting
# 2. rename skeleton into something meaningful
# 3. break the while loop
# 4. this table is too wide, consider using smaller excels
# IMPORTANT: for now do NOT optimise. just recycle the old code!

library(data.table)

#' get_granularity_geo
#' @param x Datatable
#' @param location_reference A location reference data.table
#' @export
get_granularity_geo <- function(x, location_reference = NULL){
  if(is.null(location_reference)){
    retval <- stringr::str_extract(x, "^[a-z]+")
    retval[retval=="norge"] <- "nation"
    return(retval)
  } else {
    return(location_reference[data.table(location_code=x), on = "location_code", granularity_geo])
  }
}


nor_loc_name_ba_wide <- function(x_year_end = 2020){

  ba <- data.table(readxl::read_excel(fs::path("data-raw", "files", "locations", "baregioner_2020.xlsx")))
  setnames(
    ba,
    1:2,
    c(
      "municip",
      "ba"
    )
  )
  ba[, municip_code := paste0(
    "municip",
    formatC(as.numeric(
      stringr::str_extract(municip, "^[0-9]+")),
      width=4,
      flag=0
    ))
  ]
  ba[, baregion_code := paste0(
    "baregion",
    formatC(as.numeric(
      stringr::str_extract(ba, "^[0-9]+")),
      width=3,
      flag=0
    ))
  ]
  ba[, baregion_name := stringr::str_remove_all(ba, "^[0-9]+ ")]


  return(ba)

}


# municip ----


nor_loc_redistricting_municip <- function(
    x_year_end = 2020,
    x_year_start = 1940,
    include_extra_vars = FALSE) {

  # variables used in data.table functions in this function
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
  faregion_name <- NULL
  faregion_code <- NULL
  realEnd <- NULL
  weighting <- NULL
  municip_code_end_new <- NULL
  weighting_new <- NULL
  # end

  # x_year_start = 2000
  # x_year_end = 2020
  ys <- x_year_start
  ye <- x_year_end
  # include_extra_vars <- F

  # load raw data
  masterData <- data.table(readxl::read_excel(fs::path("data-raw", "files", "locations", "norway_locations.xlsx")))
  masterData <- masterData[!county_code %in% c("missingcounty99", "notmainlandcounty21", "notmainlandcounty22")]
  masterData[is.na(weighting), weighting := 1]

  # rawd <- copy(masterData)
  md <- copy(masterData)

  # assign the beginning year to 2000
  md[year_start <= ys, year_start := ys]

  # greater than 2000 (start) OR continuing(na year_end)
  md <- md[year_start >= ys | is.na(year_end)]

  # take smaller than 2020 (last) OR continuing
  md <- md[year_end <= ye | is.na(year_end)]

  # last year, make it continue (same pre/post municip_code)
  md[year_end == ye, municip_code_end := NA]
  md[year_end == ye, year_end := NA]

  md[is.na(municip_code_end), municip_code_end := municip_code]
  md[is.na(year_end), year_end := ye]
  setnames(md, "year_start", "year")

  # 732 rows

  # expand for each year ---- #
  retval <- vector("list", 10000)
  for (i in 1:nrow(md)) {
    p <- md[i, ]
    years <- p$year:p$year_end
    temp <- p[rep(1, length(years))]
    temp[, year := years]
    retval[[i]] <- temp
  }
  skeleton <- rbindlist(retval)
  setorder(skeleton, year, municip_code)

  municip_long <- copy(skeleton)
  # skeleton <- municip_long
  # 8945 rows

  merger <- unique(skeleton[municip_code != municip_code_end, c("municip_code", "municip_code_end", "weighting")])
  # 376 rows, ever changed code

  setnames(
    merger,
    c("municip_code_end", "weighting"),
    c("municip_code_end_new", "weighting_new")
  )

  skeleton[!is.na(municip_code_end_new)]
  # binds long's municip_code_end with unique's municip_code
  ### tempted to fix this!!!
  continue_with_merging <- TRUE
  while (continue_with_merging) {
    # print("merging!")
    # add municip_code_end_new, weighting_new
    skeleton <- merge(
      skeleton,
      merger,
      by.x = c("municip_code_end"),
      by.y = c("municip_code"),
      all.x = T
    )

    # 8981 rows
    if (sum(!is.na(skeleton$municip_code_end_new)) == 0) {
      continue_with_merging <- FALSE
    }

    skeleton[!is.na(municip_code_end_new), municip_code_end := municip_code_end_new]
    skeleton[!is.na(weighting_new), weighting := weighting * weighting_new]
    skeleton[, municip_code_end_new := NULL]
    skeleton[, weighting_new := NULL]
  }

  skeletonFinal <- unique(skeleton[year == max(year), c(
    "municip_code",
    "municip_name",
    "county_code",
    "county_name",
    "region_code",
    "region_name",
    'faregion_name',
    'faregion_code'
  )])

  skeleton[, year_end := NULL]
  skeleton[, municip_name := NULL]
  skeleton[, county_code := NULL]
  skeleton[, county_name := NULL]
  skeleton[, region_code := NULL]
  skeleton[, region_name := NULL]
  skeleton[, faregion_code := NULL]
  skeleton[, faregion_name := NULL]


  skeleton <- merge(
    skeleton,
    skeletonFinal,
    by.x = c("municip_code_end"),
    by.y = c("municip_code")
  )

  setnames(skeleton, "municip_code_end", "municip_code_current")
  setnames(skeleton, "municip_code", "municip_code_original")

  setcolorder(
    skeleton,
    c(
      "municip_code_current",
      "municip_code_original",
      "year",
      "weighting",
      "municip_name",
      "county_code",
      "county_name",
      "region_code",
      "region_name",
      'faregion_name',
      'faregion_code'

    )
  )

  if (!include_extra_vars) {
    skeleton[, municip_name := NULL]
    skeleton[, county_code := NULL]
    skeleton[, county_name := NULL]
    skeleton[, region_code := NULL]
    skeleton[, region_name := NULL]
    skeleton[, faregion_code := NULL]
    skeleton[, faregion_name := NULL]
  }

  extra_years <- max(skeleton$year) + c(1:10)
  for (i in extra_years) {
    temp <- skeleton[year == max(year)]
    temp[, year := i]
    skeleton <- rbind(skeleton, temp)
  }

  d <- copy(skeleton)

  return(d)
}




nor_loc_redistricting_missingmunicip <- function(
    x_year_end = 2020,
    x_year_start = 1940,
    include_extra_vars = FALSE
){

  # x_year_end <- 2020
  # masterData <- data.table(readxl::read_excel(system.file("rawdata", "locations", "norway_locations.xlsx", package = "spldata")))
  # masterData <- masterData[county_code == "missingcounty99"]
  # masterData[is.na(weighting), weighting := 1]

  # stopifnot(nrow(masterData) == 1) # only missingmunicip9999

  retval <- data.table(location_code_current = "missingmunicip9999",
                       location_code_original = "missingmunicip9999",
                       year = seq(x_year_start, x_year_end+10, by = 1),
                       weighting = 1)

  d <- copy(retval)
  setnames(d, c("location_code_current", "location_code_original", "year", "weighting"))

  if(include_extra_vars == T){
    retval[, municip_name := "Ukjent kommune"]
    retval[, county_code := "missingcounty99"]
    retval[, county_name := "Ukjent fylke"]
    retval[, region_code := NA_character_]
    retval[, region_name := NA_character_]
    retval[, faregion_code := NA_character_]
    retval[, faregion_name := NA_character_]

    d <- copy(retval)

  }

  return(d)
}


# redistricting_notmainlandmunicip()
nor_loc_redistricting_notmainlandmunicip <- function(
    x_year_end = 2020,
    x_year_start = 1940,
    include_extra_vars = F){

  retval <- list()
  # municip2100 before 2018
  retval[[length(retval)+1]] <- data.table(
    location_code_current = "notmainlandmunicip2100",
    location_code_original = "notmainlandmunicip2111",
    year = seq(x_year_start, 2018, by = 1),
    weighting = 1
  )

  retval[[length(retval)+1]] <- data.table(
    location_code_current = "notmainlandmunicip2100",
    location_code_original = "notmainlandmunicip2121",
    year = seq(x_year_start, 2018, by = 1),
    weighting = 1
  )

  retval[[length(retval)+1]] <- data.table(
    location_code_current = "notmainlandmunicip2100",
    location_code_original = "notmainlandmunicip2131",
    year = seq(x_year_start, 2018, by = 1),
    weighting = 1
  )

  # municip2100 after 2018
  retval[[length(retval)+1]] <- data.table(
    location_code_current = "notmainlandmunicip2100",
    location_code_original = "notmainlandmunicip2100",
    year = seq(2018, x_year_end + 10, by = 1),
    weighting = 1
  )

  # municip2200
  retval[[length(retval)+1]] <- data.table(
    location_code_current = "notmainlandmunicip2200",
    location_code_original = "notmainlandmunicip2200",
    year = seq(x_year_start, x_year_end + 10, by = 1),
    weighting = 1
  )


  retval <- rbindlist(retval)


  if(include_extra_vars == T){
    retval[location_code_current == "notmainlandmunicip2100", municip_name := "Svalbard"]
    retval[location_code_current == "notmainlandmunicip2100", county_code := "notmainlandcounty21"]
    retval[location_code_current == "notmainlandmunicip2100", county_name := "Utenfor fastlands-Norge (Svalbard)"]

    retval[location_code_current == "notmainlandmunicip2200", municip_name := "Jan Mayen"]
    retval[location_code_current == "notmainlandmunicip2200", county_code := "notmainlandcounty22"]
    retval[location_code_current == "notmainlandmunicip2200", county_name := "Utenfor fastlands-Norge (Jan Mayen)"]

    retval[, region_code := NA_character_]
    retval[, region_name := NA_character_]
    retval[, faregion_code := NA_character_]
    retval[, faregion_name := NA_character_]

    d <- copy(retval)

  }

  d <- retval
  return(d)

}




# ward ----

# redistricting ward

nor_loc_redistricting_ward <- function(
    x_year_end = 2020,
    x_year_start = 1940,
    include_extra_vars = F) {


  masterData <- data.table(readxl::read_excel(
    fs::path("data-raw", "files", "locations", "norway_locations_ward.xlsx"),
    col_types = c(
      "numeric",
      "numeric",
      "text",
      "numeric",
      "text",
      "text",
      "text",
      "text"
    )
  ))
  masterData[is.na(weighting), weighting := 1]

  masterData[year_start <= x_year_start, year_start := x_year_start]
  masterData <- masterData[year_start <= x_year_end]

  masterData <- masterData[year_start >= x_year_start | is.na(year_end)]
  setnames(masterData, "year_start", "year")

  masterData <- masterData[year_end >= x_year_start | is.na(year_end)]
  masterData <- masterData[year_end <= x_year_end | is.na(year_end)]
  masterData[year_end == x_year_end, ward_code_end := NA]
  masterData[year_end == x_year_end, year_end := NA]

  masterData[is.na(ward_code_end), ward_code_end := ward_code]
  masterData[is.na(year_end), year_end := x_year_end]

  retval <- vector("list", 10000)
  for (i in 1:nrow(masterData)) {
    p <- masterData[i, ]
    years <- p$year:p$year_end
    temp <- p[rep(1, length(years))]
    temp[, year := years]
    retval[[i]] <- temp
  }
  skeleton <- rbindlist(retval)
  setorder(skeleton, year, ward_code)

  merger <- unique(skeleton[ward_code != ward_code_end, c("ward_code", "ward_code_end", "weighting")])
  setnames(
    merger,
    c("ward_code_end", "weighting"),
    c("ward_code_end_new", "weighting_new")
  )

  continue_with_merging <- TRUE
  while (continue_with_merging) {
    # print("merging!")
    skeleton <- merge(
      skeleton,
      merger,
      by.x = c("ward_code_end"),
      by.y = c("ward_code"),
      all.x = T
    )
    if (sum(!is.na(skeleton$ward_code_end_new)) == 0) {
      continue_with_merging <- FALSE
    }

    skeleton[!is.na(ward_code_end_new), ward_code_end := ward_code_end_new]
    skeleton[!is.na(weighting_new), weighting := weighting * weighting_new]
    skeleton[, ward_code_end_new := NULL]
    skeleton[, weighting_new := NULL]
  }

  skeletonFinal <- unique(skeleton[year == max(year), c(
    "ward_code",
    "ward_name",
    "municip_code",
    "municip_name"
  )])

  skeleton[, year_end := NULL]
  skeleton[, ward_name := NULL]
  skeleton[, municip_code := NULL]
  skeleton[, municip_name := NULL]

  skeleton <- merge(
    skeleton,
    skeletonFinal,
    by.x = c("ward_code_end"),
    by.y = c("ward_code")
  )

  setnames(skeleton, "ward_code_end", "ward_code_current")
  setnames(skeleton, "ward_code", "ward_code_original")

  setcolorder(
    skeleton,
    c(
      "ward_code_current",
      "ward_code_original",
      "year",
      "weighting",
      "ward_name",
      "municip_code",
      "municip_name"
    )
  )

  if (!include_extra_vars) {
    skeleton[, ward_name := NULL]
    skeleton[, municip_code := NULL]
    skeleton[, municip_name := NULL]
  }

  extra_years <- max(skeleton$year) + c(1:10)
  for (i in extra_years) {
    temp <- skeleton[year == max(year)]
    temp[, year := i]
    skeleton <- rbind(skeleton, temp)
  }

  skeleton <- skeleton[!stringr::str_detect(ward_code_current, "ward[0-9]")]

  skeleton[ward_code_current=="wardoslo030116", ward_code_current := "extrawardoslo030116"]
  skeleton[ward_code_current=="wardoslo030117", ward_code_current := "extrawardoslo030117"]
  skeleton[ward_code_original=="wardoslo030116", ward_code_original := "extrawardoslo030116"]
  skeleton[ward_code_original=="wardoslo030117", ward_code_original := "extrawardoslo030117"]

  d <- copy(skeleton)

  return(d)
}













nor_loc_redistricting_missingward <- function(
    x_year_end = 2020,
    x_year_start = 1940,
    include_extra_vars = FALSE){

  retval <- list()
  retval[[length(retval)+1]] <- data.table(
    location_code_current = "missingwardoslo030199",
    location_code_original = "missingwardoslo030199",
    location_name = "Ukjent bydel i Oslo",
    year = seq(x_year_start, x_year_end+10, by = 1),
    weighting = 1,
    municip_code = "municip0301",
    municip_name = "Oslo"
  )

  retval[[length(retval)+1]] <- data.table(
    location_code_current = "missingwardbergen460199",
    location_code_original = "missingwardbergen460199",
    location_name = "Ukjent bydel i Bergen",
    year = seq(x_year_start, x_year_end+10, by = 1),
    weighting = 1,
    municip_code = "municip4601",
    municip_name = "Bergen"
  )

  retval[[length(retval)+1]] <- data.table(
    location_code_current = "missingwardtrondheim500199",
    location_code_original = "missingwardtrondheim500199",
    location_name = "Ukjent bydel i Trondheim",
    year = seq(x_year_start, x_year_end+10, by = 1),
    weighting = 1,
    municip_code = "municip5001",
    municip_name = "Trondheim"
  )

  retval[[length(retval)+1]] <- data.table(
    location_code_current = "missingwardstavanger110399",
    location_code_original = "missingwardstavanger110399",
    location_name = "Ukjent bydel i Stavanger",
    year = seq(x_year_start, x_year_end+10, by = 1),
    weighting = 1,
    municip_code = "municip1103",
    municip_name = "Stavanger"
  )

  retval <- rbindlist(retval)

  if (!include_extra_vars) {
    retval[, location_name := NULL]
    retval[, municip_code := NULL]
    retval[, municip_name := NULL]
  }

  d <- retval
  return(d)

}




# county (require pop) ----



# Creates the norway_county_merging (fylkesammenslaaing) data.table
nor_loc_redistricting_county <- function(
  x_year_end = 2020,
  x_year_start = 2000
) {
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
  municips <- nor_loc_redistricting_municip(x_year_end = x_year_end,x_year_start = x_year_start)

  # population ----
  pops <- readRDS("data-raw/data-temp/nor_population_by_age_b0000.rds")
  pops <- pops[
    granularity_geo=="municip",
    .(
      pop_jan1_n = sum(pop_jan1_n)
    ),
    keyby = .(
      municip_code = location_code,
      year
    )
  ]


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

  x[, weighting := weighting * pop_jan1_n]
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


nor_loc_redistricting_missingcounty <- function(
    x_year_end = 2020,
    x_year_start = 1940){

  retval <- data.table(location_code_current = "missingcounty99",
                       location_code_original = "missingcounty99",
                       year = seq(x_year_start, x_year_end+10, by = 1),
                       weighting = 1)

  return(retval)

}


nor_loc_redistricting_notmainlandcounty<- function(
    x_year_end = 2020,
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



# ____ all ____ ====

nor_loc_redistricting_all <- function(border = 2020){

  stopifnot(border == 2020) # for the future


  # municip, ward
  d_norway_locations_redistricting_municip_b2020 <- nor_loc_redistricting_municip()
  d_norway_locations_redistricting_ward_b2020 <- nor_loc_redistricting_ward()
  d_norway_locations_redistricting_notmainlandmunicip_b2020 <- nor_loc_redistricting_notmainlandmunicip()
  d_norway_locations_redistricting_missingmunicip_b2020 <- nor_loc_redistricting_missingmunicip()
  d_norway_locations_redistricting_missingward_b2020 <- nor_loc_redistricting_missingward()

  # county
  d_norway_locations_redistricting_county_b2020 <- nor_loc_redistricting_county()
  d_norway_locations_redistricting_missingcounty_b2020 <- nor_loc_redistricting_missingcounty()
  d_norway_locations_redistricting_notmainlandcounty_b2020 <- nor_loc_redistricting_notmainlandcounty()


  # some colname consistency
  setnames(d_norway_locations_redistricting_municip_b2020, "municip_code_current", "location_code_current")
  setnames(d_norway_locations_redistricting_municip_b2020, "municip_code_original", "location_code_original")
  setnames(d_norway_locations_redistricting_ward_b2020, "ward_code_current", "location_code_current")
  setnames(d_norway_locations_redistricting_ward_b2020, "ward_code_original", "location_code_original")


  # bind with nation level
  d_norway_locations_redistricting_b2020 <- rbind(
    # nation
    data.table::data.table(
      location_code_current = "norge",
      location_code_original = "norge",
      year = 1975:(lubridate::year(lubridate::today())+10),
      weighting = 1
    ),
    # local
    d_norway_locations_redistricting_county_b2020,
    d_norway_locations_redistricting_municip_b2020,
    d_norway_locations_redistricting_ward_b2020,
    d_norway_locations_redistricting_notmainlandcounty_b2020,
    d_norway_locations_redistricting_notmainlandmunicip_b2020,
    d_norway_locations_redistricting_missingcounty_b2020,
    d_norway_locations_redistricting_missingmunicip_b2020,
    d_norway_locations_redistricting_missingward_b2020
  )
  d_norway_locations_redistricting_b2020[, granularity_geo := get_granularity_geo(location_code_current)]
  setnames(d_norway_locations_redistricting_b2020, "year", "calyear")


  return(d_norway_locations_redistricting_b2020)
}

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







nor_locations_redistricting_b2020 <- nor_loc_redistricting_all(2020)
usethis::use_data(nor_locations_redistricting_b2020, overwrite = TRUE, internal = TRUE, compress = "xz", version = 3)


nor_locations_hierarchy_b2020 <- nor_loc_hierarchy_all(x_year_end = 2020)
usethis::use_data(nor_locations_hierarchy_b2020, overwrite = TRUE, internal = TRUE, compress = "xz", version = 3)

