# location_code_current: current for 2020
# location_code_original: code of year x (from 1975 to 2030)
# one for each calyear



# understand the raw data ----

rawd <- data.table(readxl::read_excel(system.file("rawdata", "locations", "norway_locations.xlsx", package = "spldata")))
rawd[county_code == 'county03']
narrowd <- rawd[, .(year_start, year_end, municip_code, municip_code_end, municip_name,
                    weighting, county_code, county_name)]

narrowd
narrowd[municip_name == 'Moss']



narrowd[]

# understand target data ----
# dloc <- fhidata::norway_locations_redistricting_b2020

# dloc
# 0104 was used until 2019, then it is 3002 (Moss)
# dloc[location_code_original == 'municip0104']
# dloc[location_code_original == 'municip3002']
# dloc[granularity_geo == 'county' & calyear == 2019 & weighting!= 1]
# dloc[granularity_geo == 'county' & calyear == 2019 ]
# dloc[granularity_geo == 'county' & calyear == 2018 & weighting != 1]
# dloc[granularity_geo == 'county' & calyear == 2017 & weighting != 1]
# dloc[granularity_geo == 'county' & calyear == 2016 & weighting != 1]
# dloc[granularity_geo == 'county' & weighting != 1 & calyear > 2005]

#  special cases:
# narrowd[weighting!= 1]
# # 1. 1850 (tysfjord to 1806, 1875 in 2019)
# # 2. 5012 (snillfjord to 5055, 5056, 5059 in 2019)
# narrowd[municip_code == 'municip5055']
#
# narrowd[municip_code == 'municip5012']
# narrowd[, year_end] %>% table # majority 2019
# # 2007, 2011, 2012, 2016, 2017, 2018
# narrowd[, year_start] %>% table
# # 2002, 2006, 2008, 2012, 2013, 2017, 2018, 2019
#
# narrowd[year_start == 2018]
# narrowd[year_start == 2018 & !is.na(year_end)]
#
# narrowd[municip_code == 'municip0710' | municip_code_end == 'municip0710']
#
# narrowd[municip_code == 'municip3802' | municip_code_end == 'municip3802']
#
#
# dloc[weighting != 1 & granularity_geo == 'municip']$location_code_current %>% unique
# dloc[weighting != 1 & granularity_geo == 'municip']$location_code_original %>% unique
# dloc[location_code_current == 'municip3802' & calyear >2017]
# dloc[location_code_current == 'county07' & calyear >2017]
# fhidata::norway_locations_redistricting(2019)[location_code_current == 'county07']
# narrowd[municip_code == 'municip1613']
#
#
#
# dloc[location_code_original == 'municip1850' & calyear >2017] # 20
# dloc[location_code_original == 'municip1806' & calyear >2017] # 2020 to 2030, weighting 1
# dloc[location_code_original == 'municip1875' & calyear >2017] # 2020 to 2030, weighting 1
#
#
#
# # start with the CURRENT, create HISTORICAL (original)
# # use municip_code_end
#
#
# #
# eg <- narrowd[municip_name == 'Moss']
# d <- eg[2,]
# municip_by_year(
#   year_start = d$year_start,
#   year_end = d$year_end,
#   municip_code = d$municip_code,
#   municip_code_end = d$municip_code_end
# )
# min(narrowd$year_end, na.rm = T)
#
#
# # make it after 2000
# # for whatever that is changed (municip_code_end != NA), take municip_code until that year
# # for those (municip_code_end == NA),
#
# # easier if just make one function per row
#
# # municip_by_year(year_start = eg$year_start)
#
# # municip_year_list <- list()
# # for(i in 1:nrow(narrowd)){
# #   d <- narrowd[i, ]
# #   municip_year_list[[i]] <- municip_by_year(
# #     year_start = d$year_start,
# #     year_end = d$year_end,
# #     municip_code_current = d$municip_code,
# #   )
# #   # cat('row', i, 'done\n')
# # }
# # municip_exist_each_year <- rbindlist(municip_year_list)
#
#
#
# # merge with the municip name, weight and county info
# municip_ref <- narrowd[, .(municip_code, municip_code_end, municip_name,weighting, county_code, county_name)]
#
# municip_year2 <- municip_year[municip_ref, on = c('municip_code')]
# municip_year2[municip_name == 'Tysfjord']
#
# municip_year2[municip_name == 'Narvik']
#
#
#
# municip_by_year <- function(year_start, year_end, municip_code, municip_code_end){
#   # year_start <- 1838
#   # year_end <- 2019
#   # municip_code <- 'municip2019'
#   # year_start <- 2020
#   # year_end <- NA
#   # municip_code <- 'municip2020'
#
#   if(year_start <= 2005){year_start <- 2005}
#   if(is.na(year_end)){year_end <- 2030}
#   if(is.na(municip_code_end)){municip_code_end <- municip_code}
#
#   dmunicip <- data.table(
#     calyear = year_start : year_end,
#     municip_code_thisyear = municip_code,
#     municip_code_to = municip_code_end
#   )
#   return(dmunicip)
#
# }
#
#
#
# # weighting, probably don't need to worry about it now
# narrowd[!is.na(weighting)]
#
# narrowd[municip_name == 'Tysfjord']
# narrowd[county_code == 'county18']
#
# narrowd[county_code == 'county50']
# # these 3 (split by municip5012 in 2019)
# municip_year2[municip_code == 'municip5055']
# municip_year2[municip_code == 'municip5056']
# municip_year2[municip_code == 'municip5059']
#
# municip_year2[municip_code == 'municip5012'] %>% unique
# municip_year2[municip_code == 'municip1613']
#
# municip_year2[calyear == 2020 & municip_code == 'municip5055']
#
#


# OLD CODE ====
# Creates the norway_municip_merging (kommunesammenslaaing) data.table
gen_norway_locations_redistricting_municip_internal <- function(
  x_year_end,
  x_year_start = 2000,
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

  rawd <- data.table(readxl::read_excel(system.file("rawdata", "locations", "norway_locations.xlsx", package = "spldata")))
  rawd[county_code == 'county03']



  masterData <- masterData[!county_code %in% c("missingcounty99", "notmainlandcounty21", "notmainlandcounty22")]
  masterData[is.na(weighting), weighting := 1]

  masterData[year_start <= x_year_start, year_start := x_year_start]
  masterData <- masterData[year_start <= x_year_end]

  masterData <- masterData[year_start >= x_year_start | is.na(year_end)]
  setnames(masterData, "year_start", "year")

  masterData <- masterData[year_end >= x_year_start | is.na(year_end)]
  masterData <- masterData[year_end <= x_year_end | is.na(year_end)]
  masterData[year_end == x_year_end, municip_code_end := NA]
  masterData[year_end == x_year_end, year_end := NA]

  masterData[is.na(municip_code_end), municip_code_end := municip_code]
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
  setorder(skeleton, year, municip_code)

  # skeleton <- skeleton[municip_code %in% c("municip1613","municip5012","municip5059")]

  merger <- unique(skeleton[municip_code != municip_code_end, c("municip_code", "municip_code_end", "weighting")])
  setnames(
    merger,
    c("municip_code_end", "weighting"),
    c("municip_code_end_new", "weighting_new")
  )

  continue_with_merging <- TRUE
  while (continue_with_merging) {
    print("merging!")
    skeleton <- merge(
      skeleton,
      merger,
      by.x = c("municip_code_end"),
      by.y = c("municip_code"),
      all.x = T
    )
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

  return(invisible(skeleton))
}

gen_norway_locations_redistricting_municip <- function(x_year_end){
  stopifnot(x_year_end==2020)

  d <- gen_norway_locations_redistricting_municip_internal(
    x_year_end = x_year_end,
    x_year_start = 1910,
    include_extra_vars = FALSE
  )
  setnames(d, c("location_code_current", "location_code_original", "year", "weighting"))

  return(d)
}
