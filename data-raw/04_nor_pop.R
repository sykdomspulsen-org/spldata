library(data.table)
devtools::load_all(".")
#' Population in Norway (2020 borders).
#'
#' We conveniently package population data taken from Statistics Norway.
#' This data is licensed under the Norwegian License for
#' Open Government Data (NLOD) 2.0.
#'
#' This dataset contains national/county/municipality/ward (city district) level population data
#' for every age (0 to 105 years old). The national level data is from year 1846, while all the
#' other levels have data from 2005.
#'
#' The counties and municipalities are updated for the 2020 borders.
#'
#' @format
#' \describe{
#' \item{year}{Year.}
#' \item{location_code}{The location code.}
#' \item{granularity_geo}{National/County/Municipality/BAregion.}
#' \item{age}{1 year ages from 0 to 105.}
#' \item{pop_jan1}{Number of people as of 1st of January.}
#' \item{imputed}{FALSE if real data. TRUE if it is the last real data point carried forward.}
#' }
#' @source \url{https://www.ssb.no/en/statbank/table/07459/tableViewLayout1/}
"norway_population_by_age_b2020"


nor_population_by_age <- function(
  x_year_end = 2020
  ) {

  nor_population_by_age_b0000 <- readRDS("data-raw/data-temp/nor_population_by_age_b0000.rds")

  # municip/ward
  pop <- merge(
    nor_population_by_age_b0000[,c("location_code", "age", "calyear", "pop_jan1_n", "imputed")],
    nor_locations_redistricting(border = x_year_end),
    by.x = c("location_code", "calyear"),
    by.y = c("location_code_original", "calyear")
  )
  pop_municip <- pop[
    ,
    .(
      pop_jan1_n = round(sum(pop_jan1_n*weighting))
      ),
                             keyby = .(
                               calyear,
                               location_code = location_code_current,
                               age,
                               imputed
                             )
  ]


  # county ----
  pop_county <- merge(
    pop_municip,
    nor_loc_hierarchy_from_to(
      from = "municip",
      to = "county"
    ),
    by.x = "location_code",
    by.y = "from_code"
  )

  # aggregate by county
  pop_county <- pop_county[, .(
    pop_jan1_n = sum(pop_jan1_n )
  ), keyby = .(
    calyear,
    location_code = to_code,
    age,
    imputed
  )]

  # ba ----

  cat("creating population for baregion ... \n")

  pop_baregion <- merge(
    pop_municip,
    nor_loc_hierarchy_from_to(
      from = "municip",
      to = "baregion"
    ),
    by.x = "location_code",
    by.y = "from_code"
  )

  # aggregate by county
  pop_baregion <- pop_baregion[, .(
    pop_jan1_n = sum(pop_jan1_n )
  ), keyby = .(
    calyear,
    location_code = to_code,
    age,
    imputed
  )]

  # norway ----
  cat("creating population for nation ... \n")

  pop_norway_raw <- data.table(utils::read.csv(url("https://data.ssb.no/api/v0/dataset/59322.csv?lang=en"), stringsAsFactors = FALSE))
  pop_norway <- pop_norway_raw[sex == "0 Both sexes"]
  pop_norway[, sex := NULL]
  pop_norway[, contents := NULL]
  pop_norway[, x := as.numeric(stringr::str_extract(age, "^[0-9][0-9][0-9]"))]
  pop_norway[, age := NULL]
  setnames(pop_norway, c("calyear", "pop_jan1_n", "age"))
  pop_norway[, imputed := FALSE]

  # 2 more years
  missing_years_national <- (max(pop_norway$calyear) + 1):max(pop_municip$calyear)
  for (i in missing_years_national) {
    popx <- pop_norway[calyear == max(calyear)]
    popx[, calyear := i]
    popx[, imputed := TRUE]
    pop_norway <- rbind(pop_norway, popx)
  }
  pop_norway[, location_code := "norge"]


  pop_all <- rbind(pop_norway, pop_county, pop_municip, pop_baregion)
  pop_all[, granularity_geo := location_code_to_granularity_geo(location_code)]

  pop_all[, sex := "total"]

  cat("done \n")


  # notmainland, missing ----
  #  svalbard + jan mayen ----/
  # age: -99
  cat("creating population for notmainland and missing ... \n")

  pop_svalbard_raw <- readxl::read_excel(fs::path("data-raw", "files", "population", "Personer_svalbard_1990-2020.xlsx"))
  # county21: all 4 rows
  # municip: everything apart from barentsburg
  pop_sv <- data.frame(t(pop_svalbard_raw[, -1]))
  calyears <- rownames(pop_sv)
  colnames(pop_sv) <- c('Longyearbyen_nyalesund1', 'Longyearbyen_nyalesund2', 'Barentsburg', 'Hornsund')

  setDT(pop_sv)
  pop_sv[, notmainlandcounty21 := rowSums(.SD, na.rm=T), .SDcols=colnames(pop_sv)]
  pop_sv[, notmainlandmunicip2100 := rowSums(.SD, na.rm=T), .SDcols=colnames(pop_sv)[c(1,2,4)]]
  pop_sv[, calyear := as.numeric(calyears)]
  pop_sv[, imputed := F]
  pop_sv[, age := -99]


  # add 2 more years
  missing_years <- (max(pop_sv$calyear) + 1):max(pop_municip$calyear)
  if (length(missing_years) > 1) {
    copied_years <- vector("list", length = length(missing_years))
    for (i in seq_along(copied_years)) {
      copied_years[[i]] <- pop_sv[calyear == missing_years[1]-1]
      copied_years[[i]][, calyear := calyear + i]
    }
    copied_years <- rbindlist(copied_years)
    copied_years[, imputed := TRUE]
    pop_sv <- rbind(pop_sv, copied_years)
  }


  # jan mayen (county22)
  pop_jm <- data.table(
    calyear = unique(c(as.numeric(calyears), missing_years)),
    notmainlandmunicip2200 = 26,
    notmainlandcounty22 = 26,
    imputed = F,
    age = -99
  )
  pop_jm[calyear>lubridate::year(lubridate::today()), imputed := T]


  # separate county, municip
  pop_notmainlandcounty21 <- pop_sv[, .(calyear, pop_jan1_n = notmainlandcounty21, imputed, age)]
  pop_notmainlandcounty21[, location_code := 'notmainlandcounty21']
  pop_notmainlandcounty21[, granularity_geo := 'notmainlandcounty']

  pop_notmainlandmunicip2100 <- pop_sv[, .(calyear, pop_jan1_n = notmainlandmunicip2100, imputed, age)]
  pop_notmainlandmunicip2100[, location_code := 'notmainlandmunicip2100']
  pop_notmainlandmunicip2100[, granularity_geo := 'notmainlandmunicip']

  pop_notmainlandcounty22 <- pop_jm[, .(calyear, pop_jan1_n = notmainlandcounty22, imputed, age)]
  pop_notmainlandcounty22[, location_code := 'notmainlandcounty22']
  pop_notmainlandcounty22[, granularity_geo := 'notmainlandcounty']

  pop_notmainlandmunicip2200 <- pop_jm[, .(calyear, pop_jan1_n = notmainlandmunicip2200, imputed, age)]
  pop_notmainlandmunicip2200[, location_code := 'notmainlandmunicip2200']
  pop_notmainlandmunicip2200[, granularity_geo := 'notmainlandmunicip']

  # match year: from 2005 to 2022
  pop_notmainlandcounty21 <- pop_notmainlandcounty21[calyear>=2005]
  pop_notmainlandmunicip2100 <- pop_notmainlandmunicip2100[calyear>=2005]
  pop_notmainlandcounty22 <- pop_notmainlandcounty22[calyear>=2005]
  pop_notmainlandmunicip2200 <- pop_notmainlandmunicip2200[calyear>=2005]





  # ukjent ----/
  # age: -99
  # pop: NA
  pop_county_unknown <- data.table(
    calyear = unique(pop_municip$calyear),
    pop_jan1_n = NA_real_,
    location_code = 'missingcounty99',
    granularity_geo = 'missingcounty',
    imputed = F,
    age = -99
  )

  pop_municip_unknown <- data.table(
    calyear = unique(pop_municip$calyear),
    pop_jan1_n = NA_real_,
    location_code = 'missingmunicip9999',
    granularity_geo = 'missingmunicip',
    imputed = F,
    age = -99
    )


  # combine svalbard and unknown, set age, force imputed T for greater than this year
  pop_notmain_missing <- rbind(
    pop_notmainlandcounty21,
    pop_notmainlandmunicip2100,
    pop_notmainlandcounty22,
    pop_notmainlandmunicip2200,
    pop_county_unknown,
    pop_municip_unknown
  )
  pop_notmain_missing[, age := -99]
  pop_notmain_missing[, sex := "total"]

  cat("done \n")

  # final ----
  final_order <- c("granularity_geo", "location_code", "age", "sex", "calyear", "pop_jan1_n", "imputed")
  setcolorder(pop_all, final_order)
  setcolorder(pop_notmain_missing, final_order)

  # finally bind these two
  pop <- rbind(pop_all, pop_notmain_missing)
  setorderv(pop, final_order)
  setkeyv(pop, final_order)


  # pop[, calyear := year]
  # pop[, pop_jan1 := pop]


  return(pop)
}


env = new.env()
load("R/sysdata.rda", envir = env)

env$nor_population_by_age_b2020 <- nor_population_by_age(2020)

for(i in names(env)){
  .GlobalEnv[[i]] <- env[[i]]
}
txt <- paste0("usethis::use_data(",paste0(names(env),collapse=","),", overwrite = TRUE, internal = TRUE, compress = 'xz', version = 3)")
eval(parse(text = txt))





