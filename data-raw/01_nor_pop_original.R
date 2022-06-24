library(data.table)
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


nor_population_by_age_original <- function(x_year_end = 2020) {

  # x_year_end <- 2020
  # variables used in data.table functions in this function
  . <- NULL
  value <- NULL
  age <- NULL
  Var2 <- NULL
  agecont <- NULL
  pop <- NULL
  municip_code <- NULL
  municip_code_current <- NULL
  year_end <- NULL
  level <- NULL
  region <- NULL
  variable <- NULL
  agenum <- NULL
  imputed <- NULL
  county_code <- NULL
  municip_code_end <- NULL
  sex <- NULL
  contents <- NULL
  x <- NULL
  # end



  # municip and ward ----
  ### gen_norway_population_by_age() has one other dataset, 2021
  cat("creating population for municip and ward ... \n")

  popFiles <- c(
    "Personer2005-2009.csv",
    "Personer2010-2014.csv",
    "Personer2015-2018.csv",
    "Personer2019.csv",
    "Personer2020.csv",
    "Personer2021.csv",
    "Personer2022.csv",
    "Personerward2001-2020.csv",
    "Personerward2021.csv",
    "Personerward2022.csv"

  )
  pop <- vector("list", length = length(popFiles))
  for (i in seq_along(pop)) {
    pop[[i]] <- fread(fs::path("data-raw", "files", "population", popFiles[i]), encoding = "UTF-8")
    pop[[i]] <- melt.data.table(pop[[i]], id.vars = c("region", "age"))
  }
  pop <- rbindlist(pop)

  # region, municip
  pop[, region := stringr::str_remove(region, "^K-")]
  pop[, municip_code := sprintf("municip%s", stringr::str_extract(region, "^[0-9][0-9][0-9][0-9]"))]

  # correctly identify ward/bydels
  pop[, ward_code := sprintf("%s", stringr::str_extract(region, "^[0-9][0-9][0-9][0-9][0-9][0-9]"))]
  pop[, ward_prefix := ""]
  pop[ward_code!="NA" & municip_code %in% c("municip0301"), ward_prefix := "wardoslo"]
  pop[ward_code!="NA" & municip_code %in% c("municip1201", "municip4601"), ward_prefix := "wardbergen"]
  pop[ward_code!="NA" & municip_code %in% c("municip1103"), ward_prefix := "wardstavanger"]
  pop[ward_code!="NA" & municip_code %in% c("municip1601", "municip5001"), ward_prefix := "wardtrondheim"]

  # ward oslo
  pop[,ward_code := paste0(ward_prefix,ward_code)]
  pop[ward_code=="wardoslo030116", ward_code := "extrawardoslo030116"]
  pop[ward_code=="wardoslo030117", ward_code := "extrawardoslo030117"]
  pop[ward_code!="NA", municip_code := ward_code]


  # add calyear, age numeric
  pop[, calyear := as.numeric(stringr::str_extract(variable, "[0-9][0-9][0-9][0-9]$"))]
  pop[, agenum := as.numeric(stringr::str_extract(age, "^[0-9]*"))]
  pop[, age := NULL]
  setnames(pop, "agenum", "age")
  pop

  # sum population by municip
  pop <- pop[municip_code != "municipNA"]
  pop_municip <- pop[, .(
    population = sum(value)
  ), keyby = .(
    municip_code, age, calyear
  )]


  # Fixing broken parts in the population data ----
  # part 1: municip0710

  pop_municip0706 <- pop_municip[municip_code == "municip0710" & calyear <= 2017]
  pop_municip0706[, population := max(population), by = age]
  pop_municip0706 <- pop_municip0706[calyear != max(calyear)]
  pop_municip0706[, municip_code := "municip0706"]
  pop_municip0706[, population := round(population/3)]
  pop_municip <- rbind(pop_municip, pop_municip0706)

  pop_municip0719 <- pop_municip[municip_code == "municip0710" & calyear <= 2017]
  pop_municip0719[, population := max(population), by = age]
  pop_municip0719 <- pop_municip0719[calyear != max(calyear)]
  pop_municip0719[, municip_code := "municip0719"]
  pop_municip0719[, population := round(population/3)]
  pop_municip <- rbind(pop_municip, pop_municip0719)

  pop_municip0720 <- pop_municip[municip_code == "municip0710" & calyear <= 2017]
  pop_municip0720[, population := max(population), by = age]
  pop_municip0720 <- pop_municip0720[calyear != max(calyear)]
  pop_municip0720[, municip_code := "municip0720"]
  pop_municip0720[, population := round(population/3)]
  pop_municip <- rbind(pop_municip, pop_municip0720)

  # # part 2: municip1756
  pop_municip1723 <- pop_municip[municip_code == "municip1756" & calyear <= 2012]
  pop_municip1723[, population := max(population), by = age]
  pop_municip1723 <- pop_municip1723[calyear != max(calyear)]
  pop_municip1723[, municip_code := "municip1723"]
  pop_municip1723[, population := round(population/2)]
  pop_municip <- rbind(pop_municip, pop_municip1723)

  pop_municip1729 <- pop_municip[municip_code == "municip1756" & calyear <= 2012]
  pop_municip1729[, population := max(population), by = age]
  pop_municip1729 <- pop_municip1729[calyear != max(calyear)]
  pop_municip1729[, municip_code := "municip1729"]
  pop_municip1729[, population := round(population/2)]
  pop_municip <- rbind(pop_municip, pop_municip1729)

  #
  # # part 3: municip5046
  pop_municip1901 <- pop_municip[municip_code == "municip5046" & calyear <= 2018]
  pop_municip1901[, population := max(population), by = age]
  pop_municip1901 <- pop_municip1901[calyear != max(calyear)]
  pop_municip1901[, municip_code := "municip1901"]
  pop_municip1901[, population := round(population/2)]
  pop_municip <- rbind(pop_municip, pop_municip1901)

  pop_municip1915 <- pop_municip[municip_code == "municip1756" & calyear <= 2018]
  pop_municip1915[, population := max(population), by = age]
  pop_municip1915 <- pop_municip1915[calyear != max(calyear)]
  pop_municip1915[, municip_code := "municip1915"]
  pop_municip1915[, population := round(population/2)]
  pop_municip <- rbind(pop_municip, pop_municip1915)


  # # part 4: municip1505

  pop_municip1503 <- pop_municip[municip_code == "municip1505" & calyear <= 2008]
  pop_municip1503[, population := max(population), by = age]
  pop_municip1503 <- pop_municip1503[calyear != max(calyear)]
  pop_municip1503[, municip_code := "municip1503"]
  pop_municip1503[, population := round(population/2)]
  pop_municip <- rbind(pop_municip, pop_municip1503)

  pop_municip1556 <- pop_municip[municip_code == "municip1505" & calyear <= 2008]
  pop_municip1556[, population := max(population), by = age]
  pop_municip1556 <- pop_municip1556[calyear != max(calyear)]
  pop_municip1556[, municip_code := "municip1556"]
  pop_municip1556[, population := round(population/2)]
  pop_municip <- rbind(pop_municip, pop_municip1556)


  pop_municip[, imputed := FALSE]

  setnames(pop_municip, "municip_code", "location_code")
  setnames(pop_municip, "population", "pop_jan1_n")

  # calyear, municip_code, age, imputed, pop
  # imputing the future (2 calyears+)
  missing_calyears <- max(pop_municip$calyear):(lubridate::year(lubridate::today()) + 10)

  if (length(missing_calyears) > 1) {
    copied_calyears <- vector("list", length = length(missing_calyears) - 1)
    for (i in seq_along(copied_calyears)) {
      copied_calyears[[i]] <- pop_municip[calyear == missing_calyears[1]]
      copied_calyears[[i]][, calyear := calyear + i]
    }
    copied_calyears <- rbindlist(copied_calyears)
    copied_calyears[, imputed := TRUE]
    pop_municip <- rbind(pop_municip, copied_calyears)
  }

  pop_municip[, granularity_geo := stringr::str_extract(location_code, "^[a-z]+")]

  return(pop_municip)

  if (original) {
    return(pop_municip)
  }

  # kommunesammenslaing ----
  # x_year_end <- 2020

  redistr_prepost_municip <- nor_loc_redistricting_municip()
  redistr_prepost_ward <- nor_loc_redistricting_ward()
  # unique(redistr_prepost_municip[, 1:2])  # 734 unique rows
  # unique(redistr_prepost_ward[, 1:2]) # 50 unique rows

  setnames(redistr_prepost_ward, names(redistr_prepost_municip))
  redistr_prepost <- rbind(redistr_prepost_municip, redistr_prepost_ward)

  # merge with population
  pop_municip <- merge(
    pop_municip,
    redistr_prepost[, c("year", "municip_code_current", "municip_code_original")],
    by.x = c("municip_code", "year"),
    by.y = c("municip_code_original", "year")
  )
  pop_municip <- pop_municip[, .(population = sum(population)),
             keyby = .(
               year,
               municip_code = municip_code_current,
               age,
               imputed
             )
  ]

  # year, municip_code, age, imputed, pop
  # imputing the future (2 years+)
  missing_years <- max(pop_municip$year):(lubridate::year(lubridate::today()) + 2)

  if (length(missing_years) > 1) {
    copied_years <- vector("list", length = length(missing_years) - 1)
    for (i in seq_along(copied_years)) {
      copied_years[[i]] <- pop_municip[year == missing_years[1]]
      copied_years[[i]][, year := year + i]
    }
    copied_years <- rbindlist(copied_years)
    copied_years[, imputed := TRUE]
    pop_municip <- rbind(pop_municip, copied_years)
  }

  pop_municip[, level := stringr::str_extract(municip_code, "^[a-z]+")]


  cat("done \n")



  # county ----
  cat("creating population for county ... \n")

  locations_municip_wide <- nor_loc_name_municip_wide()

  pop_municip_match_county <- merge(
    pop_municip,
    locations_municip_wide[, .(municip_code, county_code)],
    by = "municip_code"
  )

  # check consistency
  check_ref_to_new(
    xref = unique(pop_municip[level=="municip"]$municip_code),
    xnew = unique(pop_municip_match_county$municip_code)
  )

  if (nrow(pop_municip_match_county) != nrow(pop_municip[level=="municip"])) {
    stop("nrow(counties) != nrow(pop)")
  }


  # aggregate by county
  pop_county <- pop_municip_match_county[, .(
    population = sum(population)
  ), keyby = .(
    year,
    municip_code = county_code,
    age,
    imputed
  )]
  pop_county[, level := "county"]


  cat("done \n")

  # ba ----

  cat("creating population for baregion ... \n")

  if(x_year_end==2020){
    baregions <- merge(
      pop_municip,
      locations_municip_wide[, c("municip_code", "baregion_code")],
      by = "municip_code"
    )

    pop_baregions <- baregions[!is.na(baregion_code), .(
      population = sum(population)
    ), keyby = .(
      year,
      municip_code = baregion_code,
      age,
      imputed
    )]
    pop_baregions[, level := "baregion"]
  }
  cat("done \n")


  # norway ----
  cat("creating population for nation ... \n")

  pop_norway_raw <- data.table(utils::read.csv(url("https://data.ssb.no/api/v0/dataset/59322.csv?lang=en"), stringsAsFactors = FALSE))
  pop_norway <- pop_norway_raw[sex == "0 Both sexes"]
  pop_norway[, sex := NULL]
  pop_norway[, contents := NULL]
  pop_norway[, x := as.numeric(stringr::str_extract(age, "^[0-9][0-9][0-9]"))]
  pop_norway[, age := NULL]
  setnames(pop_norway, c("year", "population", "age"))
  pop_norway[, level := "nation"]
  pop_norway[, municip_code := "norge"]
  pop_norway[, imputed := FALSE]

  # 2 more years
  missing_years_national <- (max(pop_norway$year) + 1):(lubridate::year(lubridate::today()) + 2)
  for (i in missing_years_national) {
    popx <- pop_norway[year == max(year)]
    popx[, year := i]
    popx[, imputed := TRUE]
    pop_norway <- rbind(pop_norway, popx)
  }


  # bind with county, municip, ba
  if(x_year_end==2020){
    pop_all <- rbind(pop_norway, pop_county, pop_municip, pop_baregions)
  } else {
    pop_all <- rbind(pop_norway, pop_county, pop_municip)
  }
  pop_all


  cat("done \n")


  # notmainland, missing ----
  #  svalbard + jan mayen ----/
  # age: -99
  cat("creating population for notmainland and missing ... \n")

  pop_svalbard_raw <- readxl::read_excel(fs::path("data-raw", "files", "population", "Personer_svalbard_1990-2020.xlsx"))
  # county21: all 4 rows
  # municip: everything apart from barentsburg
  pop_sv <- data.frame(t(pop_svalbard_raw[, -1]))
  years <- rownames(pop_sv)
  colnames(pop_sv) <- c('Longyearbyen_nyalesund1', 'Longyearbyen_nyalesund2', 'Barentsburg', 'Hornsund')

  setDT(pop_sv)
  pop_sv[, notmainlandcounty21 := rowSums(.SD, na.rm=T), .SDcols=colnames(pop_sv)]
  pop_sv[, notmainlandmunicip2100 := rowSums(.SD, na.rm=T), .SDcols=colnames(pop_sv)[c(1,2,4)]]
  pop_sv[, year := as.numeric(years)]
  pop_sv[, imputed := F]



  # add 2 more years
  if (length(missing_years) > 1) {
    copied_years <- vector("list", length = length(missing_years) - 1)
    for (i in seq_along(copied_years)) {
      copied_years[[i]] <- pop_sv[year == missing_years[1]]
      copied_years[[i]][, year := year + i]
    }
    copied_years <- rbindlist(copied_years)
    copied_years[, imputed := TRUE]
    pop_sv <- rbind(pop_sv, copied_years)
  }


  # jan mayen (county22)
  pop_jm <- data.table(year = unique(c(as.numeric(years), missing_years)),
                       notmainlandmunicip2200 = 26,
                       notmainlandcounty22 = 26,
                       imputed = F)
  pop_jm[year>lubridate::year(lubridate::today()), imputed := T]


  # separate county, municip
  pop_notmainlandcounty21 <- pop_sv[, .(year, population = notmainlandcounty21, imputed)]
  pop_notmainlandcounty21[, municip_code := 'notmainlandcounty21']
  pop_notmainlandcounty21[, level := 'notmainlandcounty']

  pop_notmainlandmunicip2100 <- pop_sv[, .(year, population = notmainlandmunicip2100, imputed)]
  pop_notmainlandmunicip2100[, municip_code := 'notmainlandmunicip2100']
  pop_notmainlandmunicip2100[, level := 'notmainlandmunicip']

  pop_notmainlandcounty22 <- pop_jm[, .(year, population = notmainlandcounty22, imputed)]
  pop_notmainlandcounty22[, municip_code := 'notmainlandcounty22']
  pop_notmainlandcounty22[, level := 'notmainlandcounty']

  pop_notmainlandmunicip2200 <- pop_jm[, .(year, population = notmainlandmunicip2200, imputed)]
  pop_notmainlandmunicip2200[, municip_code := 'notmainlandmunicip2200']
  pop_notmainlandmunicip2200[, level := 'notmainlandmunicip']

  # match year: from 2005 to 2022
  pop_notmainlandcounty21 <- pop_notmainlandcounty21[year>=2005]
  pop_notmainlandmunicip2100 <- pop_notmainlandmunicip2100[year>=2005]
  pop_notmainlandcounty22 <- pop_notmainlandcounty22[year>=2005]
  pop_notmainlandmunicip2200 <- pop_notmainlandmunicip2200[year>=2005]





  # ukjent ----/
  # age: -99
  # pop: NA
  pop_county_unknown <- data.table(year = unique(pop_municip$year),
                                   population = NA_real_,
                                   municip_code = 'missingcounty99',
                                   level = 'missingcounty',
                                   imputed = F)

  pop_municip_unknown <- data.table(year = unique(pop_municip$year),
                                    population = NA_real_,
                                    municip_code = 'missingmunicip9999',
                                    level = 'missingmunicip',
                                    imputed = F)


  # combine svalbard and unknown, set age, force imputed T for greater than this year
  pop_notmain_missing <- rbind(pop_notmainlandcounty21,
                pop_notmainlandmunicip2100,
                pop_notmainlandcounty22,
                pop_notmainlandmunicip2200,
                pop_county_unknown,
                pop_municip_unknown)
  pop_notmain_missing[, age := -99]
  pop_notmain_missing[, granularity_geo := level]
  pop_notmain_missing[year>lubridate::year(lubridate::today()), imputed := T]



  cat("done \n")

  # final ----
  pop_all[, granularity_geo := level]
  final_order <- c("year", "municip_code", "granularity_geo", "level", "age", "population", "imputed")
  setorderv(pop_all, final_order)
  setcolorder(pop_all, final_order)
  setcolorder(pop_notmain_missing, final_order)

  # finally bind these two
  pop <- rbind(pop_all, pop_notmain_missing)
  setnames(pop, "municip_code", "location_code")


  # pop[, calyear := year]
  # pop[, pop_jan1 := pop]


  return(pop)
}

nor_population_by_age_b0000 <- nor_population_by_age_original(2020)
saveRDS(nor_population_by_age_b0000, "data-raw/data-temp/nor_population_by_age_b0000.rds")





