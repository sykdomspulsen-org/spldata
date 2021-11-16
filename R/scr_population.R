# need to compare the difference between multiple copies of population..


# old ====
# this is what redistricting county depends on


gen_norway_population <- function(x_year_end, original = FALSE) {

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


  popFiles <- c(
    "Personer2005-2009.csv",
    "Personer2010-2014.csv",
    "Personer2015-2018.csv",
    "Personer2019.csv",
    "Personer2020.csv",
    "Personer2021.csv",
    "Personerward2001-2020.csv",
    "Personerward2021.csv"

  )
  pop <- vector("list", length = length(popFiles))
  for (i in seq_along(pop)) {
    pop[[i]] <- fread(system.file("rawdata", "population", popFiles[i],
                                  package = "spldata"), encoding = "UTF-8")
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


  # add year, age numeric
  pop[, year := as.numeric(stringr::str_extract(variable, "[0-9][0-9][0-9][0-9]$"))]
  pop[, agenum := as.numeric(stringr::str_extract(age, "^[0-9]*"))]
  pop[, age := NULL]
  setnames(pop, "agenum", "age")
  pop

  # sum population by municip
  pop <- pop[municip_code != "municipNA"]
  pop_municip <- pop[, .(
    population = sum(value)
  ), keyby = .(
    municip_code, age, year
  )]

  # !!!! Fixing broken parts in the population data ----
  # part 1: municip0710
  # pop2 <- pop[municip_code == "municip0710" & year <= 2017]
  # pop2[, pop := max(pop), by = age]
  # pop2 <- pop2[year != max(year)]
  # pop2[, municip_code := "municip0706"]
  # pop2[, pop := round(pop / 3)]
  # pop <- rbind(pop, pop2)
  #
  # pop2 <- pop[municip_code == "municip0710" & year <= 2017]
  # pop2[, pop := max(pop), by = age]
  # pop2 <- pop2[year != max(year)]
  # pop2[, municip_code := "municip0719"]
  # pop2[, pop := round(pop / 3)]
  # pop <- rbind(pop, pop2)
  #
  # pop2 <- pop[municip_code == "municip0710" & year <= 2017]
  # pop2[, pop := max(pop), by = age]
  # pop2 <- pop2[year != max(year)]
  # pop2[, municip_code := "municip0720"]
  # pop2[, pop := round(pop / 3)]
  # pop <- rbind(pop, pop2)
  #
  # # part 2: municip1756
  # pop2 <- pop[municip_code == "municip1756" & year <= 2012]
  # pop2[, pop := max(pop), by = age]
  # pop2 <- pop2[year != max(year)]
  # pop2[, municip_code := "municip1723"]
  # pop2[, pop := round(pop / 2)]
  # pop <- rbind(pop, pop2)
  #
  # pop2 <- pop[municip_code == "municip1756" & year <= 2012]
  # pop2[, pop := max(pop), by = age]
  # pop2 <- pop2[year != max(year)]
  # pop2[, municip_code := "municip1729"]
  # pop2[, pop := round(pop / 2)]
  # pop <- rbind(pop, pop2)
  #
  # # part 3: municip5046
  # pop2 <- pop[municip_code == "municip5046" & year <= 2018]
  # pop2[, pop := max(pop), by = age]
  # pop2 <- pop2[year != max(year)]
  # pop2[, municip_code := "municip1901"]
  # pop2[, pop := round(pop / 2)]
  # pop <- rbind(pop, pop2)
  #
  # pop2 <- pop[municip_code == "municip1756" & year <= 2018]
  # pop2[, pop := max(pop), by = age]
  # pop2 <- pop2[year != max(year)]
  # pop2[, municip_code := "municip1915"]
  # pop2[, pop := round(pop / 2)]
  # pop <- rbind(pop, pop2)
  #
  # # part 4: municip1505
  # pop2 <- pop[municip_code == "municip1505" & year <= 2008]
  # pop2[, pop := max(pop), by = age]
  # pop2 <- pop2[year != max(year)]
  # pop2[, municip_code := "municip1503"]
  # pop2[, pop := round(pop / 2)]
  # pop <- rbind(pop, pop2)
  #
  # pop2 <- pop[municip_code == "municip1505" & year <= 2008]
  # pop2[, pop := max(pop), by = age]
  # pop2 <- pop2[year != max(year)]
  # pop2[, municip_code := "municip1556"]
  # pop2[, pop := round(pop / 2)]
  # pop <- rbind(pop, pop2)


  pop_municip[, imputed := FALSE]




  if (original) {
    return(pop_municip)
  }

  # kommunesammenslaing ----
  # x_year_end <- 2020
  # this requires municip_merging! ---- !!!!

  # merging? redistri?
  merging_municip <- gen_norway_municip_merging(x_year_end = x_year_end)
  # municip_code_current, municip_code_original, year, weighting
  # 2000-2030, municips
  # 734 unique rows
  merging_ward <- gen_norway_ward_merging(x_year_end = x_year_end)
  # 2000-2030, wards
  # 50 unique rows

  setnames(merging_ward, names(merging_municip))
  norway_merging <- rbind(merging_1, merging_2)

  # merge with population
  pop <- merge(
    pop,
    norway_merging[, c("year", "municip_code_current", "municip_code_original")],
    by.x = c("municip_code", "year"),
    by.y = c("municip_code_original", "year")
  )
  pop <- pop[, .(pop = sum(pop)),
             keyby = .(
               year,
               municip_code = municip_code_current,
               age,
               imputed
             )
  ]

  # so far 2005 to 2020
  # year, municip_code, age, imputed, pop
  # imputing the future (2 years+)
  missingYears <- max(pop$year):(lubridate::year(lubridate::today()) + 2)
  if (length(missingYears) > 1) {
    copiedYears <- vector("list", length = length(missingYears) - 1)
    for (i in seq_along(copiedYears)) {
      copiedYears[[i]] <- pop[year == missingYears[1]]
      copiedYears[[i]][, year := year + i]
    }
    copiedYears <- rbindlist(copiedYears)
    copiedYears[, imputed := TRUE]
    pop <- rbind(pop, copiedYears)
  }

  pop[, level := stringr::str_extract(municip_code, "^[a-z]+")]





  # county, ba  ----
  # county ----/
  counties <- merge(
    pop,
    gen_norway_locations(x_year_end = x_year_end)[, c("municip_code", "county_code")],
    by = "municip_code"
  )

  check_ref_to_new(
    xref = unique(pop[level=="municip"]$municip_code),
    xnew = unique(counties$municip_code)
  )

  if (nrow(counties) != nrow(pop[level=="municip"])) {
    stop("nrow(counties) != nrow(pop)")
  }

  counties <- counties[, .(
    pop = sum(pop)
  ), keyby = .(
    year,
    municip_code = county_code,
    age,
    imputed
  )]
  counties[, level := "county"]

  # making baregion pop data ---- /
  if(x_year_end==2020){
    baregions <- merge(
      pop,
      gen_norway_locations(x_year_end = x_year_end)[, c("municip_code", "baregion_code")],
      by = "municip_code"
    )

    baregions <- baregions[!is.na(baregion_code), .(
      pop = sum(pop)
    ), keyby = .(
      year,
      municip_code = baregion_code,
      age,
      imputed
    )]
    baregions[, level := "baregion"]
  }

  # norway ----
  norway <- data.table(utils::read.csv(url("https://data.ssb.no/api/v0/dataset/59322.csv?lang=en"), stringsAsFactors = FALSE))
  norway <- norway[sex == "0 Both sexes"]
  norway[, sex := NULL]
  norway[, contents := NULL]
  norway[, x := as.numeric(stringr::str_extract(age, "^[0-9][0-9][0-9]"))]
  norway[, age := NULL]
  setnames(norway, c("year", "pop", "age"))
  norway[, level := "nation"]
  norway[, municip_code := "norge"]
  norway[, imputed := FALSE]
  missingYearsNational <- (max(norway$year) + 1):(lubridate::year(lubridate::today()) + 2)
  for (i in missingYearsNational) {
    popx <- norway[year == max(year)]
    popx[, year := i]
    popx[, imputed := TRUE]
    norway <- rbind(norway, popx)
  }


  if(x_year_end==2020){
    pop <- rbind(norway, counties, pop, baregions)
  } else {
    pop <- rbind(norway, counties, pop)
  }


  # notmainland, missing ----
  #  svalbard + jan mayen ----/
  # age: -99
  pop_svalbard_raw <- readxl::read_excel(system.file("rawdata", "population", "Personer_svalbard_1990-2020.xlsx", package = "fhidata"))
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



  # add missing years
  if (length(missingYears) > 1) {
    copiedYears <- vector("list", length = length(missingYears) - 1)
    for (i in seq_along(copiedYears)) {
      copiedYears[[i]] <- pop_sv[year == missingYears[1]]
      copiedYears[[i]][, year := year + i]
    }
    copiedYears <- rbindlist(copiedYears)
    copiedYears[, imputed := TRUE]
    pop_sv <- rbind(pop_sv, copiedYears)
  }


  # jan mayen (county22)
  pop_jm <- data.table(year = unique(c(as.numeric(years), missingYears)),
                       notmainlandmunicip2200 = 26,
                       notmainlandcounty22 = 26,
                       imputed = F)
  pop_jm[year>lubridate::year(lubridate::today()), imputed := T]


  # separate county, municip
  pop_notmainlandcounty21 <- pop_sv[, .(year, pop = notmainlandcounty21, imputed)]
  pop_notmainlandcounty21[, municip_code := 'notmainlandcounty21']
  pop_notmainlandcounty21[, level := 'notmainlandcounty']

  pop_notmainlandmunicip2100 <- pop_sv[, .(year, pop = notmainlandmunicip2100, imputed)]
  pop_notmainlandmunicip2100[, municip_code := 'notmainlandmunicip2100']
  pop_notmainlandmunicip2100[, level := 'notmainlandmunicip']

  pop_notmainlandcounty22 <- pop_jm[, .(year, pop = notmainlandcounty22, imputed)]
  pop_notmainlandcounty22[, municip_code := 'notmainlandcounty22']
  pop_notmainlandcounty22[, level := 'notmainlandcounty']

  pop_notmainlandmunicip2200 <- pop_jm[, .(year, pop = notmainlandmunicip2200, imputed)]
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
  pop_county_unknown <- data.table(year = unique(pop$year),
                                   pop = NA_real_,
                                   municip_code = 'missingcounty99',
                                   level = 'missingcounty',
                                   imputed = F)

  pop_municip_unknown <- data.table(year = unique(pop$year),
                                    pop = NA_real_,
                                    municip_code = 'missingmunicip9999',
                                    level = 'missingmunicip',
                                    imputed = F)


  # combine svalbard and unknown, set age, force imputed T for greater than this year
  popx <- rbind(pop_notmainlandcounty21,
                pop_notmainlandmunicip2100,
                pop_notmainlandcounty22,
                pop_notmainlandmunicip2200,
                pop_county_unknown,
                pop_municip_unknown)
  popx[, age := -99]
  popx[, granularity_geo := level]
  popx[year>lubridate::year(lubridate::today()), imputed := T]


  # final ----
  pop[, granularity_geo := level]
  final_order <- c("year", "municip_code", "granularity_geo", "level", "age", "pop", "imputed")
  setorderv(pop, final_order)
  setcolorder(pop, final_order)
  setcolorder(popx, final_order)

  # finally bind these two
  pop <- rbind(pop, popx)
  setnames(pop, "municip_code", "location_code")


  # pop[, calyear := year]
  # pop[, pop_jan1 := pop]


  return(invisible(pop))
}



