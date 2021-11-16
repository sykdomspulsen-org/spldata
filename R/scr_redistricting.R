# check the old and new merging

# old (gen_norway_municip_merging) ----
# 1. rename the function, it is the same as redistricting
# 2. rename skeleton into something meaningful
# 3. break the while loop
# 4. this table is too wide, consider using smaller excels
# IMPORTANT: for now do NOT optimise. just recycle the old code!


gen_norway_municip_merging <- function(
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

  # x_year_start = 2000
  # x_year_end = 2020
  ys <- x_year_start
  ye <- x_year_end

  masterData <- data.table(readxl::read_excel(system.file("rawdata", "locations", "norway_locations.xlsx", package = "spldata")))
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

  # expand for each year ----
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

  return(invisible(skeleton))
}

gen_norway_fixing_merged_municips <- function(x_year_end, include_extra_vars = FALSE) {
  border_end <- NULL
  border_start <- NULL
  municip_code_end_new <- NULL
  weighting_new <- NULL

  plan <- expand.grid(
    border_start = 2000:2020,
    border_end = 2019:x_year_end
  )
  setDT(plan)
  plan <- plan[border_end >= border_start]

  retval <- vector("list", length = nrow(plan))
  for (i in seq_along(retval)) {
    print(i)
    temp <- gen_norway_municip_merging(
      x_year_end = plan$border_end[i],
      x_year_start = plan$border_start[i],
      include_extra_vars = include_extra_vars
    )
    temp[, border_start := plan$border_start[i]]
    temp[, border_end := plan$border_end[i]]

    past_years <- c(2000:plan$border_start[i])
    past_years <- past_years[-length(past_years)]
    if (length(past_years) > 0) {
      for (j in past_years) {
        temp1 <- copy(temp[year == min(year)])
        temp1[, year := year - 1]
        temp <- rbind(temp, temp1)
      }
    }

    retval[[i]] <- temp
  }

  retval <- rbindlist(retval)

  return(retval)
}





