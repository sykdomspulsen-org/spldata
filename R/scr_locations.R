#  POPULATION depends on this -----
# gen_norway_locations(2020)
# original: fhidata::norway_locations_b2020
# NEW NAME: location_name_municip_wide

gen_location_name_municip_wide <- function(x_year_end) {

  # variables used by data.table
  is_current <- NULL
  # year_end <- NULL

  # x_year_end <- 2020

  # municip ----
  municip <- redistricting_municip(x_year_end = x_year_end, include_extra_vars = T)

  municip <- municip[year == max(year)]
  municip <- unique(municip[, c("municip_code_current", "municip_name",
                                "county_code", "county_name",
                                'faregion_name','faregion_code')])
  setnames(municip, "municip_code_current", "municip_code")
  locations <- copy(municip)

  # remove svalbard and missing
  locations <- locations[!municip_code %in% c("notmainlandmunicip2100",
                                              "notmainlandmunicip2200",
                                              "missingmunicip9999")]

  # ba ----
  # if x_year_end = 2020 then include baregions (bo- og arbeidsregioner)
  if(x_year_end == 2020){
    ba <- data.table(readxl::read_excel(system.file("rawdata", "locations", "baregioner_2020.xlsx", package = "spldata")))
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


    locations[
      ba,
      on="municip_code",
      baregion_code := baregion_code
    ]
    locations[
      ba,
      on="municip_code",
      baregion_name := baregion_name
    ]


  }

  d <- copy(locations)
  return(d)
}



