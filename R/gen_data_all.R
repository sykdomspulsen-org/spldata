# Creates the norway_locations, norway_municip_merging, and norway_population data.table
# @param base_loc Folder where data will be saved

gen_data_all <- function(base_loc) {
  # gen_data_all(file.path(getwd(),"data"))
  # base_loc = file.path(getwd(),"data")


  # nordic ----
  denmark_locations_names_b2020 <- gen_denmark_locations_names(2020)
  save(denmark_locations_names_b2020, file = file.path(base_loc, paste0("denmark_locations_names_b2020",".rda")), compress = "xz")

  sweden_locations_names_b2020 <- gen_sweden_locations_names(2020)
  save(sweden_locations_names_b2020, file = file.path(base_loc, paste0("sweden_locations_names_b2020",".rda")), compress = "xz")

  finland_locations_names_b2020 <- gen_finland_locations_names(2020)
  save(finland_locations_names_b2020, file = file.path(base_loc, paste0("finland_locations_names_b2020",".rda")), compress = "xz")

  iceland_locations_names_b2020 <- gen_iceland_locations_names(2020)
  save(iceland_locations_names_b2020, file = file.path(base_loc, paste0("iceland_locations_names_b2020",".rda")), compress = "xz")

  # denmark_population_by_age_b2020 <- gen_denmark_population_by_age(2020)
  # save(denmark_population_by_age_b2020, file = file.path(base_loc, paste0("denmark_population_by_age_b2020",".rda")), compress = "xz")
  #
  # sweden_population_by_age_b2020 <- gen_sweden_population_by_age(2020)
  # save(sweden_population_by_age_b2020, file = file.path(base_loc, paste0("sweden_population_by_age_b2020",".rda")), compress = "xz")
  #
  # finland_population_by_age_b2020 <- gen_finland_population_by_age(2020)
  # save(finland_population_by_age_b2020, file = file.path(base_loc, paste0("finland_population_by_age_b2020",".rda")), compress = "xz")
  #
  # iceland_population_by_age_b2020 <- gen_iceland_population_by_age(2020)
  # save(iceland_population_by_age_b2020, file = file.path(base_loc, paste0("iceland_population_by_age_b2020",".rda")), compress = "xz")

  # icpc_codes ----
  icpc2_codes <- gen_icpc2_codes()
  save(icpc2_codes, file = file.path(base_loc, paste0("icpc2_codes",".rda")), compress = "xz")






}





