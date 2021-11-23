# Creates the norway_locations, norway_municip_merging, and norway_population data.table
# @param base_loc Folder where data will be saved

gen_data_all <- function(base_loc) {
  # gen_data_all(file.path(getwd(),"data"))
  # base_loc = file.path(getwd(),"data")

  # ____ norway locations ____ ----

  # 1. redistricting ----
  norway_locations_redistricting_b2020 <- nor_loc_redistricting_all(2020)
  save(norway_locations_redistricting_b2020, file = file.path(base_loc, paste0("norway_locations_redistricting_b2020",".rda")), compress = "xz")


  # 2. hierarchy ----
  norway_locations_hierarchy_b2020 <- nor_loc_hierarchy_all(x_year_end = 2020)
  save(norway_locations_hierarchy_b2020, file = file.path(base_loc, paste0("norway_locations_hierarchy_b2020",".rda")), compress = "xz")


  # 3. names ----
  norway_locations_names_b2020 <- nor_loc_name_all(2020)
  save(norway_locations_names_b2020, file = file.path(base_loc, paste0("norway_locations_names_b2020",".rda")), compress = "xz")


  # ____ norway population ____ ----
  norway_population_by_age_b2020 <- nor_population_by_age(2020)
  save(norway_population_by_age_b2020, file = file.path(base_loc, paste0("norway_population_by_age_b2020",".rda")), compress = "xz")



  # ____ nordic ____----

  # locations ---- #
  denmark_locations_names_b2020 <- dnk_loc_names(2020)
  save(denmark_locations_names_b2020, file = file.path(base_loc, paste0("denmark_locations_names_b2020",".rda")), compress = "xz")

  sweden_locations_names_b2020 <- swe_loc_names(2020)
  save(sweden_locations_names_b2020, file = file.path(base_loc, paste0("sweden_locations_names_b2020",".rda")), compress = "xz")

  finland_locations_names_b2020 <- fin_loc_names(2020)
  save(finland_locations_names_b2020, file = file.path(base_loc, paste0("finland_locations_names_b2020",".rda")), compress = "xz")

  iceland_locations_names_b2020 <- isl_loc_names(2020)
  save(iceland_locations_names_b2020, file = file.path(base_loc, paste0("iceland_locations_names_b2020",".rda")), compress = "xz")

  # population ---- #
  denmark_population_by_age_b2020 <- dnk_population_by_age(2020)
  save(denmark_population_by_age_b2020, file = file.path(base_loc, paste0("denmark_population_by_age_b2020",".rda")), compress = "xz")

  sweden_population_by_age_b2020 <- swe_population_by_age(2020)
  save(sweden_population_by_age_b2020, file = file.path(base_loc, paste0("sweden_population_by_age_b2020",".rda")), compress = "xz")

  finland_population_by_age_b2020 <- fin_population_by_age(2020)
  save(finland_population_by_age_b2020, file = file.path(base_loc, paste0("finland_population_by_age_b2020",".rda")), compress = "xz")

  iceland_population_by_age_b2020 <- isl_population_by_age(2020)
  save(iceland_population_by_age_b2020, file = file.path(base_loc, paste0("iceland_population_by_age_b2020",".rda")), compress = "xz")



  # ____misc ____ ----
  # icpc_codes ----
  icpc2_codes <- gen_icpc2_codes()
  save(icpc2_codes, file = file.path(base_loc, paste0("icpc2_codes",".rda")), compress = "xz")




}





