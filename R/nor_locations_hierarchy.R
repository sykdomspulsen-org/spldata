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

  x <- get0("nor_locations_hierarchy_b2020", envir = asNamespace("spldata"))
  d <- copy(x)
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

#' Hierarchies in Norway (programmable borders).
#'
#' @param from wardoslo, wardbergen, wardtrondheim, wardstavanger, municip, baregion, county, region, faregion, notmainlandmunicip, notmainlandcounty, missingmunicip, missingcounty
#' @param to wardoslo, wardbergen, wardtrondheim, wardstavanger, municip, baregion, county, region, faregion, notmainlandmunicip, notmainlandcounty, missingmunicip, missingcounty
#' @param include_to_name Do you want to include the name of the 'to' location?
#' @param border The border year
#' @examples
#' spldata::norway_locations_hierarchy_from_to(from="wardoslo", to="county")
#' spldata::norway_locations_hierarchy_from_to(from="municip", to="baregion")
#' @export
norway_locations_hierarchy_from_to <- function(
  from,
  to,
  include_to_name = FALSE,
  border = spldata::config$border
){
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
