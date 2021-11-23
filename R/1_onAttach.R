.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "spldata",
    utils::packageDescription("spldata")$Version,
    "https://docs.sykdomspulsen.no/spldata/"
  ))
}
