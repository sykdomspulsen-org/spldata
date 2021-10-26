.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "spldata",
    utils::packageDescription("spldata")$Version,
    "https://folkehelseinstituttet.github.io/spldata/"
  ))
}
