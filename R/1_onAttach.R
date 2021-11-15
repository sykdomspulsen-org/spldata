.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "spldata",
    utils::packageDescription("spldata")$Version,
    "https://sykdomspulsen-org.github.io/spldata/"
  ))
}
