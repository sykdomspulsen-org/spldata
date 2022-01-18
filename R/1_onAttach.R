.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "spldata ",
    utils::packageDescription("spldata")$Version,
    "\n",
    "https://docs.sykdomspulsen.no/spldata"
  ))
}
