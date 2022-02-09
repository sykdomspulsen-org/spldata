.onAttach <- function(libname, pkgname) {
  version <- tryCatch(
    utils::packageDescription("spldata", fields = "Version"),
    warning = function(w){
      1
    }
  )

  packageStartupMessage(paste0(
    "spldata ",
    version,
    "\n",
    "https://docs.sykdomspulsen.no/spldata"
  ))
}
