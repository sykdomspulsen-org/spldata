# spldata <a href="https://docs.sykdomspulsen.no/spldata"><img src="man/figures/logo.png" align="right" width="120" /></a>

## Overview 

[spldata](https://docs.sykdomspulsen.no/spldata) contains preformatted structural data for Norway.

Datasets relating to maps, population in municipalities, municipality/county matching, and how different municipalities have merged/redistricted over time from 2006 to 2020.

Read the introduction vignette [here](http://docs.sykdomspulsen.no/splmaps/articles/spldata.html) or run `help(package="spldata")`.

## splverse

<a href="https://docs.sykdomspulsen.no/packages"><img src="https://docs.sykdomspulsen.no/packages/splverse.png" align="right" width="120" /></a>

The [splverse](https://docs.sykdomspulsen.no/packages) is a set of R packages developed to help solve problems that frequently occur when performing infectious disease surveillance.

If you want to install the dev versions (or access packages that haven't been released on CRAN), run `usethis::edit_r_profile()` to edit your `.Rprofile`. 

Then write in:

```
options(
  repos = structure(c(
    SPLVERSE  = "https://docs.sykdomspulsen.no/drat/",
    CRAN      = "https://cran.rstudio.com"
  ))
)
```

Save the file and restart R.

You can now install [splverse](https://docs.sykdomspulsen.no/packages) packages from our [drat registry](https://docs.sykdomspulsen.no/drat).

```
install.packages("splmaps")
```

