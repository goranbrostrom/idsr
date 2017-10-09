<!-- README.md is generated from README.Rmd. Please edit that file -->
idsr
====

*idsr* is an **R** package for reading data from data bases stored in the IDS format (Alter and Mandemakers, 2014) and creating data in a form suitable for statistical analysis (in **R**).

Installation
============

Use Hadley Wickham's *devtools*:

    > install.packages(c("devtools", "dplyr", "tidyr", "readxl"))
    > devtools::install_github("goranbrostrom/idsr", build_vignettes = TRUE)
    library(idsr)

and then (*chronicle* and *personal* are included example data frames)

``` r
library(idsr)
dat <- episodes(chronicle, personal)
head(dat)
```

    ## # A tibble: 6 x 8
    ##      Id_I civil_status occupation start_date  enter   exit event    Sex
    ##     <dbl>        <chr>      <chr>     <date>  <dbl>  <dbl> <lgl>  <chr>
    ## 1 1148964      married     Farmer 1804-11-08 20.422 50.999  TRUE   Male
    ## 2 1237852      married       <NA> 1807-04-12 17.999 31.010  TRUE Female
    ## 3 1378563         <NA>       <NA> 1853-02-18  0.000 21.783 FALSE Female
    ## 4 1479856         <NA>       <NA> 1831-12-15 20.132 49.918 FALSE   Male
    ## 5 1567526      married       <NA> 1821-07-26 30.142 64.026  TRUE Female
    ## 6 1897563         <NA>       <NA> 1819-08-05 17.287 32.439 FALSE   Male

and read the documentation and vignettes.

You may need additional tools, you will be told if something is missing.

Reference
=========

Alter, George & Kees Mandemakers, 'The Intermediate Data Structure (IDS) for Longitudinal Historical Microdata, version 4', *Historical Life Course Studies* 1 (2014), 1-26.
