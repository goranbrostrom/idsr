<!-- README.md is generated from README.Rmd. Please edit that file -->
News
====

This is *idsr Version 0.1.0*, the first *release* (21 May 2017).

idsr
====

*idsr* is an **R** package for reading data from data bases stored in the IDS format (Alter and Mandemakers, 2014) and creating data in a form suitable for statistical analysis (in **R**).

It is still under development and you use it on your own risk. But please try it and report!

### Installation

Use Hadley Wickham's *devtools*:

    > install.packages(c("devtools", "dplyr", "tidyr", "readxl"))
    > devtools::install_github("goranbrostrom/idsr", build_vignettes = TRUE)

and then

    > library(idsr)
    > episodes <- episodes(chronicle, personal)

and read the documentation and vignette.

You may need additional tools, you will be told if something is missing.

Reference
---------

Alter, George & Kees Mandemakers, 'The Intermediate Data Structure (IDS) for Longitudinal Historical Microdata, version 4', *Historical Life Course Studies* 1 (2014), 1-26.
