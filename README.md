---
title: idsr
output: html_document
references:
- id: altermande14
  title: The Intermediate Data Structure (IDS) for longitudinal historical microdata, version 4
  author:
  - family: Alter
    given: George
  - family: Mandemakers
    given: Kees
  container-title: Historical Life Course Studies
  volume: 1
  URL: http://hdl.handle.net/10622
  page: 1-26
  type: article-journal
  issued:
    year: 2014
---

*idsr* is an **R** package for reading data from data bases stored in the IDS format [@altermande14] and create data in a form suitable for statistical analysis (in **R**).

## Installation

Use Hadley Wickham's *devtools*:

```
> install.packages("devtools")
> devtools::install_github("goranbrostrom/idsr")
```

and then

```
> library(idsr)
> episodes <- efc(indi, varset)
```
and read the documentation.

# Reference

