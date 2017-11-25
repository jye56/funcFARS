## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(funcFARS)


## ------------------------------------------------------------------------

filename <- make_filename(2015)
filepath <- system.file("extdata", filename, package = "funcFARS")
myfile <- fars_read(filepath)

head(myfile)


## ------------------------------------------------------------------------
oldwd <- getwd()
setwd(system.file("extdata", package = "funcFARS"))
fars_read_years(c(2013,2015))
setwd(oldwd)


## ------------------------------------------------------------------------

oldwd <- getwd()
setwd(system.file("extdata", package = "funcFARS"))
fars_summarize_years(c(2013,2014,2015))

setwd(oldwd)


## ---- fig.show='hold'----------------------------------------------------

oldwd <- getwd()
setwd(system.file("extdata", package = "funcFARS"))
fars_map_state(12,2015)
setwd(oldwd)


