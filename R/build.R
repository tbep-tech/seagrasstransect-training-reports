library(tbeptools)

source(here::here('R/funcs.R'))

# get training data, all years ----------------------------------------------------------------

trndat <- read_transect(training = TRUE)

save(trndat, file = here('data/trndat.rda'), compress = 'bzip2', version = 2)

# create reports for the year -----------------------------------------------------------------

yr <- 2024

trndattmp <- trndat |>
  dplyr::filter(yr == !!yr) |>
  dplyr::filter(grp == 'A')

proc_grp(trndattmp, yr, quiet = F)
proc_grp(trndat, yr, quiet = F)
