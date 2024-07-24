library(tbeptools)

source(here::here('R/funcs.R'))

# get training data, all years ----------------------------------------------------------------

trndat <- read_transect(training = TRUE)

save(trndat, file = here('data/trndat.rda'), compress = 'bzip2', version = 2)

# create reports for the year -----------------------------------------------------------------

data(trndat)

yrs <- 2024
# yrs <- unique(trndat$yr)

purrr::walk(yrs, ~ proc_grp(trndat, .x, quiet = F))

# trndattmp <- trndat |>
#   dplyr::filter(yr == !!yrs) |>
#   dplyr::filter(grp == 'B')
# 
# proc_grp(trndattmp, yr, quiet = F)

# create index --------------------------------------------------------------------------------

data(trndat)

writeindex_fun(trndat)
