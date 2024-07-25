library(tbeptools)

source(here::here('R/funcs.R'))

# get training data, all years ----------------------------------------------------------------

trndat <- read_transect(training = TRUE) |> 
  dplyr::filter(Species %in% c('Halodule', 'Syringodium', 'Thalassia', 'Halophila', 'Ruppia'))

save(trndat, file = here::here('data/trndat.rda'), compress = 'bzip2', version = 2)

# create reports for the year -----------------------------------------------------------------

data(trndat)

# yrs <- 2024
yrs <- unique(trndat$yr)

purrr::walk(yrs, ~ proc_grp(trndat, .x, quiet = F))

# trndattmp <- trndat |>
#   dplyr::filter(yr == !!yrs) |>
#   dplyr::filter(grp == 'B')
# 
# proc_grp(trndattmp, yrs, quiet = F)

# create index --------------------------------------------------------------------------------

data(trndat)

writeindex_fun(trndat)
