library(tbeptools)

source(here::here('R/funcs.R'))

# get training data, all years ----------------------------------------------------------------

trndat <- read_transect(training = TRUE) |> 
  dplyr::filter(!Species %in% 'No Cover') |> 
  dplyr::filter(!is.na(aveval)) # aveval entries as NA will break report generation

save(trndat, file = here::here('data/trndat.rda'), compress = 'bzip2', version = 2)

# create reports for the year -----------------------------------------------------------------

data(trndat)

yrs <- 2025
# yrs <- unique(trndat$yr)

purrr::walk(yrs, ~ proc_grp(trndat, .x, quiet = F))

# trndattmp <- trndat |>
#   dplyr::filter(yr == !!yrs) |>
#   dplyr::filter(grp == 'A')
# 
# proc_grp(trndattmp, yrs, quiet = F)

# get all group scores across years -----------------------------------------------------------

data(trndat)

allyrscrs <- allyrscr_fun(trndat)

save(allyrscrs, file = here::here("app/data/allyrscrs.RData"))

# create index --------------------------------------------------------------------------------

data(trndat)

writeindex_fun(trndat)
