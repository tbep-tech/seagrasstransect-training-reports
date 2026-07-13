library(tbeptools)

source(here::here('R/funcs.R'))

# get training data, all years ----------------------------------------------------------------

trndat <- read_transect(training = TRUE) |> 
  dplyr::filter(!Species %in% 'No Cover') |> 
  dplyr::filter(!anyNA(aveval), .by = c(yr, grp, Site, Species)) # aveval entries as NA will break report generation, this also removes blade length and short shoot density that have entries where abundance is NA for the species

# zero out 2026 MCDNR blade length and short shoot at all transects except 2, 7, and 8
trndat <- trndat |> 
  dplyr::mutate(
    aveval = dplyr::case_when(
      yr == 2026 & MonitoringAgency == 'MCNRD' & !Site %in% c('2', '7', '8') & var %in% c('Blade Length', 'Short Shoot Density') ~ 0,
      TRUE ~ aveval
    ),
    sdval = dplyr::case_when(
      yr == 2026 & MonitoringAgency == 'MCNRD' & !Site %in% c('2', '7', '8') & var %in% c('Blade Length', 'Short Shoot Density') ~ 0,
      TRUE ~ sdval
    )
  )

save(trndat, file = here::here('data/trndat.rda'), compress = 'bzip2', version = 2)

# create reports for the year -----------------------------------------------------------------

data(trndat)

# yrs <- 2025
yrs <- unique(trndat$yr)

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

# build dashboard -----------------------------------------------------------------------------

shinylive::export('app', 'docs/app')
httpuv::runStaticServer("docs/app") # test
