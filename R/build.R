library(tbeptools)

source(here::here('R/funcs.R'))

# get training data, all years ----------------------------------------------------------------

trndat <- read_transect(training = TRUE) |> 
  dplyr::filter(!Species %in% 'No Cover') |> 
  dplyr::filter(!anyNA(aveval), .by = c(yr, grp, Site, Species)) # aveval entries as NA will break report generation, this also removes blade length and short shoot density that have entries where abundance is NA for the species

# get misc on dev
url <- 'https://dev.tampabay.wateratlas.usf.edu/seagrass-transect-data-portal/api/assessments/training'
dat <- jsonlite::fromJSON(url)
dev26 <- read_formtransect(dat, training = T, raw = FALSE) |> 
  dplyr::filter(!Species %in% 'No Cover') |> 
  dplyr::filter(!anyNA(aveval), .by = c(yr, grp, Site, Species)) |> 
  dplyr::filter(yr == 2026)

prd26 <- trndat |> 
  dplyr::filter(yr == 2026)

all26 <- dplyr::bind_rows(dev26, prd26) |> 
  dplyr::mutate(
    grp = factor(grpact, levels = unique(sort(grpact)), labels = toupper(letters[1:length(unique(grpact))])),
    grp = as.character(grp)
  ) |> 
  dplyr::arrange(grp)

trndat <- trndat |> 
  dplyr::filter(yr != 2026) |> 
  dplyr::bind_rows(all26)

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
