library(tbeptools)
library(here)

source(here('R/funcs.R'))

# get training data, all years ----------------------------------------------------------------

trndat <- read_transect(training = TRUE)

save(trndat, file = here('data/trndat.rda'), compress = 'bzip2', version = 2)

# create reports for the year -----------------------------------------------------------------

yr <- 2024

proc_grp(trndat, yr, quiet = F)