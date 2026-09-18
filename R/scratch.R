library(tidyverse)
library(here)

source(here('R', 'funcs.R'))

load(file = here('data/trndat.rda'))

tmp <- calibrate_scr_fun(trndat)

yr <- 2020
tv <- truvar_fun(trndat, yr)
tmp <- allgrpscr_fun(trndat, yr, tv, raw_diff = TRUE)
