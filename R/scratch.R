
data(trndat)

source(here::here('R/funcs.R'))

yr <- 2024

grp <- "2024: FWRI (SS, AP, MW, SH, LB, BH)"

truvar <- truvar_fun(trndat, yr)

allgrpscr <- allgrpscr_fun(trndat, yr, truvar)

evalgrp <- evalgrp_fun(trndat, yr, grp, truvar)

# evaltrntab_fun(evalgrp)

card_fun(evalgrp, grp, allgrpscr, 'Abundance')

scrsum_fun(allgrpscr, grp)
