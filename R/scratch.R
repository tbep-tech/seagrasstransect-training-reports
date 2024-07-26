
data(trndat)

source(here::here('R/funcs.R'))

yr <- 2023

grp <- "2023: EPCHC (Campbell, Kevin)"

truvar <- truvar_fun(trndat, yr)

allgrpscr <- allgrpscr_fun(trndat, yr, truvar)

evalgrp <- evalgrp_fun(trndat, yr, grp, truvar)

evaltrntab_fun(evalgrp)

card_fun(evalgrp, grp, allgrpscr, 'Blade Length')

scrsum_fun(allgrpscr, grp)

scrimp_fun(allgrpscr, grp, 'Abundance')

sppimp_fun(evalgrp, 'macroalgae')
