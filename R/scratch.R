
data(trndat)

source(here::here('R/funcs.R'))

yr <- 2024

grp <- "2024: EPCHC (Kevin Campbell, Kirsti Martinez, Anthony Chacour)"

truvar <- truvar_fun(trndat, yr)

evalgrp <- evalgrp_fun(trndat, yr, grp, truvar)

evaltrntab_fun(evalgrp)

card_fun(evalgrp, 'Blade Length')

allgrpscr_fun(trndat, yr, truvar)
