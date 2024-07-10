
data(trndat)

source(here::here('R/funcs.R'))

yr <- 2024

grp <- "2024: FWRI (SS, AP, MW, SH, LB, BH)"

truvar <- truvar_fun(trndat, yr)

allgrpscr <- allgrpscr_fun(trndat, yr, truvar)

grpscr <- allgrpscr |> 
  dplyr::filter(grpact == !!grp) |>
  dplyr::select(-grpact)

evalgrp <- evalgrp_fun(trndat, yr, grp, truvar)

# evaltrntab_fun(evalgrp)

card_fun(evalgrp, grpscr, 'Abundance')
