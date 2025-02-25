#' Process training report by group
#'
#' @param trndat data frame, training data
#' @param yr integer, year
#' @param quiet logical, suppress messages
#'
#' @value html files are saved to the doc folder
proc_grp <- function(trndat, yr, quiet = F){
  
  # get data and format
  datyr <- trndat |> 
    dplyr::filter(yr == !!yr) |> 
    dplyr::select(-yr)
  
  transect <- datyr$Site |> 
    unique() |> 
    as.numeric() |> 
    sort()
  transect <- paste('Transect', transect)
  
  grps <- datyr$grpact |> 
    unique()
  
  # "true" scores as average
  truvar <- truvar_fun(trndat, yr)
  
  # group report card
  allgrpscr <- allgrpscr_fun(trndat, yr, truvar)
  
  for(grp in grps){
    
    if(!quiet){
      cat('Processing group: ', grp, '\n')
    }
    
    grpnoyr <- gsub('^\\d{4}:\\s', '', grp)
    
    # scores for metrics and total
    grpscr <- allgrpscr |> 
      dplyr::filter(grpact == !!grp) |>
      dplyr::select(-grpact)
    
    # total score summary compared to other groups
    scrsum <- scrsum_fun(allgrpscr, grp)
    
    # define parameters
    params <- list(
      yr = yr, 
      grp = grp,
      grpnoyr = grpnoyr,
      transect = transect,
      truvar = truvar,
      grpscr = grpscr,
      allgrpscr = allgrpscr,
      scrsum = scrsum
    )

    outputfl <- trndat |> 
      dplyr::filter(yr == !!yr) |> 
      dplyr::filter(grpact == !!grp) |> 
      dplyr::select(grp, yr) |> 
      tidyr::unite('fl', grp, yr) |> 
      unique() |> 
      dplyr::mutate(fl = paste0(fl, '.html')) |>
      dplyr::pull(fl)

    quarto::quarto_render(
      input = here::here('template.qmd'),
      execute_params = params,
      output_file = outputfl, 
      quiet = T
    )
    
    file.rename(
      from = outputfl,
      to = here::here('docs', outputfl)
    )
  }
  
}

#' Write index.html file for training report cards
#' 
#' @param trndata data frame of transect training data
writeindex_fun <- function(trndat){
  
  fls <- list.files(here::here('docs'), full.names = F)
  fls <- fls[!grepl('index.html', fls)]
  
  # create html string for lists by year and group
  flsdf <- tibble::tibble(
    fls = fls
  ) |> 
    dplyr::mutate(
      yr = gsub(fls, pattern = '.*_(\\d{4}).*', replacement = '\\1'), 
      grp = gsub(fls, pattern = '(.*)_(\\d{4}).*', replacement = '\\1')
    ) |> 
    dplyr::rowwise() |> 
    dplyr::mutate( 
      grpact = unique(trndat$grpact[trndat$yr == yr & trndat$grp == grp]), 
      grpact = gsub('^\\d{4}: ', '', grpact),
      grphtml = paste0('<li><a href="', fls, '">', grpact, '</a></li>')
    ) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(yr, grp) |> 
    dplyr::select(yr, grphtml) |> 
    dplyr::mutate(
      yr = paste0('<h2>', yr, '</h2>\n')
    ) |> 
    dplyr::group_nest(yr) |> 
    dplyr::arrange(desc(yr)) |> 
    dplyr::mutate(
      data = purrr::map(data, ~ dplyr::pull(.x) |> paste0(collapse = '\n')),
      data = purrr::map(data, ~ paste0('<ul>\n', .x, '\n</ul>\n'))
    ) |> 
    tidyr::unnest('data') |> 
    tidyr::unite('yr', yr, data, sep = '') |> 
    dplyr::pull('yr') |> 
    paste0(collapse = '') 
  
  # add html header tags
  towrt <- paste0('<html>\n<body>\n<h1>Transect training report cards</h1>\n', flsdf, '</body>\n</html>')
  
  # write output
  writeLines(towrt, con = here::here('docs/index.html'))
  
}

#' Get "true" values from training data for a given year
#' 
#' @param trndat data frame, training data
#' @param yr integer, year
truvar_fun <- function(trndat, yr){
  
  abulev <- c('0', '0.1', '0.5', '1', '2', '3', '4', '5')
  abulab <- c('no coverage', 'solitary', 'few', '<5%', '5-25%', '25-50%', '51-75%', '76-100%')
  
  out <- trndat |> 
    dplyr::filter(yr == !!yr) |>
    tidyr::pivot_wider(names_from = var, values_from = aveval) |>
    dplyr::mutate(
      Abundance = factor(Abundance, levels = abulev), 
      Abundance = as.numeric(Abundance)
    ) |> 
    dplyr::summarise(
      Abundance = round(mean(Abundance, na.rm = T), 0),
      `Blade Length` = mean(`Blade Length`, na.rm = T),
      `Short Shoot Density` = mean(`Short Shoot Density`, na.rm = T), 
      .by = c(Site, Species)
    ) |> 
    dplyr::mutate(
      Abundance = factor(Abundance, levels = seq_along(abulev), labels = abulev),
      Abundance = as.numeric(as.character(Abundance))
    ) |> 
    tidyr::pivot_longer(
      cols = -c(Site, Species),
      names_to = 'var',
      values_to = 'truval'
    ) 
  
  return(out)
  
}

#' Get group difference from "true"
#' 
#' @param datyr data frame, training data for selected year
#' @param grp character, group name
#' @param truvar data frame, true values
#' 
#' @return data frame of group data compard to "true" values
evalgrp_fun <- function(trndat, yr, grp, truvar){
  
  abulev <- c('0', '0.1', '0.5', '1', '2', '3', '4', '5')
  abulab <- c('no coverage', 'solitary', 'few', '<5%', '5-25%', '25-50%', '51-75%', '76-100%')
  
  datyrgrp <- trndat |> 
    dplyr::filter(yr == !!yr) |> 
    dplyr::filter(grpact == !!grp) |> 
    dplyr::select(Site, Species, var, aveval)

  out <- datyrgrp |> 
    dplyr::full_join(truvar, by = c('Site', 'Species', 'var')) |> 
    dplyr::filter(!(aveval == 0 & truval == 0)) |> 
    tidyr::pivot_longer(
      cols = c(aveval, truval),
      names_to = 'valtype',
      values_to = 'val'
    ) |>
    tidyr::unite('valtype', c(var, valtype), sep = ' ') |>
    tidyr::pivot_wider(
      names_from = valtype,
      values_from = val
    ) |>
    dplyr::mutate(
      `Abundance aveval` = factor(`Abundance aveval`, levels = abulev, labels = abulab),
      `Abundance truval` = factor(`Abundance truval`, levels = abulev, labels = abulab)
    ) |> 
    dplyr::mutate_if(is.numeric, ~sprintf('%0.1f', .)) |> 
    dplyr::mutate_if(is.character, ~ifelse(. == 'NA', NA_character_, .))
  
  return(out)
  
}

#' Create a summary gt table for all transects and species
#' 
#' @param evalgrp data frame, group evaluation data
evaltrntab_fun <- function(evalgrp){
  
  rptcol <- '#004F7E'
  trucol <- '#958984'
  
  totab <- evalgrp |> 
    dplyr::select(Site, Species = Species, abuaveval = `Abundance aveval`, 
           abutruval = `Abundance truval`, blavenum = `Blade Length aveval`, 
           bltrunum = `Blade Length truval`,
           ssavenum = `Short Shoot Density aveval`, 
           sstrunum = `Short Shoot Density truval`
    ) |> 
    dplyr::mutate(
      Site = as.numeric(Site), 
      abuavenum = as.numeric(abuaveval) - 1, 
      abutrunum = as.numeric(abutruval) - 1,
      blavenum = as.numeric(blavenum),
      bltrunum = as.numeric(bltrunum),
      ssavenum = as.numeric(ssavenum),
      sstrunum = as.numeric(sstrunum),
      `Abundance truval` = paste0('(', abutruval, ')'),
      `Blade Length truval` = paste0('(', bltrunum, ')'),
      `Short Shoot Density truval` = paste0('(', sstrunum, ')')
    ) |> 
    tidyr::unite('Abundance reported (most common)', abuaveval, `Abundance truval`, sep = ' ', remove = FALSE) |>
    tidyr::unite('Blade Length reported (average)', blavenum, `Blade Length truval`, sep = ' ', remove = FALSE) |>
    tidyr::unite('Short Shoot Density reported (average)', ssavenum, `Short Shoot Density truval`, sep = ' ', remove = FALSE) |>
    dplyr::select(-abuaveval, -abutruval, -`Abundance truval`, -`Blade Length truval`, -`Short Shoot Density truval`) |> 
    dplyr::mutate_all(~ ifelse(. == 'NA (NA)', '', .)) |> 
    dplyr::mutate_at(c('Abundance reported (most common)', 'Blade Length reported (average)', 'Short Shoot Density reported (average)'), ~ gsub('^NA', '-', .)) |>
    dplyr::mutate_at(c('abuavenum', 'blavenum', 'ssavenum'), ~ ifelse(is.na(.x), 0, .x)) |> # bullet won't plot if target is NA
    dplyr::arrange(Site, Species) |> 
    dplyr::mutate(Site = paste('Transect', Site)) |> 
    dplyr::group_by(Site)

  abubultxt <- paste0('<span style="color:', rptcol, ';display:inline;"><b>Abundance reported</b></span> <span style="color:', trucol, ';display:inline;"><b>(most common)</b></span>')
  
  blbultxt <- gsub('Abundance reported', 'Blade length reported cm', abubultxt)

  ssbultxt <- gsub('Abundance reported', 'Short shoot density reported per m <sup>2</sup>', abubultxt)

  out <- gt::gt(totab) |> 
    gtExtras::gt_plt_bullet(column = abutrunum, target = abuavenum,
                  palette = c(trucol, rptcol)) |>
    gtExtras::gt_plt_bullet(column = bltrunum, target = blavenum, 
                  palette = c(trucol, rptcol)) |> 
    gtExtras::gt_plt_bullet(column = sstrunum, target = ssavenum,
                  palette = c(trucol, rptcol)) |>
    gt::cols_label(
      `Abundance reported (most common)`= gt::html(abubultxt),
      `Blade Length reported (average)` = gt::html(blbultxt), 
      `Short Shoot Density reported (average)` = gt::html(ssbultxt),
      abutrunum = '',
      bltrunum = '',
      sstrunum = ''
    ) |> 
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = gt::cells_body(
        columns = 'Species'
      )
    ) |>
    gt::tab_options(row_group.as_column = TRUE) |> 
    gt::text_transform(
      locations = gt::cells_body(
        columns = c(`Abundance reported (most common)`, `Blade Length reported (average)`, `Short Shoot Density reported (average)`)
      ),
      fn = function(x) {
        x <- gsub('^(.*)\\s(.*)$', paste0('<span style="color:', rptcol, ';display:inline;"><b>\\1</b></span> <span style="color:', trucol, ';display:inline;"><b>\\2</b></span>'), x)
        x
      }
    ) |>
    gt::cols_move(
      columns = abutrunum,
      after = `Abundance reported (most common)`
    ) |> 
    gt::cols_move(
      columns = bltrunum,
      after = `Blade Length reported (average)`
    ) |>
    gt::cols_move(
      columns = sstrunum,
      after = `Short Shoot Density reported (average)`
    ) |> 
    gt::cols_align('left')# |> 
    # gt::opt_interactive(
    #   use_pagination = F, 
    #   use_pagination_info = F,
    #   use_sorting = F, 
    #   use_filters = T,
    #   pagination_type = 'simple'
    # )
  
  return(out)
  
}

#' Create summary of metrics across transects for each species
#' 
#' @param evalgrp Data frame of evaluation group
#' @param vr Character vector of variable names
sppdiff_fun <- function(evalgrp, vr = c('Abundance', 'Blade Length', 'Short Shoot Density')){
  
  vr <- match.arg(vr)
  
  out <- evalgrp |> 
    dplyr::filter(Species %in% savspecies()) |>
    dplyr::rename(
      aveval = paste(vr, 'aveval'),
      truval = paste(vr, 'truval')
    ) 
  
  if(vr == 'Abundance'){

    out <- out |> 
      dplyr::select(
        Species, 
        aveval,
        truval
      ) |> 
      dplyr::mutate(across(-Species, as.numeric)) |>
      dplyr::summarise(
        aveval = round(mean(aveval, na.rm = T), 0),
        sdtruv = round(sd(truval, na.rm = T), 0),
        truval = round(mean(truval, na.rm = T), 0),
        .by = 'Species'
      ) |> 
      dplyr::mutate(
        avediff = aveval - truval
      )
    
  } else {

    out <- out |> 
      dplyr::filter(
        sum(!is.na(truval)) > 0, # remove species where short shoot or blade length is not measured
        .by = Species
      ) |>
      dplyr::select(Species, aveval, truval) |> 
      dplyr::mutate(across(-Species, as.numeric)) |>
      dplyr::summarise(
        aveval = ifelse(all(aveval == 0 | is.na(aveval)), NA, mean(aveval, na.rm = T)),
        sdtruv = ifelse(all(truval == 0 | is.na(truval)), NA, sd(truval, na.rm = T)),
        truval = ifelse(all(truval == 0 | is.na(truval)), NA, mean(truval, na.rm = T)), 
        .by = 'Species'
      ) |> 
      dplyr::mutate(
        avediff = aveval - truval
      )
    
  }
  
  out <- out |> 
    dplyr::mutate(dplyr::across(-Species, \(x) round(x, 1))) |> 
    dplyr::arrange(Species)
  
  return(out)
  
}

#' Create summary card for species metric
#' 
#' @param evalgrp data frame with evaluation group data
#' @param grp character with group name
#' @param allgrpscr data frame with all group scores
#' @param vr character vector with variable name
card_fun <- function(evalgrp, grp, allgrpscr, vr = c('Abundance', 'Blade Length', 'Short Shoot Density')){
  
  vr <- match.arg(vr)
  
  vruni <- c('Abundance' = 'mean BB categories away', 
             'Blade Length' = 'cm difference on average',
             'Short Shoot Density' = 'shoots per m<sup>2</sup> difference on average')
  vruni <- vruni[[vr]]
  
  rptcol <- '#004F7E'
  trucol <- '#958984'
  
  sppdiff <- sppdiff_fun(evalgrp, vr)

  grpscr <- allgrpscr |> 
    dplyr::filter(grpact == !!grp) |>
    dplyr::select(-grpact)
  
  scr <- as.character(grpscr[[vr]])
  
  hiscr <- scr %in% c('A-', 'B+', 'B')
  
  # overall diff
  sgndff <- sppdiff |> 
    dplyr::summarise(
      avediff = round(mean(avediff, na.rm = T), 1)
    ) |> 
    dplyr::pull() |> 
    sign()
  sgndff <- ifelse(sgndff == 0, '', ifelse(sgndff == 1, '+', '-'))

  # spp summary text
  spptxt <- sppdiff |> 
    dplyr::mutate(
      sgndff = sign(avediff),
      sgndff = dplyr::case_when(
        is.na(sgndff) ~ '',
        sgndff == 0 ~ '',
        sgndff == 1 ~ '+',
        sgndff == -1 ~ '' # already a negative prefix
      ),
      avediff = ifelse(is.na(avediff), 'not recorded', as.character(avediff)), 
      avediff = dplyr::case_when(
        avediff == 'not recorded' ~ avediff,
        T ~ paste(sgndff, avediff, ' ', vruni, ' across transects', sep = '')
      ),
      Species = paste0('<i><b>', Species, '</b></i>')
    ) |> 
    tidyr::unite('Species', Species, avediff, sep = ' ') |>
    dplyr::mutate(
      Species = paste0('<span>', Species, '</span>')
    ) |> 
    dplyr::pull(Species) |> 
    paste0(collapse = '</p><p>')
  spptxt < paste0('<p>', spptxt, '</p>')
  
  # barplot prep
  sppdiff <- sppdiff |> 
    dplyr::filter(!is.na(aveval)) |> 
    dplyr::mutate(
      Species = factor(Species), 
      Savnum = as.numeric(Species)
    )
  
  # barplot y axis and hover text differs if abundance or not
  ttl <- paste0('Average <span style="color:', rptcol, ';display:inline;"><b>reported</b></span> vs <span style="color:', trucol, ';display:inline;"><b>true</b></span>')
  yxs <- list(title = ttl )
  hovtxttr <- paste0('True, ', sppdiff$truval)
  hovtxtrp <- paste0('Reported, ', sppdiff$aveval)
  if(vr == 'Abundance'){
    abulv <- 0:7
    abulb <- c('no coverage', 'solitary', 'few', '<5%', '5-25%', '25-50%', '51-75%', '76-100%')
    yxs <- list(title = ttl, tickvals = abulv, ticktext = abulb)
    hovtxttr <- paste0('True, ', factor(sppdiff$truval, levels = abulv, labels = abulb))
    hovtxtrp <- paste0('Reported, ', factor(sppdiff$aveval, levels = abulv, labels = abulb))
  }

  # bar plot
  p <- plotly::plot_ly(
      sppdiff,
      x = ~ Savnum,
      y = ~ truval,
      type = 'bar',
      marker = list(color = trucol), 
      name = 'True value',
      text = hovtxttr,
      hoverinfo = 'text',
      textposition = 'none', 
      error_y = ~list(array = sdtruv,
                      color = 'grey', width = 0)
    ) |> 
    plotly::add_segments(
      data = sppdiff,
      x = ~ Savnum - 0.4,
      xend = ~ Savnum + 0.4,
      y = ~ aveval,
      yend = ~ aveval,
      line = list(color = rptcol, width = 7), 
      name = 'Reported value',
      text = hovtxtrp,
      hoverinfo = 'text',
      textposition = 'none',
      inherit = F
    ) |>
    plotly::config(displayModeBar = F) |> 
    plotly::layout(
      xaxis = list(title = '', ticktext = levels(sppdiff$Species), tickvals = sppdiff$Savnum),
      yaxis = yxs,
      showlegend = F
    ) |> 
    plotly::config(displayModeBar = F)

  troph <- troph_fun(scr)
  
  txtdev <- dplyr::case_when(
    scr == 'A' ~ 'reported values deviate very little from the average',
    hiscr ~ 'reported values deviate a little from the average',
    !hiscr ~ 'reported values deviate a lot from the average',
  )
  
  sgndffuni <- unique(sign(sppdiff$avediff))
  
  txtdir <- dplyr::case_when(
    scr == 'A' ~ ', good job!',
    length(sgndffuni) > 1 & scr != 'A' ~ ', varies by species',
    sgndff == '+' & hiscr & scr != 'A' ~ ', slightly higher',
    sgndff == '-' & hiscr & scr != 'A' ~ ', slightly lower',
    sgndff == '+' & !hiscr & scr != 'A' ~ ', much higher',
    sgndff == '-' & !hiscr & scr != 'A' ~ ', much lower', 
    T ~ ''
  )
  txtdsc <- paste0('<span><h3><b>', scr, '&nbsp;', troph, '</b></h3><h4> ', txtdev, txtdir, '</h4></span>')
  
  bslib::value_box(
    title = gt::html(paste0('<b>', vr, ' summary</b>')),
    value = gt::html(txtdsc),
    gt::html(spptxt), 
    showcase = p,
    showcase_layout = bslib::showcase_left_center(max_height = "300px", width = 0.4)
  ) 
  
}

#' Calculate scores for all groups based on distribution of scores
#' 
#' @param trndat data frame of all seagrass transect training data
#' @param yr integer, year
#' @param truvar data frame "true" values from training data for a given year
allgrpscr_fun <- function(trndat, yr, truvar){

  scrs <- trndat |> 
    dplyr::filter(yr == !!yr) |> 
    dplyr::select(grpact) |> 
    dplyr::distinct() |> 
    dplyr::group_nest(grpact, .key = 'evalgrp') |> 
    dplyr::mutate(
      evalgrp = purrr::map2(grpact, evalgrp, ~ evalgrp_fun(trndat, yr, .x, truvar))
    ) |> 
    tidyr::crossing(var = c('Abundance', 'Blade Length', 'Short Shoot Density')) |> 
    dplyr::mutate(
      avediff = purrr::pmap(list(evalgrp, var), function(evalgrp, var){
        sppdiff_fun(evalgrp, var) |> 
          dplyr::summarise(
            avediff = mean(abs(avediff), na.rm = T)
          ) |> 
          dplyr::pull(avediff)
      })
    ) |> 
    dplyr::select(-evalgrp) |> 
    tidyr::unnest(avediff) |>
    tidyr::pivot_wider(names_from = var, values_from = avediff) |> 
    dplyr::mutate(
      dplyr::across(`Abundance`:`Short Shoot Density`, ~ scales::rescale(abs(.x), to = c(100, 50))),
      `Total` = (`Blade Length` + `Short Shoot Density` + `Abundance`) / 3
    )

  grades <- c('A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D')
  grdbrk <- c(101, 95, 90, 85, 80, 75, 70, 65, 60, 55, 0)

  out <- scrs |> 
    dplyr::mutate(
      dplyr::across(`Abundance`:`Total`, ~ cut(-.x, breaks = -grdbrk, labels = grades) |> as.character())
    )

  return(out)
  
}

#' Get text summaries of a groups total score relative to all others
#' 
#' @param allgrpscr data frame as returned by \code{\link{allgrpscr_fun}}
#' @param grp character, group to summarize
scrsum_fun <- function(allgrpscr, grp){
  
  grdlvs <- c('A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D')
  
  # group total scoreore
  totscr <- allgrpscr |> 
    dplyr::filter(grpact == !!grp) |>
    dplyr::select(-grpact) |> 
    dplyr::pull(Total) |> 
    factor(levels = grdlvs)
  
  # compare group score to all others
  alltot <- factor(allgrpscr$Total, levels = grdlvs)
  
  higher <- sum(as.numeric(totscr) > as.numeric(alltot)) |> 
    english::english()
  higher <- paste0(toupper(substring(higher, 1, 1)), substring(higher, 2))
  lower <- sum(as.numeric(totscr) < as.numeric(alltot)) |> 
    english::english()
  lower <- paste0(toupper(substring(lower, 1, 1)), substring(lower, 2))
  equal <- (sum(as.numeric(totscr) == as.numeric(alltot)) - 1) |> 
    english::english()
  equal <- paste0(toupper(substring(equal, 1, 1)), substring(equal, 2))
  
  spcs1 <- rep('&nbsp;', 12) |> 
    paste0(collapse = '')
  spcs2 <- rep('&nbsp;', 20) |> 
    paste0(collapse = '')
  
  troph <- troph_fun(totscr)
  
  # convert all to html
  tottxt <- paste0('<h1>', spcs1, '<b>', as.character(totscr), '</b>', ' overall score', troph, '</h1>')
  higher <- paste0('<h3>', spcs2, '<b>', higher, '</b>', ' groups had a higher score', '</h3>')
  higher <- ifelse(grepl('One', higher), gsub('groups', 'group', higher), higher)
  lower <- paste0('<h3>', spcs2, '<b>', lower, '</b>', ' groups had a lower score', '</h3>')
  lower <- ifelse(grepl('One', lower), gsub('groups', 'group', lower), lower)
  equal <- paste0('<h3>', spcs2, '<b>', equal, '</b>', ' groups had the same score', '</h3>')
  equal <- ifelse(grepl('One', equal), gsub('groups', 'group', equal), equal)
  
  # fix to not show higher or lower text grade is top or bottom
  if(totscr == grdlvs[1])
    higher <- NULL
  if(totscr == grdlvs[length(grdlvs)])
    lower <- NULL
  
  screxp <- 'The overall score is based on the average of the scores below for species abundance, blade length, and short shoot density. Each of these three scores is based on how close the reported values are to the overall averages ("true") across all groups participating in the transect training.  Reported values summarized for each species across all transects that deviate largely from the averages are given lower scores.  The overall score is then ranked relative to all other groups.'
  
  # ouput as list
  out <- paste0('
    <table>
      <tr>
        <td>', tottxt, higher, lower, equal, '</td>', 
        '<td><h2><b>How are scores calculated?</b></h2><h4>', screxp, '</h4></td>',
      '</tr>
    </table>'
  )
  
  return(out)
  
}
 
#' Get text summary of how to improve score for a variable
#' 
#' @param allgrpscr data frame as returned by \code{\link{allgrpscr_fun}}
#' @param grp character, group to summarize
#' @param vr character vector with variable name
scrimp_fun <- function(allgrpscr, grp, vr = c('Abundance', 'Blade Length', 'Short Shoot Density')){
  
  vr <- match.arg(vr)
  
  vdlnk <- list(
    Abundance = 'https://youtu.be/jfnVlIjJ-o4?list=PLfJ6-D-exF9RKU6i3A7z0uwfeiyayULqk&t=320',
    `Blade Length` = 'https://youtu.be/jfnVlIjJ-o4?list=PLfJ6-D-exF9RKU6i3A7z0uwfeiyayULqk&t=515',
    `Short Shoot Density` = 'https://youtu.be/jfnVlIjJ-o4?list=PLfJ6-D-exF9RKU6i3A7z0uwfeiyayULqk&t=555'
  )
  
  scr <- allgrpscr |> 
    dplyr::filter(grpact == !!grp) |> 
    dplyr::pull(!!vr)
  
  troph <- troph_fun(scr)
  
  out <- dplyr::case_when(
    scr %in% c('A', 'A-') ~ "Looks good, keep doing what you're doing!", 
    T ~ paste0('Room for improvement! Learn how to brush up on ', tolower(vr), ' by viewing the link <a target="_blank" href="', vdlnk[[vr]], '">here</a>.') 
  )
  out <- paste0('<span><h3><b>', scr, '&nbsp;', troph, '</b></h3><h4>', out, '</h4></span>')
  out <- gt::html(out)
  
  return(out)
  
}
 
#' Return fontawsome trophy text if scr is an A
#' 
#' @param scr character, score
troph_fun <- function(scr){
  
  # fontawsome::fa('trophy', fill = 'gold')
  out <- NULL
  
  if(scr %in% c('A', 'A-'))
    out <- '<svg aria-hidden="true" role="img" viewBox="0 0 576 512" style="height:1em;width:1.12em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:gold;overflow:visible;position:relative;"><path d="M400 0H176c-26.5 0-48.1 21.8-47.1 48.2c.2 5.3 .4 10.6 .7 15.8H24C10.7 64 0 74.7 0 88c0 92.6 33.5 157 78.5 200.7c44.3 43.1 98.3 64.8 138.1 75.8c23.4 6.5 39.4 26 39.4 45.6c0 20.9-17 37.9-37.9 37.9H192c-17.7 0-32 14.3-32 32s14.3 32 32 32H384c17.7 0 32-14.3 32-32s-14.3-32-32-32H357.9C337 448 320 431 320 410.1c0-19.6 15.9-39.2 39.4-45.6c39.9-11 93.9-32.7 138.2-75.8C542.5 245 576 180.6 576 88c0-13.3-10.7-24-24-24H446.4c.3-5.2 .5-10.4 .7-15.8C448.1 21.8 426.5 0 400 0zM48.9 112h84.4c9.1 90.1 29.2 150.3 51.9 190.6c-24.9-11-50.8-26.5-73.2-48.3c-32-31.1-58-76-63-142.3zM464.1 254.3c-22.4 21.8-48.3 37.3-73.2 48.3c22.7-40.3 42.8-100.5 51.9-190.6h84.4c-5.1 66.3-31.1 111.2-63 142.3z"/></svg>'

  return(out)

}

#' Get text summary of how to improve species id
#'
#' @param evalgrp data frame as returned by \code{\link{evalgrp_fun}}
#' @param spp character, species to summarize
sppimp_fun <- function(evalgrp, spp = c('seagrass', 'macroalgae')){
  
  lnk <- 'https://drive.google.com/file/d/1naZpND_Ur90abqND-ZhZ_fJtBBiuetjt/view'
  
  spp <- match.arg(spp)

  if(spp == 'seagrass')
    sppid <- evalgrp |> 
      dplyr::filter(Species %in% savspecies()) 
  
  if(spp == 'macroalgae')
    sppid <- evalgrp |> 
      dplyr::filter(!Species %in% savspecies())

  sppmiss <- sppid |> 
    dplyr::filter(is.na(`Abundance aveval`)) |> 
    dplyr::select(Species) |> 
    dplyr::distinct()
  
  txtout <- 'All species found, good job!'
  if(nrow(sppmiss) > 0){
    
    sppmiss <- sppmiss |> 
      dplyr::pull(Species) |> 
      sort()
    
    nmiss <- 'a few'
    if(length(sppmiss) == 1)
      nmiss <- 'one'
 
    sppmiss <- sppmiss |> 
      paste(collapse = ', ')
    
    txtout <- paste0('Missed ', nmiss, ' (', sppmiss, ')! Check out the species guide at the link <a target="_blank" href="', lnk, '">here</a>.')
    
  }
    
  out <- paste0('<span><h4>', txtout, '</h4></span>')
  out <- gt::html(out)
  
  return(out)
  
}

#' Get character vector of sav species for filtering
savspecies <- function(){
  
  out <- c('Halodule', 'Syringodium', 'Thalassia', 'Halophila', 'Ruppia')
  
  return(out)
  
}
