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
  
  abulev <- c('0', '0.1', '0.5', '1', '2', '3', '4', '5')
  abulab <- c('no coverage', 'solitary', 'few', '<5%', '5-25%', '25-50%', '51-75%', '76-100%')
  
  truvar <- datyr |> 
    tidyr::pivot_wider(names_from = var, values_from = aveval) |>
    dplyr::mutate(
      Abundance = factor(Abundance, levels = abulev), 
      Abundance = as.numeric(Abundance)
    ) |> 
    dplyr::summarise(
      Abundance = round(mean(Abundance, na.rm = T), 0),
      `Blade Length` = mean(`Blade Length`, na.rm = T),
      `Short Shoot Density` = mean(`Short Shoot Density`, na.rm = T), 
      .by = c(Site, Savspecies)
    ) |> 
    dplyr::mutate(
      Abundance = factor(Abundance, levels = seq_along(abulev), labels = abulev),
      Abundance = as.numeric(as.character(Abundance))
    ) |> 
    tidyr::pivot_longer(
      cols = -c(Site, Savspecies),
      names_to = 'var',
      values_to = 'truval'
    ) 
  
  for(grp in grps){
    
    if(!quiet){
      cat('Processing group: ', grp, '\n')
    }
    
    grpnoyr <- gsub('^\\d{4}:\\s', '', grp)
    
    # define parameters
    params <- list(
      yr = yr, 
      grp = grp,
      grpnoyr = grpnoyr,
      transect = transect,
      truvar = truvar
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

#' Get group difference from "true"
#' 
#' @param datyr data frame, training data for selected year
#' @param grp character, group name
#' @param truvar data frame, true values
#' 
#' @return data frame of group data compard to "true" values
evalgrp_fun <- function(trndat, yr, grp, truvar){
  
  datyrgrp <- trndat |> 
    dplyr::filter(yr == !!yr) |> 
    dplyr::filter(grpact == !!grp) |> 
    dplyr::select(Site, Depth, Savspecies, var, aveval)
  
  abulev <- c('0', '0.1', '0.5', '1', '2', '3', '4', '5')
  abulab <- c('no coverage', 'solitary', 'few', '<5%', '5-25%', '25-50%', '51-75%', '76-100%')

  out <- datyrgrp |> 
    dplyr::full_join(truvar, by = c('Site', 'Savspecies', 'var')) |> 
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
  
  totab <- evalgrp |> 
    dplyr::select(Site, Species = Savspecies, abuaveval = `Abundance aveval`, 
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
    dplyr::arrange(Site, Species) |> 
    dplyr::mutate(Site = paste('Transect', Site)) |> 
    dplyr::group_by(Site)

  abubultxt <- '<span style="color:#00806E;display:inline;"><b>Abundance reported</b></span> <span style="color:#004F7E;display:inline;"><b>(most common)</b></span>'
  
  blbultxt <- '<span style="color:#00806E;display:inline;"><b>Blade length reported</b></span> <span style="color:#004F7E;display:inline;"><b>(average)</b></span>'

  ssbultxt <- '<span style="color:#00806E;display:inline;"><b>Short shoot density reported</b></span> <span style="color:#004F7E;display:inline;"><b>(average)</b></span>'

  out <- gt::gt(totab) |> 
    gtExtras::gt_plt_bullet(column = abuavenum, target = abutrunum, 
                  palette = c('#00806E', '#004F7E')) |> 
    gtExtras::gt_plt_bullet(column = blavenum, target = bltrunum, 
                  palette = c('#00806E', '#004F7E')) |> 
    gtExtras::gt_plt_bullet(column = ssavenum, target = sstrunum,
                  palette = c('#00806E', '#004F7E')) |>
    gt::cols_label(
      `Abundance reported (most common)`= gt::html(abubultxt),
      `Blade Length reported (average)` = gt::html(blbultxt), 
      `Short Shoot Density reported (average)` = gt::html(ssbultxt),
      abuavenum = '',
      blavenum = '',
      ssavenum = ''
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
        x <- gsub('^(.*)\\s(.*)$', '<span style="color:#00806E;display:inline;"><b>\\1</b></span> <span style="color:#004F7E";display:inline;"><b>\\2</b></span>', x)
        x
      }
    ) |>
    gt::cols_move(
      columns = abuavenum,
      after = `Abundance reported (most common)`
    ) |> 
    gt::cols_move(
      columns = blavenum,
      after = `Blade Length reported (average)`
    ) |>
    gt::cols_move(
      columns = ssavenum,
      after = `Short Shoot Density reported (average)`
    ) |> 
    gt::cols_align('left')
  
  return(out)
  
}

#' Create summary of metrics across transects for each species
#' 
#' @param evalgrp Data frame of evaluation group
#' @param vr Character vector of variable names
sppdiff_fun <- function(evalgrp, vr = c('Abundance', 'Blade Length', 'Short Shoot Density')){
  
  vr <- match.arg(vr)
  
  out <- evalgrp |> 
    dplyr::rename(
      aveval = paste(vr, 'aveval'),
      truval = paste(vr, 'truval')
    ) 
  
  if(vr == 'Abundance'){

    out <- out |> 
      dplyr::select(
        Savspecies, 
        aveval,
        truval
      ) |> 
      dplyr::mutate(across(-Savspecies, as.numeric)) |>
      dplyr::summarise(
        aveval = round(mean(aveval, na.rm = T), 0), 
        truval = round(mean(truval, na.rm = T), 0),
        .by = 'Savspecies'
      ) |> 
      dplyr::mutate(
        avediff = aveval - truval
      )
    
  } else {
    
    out <- out |> 
      dplyr::select(Savspecies, aveval, truval) |> 
      dplyr::mutate(across(-Savspecies, as.numeric)) |>
      dplyr::summarise(
        aveval = ifelse(all(aveval == 0 | is.na(aveval)), NA, mean(aveval, na.rm = T)),
        truval = ifelse(all(truval == 0 | is.na(truval)), NA, mean(truval, na.rm = T)), 
        .by = 'Savspecies'
      ) |> 
      dplyr::mutate(
        avediff = aveval - truval
      )
    
  }
  
  out <- out |> 
    dplyr::mutate(dplyr::across(-Savspecies, \(x) round(x, 1))) |> 
    dplyr::arrange(Savspecies)
  
  return(out)
  
}

#' Create summary card for species metric
#' 
#' @param evalgrp data frame with evaluation group data
#' @param vr character vector with variable name
card_fun <- function(evalgrp, vr = c('Abundance', 'Blade Length', 'Short Shoot Density')){
  
  vr <- match.arg(vr)
  
  vruni <- c('Abundance' = 'mean BB categories away', 
             'Blade Length' = 'cm difference on average',
             'Short Shoot Density' = 'shoots per m<sup>2</sup> difference on average')
  vruni <- vruni[[vr]]
  
  sppdiff <- sppdiff_fun(evalgrp, vr)
  
  if(vr == 'Abundance'){
    
    sumtxt <- sppdiff |> 
      dplyr::summarise(
        avediff = round(mean(avediff, na.rm = T), 0)
      )
    sgndff <- sign(sumtxt$avediff)
    sgndff <- ifelse(sgndff == 0, '', ifelse(sgndff == 1, '+', '-'))
    sumtxt <- paste(sgndff, sumtxt$avediff, ' ', vruni, ' across transects', sep = '')
    sumtxt <- paste0('<span><b><i>All species</i/></b> ', sumtxt, '</span>')
    
    txtdsc <- dplyr::case_when(
      sgndff == '' ~ 'Reported values are close to average, good job!',
      sgndff == '+' ~ 'Reported values are generally higher than average',
      sgndff == '' ~ 'Reported values are generally higher than average'
    )
    txtdsc <- paste0('<h3>', txtdsc, '</h3>')
    
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
        Savspecies = paste0('<i><b>', Savspecies, '</b></i>')
      ) |> 
      tidyr::unite('Savspecies', Savspecies, avediff, sep = ' ') |>
      dplyr::mutate(
        Savspecies = paste0('<span>', Savspecies, '</span>')
      ) |> 
      dplyr::pull(Savspecies) |> 
      paste0(collapse = '</p><p>')
    spptxt <- paste0(sumtxt, '</p><p>', spptxt)
    spptxt < paste0('<p>', spptxt, '</p>')

    # create plotly barplot w/ lines
    sppdiff <- sppdiff |> 
      dplyr::filter(!is.na(avediff)) |> 
      dplyr::mutate(
        Savspecies = factor(Savspecies), 
        Savnum = as.numeric(Savspecies)
      )
  
    p <- plotly::plot_ly(
        sppdiff,
        x = ~ Savnum,
        y = ~ aveval,
        type = 'bar',
        marker = list(color = '#00806E'), 
        name = 'Reported value'
      ) |> 
      plotly::add_segments(
        x = ~ Savnum - 0.4,
        xend = ~ Savnum + 0.4,
        y = ~ truval,
        yend = ~ truval,
        line = list(color = '#004F7E', width = 7), 
        name = 'True value',
        inherit = F
      ) |>
      plotly::layout(
        xaxis = list(title = '', ticktext = levels(sppdiff$Savspecies), tickvals = sppdiff$Savnum),
        yaxis = list(title = 'Average <span style="color:#00806E;display:inline;"><b>reported</b></span> vs <span style="color:#004F7E;display:inline;"><b>true</b></span>', tickvals = 0:7, ticktext = c('no coverage', 'solitary', 'few', '<5%', '5-25%', '25-50%', '51-75%', '76-100%')), 
        showlegend = F
      ) |> 
      plotly::config(displayModeBar = F)    
    
  }
  
  if(vr != 'Abundance'){
    
    sumtxt <- sppdiff |> 
      dplyr::summarise(
        avediff = round(mean(avediff, na.rm = T), 1)
      )
    sgndff <- sign(sumtxt$avediff)
    sgndff <- ifelse(sgndff == 0, '', ifelse(sgndff == 1, '+', '-'))
    sumtxt <- paste(gsub('\\-', '', sgndff), sumtxt$avediff, ' ', vruni, ' across transects', sep = '')
    sumtxt <- paste0('<span><b><i>All species</i></b> ', sumtxt, '</span>')
    
    txtdsc <- dplyr::case_when(
      sgndff == '' ~ 'Reported values are close to average, good job!',
      sgndff == '+' ~ 'Reported values are generally higher than average',
      sgndff == '-' ~ 'Reported values are generally lower than average'
    )
    txtdsc <- paste0('<h3>', txtdsc, '</h3>')

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
        Savspecies = paste0('<i><b>', Savspecies, '</b></i>')
      ) |> 
      tidyr::unite('Savspecies', Savspecies, avediff, sep = ' ') |>
      dplyr::mutate(
        Savspecies = paste0('<span>', Savspecies, '</span>')
      ) |> 
      dplyr::pull(Savspecies) |> 
      paste0(collapse = '</p><p>')
    spptxt <- paste0(sumtxt, '</p><p>', spptxt)
    spptxt < paste0('<p>', spptxt, '</p>')

    # create plotly barplot w/ lines
    sppdiff <- sppdiff |> 
      dplyr::filter(!is.na(aveval)) |> 
      dplyr::mutate(
        Savspecies = factor(Savspecies), 
        Savnum = as.numeric(Savspecies)
      )
    
    p <- plotly::plot_ly(
      sppdiff,
      x = ~ Savnum,
      y = ~ aveval,
      type = 'bar',
      marker = list(color = '#00806E'), 
      name = 'Reported value'
    ) |> 
      plotly::add_segments(
        x = ~ Savnum - 0.4,
        xend = ~ Savnum + 0.4,
        y = ~ truval,
        yend = ~ truval,
        line = list(color = '#004F7E', width = 7), 
        name = 'True value',
        inherit = F
      ) |>
      plotly::layout(
        xaxis = list(title = '', ticktext = levels(sppdiff$Savspecies), tickvals = sppdiff$Savnum),
        yaxis = list(title = 'Average <span style="color:#00806E;display:inline;"><b>reported</b></span> vs <span style="color:#004F7E;display:inline;"><b>true</b></span>'), 
        showlegend = F
      ) |> 
      plotly::config(displayModeBar = F)    
    
  }
  
  bslib::value_box(
    title = gt::html(paste0('<b>', vr, ' summary</b>')),
    value = gt::html(txtdsc),
    gt::html(spptxt), 
    showcase = p,
    showcase_layout = bslib::showcase_left_center(max_height = "300px", width = 0.4)
  ) 
  
}