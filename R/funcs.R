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
      Abundance = floor(median(Abundance, na.rm = T)),
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
      `Abundance truval` = factor(`Abundance truval`, levels = abulev, labels = abulab),
      `Abundance diff` = as.numeric(`Abundance aveval`) - as.numeric(`Abundance truval`),
      `Abundance perdiff` = (`Abundance diff` / 8) * 100,
      `Blade Length perdiff` = (`Blade Length aveval` - `Blade Length truval`) / `Blade Length truval` * 100,
      `Short Shoot Density perdiff` = (`Short Shoot Density aveval` - `Short Shoot Density truval`) / `Short Shoot Density truval` * 100
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