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
  
  truvar <- datyr |> 
    tidyr::pivot_wider(names_from = var, values_from = aveval) |>
    dplyr::summarise(
      Abundance = floor(median(Abundance, na.rm = T)),
      `Blade Length` = mean(`Blade Length`, na.rm = T),
      `Short Shoot Density` = mean(`Short Shoot Density`, na.rm = T), 
      .by = c(Site, Savspecies)
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
      to = here('docs', outputfl)
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
    dplyr::left_join(truvar, by = c('Site', 'Savspecies', 'var')) |> 
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

#' Get transect specific summaries
#' 
#' @param evalgrp data frame, group evaluation data
#' @param trn character, transect name
#' 
evaltrn_fun <- function(evalgrp, trn){
  
  trn <- gsub('Transect\\s', '', trn)

  evalgrptrn <- evalgrp |> 
    dplyr::filter(Site == trn)
  
  allspp <- evalgrptrn$Savspecies |> 
    unique()
  
  out <- list(
    Halodule = NULL,
    Thalassia = NULL,
    Syringodium = NULL,
    Halophila = NULL,
    Ruppia = NULL
  )
  
  if('Halodule' %in% allspp) out$Halodule <- evaltrnspp_fun(evalgrptrn, 'Halodule')
  if('Thalassia' %in% allspp) out$Thalassia <- evaltrnspp_fun(evalgrptrn, 'Thalassia')
  if('Syringodium' %in% allspp) out$Syringodium <- evaltrnspp_fun(evalgrptrn, 'Syringodium')
  if('Halophila' %in% allspp) out$Halophila <- evaltrnspp_fun(evalgrptrn, 'Halophila')
  if('Ruppia' %in% allspp) out$Ruppia <- evaltrnspp_fun(evalgrptrn, 'Ruppia')

  # remove NULL from out
  out <- out[!sapply(out, is.null)]
  
  return(out)
  
}

#' Get species specific summaries for a transect
#' 
#' @param evalgrptrn data frame, group evaluation data for a transect
#' @param spp character, species name
evaltrnspp_fun <- function(evalgrptrn, spp = c('Halodule', 'Thalassia', 'Syringodium', 'Halophila', 'Ruppia')){
  
  spp <- match.arg(spp)
  
  flt <- evalgrptrn |> 
    dplyr::filter(Savspecies == spp)
  
  out <- list(
    abu = NULL,
    bl = NULL, 
    ssd = NULL
  )
    
  abuave <- paste('Reported abundance:', flt$`Abundance aveval`)
  abutru <- paste('Most common abundance:', flt$`Abundance truval`)
  abudiff <- paste('Difference in abundance:', as.integer(flt$`Abundance diff`))
  out$abu <- htmltools::span(paste(abuave, abutru, abudiff, sep = '<br>'))

  if(!is.na(flt$`Blade Length aveval`)){
    blave <- paste('Reported blade length:', flt$`Blade Length aveval`)
    bltru <- paste('Mean blade length across groups:', flt$`Blade Length truval`)
    bldiff <- paste('% diff:', flt$`Blade Length perdiff`)
    out$bl <- htmltools::span(paste(blave, bltru, bldiff, sep = '<br>'))
  }
    
  if(!is.na(flt$`Short Shoot Density aveval`)){
    ssdave <- paste('Reported short shoot density:', flt$`Short Shoot Density aveval`)
    ssdtru <- paste('Mean short shoot density across groups:', flt$`Short Shoot Density truval`)
    ssddiff <- paste('% diff:', flt$`Short Shoot Density perdiff`)
    out$ssd <- htmltools::span(paste(ssdave, ssdtru, ssddiff, sep = '<br>'))
  }
  
  # remove NULL from out
  out <- out[!sapply(out, is.null)]
  
  # out$abu <- value_box(title = spp, value = out$abu)
  
  return(out)
  
}