#' Process training report by group
#'
#' @param trndat data frame, training data
#' @param yr integer, year
#' @param quiet logical, suppress messages
#'
proc_grp <- function(trndat, yr, quiet = F){
  
  # get data and format
  datyr <- trndat |> 
    dplyr::filter(yr == !!yr) |> 
    dplyr::select(-yr)
  
  grps <- datyr$grpact |> 
    unique()
  
  for(grp in grps){
    
    if(!quiet){
      cat('Processing group: ', grp, '\n')
    }
    
    grpnoyr <- gsub('^\\d{4}:\\s', '', grp)
    
    # define parameters
    params <- list(
      yr = yr, 
      grp = grp,
      grpnoyr = grpnoyr
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