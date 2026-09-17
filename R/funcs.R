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

  # calibration constants from historical data
  cal <- calibrate_scr_fun(trndat)

  # group report card
  allgrpscr <- allgrpscr_fun(trndat, yr, truvar, cal = cal)
  
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
      quiet = quiet
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

  fls <- list.files(here::here('docs'), pattern = '\\d\\.html$', full.names = F)

  # build card markup grouped by year (newest first)
  content <- tibble::tibble(fls = fls) |>
    dplyr::mutate(
      yr  = gsub(fls, pattern = '.*_(\\d{4}).*',    replacement = '\\1'),
      grp = gsub(fls, pattern = '(.*)_(\\d{4}).*', replacement = '\\1')
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      grpact  = unique(trndat$grpact[trndat$yr == yr & trndat$grp == grp]),
      grpact  = gsub('^\\d{4}: ', '', grpact),
      grphtml = paste0('<div class="group-card"><a href="', fls, '" target="_blank">', grpact, '</a></div>')
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(yr, grp) |>
    dplyr::select(yr, grphtml) |>
    dplyr::group_nest(yr) |>
    dplyr::arrange(dplyr::desc(yr)) |>
    dplyr::mutate(
      cards   = purrr::map_chr(data, ~ paste0(dplyr::pull(.x), collapse = '\n')),
      section = paste0(
        '<div class="year-section">\n',
        '<div class="year-heading">', yr, '</div>\n',
        '<div class="group-grid">\n', cards, '\n</div>\n',
        '</div>'
      )
    ) |>
    dplyr::pull(section) |>
    paste0(collapse = '\n')

  # base64-encode logo so the output HTML is self-contained
  logo_b64 <- base64enc::base64encode(here::here('images', 'tarponlogo.png'))
  logo_src  <- paste0('data:image/png;base64,', logo_b64)

  # inject logo and content into template, then write
  template <- readLines(here::here('docs', 'index_template.html')) |> paste0(collapse = '\n')
  towrt <- gsub('__LOGO_SRC__',            logo_src, template, fixed = TRUE)
  towrt <- gsub('<!-- REPORT_CONTENT -->', content,  towrt,    fixed = TRUE)

  writeLines(towrt, con = here::here('docs/index.html'))

}

#' Get consensus species list for a given year
#'
#' @param trndat data frame, training data
#' @param yr integer, year
#'
#' @return data frame of Site x Species where >= 2 groups reported non-zero Abundance
truespp_fun <- function(trndat, yr){

  trndat |>
    dplyr::filter(yr == !!yr, var == 'Abundance') |>
    dplyr::filter(aveval != '0') |>
    dplyr::distinct(Site, Species, grpact) |>
    dplyr::summarise(n_grps = dplyr::n_distinct(grpact), .by = c(Site, Species)) |>
    dplyr::filter(n_grps >= 2) |>
    dplyr::select(Site, Species)

}

#' Get "true" values from training data for a given year
#'
#' @param trndat data frame, training data
#' @param yr integer, year
truvar_fun <- function(trndat, yr){

  truespp <- truespp_fun(trndat, yr)

  abulev <- c('0', '0.1', '0.5', '1', '2', '3', '4', '5')
  abulab <- c('no coverage', 'solitary', 'few', '<5%', '5-25%', '25-50%', '51-75%', '76-100%')

  abulev_num <- c(0, 0.1, 0.5, 1, 2, 3, 4, 5)

  out <- trndat |>
    dplyr::filter(yr == !!yr) |>
    dplyr::semi_join(truespp, by = c('Site', 'Species')) |>
    tidyr::pivot_wider(names_from = var, values_from = aveval) |>
    dplyr::mutate(
      Abundance = as.numeric(Abundance)
    ) |>
    dplyr::summarise(
      Abundance = mean(Abundance, na.rm = T),
      `Blade Length` = mean(`Blade Length`, na.rm = T),
      `Short Shoot Density` = mean(`Short Shoot Density`, na.rm = T),
      .by = c(Site, Species)
    ) |>
    dplyr::mutate(
      Abundance = sapply(Abundance, function(x) {
        if (is.na(x)) NA_real_ else abulev_num[which.min(abs(x - abulev_num))]
      })
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
#' @details in addition to each metric's \verb{aveval}/\verb{truval} columns,
#'   the returned data frame carries a \verb{<metric> sdgrp} column per
#'   transect (Site) and species: the standard deviation, across all groups
#'   that reported at that transect (not just \code{grp}), of their
#'   individually reported values for that metric. For Abundance this is
#'   computed on the ordinal Braun-Blanquet category position (matching the
#'   scale \code{\link{sppdiff_fun}} computes deviations on), not the raw BB
#'   value. This is a measure of how much groups disagreed with each other at
#'   that specific transect, i.e. how hard that species/metric was to pin
#'   down there, and is distinct from (and unrelated to) how much the "true"
#'   value itself might vary from one transect to another. It is \code{NA}
#'   when fewer than two groups reported a value at that transect.
#'
#' @return data frame of group data compard to "true" values
evalgrp_fun <- function(trndat, yr, grp, truvar){

  abulev <- c('0', '0.1', '0.5', '1', '2', '3', '4', '5')
  abulab <- c('no coverage', 'solitary', 'few', '<5%', '5-25%', '25-50%', '51-75%', '76-100%')

  datyrgrp <- trndat |>
    dplyr::filter(yr == !!yr) |>
    dplyr::filter(grpact == !!grp) |>
    dplyr::select(Site, Species, var, aveval)

  # cross-group spread at each transect: how much did the different groups'
  # own reports disagree with each other at that specific site, for this
  # species/metric (restricted to the same Site/Species/var combos truvar
  # covers). Computed once across all groups, same for every group evaluated.
  sprdgrp <- trndat |>
    dplyr::filter(yr == !!yr) |>
    dplyr::semi_join(truvar, by = c('Site', 'Species', 'var')) |>
    dplyr::mutate(
      val = dplyr::if_else(
        var == 'Abundance',
        as.numeric(factor(as.character(aveval), levels = abulev)),
        as.numeric(aveval)
      )
    ) |>
    dplyr::summarise(
      sdgrp = dplyr::if_else(sum(!is.na(val)) < 2, NA_real_, sd(val, na.rm = TRUE)),
      .by = c(Site, Species, var)
    ) |>
    tidyr::pivot_wider(names_from = var, values_from = sdgrp, names_glue = '{var} sdgrp')

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
    dplyr::mutate_if(is.character, ~ifelse(. == 'NA', NA_character_, .)) |>
    # joined after the aveval/truval formatting above (which rounds to 1
    # decimal for display) so sdgrp keeps full precision for the weight
    # calculation in sppdiff_fun
    dplyr::left_join(sprdgrp, by = c('Site', 'Species'))
  
  return(out)
  
}

#' Create a summary gt table for all species and transects
#'
#' @param evalgrp data frame, group evaluation data
#'
#' @details Rows are grouped by species (not transect), matching how the
#'   metrics are actually aggregated for scoring (Step 4 combines a species'
#'   values across transects, not a transect's values across species).
#'   Species not on the consensus SAV list (\code{\link{savspecies}}, i.e.
#'   macroalgae) are listed as a separate, visually muted block at the
#'   bottom of the table: they are scored on Abundance (a missed or
#'   falsely-reported species counts there regardless of species type,
#'   Step 3), but not on Blade Length or Short Shoot Density, which are not
#'   measured for macroalgae.
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
    dplyr::mutate_at(c('Abundance reported (most common)', 'Blade Length reported (average)', 'Short Shoot Density reported (average)'), ~ gsub('\\(NA\\)', '(--)', .)) |>
    dplyr::mutate_at(c('abuavenum', 'abutrunum', 'blavenum', 'bltrunum', 'ssavenum', 'sstrunum'), ~ ifelse(is.na(.x), 0, .x)) |> # bullet won't plot if value is NA
    dplyr::mutate(is_sav = Species %in% savspecies()) |>
    dplyr::arrange(dplyr::desc(is_sav), Species, Site) |>
    dplyr::mutate(Transect = paste('Transect', Site)) |>
    dplyr::select(-Site) |>
    dplyr::group_by(Species)

  nonsav_spp <- totab |>
    dplyr::ungroup() |>
    dplyr::filter(!is_sav) |>
    dplyr::distinct(Species) |>
    dplyr::pull(Species)

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
      Transect = '',
      `Abundance reported (most common)`= gt::html(abubultxt),
      `Blade Length reported (average)` = gt::html(blbultxt),
      `Short Shoot Density reported (average)` = gt::html(ssbultxt),
      abutrunum = '',
      bltrunum = '',
      sstrunum = ''
    ) |>
    gt::cols_hide(columns = is_sav) |>
    gt::cols_move_to_start(columns = Transect) |>
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = gt::cells_row_groups()
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
    gt::cols_align('left')

  if(length(nonsav_spp) > 0){
    out <- out |>
      gt::tab_style(
        style = list(gt::cell_fill(color = '#f0f0f0'), gt::cell_text(color = '#767676', style = 'italic')),
        locations = gt::cells_row_groups(groups = nonsav_spp)
      ) |>
      gt::tab_source_note(
        source_note = 'Species shaded grey (non-SAV) are only scored on abundance.'
      )
  }

  return(out)

}

#' Create summary of metrics across transects for each species
#'
#' @param evalgrp Data frame of evaluation group
#' @param vr Character vector of variable names
#'
#' @details The deviation is computed, and weighted, at the transect level
#'   before being rolled up to a single per-species number. At each transect:
#'   the deviation is the reported minus true value (\code{dif}, in the
#'   metric's native/ordinal units) or the symmetric percent difference
#'   (\code{pct}); the weight is based on \code{sdgrp} (see
#'   \code{\link{evalgrp_fun}}), the standard deviation of all groups'
#'   individual reports at that transect, i.e. how much trouble groups had
#'   agreeing with each other there, not how the true value varies from one
#'   transect to another (that spatial variability is not used anywhere in
#'   scoring). \code{dif} is weighted by \code{1 / (1 + sdgrp)} and \code{pct}
#'   by \code{1 / (1 + sdgrp / truval)} (a coefficient of variation, so the
#'   weight is scale-free for metrics scored on a percent basis); a transect
#'   where fewer than two groups reported (so \code{sdgrp} is undefined) gets
#'   full weight, same convention as an undefined weight anywhere else in
#'   scoring. \code{avediff} and \code{aveperc} are a true weighted average
#'   across transects, \code{sum(w * d) / sum(w)}, not \code{mean(w * d)}: a
#'   transect's influence on the result scales with its weight, rather than
#'   every transect counting equally toward a fixed denominator while only
#'   its contributed value shrinks. \code{sdtruv}/\code{cvtruv} report the
#'   average of the per-transect \code{sdgrp}/CV across transects, for
#'   reference only (not used in any further calculation). There is
#'   deliberately no measure here of how consistent a single species' own
#'   deviation was across transects (an earlier \code{devsd}/\code{devcv}):
#'   with typically only 2-4 transects per species per group, that estimate
#'   was dominated by sample-size noise and by the true value's own spatial
#'   pattern rather than by anything about the group's performance, and there
#'   was no way to separate the two at that sample size. A transect where
#'   only one side of \code{aveval}/\code{truval} is known (a missed report,
#'   or a species not on the consensus list there) is excluded from the
#'   deviation rather than skewing \code{aveval} or \code{truval}
#'   independently. \code{aveval} and \code{truval} in the returned data
#'   frame are for display only (rounded to a whole category for Abundance)
#'   and are not the values \code{avediff} is derived from. Abundance is
#'   scored for every consensus species, including macroalgae, since a
#'   missed or falsely-reported species is itself a scoreable event (Step 3)
#'   regardless of species type. A false positive (\code{truval} unresolved
#'   because no other group corroborated the species there) contributes a
#'   flat one-category \code{dif} of 1, not the reported category minus the
#'   imputed true value: what category the group happened to report is not
#'   evidence about whether the species was actually there, so it should not
#'   scale the size of the penalty the way a genuine, both-sides-observed
#'   disagreement does. A missed species (the group failed to report
#'   something every other group agreed was there) is unaffected by this and
#'   keeps the full reported-minus-true deviation, since the true category is
#'   itself well established there. For Abundance, the returned data frame
#'   also carries \code{any_fp}/\code{any_missed}, whether any of that
#'   species' transects were a false positive or a missed report; these are
#'   for display only (e.g. \code{\link{card_fun}}) and play no part in
#'   scoring, which already reflects them through \code{dif}. Blade Length
#'   and Short Shoot Density are restricted to \code{\link{savspecies}} (SAV
#'   only), since those measurements are not taken for macroalgae.
sppdiff_fun <- function(evalgrp, vr = c('Abundance', 'Blade Length', 'Short Shoot Density')){

  vr <- match.arg(vr)

  out <- evalgrp |>
    dplyr::rename(
      aveval = paste(vr, 'aveval'),
      truval = paste(vr, 'truval'),
      sdgrp  = paste(vr, 'sdgrp')
    )

  if(vr == 'Abundance'){

    out <- out |>
      dplyr::select(
        Species,
        aveval,
        truval,
        sdgrp
      ) |>
      dplyr::mutate(across(-Species, as.numeric)) |>
      dplyr::mutate(
        # a false positive (species reported that no other group
        # corroborated at that transect, so truval is unresolved) is
        # flagged before either side is imputed, so its penalty can be
        # fixed rather than scaled by what was reported
        is_fp     = is.na(truval) & !is.na(aveval),
        is_missed = is.na(aveval) & !is.na(truval),
        aveval = dplyr::if_else(is_missed, 1, aveval),
        truval = dplyr::if_else(is_fp, 1, truval),
        # per-transect deviation, computed before averaging (same rationale
        # as the continuous branch below): the group's aveval and the
        # consensus truval are both already resolved per transect (imputed
        # to 'no coverage' on whichever side is missing), so the deviation
        # is taken transect by transect and only rounded to a whole category
        # afterward for the Reported/True display columns, not before.
        # The weight is also transect-specific (cross-group agreement at
        # that transect), applied here before averaging across transects.
        # A false positive is scored as a flat one-category miss rather
        # than reported minus imputed-true: the category the group happened
        # to report has nothing to do with whether the species was really
        # there, so it should not scale the penalty the way a genuine,
        # both-sides-observed disagreement does.
        dif    = dplyr::if_else(is_fp, 1, aveval - truval),
        pct    = ifelse(truval == 0, NA, (aveval - truval) / ((aveval + truval) / 2)),
        cvgrp  = ifelse(is.na(sdgrp) | truval == 0, NA, sdgrp / truval),
        wt_abs = 1 / (1 + dplyr::coalesce(sdgrp, 0)),
        wt_pct = 1 / (1 + dplyr::coalesce(cvgrp, 0))
      ) |>
      dplyr::summarise(
        # true weighted average (sum(w*x)/sum(w)), not mean(w*x): a transect's
        # relative influence on avediff/aveperc should scale with wt_abs/wt_pct,
        # not just its contributed value while every transect still counts
        # equally toward the denominator (T), the same reasoning that makes
        # allgrpscr_fun's species-into-metric roll-up a weighted.mean too
        avediff = weighted.mean(dif, wt_abs, na.rm = T),
        aveperc = ifelse(all(is.na(pct)), NA, weighted.mean(pct, wt_pct, na.rm = T)),
        sdtruv  = ifelse(all(is.na(sdgrp)), NA, mean(sdgrp, na.rm = T)),
        cvtruv  = ifelse(all(is.na(cvgrp)), NA, mean(cvgrp, na.rm = T)),
        aveval  = round(mean(aveval, na.rm = T), 0),
        truval  = round(mean(truval, na.rm = T), 0),
        # species-level flags for whether any of its transects were a
        # missed report or a false positive, for display purposes only
        # (not used anywhere in scoring, which already reflects these via dif)
        any_fp     = any(is_fp, na.rm = T),
        any_missed = any(is_missed, na.rm = T),
        .by = 'Species'
      )

  } else {

    out <- out |>
      dplyr::filter(Species %in% savspecies()) |> # not measured for macroalgae
      dplyr::filter(
        sum(!is.na(truval)) > 0, # remove species where short shoot or blade length is not measured
        .by = Species
      ) |>
      dplyr::select(Species, aveval, truval, sdgrp) |>
      dplyr::mutate(across(-Species, as.numeric)) |>
      dplyr::mutate(
        # per-transect deviation, computed before averaging so a group's
        # deviation only ever compares matched (aveval, truval) pairs at the
        # same transect; a transect where only one side is known (a missed
        # report, or a species not on the consensus list there) drops out of
        # the deviation entirely rather than skewing aveval's or truval's mean
        # independently. Weighted by the transect-specific cross-group spread
        # (sdgrp/cvgrp) before averaging across transects, same as Abundance.
        dif    = ifelse(is.na(aveval) | is.na(truval), NA, aveval - truval),
        pct    = ifelse(is.na(aveval) | is.na(truval) | truval == 0, NA, (aveval - truval) / ((aveval + truval) / 2)),
        cvgrp  = ifelse(is.na(sdgrp) | is.na(truval) | truval == 0, NA, sdgrp / truval),
        wt_abs = 1 / (1 + dplyr::coalesce(sdgrp, 0)),
        wt_pct = 1 / (1 + dplyr::coalesce(cvgrp, 0))
      ) |>
      dplyr::summarise(
        aveval  = ifelse(all(aveval == 0 | is.na(aveval)), NA, mean(aveval, na.rm = T)),
        truval  = ifelse(all(truval == 0 | is.na(truval)), NA, mean(truval, na.rm = T)),
        # true weighted average, see the Abundance branch above for why
        avediff = ifelse(all(is.na(dif)), NA, weighted.mean(dif, wt_abs, na.rm = T)),
        aveperc = ifelse(all(is.na(pct)), NA, weighted.mean(pct, wt_pct, na.rm = T)),
        sdtruv  = ifelse(all(is.na(sdgrp)), NA, mean(sdgrp, na.rm = T)),
        cvtruv  = ifelse(all(is.na(cvgrp)), NA, mean(cvgrp, na.rm = T)),
        .by = 'Species'
      )

  }

  out <- out |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c('aveval', 'sdtruv', 'truval', 'avediff')), \(x) round(x, 1)),
      # aveperc/cvtruv are fractions (roughly -2 to 2), not native-unit
      # measurements; rounding them to 1 decimal is a 10-percentage-point
      # bucket, coarse enough to visibly disagree with a full-precision
      # calculation of the same quantity, so they get finer rounding here
      dplyr::across(dplyr::any_of(c('aveperc', 'cvtruv')), \(x) round(x, 3))
    ) |>
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
  
  # display units differ by metric: Abundance is scored on raw category
  # positions, Blade Length/Short Shoot Density on percent difference (Step 4)
  vruni <- c('Abundance' = 'mean BB categories away',
             'Blade Length' = 'difference on average',
             'Short Shoot Density' = 'difference on average')
  vruni <- vruni[[vr]]

  rptcol  <- '#004F7E'
  trucol  <- '#958984'
  fpcol   <- '#B2182B' # false positive (Abundance only)
  misscol <- '#E08214' # missed species (Abundance only)

  # SAV species listed first (matching the order groups are trained to
  # prioritize), non-SAV (macroalgae) after, alphabetical within each group
  # rather than the plain alphabetical order sppdiff_fun returns
  sppdiff <- sppdiff_fun(evalgrp, vr) |>
    dplyr::arrange(dplyr::desc(Species %in% savspecies()), Species)

  # for Abundance, a false positive or missed species has a mechanically
  # forced sign (always +1 for a false positive, always <= 0 for a missed
  # species, Step 3): including them here would make "how consistent is the
  # direction of error across species" reflect species-ID events instead of
  # genuine reporting bias. This subset (not sppdiff itself) is what decides
  # the overall direction (sgndff) and cross-species consistency (sgndffuni)
  # below. Blade Length/Short Shoot Density have no such flags and are
  # unaffected. Falls back to the full set if every species had an event.
  sppdiff_genuine <- if(vr == 'Abundance'){
    out <- sppdiff |> dplyr::filter(!any_fp, !any_missed, !is.na(.data[['avediff']]))
    if(nrow(out) == 0) sppdiff else out
  } else {
    sppdiff
  }

  grpscr <- allgrpscr |>
    dplyr::filter(grpact == !!grp) |>
    dplyr::select(-grpact)

  scr <- as.character(grpscr[[vr]])

  hiscr <- scr %in% c('A-', 'B+', 'B')

  # column actually used for scoring this metric (Step 4): category
  # difference for Abundance, percent difference for the other two
  devcol <- if(vr == 'Abundance') 'avediff' else 'aveperc'

  # overall diff (genuine species only for Abundance, see sppdiff_genuine above)
  sgndff <- sppdiff_genuine |>
    dplyr::summarise(
      dispval = round(mean(.data[[devcol]], na.rm = T), 1)
    ) |>
    dplyr::pull() |>
    sign()
  sgndff <- ifelse(sgndff == 0, '', ifelse(sgndff == 1, '+', '-'))

  # spp summary text
  spptxt <- sppdiff |>
    dplyr::mutate(
      dispval = .data[[devcol]],
      sgndff = sign(dispval),
      sgndff = dplyr::case_when(
        is.na(sgndff) ~ '',
        sgndff == 0 ~ '',
        sgndff == 1 ~ '+',
        sgndff == -1 ~ '' # already a negative prefix
      ),
      dispval = if(vr == 'Abundance') as.character(dispval) else paste0(round(dispval * 100, 0), '%'),
      dispval = ifelse(is.na(.data[[devcol]]), 'not recorded', dispval),
      dispval = dplyr::case_when(
        dispval == 'not recorded' ~ dispval,
        T ~ paste(sgndff, dispval, ' ', vruni, ' across transects', sep = '')
      )
    )

  # for Abundance, a false positive or missed species swaps in a short
  # explanation instead of the generic category-difference text: the
  # numeric difference alone (a flat 1 for a false positive, Step 3) does
  # not say why, and is not informative on its own
  if(vr == 'Abundance'){
    abulab <- c('no coverage', 'solitary', 'few', '<5%', '5-25%', '25-50%', '51-75%', '76-100%')
    spptxt <- spptxt |>
      dplyr::mutate(
        dispval = dplyr::case_when(
          any_fp ~ 'recorded but not confirmed by any other group',
          any_missed ~ paste0('missed, true cover was ', abulab[truval]),
          T ~ dispval
        )
      )
  }

  spptxt <- spptxt |>
    dplyr::mutate(
      Species = paste0('<i><b>', Species, '</b></i>')
    ) |>
    tidyr::unite('Species', Species, dispval, sep = ' ') |>
    dplyr::mutate(
      Species = paste0('<span>', Species, '</span>')
    ) |>
    dplyr::pull(Species) |>
    paste0(collapse = '</p><p>')
  spptxt <- paste0('<p>', spptxt, '</p>')
  # bslib's value-box-area is a flex column (title, value, then this content)
  # centered as a block by default, leaving blank space above and below on a
  # short species list. flex: 1 1 auto lets this div claim that leftover
  # space instead (min-height: 0 lets it actually shrink below its content
  # size, the usual flexbox gotcha, without that overflow-y wouldn't engage).
  # flex-grow alone has nothing to overflow against, though: without an
  # upper bound the div just grows to fit all its content and the scrollbar
  # never appears, so max-height puts a ceiling back on it. Below that
  # ceiling it still grows to fill the card's leftover space; at or above
  # it, it scrolls instead of pushing past the card.
  spptxt <- paste0('<div style="flex: 1 1 auto; min-height: 0; max-height: 200px; overflow-y: auto;">', spptxt, '</div>')

  # barplot prep: species-level difference from true (bar{d}_s), centered at
  # zero, on the same basis Step 4 actually scores on (category difference
  # for Abundance, percent difference for Blade Length/Short Shoot Density).
  # No error bar here: an earlier version showed a per-species consistency
  # estimate (gamma_s), but that was dropped from scoring entirely (see
  # sppdiff_fun) since with typically only 2-4 transects per species it was
  # mostly sample-size noise, so there is nothing reliable left to plot.
  sppdiff <- sppdiff |>
    dplyr::filter(!is.na(.data[[devcol]]))

  # a false positive's bar is always the same flat 1-category difference
  # (Step 3), so it adds visual clutter without adding information the plot
  # is meant to convey; it's still covered in the text narrative to the
  # right. Missed species are left in the plot, their bar height still
  # reflects the true abundance that was missed (Step 3).
  if(vr == 'Abundance'){
    sppdiff <- sppdiff |> dplyr::filter(!any_fp)
  }

  sppdiff <- sppdiff |>
    dplyr::mutate(
      # preserve the SAV-first row order set above as the factor's level
      # order, rather than factor()'s default alphabetical re-sort
      Species = factor(Species, levels = unique(Species)),
      Savnum = as.numeric(Species),
      plotval = if(vr == 'Abundance') avediff else aveperc * 100,
      # bar color flags a false positive or missed species (Abundance only,
      # see Step 3): the bar height alone doesn't distinguish a genuine
      # disagreement from a species-ID error
      barcol = if(vr == 'Abundance'){
        dplyr::case_when(
          any_fp ~ fpcol,
          any_missed ~ misscol,
          T ~ rptcol
        )
      } else rptcol
    )

  # symmetric range so the zero line sits in the middle of the plot
  rngmax <- max(abs(sppdiff$plotval), na.rm = T)
  rngmax <- if(!is.finite(rngmax) || rngmax == 0) 1 else rngmax * 1.1
  yrng <- c(-rngmax, rngmax)

  # y-axis units and hover text differ by metric: category difference for
  # Abundance, percent difference for Blade Length/Short Shoot Density
  if(vr == 'Abundance'){
    yxs <- list(title = 'Avg. diff. from true<br>(+/- categories)', range = yrng, zeroline = TRUE, zerolinewidth = 2, zerolinecolor = '#444444')
    hovtxt <- paste0(
      sppdiff$Species, ': ', ifelse(sppdiff$plotval > 0, '+', ''), sppdiff$plotval, ' categories',
      dplyr::case_when(
        sppdiff$any_fp ~ ' (false positive)',
        sppdiff$any_missed ~ ' (missed species)',
        T ~ ''
      )
    )
  } else {
    yxs <- list(title = 'Avg. diff. from true (%)', range = yrng, zeroline = TRUE, zerolinewidth = 2, zerolinecolor = '#444444')
    hovtxt <- paste0(sppdiff$Species, ': ', ifelse(sppdiff$plotval > 0, '+', ''), round(sppdiff$plotval, 0), '%')
  }

  # bar plot: one bar per species, centered at zero
  p <- plotly::plot_ly(
      sppdiff,
      x = ~ Savnum,
      y = ~ plotval,
      type = 'bar',
      marker = list(color = ~ barcol),
      text = hovtxt,
      hoverinfo = 'text',
      textposition = 'none'
    ) |>
    plotly::config(displayModeBar = F) |>
    plotly::layout(
      xaxis = list(title = '', ticktext = levels(sppdiff$Species), tickvals = sppdiff$Savnum),
      yaxis = yxs,
      showlegend = F
    ) |>
    plotly::config(displayModeBar = F)

  troph <- troph_fun(scr)
  
  # the grade (and hiscr) already reflects how much groups varied from each
  # other this year, not an absolute magnitude, so the wording says that
  # explicitly rather than "a little"/"a lot", which reads as an absolute
  # claim and can look contradicted by the per-species percent differences
  # shown just below (e.g. a grade of A doesn't mean those are near zero,
  # only that this group was more consistent than most others that year)
  txtdev <- dplyr::case_when(
    scr == 'A' ~ 'reported values are more consistent than other groups',
    hiscr ~ 'reported values are about as consistent as other groups',
    !hiscr ~ 'reported values are less consistent than other groups',
  )
  
  sgndffuni <- unique(sign(sppdiff_genuine[[devcol]]))
  
  txtdir <- dplyr::case_when(
    scr == 'A' ~ ', good job!',
    length(sgndffuni) > 1 & scr != 'A' ~ ', direction varies by species',
    sgndff == '+' & hiscr & scr != 'A' ~ ', typically slightly higher',
    sgndff == '-' & hiscr & scr != 'A' ~ ', typically slightly lower',
    sgndff == '+' & !hiscr & scr != 'A' ~ ', typically much higher',
    sgndff == '-' & !hiscr & scr != 'A' ~ ', typically much lower',
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

#' Compute per-metric calibration constants from historical within-year spread
#'
#' For each year, computes the within-year SD of group weighted-mean
#' deviations per metric (how spread out groups were relative to each other).
#' Returns the mean and SD of those yearly spreads so a focal year can be
#' z-scored against history to adjust the grade floor.
#'
#' @param trndat data frame, training data
#' @param metric named character vector giving the deviation basis to use per
#'   score variable (or a single unnamed \code{'abs'}/\code{'pct'}, recycled to
#'   all three). \code{'abs'} uses raw absolute deviations (original units,
#'   e.g. cm or shoots/m2); \code{'pct'} uses the symmetric percent difference
#'   instead so the spread doesn't scale with the magnitude of the true value
#'   (see \code{\link{sppdiff_fun}}). Default keeps Abundance on its ordinal
#'   absolute scale (already unitless and applied the same way to every
#'   species) and scores Blade Length and Short Shoot Density on percent
#'   difference (their natural scale varies by species and by the true mean)
#'
#' @return named list with element \code{mean_sd}, a named list of per-metric
#'   historical mean within-year spreads
calibrate_scr_fun <- function(trndat, metric = c(Abundance = 'abs', `Blade Length` = 'pct', `Short Shoot Density` = 'pct')){

  yrs <- unique(trndat$yr)

  yr_spreads <- purrr::map(yrs, function(yr){
    truvar <- truvar_fun(trndat, yr)
    allgrpscr_fun(trndat, yr, truvar, raw_diff = TRUE, metric = metric) |>
      dplyr::summarise(dplyr::across(Abundance:`Short Shoot Density`,
                                     ~ sd(.x, na.rm = TRUE))) |>
      dplyr::mutate(yr = yr)
  }) |>
    dplyr::bind_rows()

  list(
    mean_sd = yr_spreads |>
      dplyr::summarise(dplyr::across(Abundance:`Short Shoot Density`,
                                     ~ mean(.x, na.rm = TRUE))) |>
      as.list()
  )

}

#' Calculate scores for all groups based on distribution of scores
#'
#' @param trndat data frame of all seagrass transect training data
#' @param yr integer, year
#' @param truvar data frame "true" values from training data for a given year
#' @param raw logical, return raw scores, otherwise letter grades
#' @param raw_diff logical, return pre-rescale weighted-mean deviations
#' @param cal named list of per-metric calibration constants from \code{\link{calibrate_scr_fun}}
#' @param k numeric, maximum floor lift in grade-points when all groups agree perfectly (default 50, giving a floor of 100)
#' @param metric named character vector giving the deviation basis to use per
#'   score variable (or a single unnamed \code{'abs'}/\code{'pct'}, recycled to
#'   all three); see \code{\link{calibrate_scr_fun}} for the default and
#'   rationale. Should match the \code{metric} used to build \code{cal}.
#'   Transect-level weighting already happened inside \code{\link{sppdiff_fun}}
#'   when species values were computed; combining species into a metric score
#'   here is a plain (unweighted) mean of each species' (already
#'   transect-weighted) absolute deviation. There is deliberately no further
#'   per-species weighting at this level: an earlier version amplified a
#'   species a group was inconsistent on across transects, but with typically
#'   only 2-4 transects per species that consistency estimate was mostly
#'   sample-size noise plus the true value's own spatial pattern, not a
#'   reliable signal about the group, so it was dropped rather than kept as
#'   an unreliable weight
allgrpscr_fun <- function(trndat, yr, truvar, raw = F, raw_diff = FALSE, cal = NULL, k = 50,
                           metric = c(Abundance = 'abs', `Blade Length` = 'pct', `Short Shoot Density` = 'pct')){

  varnms <- c('Abundance', 'Blade Length', 'Short Shoot Density')
  if(is.null(names(metric)))
    metric <- stats::setNames(rep_len(metric, length(varnms)), varnms)
  metric <- metric[varnms]
  stopifnot("metric must be 'abs' or 'pct' for each of Abundance, Blade Length, Short Shoot Density" =
              all(metric %in% c('abs', 'pct')))

  scrs <- trndat |>
    dplyr::filter(yr == !!yr) |>
    dplyr::select(grpact) |>
    dplyr::distinct() |>
    dplyr::group_nest(grpact, .key = 'evalgrp') |>
    dplyr::mutate(
      evalgrp = purrr::map2(grpact, evalgrp, ~ evalgrp_fun(trndat, yr, .x, truvar))
    ) |>
    tidyr::crossing(var = varnms) |>
    dplyr::mutate(
      avediff = purrr::pmap(list(evalgrp, var), function(evalgrp, var){
        sppdiff_fun(evalgrp, var) |>
          dplyr::mutate(
            devval = if(metric[[var]] == 'pct') aveperc else avediff
          ) |>
          dplyr::summarise(
            avediff = mean(abs(devval), na.rm = T)
          ) |>
          dplyr::pull(avediff)
      })
    ) |>
    dplyr::select(-evalgrp) |>
    tidyr::unnest(avediff) |>
    tidyr::pivot_wider(names_from = var, values_from = avediff)

  if(raw_diff)
    return(scrs)

  if(!is.null(cal)){
    # ratio of this year's within-year spread to historical mean; < 1 means tight year
    yr_sd <- scrs |>
      dplyr::summarise(dplyr::across(Abundance:`Short Shoot Density`,
                                     ~ sd(.x, na.rm = TRUE)))
    safe_ratio <- function(val, mn) ifelse(is.na(mn) | mn == 0, 1, val / mn)
    floor_abu <- max(50, 50 + (1 - safe_ratio(yr_sd$Abundance,             cal$mean_sd$Abundance))             * k)
    floor_bl  <- max(50, 50 + (1 - safe_ratio(yr_sd$`Blade Length`,        cal$mean_sd$`Blade Length`))        * k)
    floor_ss  <- max(50, 50 + (1 - safe_ratio(yr_sd$`Short Shoot Density`, cal$mean_sd$`Short Shoot Density`)) * k)
    scrs <- scrs |>
      dplyr::mutate(
        Abundance             = scales::rescale(abs(Abundance),             to = c(100, floor_abu)),
        `Blade Length`        = scales::rescale(abs(`Blade Length`),        to = c(100, floor_bl)),
        `Short Shoot Density` = scales::rescale(abs(`Short Shoot Density`), to = c(100, floor_ss)),
        `Total` = (`Blade Length` + `Short Shoot Density` + Abundance) / 3
      )
  } else {
    scrs <- scrs |>
      dplyr::mutate(
        dplyr::across(`Abundance`:`Short Shoot Density`, ~ scales::rescale(abs(.x), to = c(100, 50))),
        `Total` = (`Blade Length` + `Short Shoot Density` + `Abundance`) / 3
      )
  }

  if(raw)
    return(scrs)
  
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
  
  screxp <- 'Your group\'s overall score reflects how closely your reported values match the group averages, calibrated against historical performance across all training years. The overall score is based on the average of the scores below for species abundance, blade length, and short shoot density. To learn more about how scores are calculated, check out the <a target="_blank" href="https://tbep-tech.github.io/seagrasstransect-training-reports/scoring.html">scoring document</a>.'
  
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
#'
#' @details Flags two kinds of species-ID error within the \code{spp} group
#'   (Step 3): a missed species (on the consensus list but not reported,
#'   \code{Abundance aveval} unresolved) and a false positive (reported but
#'   not on the consensus list, \code{Abundance truval} unresolved). Both are
#'   reported together when both occur.
sppimp_fun <- function(evalgrp, spp = c('seagrass', 'non-sav')){

  lnk <- 'https://drive.google.com/file/d/1naZpND_Ur90abqND-ZhZ_fJtBBiuetjt/view'

  spp <- match.arg(spp)

  if(spp == 'seagrass')
    sppid <- evalgrp |>
      dplyr::filter(Species %in% savspecies())

  if(spp == 'non-sav')
    sppid <- evalgrp |>
      dplyr::filter(!Species %in% savspecies())

  sppmiss <- sppid |>
    dplyr::filter(is.na(`Abundance aveval`) & !is.na(`Abundance truval`)) |>
    dplyr::select(Species) |>
    dplyr::distinct()

  sppfp <- sppid |>
    dplyr::filter(!is.na(`Abundance aveval`) & is.na(`Abundance truval`)) |>
    dplyr::select(Species) |>
    dplyr::distinct()

  # a count of 1 vs more reads as 'one'/'a few' in both messages below
  cnttxt <- function(x) if(length(x) == 1) 'one' else 'a few'

  txtmiss <- NULL
  if(nrow(sppmiss) > 0){
    sppmiss <- sppmiss |> dplyr::pull(Species) |> sort()
    txtmiss <- paste0('Missed ', cnttxt(sppmiss), ' (', paste(sppmiss, collapse = ', '), ')! Check out the species guide at the link <a target="_blank" href="', lnk, '">here</a>.')
  }

  txtfp <- NULL
  if(nrow(sppfp) > 0){
    sppfp <- sppfp |> dplyr::pull(Species) |> sort()
    txtfp <- paste0('Reported ', cnttxt(sppfp), ' species not confirmed by any other group (', paste(sppfp, collapse = ', '), ')! Check out the species guide at the link <a target="_blank" href="', lnk, '">here</a>.')
  }

  txtout <- paste(c(txtmiss, txtfp), collapse = ' ')
  if(is.null(txtmiss) & is.null(txtfp))
    txtout <- 'All species found, good job!'

  out <- paste0('<span><h4>', txtout, '</h4></span>')
  out <- gt::html(out)

  return(out)

}

#' Get character vector of sav species for filtering
savspecies <- function(){
  
  out <- c('Halodule', 'Syringodium', 'Thalassia', 'Halophila', 'Ruppia')
  
  return(out)
  
}

#' Get all grades for groups over time
#' 
#' @param trndat data frame, transect training data
#' @param na.rm logical, remove groups with no affiliation from results
#' @param usemon logical, return only groups that are within tbeptools::trnlns
#' @param metric named character vector (or a single unnamed \code{'abs'}/
#'   \code{'pct'}) passed to \code{\link{calibrate_scr_fun}} and
#'   \code{\link{allgrpscr_fun}} (ignored when \code{cal} is supplied
#'   directly); see \code{\link{calibrate_scr_fun}} for the default and
#'   rationale
#'
#' @details some years have mroe than one group participating, e.g., two SWFWMD groups, scores are averaged in these cases
allyrscr_fun <- function(trndat, na.rm = T, usemon = TRUE, cal = NULL,
                          metric = c(Abundance = 'abs', `Blade Length` = 'pct', `Short Shoot Density` = 'pct')){

  data(file = 'trnlns', package = 'tbeptools')

  grades <- c('A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D')
  grdbrk <- c(101, 95, 90, 85, 80, 75, 70, 65, 60, 55, 0)

  if(is.null(cal))
    cal <- calibrate_scr_fun(trndat, metric = metric)

  yrs <- unique(trndat$yr)

  out <- tibble::tibble(yr = yrs) |>
    dplyr::group_nest(yr) |>
    dplyr::mutate(
      data = purrr::map(yr, function(x){

        truvar <- truvar_fun(trndat, x)

        out <- allgrpscr_fun(trndat, x, truvar, raw = T, cal = cal, metric = metric)

        return(out)

      })
    ) |>
    tidyr::unnest('data') |> 
    tidyr::pivot_longer(cols = c(Abundance:Total),
                 names_to = 'var',
                 values_to = 'scr') |> 
    dplyr::mutate(
      grp = gsub('^.*:\\s(.*?)\\s\\(.*$', '\\1', grpact)
    ) |> 
    dplyr::summarise(
      scr = mean(scr, na.rm = T), 
      .by = c(yr, grp, var)
    ) |> 
    dplyr::mutate(
      grd = as.character(cut(-scr, breaks = -grdbrk, labels = grades))
    )
  
  if(na.rm)
    out <- out |> 
      dplyr::filter(!grp %in% 'NA') 
  
  if(usemon)
    out <- out |> 
      dplyr::filter(grp %in% tbeptools::trnlns$MonAgency)
  
  return(out)
  
}