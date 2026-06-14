# Calibration diagnostic plots
# Compares score distributions before and after z-score calibration and shows
# the within-year spread (SD of group deviations) that drives the adjustment.

library(tbeptools)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(patchwork)
library(here)

source(here('R/funcs.R'))
load(here('data/trndat.rda'))

# ---- collect data -----------------------------------------------------------

cal  <- calibrate_scr_fun(trndat)
yrs  <- sort(unique(trndat$yr))

# within-year SD of group avediffs per year x metric, with z-scores and floor
cal_lkp <- tibble::tibble(
  metric  = c('Abundance', 'Blade Length', 'Short Shoot Density'),
  mean_sd = c(cal$mean_sd$Abundance, cal$mean_sd$`Blade Length`,
              cal$mean_sd$`Short Shoot Density`),
  sd_sd   = c(cal$sd_sd$Abundance,   cal$sd_sd$`Blade Length`,
              cal$sd_sd$`Short Shoot Density`)
)

yr_spreads <- purrr::map(yrs, function(yr){
  truvar <- truvar_fun(trndat, yr)
  allgrpscr_fun(trndat, yr, truvar, raw_diff = TRUE) |>
    dplyr::summarise(dplyr::across(Abundance:`Short Shoot Density`,
                                   ~ sd(.x, na.rm = TRUE))) |>
    dplyr::mutate(yr = yr)
}) |>
  dplyr::bind_rows() |>
  tidyr::pivot_longer(-yr, names_to = 'metric', values_to = 'sd_avediff') |>
  dplyr::left_join(cal_lkp, by = 'metric') |>
  dplyr::mutate(
    z_score   = (sd_avediff - mean_sd) / sd_sd,
    floor_adj = as.integer(pmax(50, 50 - z_score * 10)),
    metric    = factor(metric, levels = c('Abundance', 'Blade Length', 'Short Shoot Density'))
  )

# numeric scores before and after calibration, all groups all years
scores_long <- purrr::map(yrs, function(yr){
  truvar <- truvar_fun(trndat, yr)
  dplyr::bind_rows(
    allgrpscr_fun(trndat, yr, truvar, raw = TRUE) |>
      dplyr::mutate(yr = yr, calibrated = 'Before'),
    allgrpscr_fun(trndat, yr, truvar, raw = TRUE, cal = cal) |>
      dplyr::mutate(yr = yr, calibrated = 'After')
  )
}) |>
  dplyr::bind_rows() |>
  tidyr::pivot_longer(Abundance:Total, names_to = 'metric', values_to = 'score') |>
  dplyr::mutate(
    metric     = factor(metric,
                        levels = c('Abundance', 'Blade Length', 'Short Shoot Density', 'Total')),
    calibrated = factor(calibrated, levels = c('Before', 'After'))
  )

# ---- plot 1: within-year spread per year per metric -------------------------
# bars coloured by z-score; dashed line = historical mean
# label each bar with z-score and the resulting score floor

p_spread <- ggplot(yr_spreads,
                   aes(x = factor(yr), y = sd_avediff, fill = z_score)) +
  geom_col(width = 0.7, colour = 'grey40', linewidth = 0.3) +
  geom_hline(aes(yintercept = mean_sd), linetype = 'dashed', linewidth = 0.6) +
  geom_text(aes(label = sprintf('z=%.1f\nfloor=%d', z_score, floor_adj)),
            vjust = -0.25, size = 2.7, lineheight = 0.9) +
  scale_fill_gradient2(low = '#2166ac', mid = '#f7f7f7', high = '#d73027',
                       midpoint = 0, name = 'Z-score') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  facet_wrap(~ metric, scales = 'free_y', nrow = 1) +
  labs(
    x    = NULL,
    y    = 'Within-year SD of group deviations',
    title    = 'Within-year spread of group deviations',
    subtitle = paste0('Dashed line = historical mean. Bar labels: z-score and calibrated score floor.',
                      '\nBlue (negative z) = tighter than average → floor rises;',
                      '  red (positive z) = looser → no adjustment.')
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = 'right',
        axis.text.x = element_text(angle = 45, hjust = 1))

# ---- plot 2: before vs after scores per group per year (Total) --------------
# each point is one group; lines connect the same group before/after within a year

p_scores <- scores_long |>
  ggplot(aes(x = calibrated, y = score)) +
  geom_hline(yintercept = c(55, 70, 85, 95),
             linetype = 'dotted', colour = 'grey55', linewidth = 0.4) +
  geom_line(aes(group = grpact), colour = 'grey70', linewidth = 0.5) +
  geom_point(aes(colour = calibrated), size = 2) +
  scale_colour_manual(values = c('Before' = 'grey55', 'After' = '#004F7E'), name = NULL) +
  scale_y_continuous(
    limits = c(45, 103), breaks = seq(50, 100, 10),
    sec.axis = sec_axis(~ ., breaks = c(55, 70, 85, 95),
                        labels = c('D', 'C', 'B', 'A-'))
  ) +
  facet_grid(metric ~ yr) +
  labs(
    x        = NULL,
    y        = 'Numeric score',
    title    = 'Scores before vs. after calibration by metric and year (each line = one group)',
    subtitle = paste0('Lines shift upward in tight years (negative z); ',
                      'no shift in loose years (z ≥ 0).')
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = 'top',
        strip.text = element_text(size = 9))

# ---- combine and display ----------------------------------------------------

p_spread / p_scores + patchwork::plot_layout(heights = c(1, 1.2))
