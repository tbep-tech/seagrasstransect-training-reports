library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(patchwork)

load(here::here('app/data/allyrscrs.RData'))

# FDEP and HC-ES dropped: they have the fewest years of data of the eight groups
alldat <- allyrscrs |>
  filter(!grp %in% c('FDEP', 'HC-ES')) |>
  mutate(yrctr = yr - min(yr))

# fixed categorical order, one hue per group (validated dataviz palette)
grpcols <- c('#2a78d6', '#eb6834', '#1baf7a', '#eda100', '#e87ba4', '#008300')
names(grpcols) <- sort(unique(alldat$grp))

# fit a random-intercept model for one score category and plot the
# population-level trend (thick) against each group's fitted trend (thin),
# with the observed scores layered on top
fit_and_plot <- function(varnm) {

  vardat <- alldat |> filter(var == varnm)
  mod <- lmer(scr ~ yrctr + (1 | grp), data = vardat)

  cfs <- summary(mod)$coefficients
  est <- cfs['yrctr', 'Estimate']
  pval <- cfs['yrctr', 'Pr(>|t|)']
  civ <- confint(mod, parm = 'yrctr', quiet = TRUE)

  fmt1 <- function(x) formatC(x, format = 'f', digits = 1)
  estlab <- paste0(fmt1(est), ' (', fmt1(civ[1, 1]), ', ', fmt1(civ[1, 2]), ')')
  plab <- if (pval >= 0.05) 'ns' else if (pval < 0.001) 'p < 0.001' else paste('p =', formatC(pval, format = 'f', digits = 3))

  prdgrd <- expand.grid(
    yrctr = seq(min(vardat$yrctr), max(vardat$yrctr), length.out = 50),
    grp = sort(unique(vardat$grp))
  )
  prdgrd$yr <- prdgrd$yrctr + min(vardat$yr)
  prdgrd$fit <- predict(mod, newdata = prdgrd, re.form = ~(1 | grp))

  ovrfit <- data.frame(yrctr = seq(min(vardat$yrctr), max(vardat$yrctr), length.out = 50))
  ovrfit$yr <- ovrfit$yrctr + min(vardat$yr)
  ovrfit$fit <- predict(mod, newdata = ovrfit, re.form = NA)

  plot <- ggplot() +
    geom_line(data = prdgrd, aes(x = yr, y = fit, color = grp, group = grp), linewidth = 0.6) +
    geom_line(data = ovrfit, aes(x = yr, y = fit), linewidth = 2, color = '#0b0b0b') +
    scale_color_manual(values = grpcols, name = 'Group') +
    labs(
      x = NULL, y = 'Score', title = varnm,
      subtitle = paste0('Change yr⁻¹: ', estlab, ', ', plab)
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())

  yrng <- range(c(prdgrd$fit, ovrfit$fit))

  list(plot = plot, yrng = yrng)
}

res <- lapply(c('Abundance', 'Blade Length', 'Short Shoot Density', 'Total'), fit_and_plot)

# common y-axis scale across all four panels
yrng <- range(unlist(lapply(res, `[[`, 'yrng')))
plts <- lapply(res, function(x) x$plot + coord_cartesian(ylim = yrng))

wrap_plots(plts, ncol = 2, guides = 'collect', axes = 'collect', axis_titles = 'collect') +
  plot_annotation(title = 'Predicted score trends by metric')

# ggsave(here::here('R/scratch_plot.png'), width = 11, height = 8, dpi = 150)
