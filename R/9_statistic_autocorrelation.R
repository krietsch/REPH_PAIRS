
# Packages
sapply( c('data.table', 'ggplot2',  'glmmTMB', 'effects', 'activity', 'nlme', 'knitr', 'DHARMa', 'patchwork'), 
        require, character.only = TRUE)

# Data
dp  = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', sep = '\t', header = TRUE, nThread = 20) |>  data.table()

# plot settings
margin_ = unit(c(2, 2, 2, 2), 'pt')
margin_top = unit(c(2, 2, 6, 2), 'pt')
sample_size_label = 2.5

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/9_statistic_autocorrelation.R', output_dir = './OUTPUTS/R_COMPILED')

setorder(dp, pairID, nestID, datetime_1)

dp[, interaction_lag1 := shift(interaction, type = "lead") |> as.numeric(), by = .(pairID, nestID, date_)]
# dp[is.na(interaction_lag1), interaction_lag1 := interaction]


# time of the day 
dp[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
dp[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]
dp[, timef := factor(round(as.numeric(scale(datetime_1)), 3))]

# nestID for each day
dp[, nestID_day := paste0(nestID, '_', datetime_rel_pair0)]

# subset data before clutch initiation
dx = dp[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
# dx = dp[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dx[, year_ := as.character(year_)]


setorder(dx, pairID, nestID, datetime_1)
dx[, time := 1:.N, .(date_, pairID)]



# model with time but without autocorrelation correction
m <- glmmTMB(interaction ~ poly(datetime_rel_pair, 2) + poly(initiation_rel, 2) +
               scale(sin_time) + scale(cos_time)  + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = FALSE,
             control = glmmTMBControl(parallel = 15)
)


# plot(allEffects(m))
summary(m)


res <- simulateResiduals(m)
res <- recalculateResiduals(res, group = dx$time)
testTemporalAutocorrelation(res, time = unique(dx$time))

testDispersion(res)


# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dp[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
dms = dms[, N_ini := .N, by = .(pairID, nestID)]
du = unique(dms, by = c('pairID', 'nestID', 'initiation_rel'))
du = du[!is.na(N_ini)]
du[, .(min(N_ini), max(N_ini))] # check min and max
du[, .(min(initiation_rel), max(initiation_rel))] # check min and max

dms = dms[interaction == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, .(N_int = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N_ini]
d0 = copy(du)

# point sizes range
du[, .(min(N_ini), max(N_ini))]

p1 = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day -5 to -1 (without lag1)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.1), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

p1






# same model with autocorrelation correction
m <- glmmTMB(interaction ~ interaction_lag1 + poly(datetime_rel_pair, 2) + poly(initiation_rel, 2) + 
               scale(sin_time) + scale(cos_time)  + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = FALSE,
             control = glmmTMBControl(parallel = 15)
)



# plot(allEffects(m))
summary(m)

res <- simulateResiduals(m)
res <- recalculateResiduals(res, group = dx$time)
testTemporalAutocorrelation(res, time = unique(dx$time))

testDispersion(res)






# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


p2 = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day -5 to -1 (with lag1)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.11), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

p2




# mean for each day
dpm = dp[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0, initiation_rel, N)]

dpm[, interaction_per_day := N_int / N]


# subset data before clutch initiation
dx = dpm[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
# dx = dp[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]


setorder(dx, pairID, nestID, datetime_rel_pair0)
dx[, time := 1:.N, .(datetime_rel_pair0, nestID)]



# model with time but without autocorrelation correction
m <- glmmTMB(interaction_per_day ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) +
               (datetime_rel_pair0 | nestID),
             family = gaussian, data = dx, REML = FALSE,
             control = glmmTMBControl(parallel = 15)
)


# plot(allEffects(m))
summary(m)



# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()



p3 = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day -5 to -1 (fitted on mean per day gaussian)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.1), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

p3


# merge plots
p1 + p2 + p3 +
  plot_layout(nrow = 2) +
  plot_annotation(tag_levels = 'a')


ggsave('./OUTPUTS/FIGURES/season_effect_day-5_to_-1.tiff', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#'Model during egg-laying
#--------------------------------------------------------------------------------------------------------------



# subset data before clutch initiation
dx = dp[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dx[, year_ := as.character(year_)]


setorder(dx, pairID, nestID, datetime_1)
dx[, time := 1:.N, .(date_, pairID)]



# model with time but without autocorrelation correction
m <- glmmTMB(interaction ~ poly(datetime_rel_pair, 2) + poly(initiation_rel, 2) +
               scale(sin_time) + scale(cos_time)  + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = FALSE,
             control = glmmTMBControl(parallel = 15)
)


# plot(allEffects(m))
summary(m)

res <- simulateResiduals(m)
res <- recalculateResiduals(res, group = dx$time)
testTemporalAutocorrelation(res, time = unique(dx$time))

testDispersion(res)



# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dp[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dms = dms[, N_ini := .N, by = .(pairID, nestID)]
du = unique(dms, by = c('pairID', 'nestID', 'initiation_rel'))
du = du[!is.na(N_ini)]
du[, .(min(N_ini), max(N_ini))] # check min and max
du[, .(min(initiation_rel), max(initiation_rel))] # check min and max

dms = dms[interaction == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, .(N_int = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N_ini]
d0 = copy(du)

# point sizes range
du[, .(min(N_ini), max(N_ini))]

p1 = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day 0 to 3 (without lag1)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

p1






# same model with autocorrelation correction
m <- glmmTMB(interaction ~ interaction_lag1 + poly(datetime_rel_pair, 2) + poly(initiation_rel, 2) + 
               scale(sin_time) + scale(cos_time)  + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = FALSE,
             control = glmmTMBControl(parallel = 15)
)



# plot(allEffects(m))
summary(m)

res <- simulateResiduals(m)
res <- recalculateResiduals(res, group = dx$time)
testTemporalAutocorrelation(res, time = unique(dx$time))

testDispersion(res)






# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


p2 = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day 0 to 3 (with lag1)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

p2





# mean for each day
dpm = dp[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0, initiation_rel, N)]
dpm[, interaction_per_day := N_int / N]


# subset data before clutch initiation

dx = dpm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]


setorder(dx, pairID, nestID, datetime_rel_pair0)
dx[, time := 1:.N, .(datetime_rel_pair0, nestID)]


# model with time but without autocorrelation correction
m <- glmmTMB(interaction_per_day ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) +
             (datetime_rel_pair0 | nestID),
             family = gaussian, data = dx, REML = FALSE,
             control = glmmTMBControl(parallel = 15)
)


# plot(allEffects(m))
summary(m)




# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()



p3 = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day 0 to 3 (fitted on mean per day)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.03), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

p3


# merge plots
p1 + p2 + p3 +
  plot_layout(nrow = 2) +
  plot_annotation(tag_levels = 'a')


ggsave('./OUTPUTS/FIGURES/season_effect_day_0_to_3.tiff', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')





