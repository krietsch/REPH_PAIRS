#==============================================================================================================
# Mate guarding
#==============================================================================================================

# Summary
# Mate guarding intensity in relation to breeding state

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr', 
          'stringr', 'ggnewscale', 'doFuture', 'patchwork', 'activity', 'glmmTMB', 'effects'), 
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/3_spatio_temporal_distance.R', output_dir = './OUTPUTS/R_COMPILED')

# Data
dp = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dr = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS_RANDOM.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table

#--------------------------------------------------------------------------------------------------------------
#' Mate guarding intensity in relation to breeding state
#--------------------------------------------------------------------------------------------------------------

# Proportion of time together breeders
dps = dp[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dps, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]

# Proportion of time together randomization
drs = dr[interaction == TRUE, .(N_int = .N), by = .(pairID, date_, datetime_rel_pair0)]
dur = unique(dr, by = c('pairID', 'date_', 'datetime_rel_pair0'))
dur = merge(dur, drs, by = c('pairID', 'date_', 'datetime_rel_pair0'), all.x = TRUE)
dur[is.na(N_int), N_int := 0]
dur[, int_prop := N_int / N]

# rbind data
du = rbind(du, dur)
du = du[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10]

### MODEL breeding pairs

# subset data for model
dm = dp[datetime_rel_pair >= -10 & datetime_rel_pair <= 10]

# relative time in seconds 
dm[, datetime_rel_pair_sec := datetime_rel_pair * 3600 * 24]

# sin and cos of datetime
dm[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
dm[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]

fm1 <- glmmTMB(interaction ~ poly(datetime_rel_pair, 2) +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm1)

# predict data
e <- allEffects(fm1, xlevels = 100)$"poly(datetime_rel_pair,2)" |>
  data.frame() |>
  setDT()

### MODEL randomization

# subset data for model
dr[, datetime_rel_pair := datetime_rel_pair0]
dmr = dr[datetime_rel_pair >= -10 & datetime_rel_pair <= 10]

# relative time in seconds 
dmr[, datetime_rel_pair_sec := datetime_rel_pair * 3600 * 24]

# sin and cos of datetime
dmr[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
dmr[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]

fm2 <- glmmTMB(interaction ~ poly(datetime_rel_pair, 2) +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dmr,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm2)

# predict data
er <- allEffects(fm2, xlevels = 100)$"poly(datetime_rel_pair,2)" |>
  data.frame() |>
  setDT()

# sample size
dss = unique(du[Np >= 0.25 & datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss = dss[, .N, by = datetime_rel_pair0]
dss

# sample size randomization (just to check)
dssr = unique(dur[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
             by = c('pairID', 'date_', 'datetime_rel_pair0'))
dssr = dssr[, .N, by = datetime_rel_pair0]
dssr

### plot proportion of time together 
ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[Np >= 0.25], 
               aes(datetime_rel_pair0, int_prop, color = type,  
                   group = interaction(type, datetime_rel_pair0)), 
               lwd = 0.4, outlier.size = 0.7) +
  scale_color_manual(values = c('firebrick4', 'dodgerblue4'), name = '', 
                     labels = c('Breeding pair', 'Male-female pair')) +
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair), size = 0.8, color = 'firebrick4') +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper), 
              fill = 'firebrick4', alpha = 0.2) +
  geom_line(data = er, aes(y = fit, x = datetime_rel_pair), size = 0.8, color = 'dodgerblue4') +
  geom_ribbon(data = er, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper), 
              fill = 'dodgerblue4', alpha = 0.2) +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.9), legend.background = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season_null_model_50breeders_new.tiff', plot = last_plot(),  width = 180, height = 120, units = c('mm'), dpi = 'print')

#--------------------------------------------------------------------------------------------------------------
#' Time spent at the nest
#--------------------------------------------------------------------------------------------------------------

### Proportion of time at the nest

# male in total at the nest
dps = dp[at_nest1 == TRUE, .(N_at_nest1 = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dps, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)

### CHECK 000000000000000

# male at nest with female
dps = dp[at_nest1 == TRUE & interaction == TRUE, .(N_at_nest1_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = merge(du, dps, by = c('pairID', 'nestID', 'datetime_rel_pair0'))

# proportion 
du[, at_nest1_prop := N_at_nest1 / N]
du[, at_nest1_int_prop := N_at_nest1_int / N]

# merge for boxplot
dup = rbind(du[, .(type = 'at nest', N_type = at_nest1_prop, pairID, nestID, datetime_rel_pair0)],
            du[, .(type = 'at nest with female', N_type = at_nest1_int_prop, pairID, nestID, datetime_rel_pair0 )])

# males at nest with female
ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey80') +
  geom_boxplot(data = dup, 
               aes(datetime_rel_pair0, N_type, 
                   group = interaction(type, datetime_rel_pair0), color = type),
               lwd = 0.4, outlier.size = 0.7) +
  scale_color_manual(values = c('firebrick4', 'dodgerblue4'), name = '', 
                     labels = c('Total', 'With female')) +
  
  geom_smooth(data = dup,
              aes(datetime_rel_pair0, N_type,
                  group = type, color = type)) +
  # geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair / 3600 / 24, ymin = lower, ymax = upper), 
  #             fill = 'black', alpha = 0.2) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  # geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0', '0.2', '0.4', '0.6', '0.8', '1'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.1, 0.9), legend.background = element_blank()) +
  ylab('Proportion / probability at nest') +
  xlab('Day relative to clutch initiation (= 0)')




