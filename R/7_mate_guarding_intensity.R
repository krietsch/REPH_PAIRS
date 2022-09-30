#==============================================================================================================
# Mate guarding
#==============================================================================================================

# Summary
# Mate guarding intensity in relation to breeding state

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr', 
          'stringr', 'ggnewscale', 'doFuture', 'patchwork', 'activity', 'glmmTMB', 'effects'), 
        require, character.only = TRUE)

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/3_spatio_temporal_distance.R', output_dir = './OUTPUTS/R_COMPILED')

# Data
dID = fread('./DATA/NANO_TAGS_UNIQUE_BY_DAY.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dp  = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dr  = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS_RANDOM.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table

# Threshold to exclude data
Np_min = 0
# Np_min = 0.25
# Np_min = 0.5

# plot settings
margin_ = unit(c(0, 4, 0, 0), 'pt')

#--------------------------------------------------------------------------------------------------------------
#' Data available relative to clutch initiation
#--------------------------------------------------------------------------------------------------------------

dIDs = unique(dID[Np >= Np_min & datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
              by = c('sex', 'nestID', 'datetime_rel_pair0'))
dIDs = dIDs[, .N, by = .(sex, datetime_rel_pair0, both_tagged_overlapping)]
dIDs

# pairwise sample size
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss = unique(du[Np >= Np_min & datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss = dss[, .N, by = datetime_rel_pair0]
dss

# plot data available
dIDs[, sex := factor(sex, levels = c('F', 'M', 'pair'))]

pa =
ggplot(data = dIDs[both_tagged_overlapping == TRUE]) +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  geom_col(aes(datetime_rel_pair0, N, fill = sex), position=position_dodge(), width = 0.8) +
  geom_col(data = dss, aes(datetime_rel_pair0, N), fill = 'grey75', width = 0.8) +
  scale_fill_manual(values = c('firebrick3', 'dodgerblue4', 'grey75'), name = '', 
                     labels = c('Female only', 'Male only', 'Both'), drop = FALSE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(expand = expansion(add = c(0, 10))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.09, 0.7), legend.background = element_blank(), plot.margin = margin_, 
        legend.key.width = unit(0.4, 'cm'), legend.key.height = unit(0.4, 'cm')) +
  ylab('N') +
  xlab('')


# plot by nest
dp[, datetime_rel_pair_min := min(datetime_rel_pair), by = nestID]

du = unique(dp, by = c('nestID'))
setorder(du, by = datetime_rel_pair_min)
dp[, nestID := factor(nestID, levels = c(du$nestID))]

ggplot(data = dp) +
  geom_tile(aes(datetime_rel_pair, nestID, fill = interaction), width = 0.5, show.legend = TRUE) +
  scale_fill_manual(values = c('TRUE' = 'yellowgreen', 'FALSE' = 'steelblue4', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 5)) +
  theme_classic(base_size = 8) + 
  theme(legend.position = c(0.05, 0.9))

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season_eachID.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')

ggplot(data = dp) +
  geom_tile(aes(datetime_rel_pair, nestID, fill = interaction), width = 0.5, show.legend = TRUE) +
  scale_fill_manual(values = c('TRUE' = 'yellowgreen', 'FALSE' = 'steelblue4', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Nest') +
  scale_x_continuous(limits = c(-12, 5)) +
  theme_classic(base_size = 8) + 
  theme(legend.position = c(0.05, 0.9))

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season_eachID_crop.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')

dp[, nestID := as.character(nestID)]

#--------------------------------------------------------------------------------------------------------------
#' Mate guarding intensity in relation to breeding state
#--------------------------------------------------------------------------------------------------------------

# Proportion of time together breeders
dps = dp[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dps, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]

dp[, datetime_rel_pair_min := NULL]
du = du[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10]

# median per day
dmd = du[, .(int_prop_median = median(int_prop)), by = datetime_rel_pair0]

### plot proportion of time together 

du[, data_quality := ifelse(Np >= 0.75, '>0.75', '<0.75')]

pb = 
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[Np >= Np_min], 
               aes(datetime_rel_pair0, int_prop,  
                   group = interaction(datetime_rel_pair0)), 
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_line(data = dmd, aes(datetime_rel_pair0, int_prop_median), color = 'yellowgreen', size = 1.2) +
  geom_jitter(data = du[Np >= Np_min], aes(datetime_rel_pair0, int_prop, shape = data_quality), size = 1) +
  scale_shape_manual(values=c(1, 16)) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.85), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)')

pb 

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season_null_model_50breeders_new.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')


pb = 
  ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[Np >= Np_min], 
               aes(datetime_rel_pair0, int_prop,  
                   group = interaction(datetime_rel_pair0)), 
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_line(data = dmd, aes(datetime_rel_pair0, int_prop_median), color = 'yellowgreen', size = 1.2) +
  geom_line(data = du[any_EPY == TRUE], aes(datetime_rel_pair0, int_prop, group = nestID), color = 'firebrick4', size = 0.7) +
  geom_jitter(data = du[Np >= Np_min], aes(datetime_rel_pair0, int_prop, shape = data_quality, color = any_EPY), size = 1) +
  scale_shape_manual(values=c(1, 16)) +
  scale_color_manual(values = c('black', 'firebrick4', 'grey'), name = '',
                     labels = c('no EPY', 'EPY', 'unknown')) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.8), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)')

pb 

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season_null_model_50breeders_new_EPY.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' Time spent at the nest
#--------------------------------------------------------------------------------------------------------------

### Proportion of time at the nest

# male in total at the nest
dps = dp[Np >= Np_min & at_nest1 == TRUE, .(N_at_nest1 = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du2 = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))

du2 = merge(du2, dps, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)

# male at nest with female
dps = dp[at_nest1 == TRUE & interaction == TRUE, .(N_at_nest1_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du2 = merge(du2, dps, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)

# male at nest alone
dps = dp[at_nest1 == TRUE & interaction == FALSE, .(N_at_nest1_no_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du2 = merge(du2, dps, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)

# proportion 
du2[is.na(N_at_nest1), N_at_nest1 := 0]
du2[, at_nest1_prop := N_at_nest1 / N]
du2[is.na(N_at_nest1_int), N_at_nest1_int := 0]
du2[, at_nest1_int_prop := N_at_nest1_int / N]
du2[is.na(N_at_nest1_no_int), N_at_nest1_no_int := 0]
du2[, N_at_nest1_no_int_prop := N_at_nest1_no_int / N]

# merge for boxplot
dup = rbind(du2[, .(type = 'at nest', N_type = at_nest1_prop, Np, pairID, nestID, datetime_rel_pair0)],
            du2[, .(type = 'at nest with female', N_type = at_nest1_int_prop, Np, pairID, nestID, datetime_rel_pair0 )])
dup = dup[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10]


### plot males at nest and males at the nest with female
pc = 
ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = dup[Np >= Np_min], 
               aes(datetime_rel_pair0, N_type, 
                   group = interaction(type, datetime_rel_pair0), color = type),
               lwd = 0.4, outlier.size = 0.7) +
  scale_color_manual(values = c('darkorange2', 'firebrick3'), name = '', 
                     labels = c('Total', 'With female')) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.1, 0.93), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion ot time at nest') +
  xlab('Day relative to clutch initiation (= 0)')

pc 


# merge plots
pa + pb + pc +
  plot_layout(nrow = 3) +
  plot_layout(heights = c(1, 4, 4)) +
  plot_annotation(tag_levels = 'A')


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/Figure_MateGuarding_NestAttendence_Np_0.tiff', plot = last_plot(),  width = 180, height = 240, units = c('mm'), dpi = 'print')
# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/Figure_MateGuarding_NestAttendence_Np_0.25.tiff', plot = last_plot(),  width = 180, height = 240, units = c('mm'), dpi = 'print')
# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/Figure_MateGuarding_NestAttendence_Np_0.5.tiff', plot = last_plot(),  width = 180, height = 240, units = c('mm'), dpi = 'print')

#--------------------------------------------------------------------------------------------------------------
#' Correlation time at the nest with mate guarding
#--------------------------------------------------------------------------------------------------------------

# subset data
dx = merge(du[type == 'breeding pair', .(pairID, nestID, datetime_rel_pair0, int_prop)], 
           du2[type == 'breeding pair', .(pairID, nestID, datetime_rel_pair0, at_nest1_prop, N_at_nest1_no_int_prop)], 
           by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)

dx = dx[datetime_rel_pair0 > -3 & datetime_rel_pair0 < 4]



### MODEL time at the nest and mate guarding
fm <- glmmTMB(int_prop ~ at_nest1_prop * datetime_rel_pair0 +
                 (1 | nestID),
               family = gaussian, data = dx,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm)

plot(allEffects(fm))

# predict data
e <- allEffects(fm, xlevels = 100)$"at_nest1_prop" |>
  data.frame() |>
  setDT()





### single lm's
# relative date -2
dx_2 = dx[datetime_rel_pair0 == -2]
fm_2 = lm(int_prop ~ at_nest1_prop, data = dx_2)

summary(fm_2)

par(mfrow = c(2, 2))
plot(fm_2)
par(mfrow = c(1, 1))

plot(allEffects(fm_2))

# relative date -1
dx_1 = dx[datetime_rel_pair0 == -1]
fm_1 = lm(int_prop ~ at_nest1_prop, data = dx_1)

summary(fm_1)

par(mfrow = c(2, 2))
plot(fm_1)
par(mfrow = c(1, 1))

plot(allEffects(fm_1))

# relative date 0
dx0 = dx[datetime_rel_pair0 == 0]
fm0 = lm(int_prop ~ at_nest1_prop, data = dx0)

summary(fm0)

par(mfrow = c(2, 2))
plot(fm0)
par(mfrow = c(1, 1))

plot(allEffects(fm0))

# relative date 1
dx1 = dx[datetime_rel_pair0 == 1]
fm1 = lm(int_prop ~ at_nest1_prop, data = dx1)

summary(fm1)

par(mfrow = c(2, 2))
plot(fm1)
par(mfrow = c(1, 1))

plot(allEffects(fm1))

# relative date 2
dx2 = dx[datetime_rel_pair0 == 2]
fm2 = lm(int_prop ~ at_nest1_prop, data = dx2)

summary(fm2)

par(mfrow = c(2, 2))
plot(fm2)
par(mfrow = c(1, 1))

plot(allEffects(fm2))

# relative date 3
dx3 = dx[datetime_rel_pair0 == 3]
fm3 = lm(int_prop ~ at_nest1_prop, data = dx3)

summary(fm3)

par(mfrow = c(2, 2))
plot(fm3)
par(mfrow = c(1, 1))

plot(allEffects(fm3))


# plot 
ggplot(data = dx) +
  geom_point(aes(at_nest1_prop, int_prop, 
                 color = as.factor(datetime_rel_pair0))) +
  geom_smooth(aes(at_nest1_prop,int_prop, 
                  color = as.factor(datetime_rel_pair0), fill = as.factor(datetime_rel_pair0)), method = 'lm', alpha = 0.2) +
  scale_colour_manual(name = 'Egg date',
                      values=c('firebrick4', 'firebrick2', 'tomato', 'steelblue1', 'steelblue3', 'dodgerblue4')) +
  scale_fill_manual(name = 'Egg date',
                    values=c('firebrick4', 'firebrick2', 'tomato', 'steelblue1', 'steelblue3', 'dodgerblue4')) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0')) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0')) +
  # guides(color = guide_legend('Egg date ')) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.93, 0.85), legend.background = element_blank()) +
  ylab('Proportion of time pair together') +
  xlab('Proportion of time male at the nest')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_male_at_nest_cor.tiff', plot = last_plot(),  width = 190, height = 190, units = c('mm'), dpi = 'print')



### MODEL time at the nest and mate guarding
fm <- glmmTMB(int_prop ~ N_at_nest1_no_int_prop * datetime_rel_pair0 +
                (1 | nestID),
              family = gaussian, data = dx,
              REML = FALSE,
              control = glmmTMBControl(parallel = 15)
)

summary(fm)

plot(allEffects(fm))

# predict data
e <- allEffects(fm, xlevels = 100)$"N_at_nest1_no_int_prop" |>
  data.frame() |>
  setDT()



# plot for males alone at the nest
ggplot(data = dx) +
  geom_point(aes(N_at_nest1_no_int_prop, int_prop, 
                 color = as.factor(datetime_rel_pair0))) +
  geom_smooth(aes(N_at_nest1_no_int_prop,int_prop, 
                  color = as.factor(datetime_rel_pair0), fill = as.factor(datetime_rel_pair0)), method = 'lm', alpha = 0.2) +
  scale_colour_manual(name = 'Egg date',
                      values=c('firebrick4', 'firebrick2', 'tomato', 'steelblue1', 'steelblue3', 'dodgerblue4')) +
  scale_fill_manual(name = 'Egg date',
                    values=c('firebrick4', 'firebrick2', 'tomato', 'steelblue1', 'steelblue3', 'dodgerblue4')) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0')) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0')) +
  # guides(color = guide_legend('Egg date ')) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.93, 0.85), legend.background = element_blank()) +
  ylab('Proportion of time pair together') +
  xlab('Proportion of time male alone at the nest')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_male_at_nest_cor_male_alone.tiff', plot = last_plot(),  width = 190, height = 190, units = c('mm'), dpi = 'print')

#--------------------------------------------------------------------------------------------------------------
#' Mate guarding intensity lower in pairs with EPY sired?
#--------------------------------------------------------------------------------------------------------------

# subset data
dx = du[!is.na(any_EPY) & type == 'breeding pair' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2]

### MODEL breeding pairs with and without EPY

# subset data for model
dm = dp[!is.na(any_EPY) & datetime_rel_pair >= -5 & datetime_rel_pair <= 2]

# relative time in seconds
dm[, datetime_rel_pair_sec := datetime_rel_pair * 3600 * 24]

# sin and cos of datetime
dm[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
dm[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]

fm1 <- glmmTMB(interaction ~ datetime_rel_pair_sec + any_EPY +
                 scale(sin_time) + scale(cos_time) +
                 (1 + datetime_rel_pair | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm1)


plot(allEffects(fm1))

# predict data
e1 <- allEffects(fm1, xlevels = 100)$"datetime_rel_pair_sec" |>
  data.frame() |>
  setDT()


# pairwise sample size
dss = merge(dx[any_EPY == FALSE, .(N_no_EPY = .N), by = datetime_rel_pair0], 
            dx[any_EPY == TRUE, .(N_EPY = .N), by = datetime_rel_pair0])

dss[, N_label := paste0(N_no_EPY, '/', N_EPY)]



# EPP in clutch of female
ggplot(data = dx) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  geom_path(data = dx[any_EPY == TRUE], aes(datetime_rel_pair0 + 0.2, int_prop), color = 'dodgerblue3', alpha = 0.5) +
  geom_boxplot(aes(datetime_rel_pair0, int_prop, color = any_EPY, 
                   group = interaction(datetime_rel_pair0, any_EPY))) +
  geom_smooth(aes(datetime_rel_pair0, int_prop, group = any_EPY, color = any_EPY, fill = any_EPY), alpha = 0.2) +
  geom_text(data = dss, aes(datetime_rel_pair0, 1.02, label = N_label),
            position = position_dodge(width = 0.9), vjust = 1, size = 3) +
  scale_color_manual(values = c('darkorange', 'dodgerblue3'), name = '', labels = c('No EPY', 'EPY')) +
  scale_fill_manual(values = c('darkorange', 'dodgerblue3'), name = '', labels = c('No EPY', 'EPY')) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  coord_cartesian(xlim = c(-5.4, 2.4), ylim = c(0, 1), expand = FALSE, clip = 'off') +
  scale_x_continuous(breaks = seq(-5, 2, 1), 
                     labels = c('', '-4', '', '2', '', '0', '', '2')) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0')) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.11, 0.11), legend.background = element_blank(), plot.margin = unit(c(15, 4, 2, 0), 'pt')) +
  ylab('Proportion ot time together') +
  xlab('Day relative to clutch initiation (= 0)')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_female_with_EPY.tiff', plot = last_plot(),  width = 180, height = 180, units = c('mm'), dpi = 'print')






