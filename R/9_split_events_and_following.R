#==============================================================================================================
# Split events and following
#==============================================================================================================

# Summary

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
dID = fread('./DATA/NANO_TAGS_UNIQUE_BY_DAY.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
da = fread('./DATA/PAIR_WISE_INTERACTIONS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
da[, year_ := year(datetime_1)]
dp  = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table

# subset data for models
dm = dp[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10]

dIDs = unique(dID[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
              by = c('sex', 'nestID', 'datetime_rel_pair0'))
dIDs = dIDs[, .N, by = .(sex, datetime_rel_pair0, both_tagged_overlapping)]
dIDs

# pairwise sample size
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss = unique(du[datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss = dss[, .N, by = datetime_rel_pair0]
dss

# plot settings
margin_ = unit(c(0, 4, 0, 0), 'pt')

#--------------------------------------------------------------------------------------------------------------
#' Define ID flying away and joining
#--------------------------------------------------------------------------------------------------------------

# subset events
das = da[split == TRUE | merge == TRUE]

# ID flying further is the one splitting or merging
das[split == TRUE, ID_splitting := ifelse(distance1_before > distance2_before, 'ID1', 'ID2')]
das[merge == TRUE, ID_merging := ifelse(distance1_before > distance2_before, 'ID1', 'ID2')]

# merge with dm
dm = merge(dm, das[, .(pairID, year_, datetime_1, datetime_2, ID_splitting, ID_merging, distance1_before, distance2_before)], 
           by = c('pairID', 'year_', 'datetime_1', 'datetime_2'), all.x = TRUE)

dm[split == TRUE, N_splits := .N, by = .(pairID, nestID, datetime_rel_pair0)]
dm[, N_splits := min(N_splits, na.rm = TRUE), by = .(pairID, nestID, datetime_rel_pair0)]

# subset events
das = da[merge == TRUE]

#--------------------------------------------------------------------------------------------------------------
#' Proportions split events by which sex
#--------------------------------------------------------------------------------------------------------------

# Male and female together
dms = dm[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]
d0 = copy(du)

# Proportion of split events
dms = dm[split == TRUE, .(N_split = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_split), N_split := 0]
du[, split_prop := N_split / N]
d1 = copy(du)

# Times male split
dms = dm[split == TRUE & ID_splitting == 'ID1', .(N_m_split = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_m_split), N_m_split := 0]
du[, m_split_prop := N_m_split / N]
d2 = copy(du)

# Times females split
dms = dm[split == TRUE & ID_splitting == 'ID2', .(N_f_split = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_f_split), N_f_split := 0]
du[, f_split_prop := N_f_split / N]
d3 = copy(du)

# merge data
du = rbindlist(list(d0[, .(pairID, nestID, datetime_rel_pair0, prop = int_prop, type = 'm_f_together')],
                    d1[, .(pairID, nestID, datetime_rel_pair0, prop = split_prop, type = 'split_prop')],
                    d2[, .(pairID, nestID, datetime_rel_pair0, prop = m_split_prop, type = 'm_split_prop')],
                    d3[, .(pairID, nestID, datetime_rel_pair0, prop = f_split_prop, type = 'f_split_prop')]
                    ))


# Males and females splitting
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[type == 'm_f_together' | type == 'split_prop'], 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'm_f_together' | type == 'split_prop'], 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), position=position_jitterdodge(), size = 0.5) +
  scale_color_manual(values = c('yellowgreen', 'dodgerblue4'), name = '', 
                     labels = c('with partner', 'split event'), drop = FALSE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.92), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time') +
  xlab('Day relative to clutch initiation (= 0)')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_split_events.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')

ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = -0.01, ymax = 0.5), fill = 'grey90') +
  geom_boxplot(data = du[type == 'm_split_prop' | type == 'f_split_prop'], 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'm_split_prop' | type == 'f_split_prop'], 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), position=position_jitterdodge(), size = 0.5) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '',
                     labels = c('Female moves away', 'Male moves away'), drop = FALSE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  # scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2),
  #                    labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
  #                    expand = expansion(add = c(0, 0.05))) +
  scale_y_continuous(limits = c(-0.01, 0.51), breaks = seq(0, 0.5, 0.2),
                     labels = c('0.0', '0.2', '0.4'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.92), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time') +
  xlab('Day relative to clutch initiation (= 0)')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_split_events_assigned.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')

#--------------------------------------------------------------------------------------------------------------
#' Distance moved away
#--------------------------------------------------------------------------------------------------------------


dms = dm[split == TRUE]

# merge male and female data for plot
dms_m = dms[ID_splitting == 'ID1', .(pairID, nestID, datetime_rel_pair0, sex = sex1, split_distance = distance1_before)]
dms_f = dms[ID_splitting == 'ID2', .(pairID, nestID, datetime_rel_pair0, sex = sex2, split_distance = distance2_before)]

dms = rbindlist(list(dms_m, dms_f))


ggplot() +
  geom_boxplot(data = dms, 
               aes(datetime_rel_pair0, split_distance, group = interaction(datetime_rel_pair0, sex), color = sex),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = dms, 
             aes(datetime_rel_pair0, split_distance, group = interaction(datetime_rel_pair0, sex), color = sex), position=position_jitterdodge(), size = 0.5) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '',
                     labels = c('Female moves away', 'Male moves away'), drop = FALSE) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.92), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Distance moved when split (m)') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_split_events_distance_moved.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')


ggplot() +
  geom_boxplot(data = dms, 
               aes(datetime_rel_pair0, split_distance, group = interaction(datetime_rel_pair0, sex), color = sex),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = dms, 
             aes(datetime_rel_pair0, split_distance, group = interaction(datetime_rel_pair0, sex), color = sex), position=position_jitterdodge(), size = 0.5) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '',
                     labels = c('Female moves away', 'Male moves away'), drop = FALSE) +
  coord_cartesian(ylim = c(0, 600))  +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.92), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Distance moved when split (m)') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_split_events_distance_moved_zoom.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')




dms = dm[merge == TRUE]

# merge male and female data for plot
dms_m = dms[ID_merging == 'ID1', .(pairID, nestID, datetime_rel_pair0, sex = sex1, merge_distance = distance1_before)]
dms_f = dms[ID_merging == 'ID2', .(pairID, nestID, datetime_rel_pair0, sex = sex2, merge_distance = distance2_before)]

dms = rbindlist(list(dms_m, dms_f))


ggplot() +
  geom_boxplot(data = dms, 
               aes(datetime_rel_pair0, merge_distance, group = interaction(datetime_rel_pair0, sex), color = sex),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = dms, 
             aes(datetime_rel_pair0, merge_distance, group = interaction(datetime_rel_pair0, sex), color = sex), position=position_jitterdodge(), size = 0.5) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '',
                     labels = c('Female moves towards', 'Male moves towards'), drop = FALSE) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.92), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Distance moved when split (m)') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_merge_events_distance_moved.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')


ggplot() +
  geom_boxplot(data = dms, 
               aes(datetime_rel_pair0, merge_distance, group = interaction(datetime_rel_pair0, sex), color = sex),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = dms, 
             aes(datetime_rel_pair0, merge_distance, group = interaction(datetime_rel_pair0, sex), color = sex), position=position_jitterdodge(), size = 0.5) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '',
                     labels = c('Female moves towards', 'Male moves towards'), drop = FALSE) +
  coord_cartesian(ylim = c(0, 600))  +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.92), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Distance moved when split (m)') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_merge_events_distance_moved_zoom.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')










