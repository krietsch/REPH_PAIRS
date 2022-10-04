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

#--------------------------------------------------------------------------------------------------------------
#' Proportions of EP interactions
#--------------------------------------------------------------------------------------------------------------

# assign parameters
dm[, m_ep_int_alone := interaction == FALSE & ID1_any_ep_int == TRUE]
dm[, m_ep_int_mg := interaction == TRUE & ID1_any_ep_int == TRUE]
dm[, f_ep_int_alone := interaction == FALSE & ID2_any_ep_int == TRUE]
dm[, f_ep_int_mg := interaction == TRUE & ID2_any_ep_int == TRUE]

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

# Proportion of ep interactions females
dms = dm[ID2_any_ep_int == TRUE, .(N_f_ep_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_f_ep_int), N_f_ep_int := 0]
du[, f_ep_int_prop := N_f_ep_int / N]
d2 = copy(du)

# Proportion of ep interactions males alone
dms = dm[m_ep_int_alone == TRUE, .(N_m_ep_int_alone = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_m_ep_int_alone), N_m_ep_int_alone := 0]
du[, m_ep_int_alone_prop := N_m_ep_int_alone / N]
d3 = copy(du)

# Proportion of ep interactions males mg
dms = dm[m_ep_int_mg == TRUE, .(N_m_ep_int_mg = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_m_ep_int_mg), N_m_ep_int_mg := 0]
du[, m_ep_int_mg_prop := N_m_ep_int_mg / N]
d4 = copy(du)

# Proportion of ep interactions females alone
dms = dm[f_ep_int_alone == TRUE, .(N_f_ep_int_alone = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_f_ep_int_alone), N_f_ep_int_alone := 0]
du[, f_ep_int_alone_prop := N_f_ep_int_alone / N]
d5 = copy(du)

# Proportion of ep interactions females mg
dms = dm[f_ep_int_mg == TRUE, .(N_f_ep_int_mg = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_f_ep_int_mg), N_f_ep_int_mg := 0]
du[, f_ep_int_mg_prop := N_f_ep_int_mg / N]
d6 = copy(du)

# merge data
du = rbindlist(list(d0[, .(pairID, nestID, datetime_rel_pair0, prop = int_prop, type = 'm_f_together')],
                    d1[, .(pairID, nestID, datetime_rel_pair0, prop = split_prop, type = 'split_prop')],
                    d2[, .(pairID, nestID, datetime_rel_pair0, prop = f_ep_int_prop, type = 'f_ep_int_prop')],
                    d3[, .(pairID, nestID, datetime_rel_pair0, prop = m_ep_int_alone_prop, type = 'm_ep_int_alone_prop')],
                    d4[, .(pairID, nestID, datetime_rel_pair0, prop = m_ep_int_mg_prop, type = 'm_ep_int_mg_prop')],
                    d5[, .(pairID, nestID, datetime_rel_pair0, prop = f_ep_int_alone_prop, type = 'f_ep_int_alone_prop')],
                    d6[, .(pairID, nestID, datetime_rel_pair0, prop = f_ep_int_mg_prop, type = 'f_ep_int_mg_prop')]
))


# Males and females extra-pair interactions
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[type == 'm_f_together' | type == 'split_prop'], 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'm_f_together' | type == 'split_prop'], 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), position=position_jitterdodge(), size = 0.7) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '', 
                     labels = c('Female', 'Male'), drop = FALSE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.92), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of extra-pair interactions') +
  xlab('Day relative to clutch initiation (= 0)')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_ep_int.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')

