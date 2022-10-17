#==============================================================================================================
# Figures & Statistics
#==============================================================================================================

# Summary
# 

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
da = fread('./DATA/PAIR_WISE_INTERACTIONS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
da[, year_ := year(datetime_1)]


con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
dn[, nest_state_date := as.POSIXct(nest_state_date, tz = 'UTC')]
DBI::dbDisconnect(con)

# plot settings
# margin_ = unit(c(0, 4, 0, 0), 'pt')
margin_ = unit(c(2, 2, 2, 2), 'pt')

# Assign polyandry
# polyandrous females
xf = c(270170564, 270170901, 270170935, 273145005, 273145036, 273145109, 273145121, 273145140)
dp[ID2 %in% xf, f_polyandrous := TRUE]
dp[is.na(f_polyandrous), f_polyandrous := FALSE]

# first nests
x1 = c("R606_19", "R805_18", "R311_19", "R206_19", "R405_19", "R904_18", "R211_19", "R901_19", "R207_19", "REPH050_19")
dp[nestID %in% x1, f_polyandrous_first := TRUE]
dp[is.na(f_polyandrous_first), f_polyandrous_first := FALSE]

# second nest
x2 = c("R222_19", "R806_18", "R210_19", "R902_19", "R406_19", "R907_18", "R217_19", "R220_19", "R911_19", "R907_19")
dp[nestID %in% x2, f_polyandrous_second := TRUE]
dp[is.na(f_polyandrous_second), f_polyandrous_second := FALSE]


# season mean 
di = dn[!is.na(year_) & plot == 'NARL', .(initiation_mean = mean(initiation, na.rm = TRUE)), by = year_]

dp = merge(dp, di, by = 'year_', all.x = TRUE)

# merge with nests
dp = merge(dp, dnID[, .(male_id, female_id, year_, nestID, initiation)], by.x = c('ID1', 'ID2', 'year_', 'nestID'), 
           by.y = c('male_id', 'female_id', 'year_', 'nestID'), all.x = TRUE)

# relative initiation date
dp[, initiation_rel := difftime(initiation, initiation_mean, units = 'days') %>% as.numeric %>% round(., 0)]

# early and late clutches?
dp[initiation_rel < -2, initiation_type := 'early']
dp[initiation_rel > 2, initiation_type := 'late']
dp[!is.na(initiation) & is.na(initiation_type), initiation_type := 'peak']

# datetime relative to nest initiation date
dp[, datetime_rel_initiation := difftime(datetime_1, initiation, units = 'days') %>% as.numeric()]

# subset data 10 days around clutch initiation
dm = dp[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10]
dmr = dr[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10]


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

# Male and female together
dms = dm[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]
d0 = copy(du)


# Proportion of ep interactions males
dms = dmr[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dmr, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]
d1 = copy(du)


# merge data
du = rbindlist(list(d0[, .(pairID, nestID, datetime_rel_pair0, prop = int_prop, Np, type = 'm_f_together')],
                    d1[, .(pairID, nestID, datetime_rel_pair0, prop = int_prop, Np, type = 'm_f_together_randomized')]
                   ))

# data quality
du[, data_quality := ifelse(Np >= 0.75, '>=0.75', '<0.75')]


# Plot time together breeding pairs vs. random pairs
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du, 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du, 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), # shape = data_quality
             position=position_jitterdodge(), size = 0.2) +
  # scale_shape_manual(values=c(20, 19)) +
  scale_color_manual(values = c('yellowgreen', 'steelblue4'), name = '', 
                     labels = c('breeding pair', 'random pair'), drop = FALSE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.9, 0.8), legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season_null_model.tiff', plot = last_plot(),  width = 177, height = 89, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' Split and merge events
#--------------------------------------------------------------------------------------------------------------

# define who flies away or joins  


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

dm = merge(dm, du[, .(pairID, nestID, datetime_rel_pair0, N_split)], by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)


# Times male split
dms = dm[split == TRUE & ID_splitting == 'ID1', .(N_m_split = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_m_split), N_m_split := 0]
du[, m_split_prop := N_m_split / N_split]
d2 = copy(du)

# Times females split
dms = dm[split == TRUE & ID_splitting == 'ID2', .(N_f_split = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_f_split), N_f_split := 0]
du[, f_split_prop := N_f_split /N_split]
d3 = copy(du)

# merge data
du = rbindlist(list(d0[, .(pairID, nestID, datetime_rel_pair0, prop = int_prop, type = 'm_f_together')],
                    d1[, .(pairID, nestID, datetime_rel_pair0, prop = split_prop, type = 'split_prop')],
                    d2[, .(pairID, nestID, datetime_rel_pair0, prop = m_split_prop, type = 'm_split_prop')],
                    d3[, .(pairID, nestID, datetime_rel_pair0, prop = f_split_prop, type = 'f_split_prop')]
))



ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[type == 'm_split_prop'], 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'm_split_prop'], 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), position=position_jitterdodge(), size = 0.5) +
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


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_split_events_proportion.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')





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
  geom_rect(aes(xmin = 0, xmax = 3, ymin = -0.01, ymax = 0.2), fill = 'grey90') +
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
  scale_y_continuous(limits = c(-0.01, 0.21), breaks = seq(0, 0.2, 0.2),
                     labels = c('0.0', '0.2'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.92), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time') +
  xlab('Day relative to clutch initiation (= 0)')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_split_events_assigned.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')

#--------------------------------------------------------------------------------------------------------------
#' Nest attendance by sex
#--------------------------------------------------------------------------------------------------------------


# assign parameters
dm[, m_at_nest := at_nest1 == TRUE | at_nest2 == TRUE & interaction == TRUE]
dm[, f_at_nest := at_nest2 == TRUE | at_nest1 == TRUE & interaction == TRUE]
dm[, both_at_nest := at_nest1 == TRUE & interaction == TRUE | at_nest2 == TRUE & interaction == TRUE]
dm[, m_alone_at_nest := at_nest1 == TRUE & interaction == FALSE]
dm[, f_alone_at_nest := at_nest2 == TRUE & interaction == FALSE]

# Male and female together
dms = dm[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]
d0 = copy(du)

# Proportion of time males at nest
dms = dm[m_at_nest == TRUE, .(N_m_at_nest = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_m_at_nest), N_m_at_nest := 0]
du[, m_at_nest_prop := N_m_at_nest / N]
d1 = copy(du)

# Proportion of time females at nest
dms = dm[f_at_nest == TRUE, .(N_f_at_nest = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_f_at_nest), N_f_at_nest := 0]
du[, f_at_nest_prop := N_f_at_nest / N]
d2 = copy(du)

# Proportion of time both at nest
dms = dm[both_at_nest == TRUE, .(N_both_at_nest = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_both_at_nest), N_both_at_nest := 0]
du[, both_at_nest_prop := N_both_at_nest / N]
d3 = copy(du)

# Proportion of time male alone at nest
dms = dm[m_alone_at_nest == TRUE, .(N_m_alone_at_nest = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_m_alone_at_nest), N_m_alone_at_nest := 0]
du[, m_alone_at_nest_prop := N_m_alone_at_nest / N]
d4 = copy(du)

# Proportion of time female alone at nest
dms = dm[f_alone_at_nest == TRUE, .(N_f_alone_at_nest = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_f_alone_at_nest), N_f_alone_at_nest := 0]
du[, f_alone_at_nest_prop := N_f_alone_at_nest / N]
d5 = copy(du)

# male alone and not at the nest
d6 = merge(d0[, .(pairID, nestID, datetime_rel_pair0, int_prop)], 
           d4[, .(pairID, nestID, datetime_rel_pair0, m_alone_at_nest_prop)], 
           by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
d6[, m_alone_prop := 1 - c(int_prop + m_alone_at_nest_prop)]

d6[, total := m_alone_prop + int_prop + m_alone_at_nest_prop]

# female alone and not at the nest
d7 = merge(d0[, .(pairID, nestID, datetime_rel_pair0, int_prop)], 
           d5[, .(pairID, nestID, datetime_rel_pair0, f_alone_at_nest_prop)], 
           by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
d7[, f_alone_prop := 1 - c(int_prop + f_alone_at_nest_prop)]

d7[, total := f_alone_prop + int_prop + f_alone_at_nest_prop]



# merge data
du = rbindlist(list(d0[, .(pairID, nestID, datetime_rel_pair0, prop = int_prop, type = 'm_f_together')],
                    d1[, .(pairID, nestID, datetime_rel_pair0, prop = m_at_nest_prop, type = 'm_at_nest_prop')],
                    d2[, .(pairID, nestID, datetime_rel_pair0, prop = f_at_nest_prop, type = 'f_at_nest_prop')],
                    d3[, .(pairID, nestID, datetime_rel_pair0, prop = both_at_nest_prop, type = 'both_at_nest_prop')],
                    d4[, .(pairID, nestID, datetime_rel_pair0, prop = m_alone_at_nest_prop, type = 'm_alone_at_nest_prop')],
                    d5[, .(pairID, nestID, datetime_rel_pair0, prop = f_alone_at_nest_prop, type = 'f_alone_at_nest_prop')],
                    d6[, .(pairID, nestID, datetime_rel_pair0, prop = m_alone_prop, type = 'm_alone_prop')],
                    d7[, .(pairID, nestID, datetime_rel_pair0, prop = f_alone_prop, type = 'f_alone_prop')]
))



# Males and females at the nest
pa = 
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[type == 'm_at_nest_prop' | type == 'f_at_nest_prop'], 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'm_at_nest_prop' | type == 'f_at_nest_prop'], 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '', 
                     labels = c('Female', 'Male'), drop = FALSE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.07, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time at nest') +
  xlab('Day relative to clutch initiation (= 0)')


# Males alone and alone at nest
pb = 
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[type == 'm_alone_prop' | type == 'm_alone_at_nest_prop'], 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'm_alone_prop' | type == 'm_alone_at_nest_prop'], 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('darkgreen', 'darkorange'), name = '', 
                     labels = c('At nest', 'Not at nest'), drop = FALSE) +
  scale_x_continuous(limits = c(-4.4, 4.4), breaks = seq(-5, 5, 1), 
                     labels = c('', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', ''),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.18, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time alone (male)') +
  xlab('Day relative to clutch initiation (= 0)')

# Female alone and alone at nest
pc = 
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[type == 'f_alone_prop' | type == 'f_alone_at_nest_prop'], 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'f_alone_prop' | type == 'f_alone_at_nest_prop'], 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('darkgreen', 'darkorange'), name = '', 
                     labels = c('At nest', 'Not at nest'), drop = FALSE) +
  scale_x_continuous(limits = c(-4.4, 4.4), breaks = seq(-5, 5, 1), 
                     labels = c('', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', ''),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.18, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time alone (female)') +
  xlab('Day relative to clutch initiation (= 0)')


# merge plots
pa + pb + pc +
  plot_layout(design = "
  11
  23
") +
  # plot_layout(heights = c(1, 4, 4)) +
  plot_annotation(tag_levels = 'A')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_alone_at_nest.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' Mate guarding effect on extra-pair paternity
#--------------------------------------------------------------------------------------------------------------

# pairwise sample size
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss = unique(du[any_EPY == FALSE & datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss = dss[, .N, by = datetime_rel_pair0]

# pairwise sample size
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss_epy = unique(du[any_EPY == TRUE & datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss_epy = dss_epy[, .N, by = datetime_rel_pair0]
dss_epy

# merge 
dss = merge(dss, dss_epy[, .(N_epy = N, datetime_rel_pair0)], by = 'datetime_rel_pair0', all.x = TRUE)
dss[, N_epy_label := paste0(N_epy, '/', N)]

# Proportion of time together breeders
dps = dp[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dps, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]

dp[, datetime_rel_pair_min := NULL]

# N 
du[, .N, by = pairID] |> nrow()
du[, .N, by = nestID] |> nrow()

du = du[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10]

# median per day
dmd = du[, .(int_prop_median = median(int_prop)), by = datetime_rel_pair0]

# order
du[, any_EPY_plot := ifelse(any_EPY == TRUE, 'EPY', 'No EPY')]

### plot proportion of time together 

ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N_epy_label), vjust = 1, size = 3) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[!is.na(any_EPY)], 
               aes(datetime_rel_pair0, int_prop, group = interaction(datetime_rel_pair0, any_EPY_plot), color = any_EPY_plot),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[!is.na(any_EPY)], 
             aes(datetime_rel_pair0, int_prop, group = interaction(datetime_rel_pair0, any_EPY_plot), color = any_EPY_plot), 
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('darkgreen', 'darkorange'), name = '', 
                     labels = c('EPY', 'No EPY'), drop = FALSE) +
  scale_x_continuous(limits = c(-4.4, 4.4), breaks = seq(-5, 5, 1), 
                     labels = c('', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', ''),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.18, 0.14), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_together_epy.tiff', plot = last_plot(),  width = 89, height = 89, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' Mate guarding intensity in relation to time within season
#--------------------------------------------------------------------------------------------------------------

# pairwise sample size for each season 
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))

# early
dss_early = unique(du[initiation_type == 'early' & datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss_early = dss_early[, .N, by = datetime_rel_pair0]

# peak
dss_peak = unique(du[initiation_type == 'peak' & datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
                   by = c('nestID', 'datetime_rel_pair0'))
dss_peak = dss_peak[, .N, by = datetime_rel_pair0]

# late 
dss_late = unique(du[initiation_type == 'late' & datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
                  by = c('nestID', 'datetime_rel_pair0'))
dss_late = dss_late[, .N, by = datetime_rel_pair0]

# merge 
dss = merge(dss_early[, .(N_early = N, datetime_rel_pair0)], dss_peak[, .(N_peak = N, datetime_rel_pair0)], 
            by = 'datetime_rel_pair0', all.x = TRUE)

dss = merge(dss, dss_late[, .(N_late = N, datetime_rel_pair0)], 
            by = 'datetime_rel_pair0', all.x = TRUE)

dss[, N_season_label := paste0(N_early, '/', N_peak, '/', N_late)]



# Male and female together
dms = dm[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]



ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N_season_label), vjust = 1, size = 3) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du, 
               aes(datetime_rel_pair0, int_prop, group = interaction(datetime_rel_pair0, initiation_type), color = initiation_type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du, 
             aes(datetime_rel_pair0, int_prop, group = interaction(datetime_rel_pair0, initiation_type), color = initiation_type), 
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue3', 'darkgreen'), name = 'Initiation') +
  scale_x_continuous(limits = c(-4.4, 4.4), breaks = seq(-5, 5, 1), 
                   labels = c('', '-4', '', '-2', '', '0', 
                              '', '2', '', '4', ''),
                   expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.12, 0.17), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_together_season.tiff', plot = last_plot(),  width = 129, height = 89, units = c('mm'), dpi = 'print')


# Differences between years

# pairwise sample size for each season 
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))

# 2018
dss_2018 = unique(du[year_ == 2018 & datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
                   by = c('nestID', 'datetime_rel_pair0'))
dss_2018 = dss_2018[, .N, by = datetime_rel_pair0]

# 2019
dss_2019 = unique(du[year_ == 2019 & datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
                  by = c('nestID', 'datetime_rel_pair0'))
dss_2019 = dss_2019[, .N, by = datetime_rel_pair0]

# merge 
dss = merge(dss_early[, .(N_2018 = N, datetime_rel_pair0)], dss_peak[, .(N_2019 = N, datetime_rel_pair0)], 
            by = 'datetime_rel_pair0', all.x = TRUE)


dss[, N_year_label := paste0(N_2018, '/', N_2019)]


# Male and female together
dms = dm[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]

du[, year_c := as.character(year_)]

# plot by year
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N_year_label), vjust = 1, size = 3) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du, 
               aes(datetime_rel_pair0, int_prop, group = interaction(datetime_rel_pair0, year_c), color = year_c),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du, 
             aes(datetime_rel_pair0, int_prop, group = interaction(datetime_rel_pair0, year_c), color = year_c), 
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue3'), name = '') +
  scale_x_continuous(limits = c(-4.4, 4.4), breaks = seq(-5, 5, 1), 
                     labels = c('', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', ''),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.12, 0.12), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_together_year.tiff', plot = last_plot(),  width = 89, height = 89, units = c('mm'), dpi = 'print')






















#--------------------------------------------------------------------------------------------------------------
#' Mate guarding intensity in relation to breeding state OLD
#--------------------------------------------------------------------------------------------------------------



# Proportion of time together breeders
dps = dp[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dps, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]

dp[, datetime_rel_pair_min := NULL]

# N 
du[, .N, by = pairID] |> nrow()
du[, .N, by = nestID] |> nrow()

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




pb = 
  ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = dr[Np >= Np_min], 
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

p = 
  ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = 3) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = -0.01, ymax = 1), fill = 'grey90') +
  # geom_boxplot(data = du[Np >= Np_min], 
  #              aes(datetime_rel_pair0, int_prop,  
  #                  group = interaction(datetime_rel_pair0)), 
  #              lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_line(data = du, aes(datetime_rel_pair0, int_prop, group = nestID, color = any_EPY), size = 0.7) +
  geom_line(data = dmd, aes(datetime_rel_pair0, int_prop_median), color = 'yellowgreen', size = 1.2) +
  geom_line(data = du[any_EPY == TRUE], aes(datetime_rel_pair0, int_prop, group = nestID), color = 'firebrick4', size = 1) +
  # geom_jitter(data = du[Np >= Np_min], aes(datetime_rel_pair0, int_prop, shape = data_quality, color = any_EPY), size = 1) +
  # scale_shape_manual(values=c(1, 16)) +
  scale_color_manual(values = c('grey75', 'firebrick4', 'grey'), name = '',
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

p 

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season_null_model_50breeders_new_EPY_lines.tiff', plot = last_plot(),  width = 250, height = 120, units = c('mm'), dpi = 'print')


# how many nests with EPY
du[any_EPY == TRUE, .N, by = .(nestID)]


