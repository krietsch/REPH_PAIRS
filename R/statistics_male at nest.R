#==============================================================================================================
# Proportion of the day at the nest
#==============================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'foreach', 'knitr',  'stringr', 
          'doFuture', 'patchwork', 'activity', 'glmmTMB', 'effects', 'performance', 'broomExtra',
          'flextable', 'officer', 'dplyr'), 
        require, character.only = TRUE)

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/Reply_to_Mihai.R', output_dir = './OUTPUTS/R_COMPILED')

# Data
dID = fread('./DATA/NANO_TAGS_UNIQUE_BY_DAY.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dp  = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dr  = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS_RANDOM.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table

# subset data for models
dm = dp[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 5]

# sin and cos of datetime
dm[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
dm[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]

# assign categories relative to clutch initiation
dm[, initiated_minus3 := fifelse(datetime_rel_pair0 < -3, "no", "yes")]
dm[, initiated_minus2 := fifelse(datetime_rel_pair0 < -2, "no", "yes")]
dm[, initiated_minus1 := fifelse(datetime_rel_pair0 < -1, "no", "yes")]
dm[, initiated        := fifelse(datetime_rel_pair0 < 0, "no", "yes")]
dm[, initiated_plus1  := fifelse(datetime_rel_pair0 < 1, "no", "yes")]
dm[, initiated_plus2  := fifelse(datetime_rel_pair0 < 2, "no", "yes")]
dm[, initiated_plus3  := fifelse(datetime_rel_pair0 < 3, "no", "yes")]

# plot settings
margin_ = unit(c(4, 4, 4, 4), 'pt')

#--------------------------------------------------------------------------------------------------------------
#' # Proportions of the day being at the nest
#--------------------------------------------------------------------------------------------------------------

# assign parameters
dm[, both_at_nest := at_nest1 == TRUE & at_nest2 == TRUE]
dm[, m_alone_at_nest := at_nest1 == TRUE & interaction == FALSE]
dm[, f_alone_at_nest := at_nest2 == TRUE & interaction == FALSE]

# Proportion of time males at nest
dms = dm[at_nest1 == TRUE, .(N_m_at_nest = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_m_at_nest), N_m_at_nest := 0]
du[, m_at_nest_prop := N_m_at_nest / N]
d1 = copy(du)

# Proportion of time females at nest
dms = dm[at_nest2 == TRUE, .(N_f_at_nest = .N), by = .(pairID, nestID, datetime_rel_pair0)]
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

# merge data
du = merge(d1, d2[, .(pairID, nestID, datetime_rel_pair0, f_at_nest_prop)], by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du = merge(du, d3[, .(pairID, nestID, datetime_rel_pair0, both_at_nest_prop)], by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du = merge(du, d4[, .(pairID, nestID, datetime_rel_pair0, m_alone_at_nest_prop)], by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du = merge(du, d5[, .(pairID, nestID, datetime_rel_pair0, f_alone_at_nest_prop)], by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)

du = rbindlist(list(d1[, .(pairID, nestID, datetime_rel_pair0, prop = m_at_nest_prop, type = 'm_at_nest_prop')],
                    d2[, .(pairID, nestID, datetime_rel_pair0, prop = f_at_nest_prop, type = 'f_at_nest_prop')],
                    d3[, .(pairID, nestID, datetime_rel_pair0, prop = both_at_nest_prop, type = 'both_at_nest_prop')],
                    d4[, .(pairID, nestID, datetime_rel_pair0, prop = m_alone_at_nest_prop, type = 'm_alone_at_nest_prop')],
                    d5[, .(pairID, nestID, datetime_rel_pair0, prop = f_alone_at_nest_prop, type = 'f_alone_at_nest_prop')]
                    ))

ggplot() +
  geom_boxplot(data = du, 
               aes(as.factor(datetime_rel_pair0), prop, color = type),
               lwd = 0.4, outlier.size = 0.7)

#--------------------------------------------------------------------------------------------------------------
#' # Model probability of male being at the nest
#--------------------------------------------------------------------------------------------------------------

# male at the nest
m_mn <- glmmTMB(at_nest1 ~ scale(datetime_rel_pair) + 
                   scale(sin_time) + scale(cos_time) +
                   (1 + poly(datetime_rel_pair, 2) | nestID),
                 family = binomial, data = dm,
                 REML = FALSE,
                 control = glmmTMBControl(parallel = 15)
)

summary(m_mn)

m_mn2 <- glmmTMB(at_nest1 ~ poly(datetime_rel_pair, 2) + 
                  scale(sin_time) + scale(cos_time) +
                  (1 + poly(datetime_rel_pair, 2) | nestID),
                family = binomial, data = dm,
                REML = FALSE,
                control = glmmTMBControl(parallel = 15)
)

summary(m_mn2)

m_mn3 <- glmmTMB(at_nest1 ~ poly(datetime_rel_pair, 2) + 
                   scale(sin_time) + scale(cos_time) +
                   (1 | nestID),
                 family = binomial, data = dm,
                 REML = FALSE,
                 control = glmmTMBControl(parallel = 15)
)

summary(m_mn3)

MuMIn::model.sel(m_mn, m_mn2, m_mn3)
# better with polynomial (comp. with 3 not meaningful - different random effects structure)

# look at plot
e1 = allEffects(m_mn, xlevels = 1000)$"scale(datetime_rel_pair)" |>
  data.frame() |>
  setDT()

e2 = allEffects(m_mn2, xlevels = 1000)$"poly(datetime_rel_pair,2)" |>
  data.frame() |>
  setDT()

e3 = allEffects(m_mn3, xlevels = 1000)$"poly(datetime_rel_pair,2)" |>
  data.frame() |>
  setDT()

e1[, type := 'scaled']
e2[, type := 'poly']
e3[, type := 'poly_nr']

e = rbindlist(list(e1, e2, e3))

# plot 
p = 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[type == 'm_at_nest_prop'], 
               aes(datetime_rel_pair0, prop, group = datetime_rel_pair0),
               lwd = 0.4, outlier.size = 0.7) +
  scale_color_manual(values = c('darkorange', 'darkgreen', 'darkred'), name = '', 
                     labels = c('poly', 'poly_nr', 'scaled')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkred'), name = '', 
                    labels = c('poly', 'poly_nr', 'scaled')) +
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair, color = type), size = 0.8) +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair, fill = type, ymin = lower, ymax = upper), alpha = 0.2) +
  scale_x_continuous(limits = c(-5.5, 5.5), breaks = seq(-5, 5, 1), 
                     labels = c('', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', ''),
                     expand = expansion(add = c(0, 0))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.1, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Probability of mates interacting') +
  xlab('Day relative to clutch initiation (= 0)') +
  ggtitle("")

p


#--------------------------------------------------------------------------------------------------------------
#' # Model probability of female being at the nest
#--------------------------------------------------------------------------------------------------------------

# female at the nest
m_fn <- glmmTMB(at_nest2 ~ scale(datetime_rel_pair) + 
                  scale(sin_time) + scale(cos_time) +
                  (1 + poly(datetime_rel_pair, 2) | nestID),
                family = binomial, data = dm,
                REML = FALSE,
                control = glmmTMBControl(parallel = 15)
)

summary(m_fn)


m_fn2 <- glmmTMB(at_nest2 ~ poly(datetime_rel_pair, 2) + 
                  scale(sin_time) + scale(cos_time) +
                  (1 + poly(datetime_rel_pair, 2) | nestID),
                family = binomial, data = dm,
                REML = FALSE,
                control = glmmTMBControl(parallel = 15)
)

summary(m_fn2)

m_fn3 <- glmmTMB(at_nest2 ~ poly(datetime_rel_pair, 2) + 
                   scale(sin_time) + scale(cos_time) +
                   (1 | nestID),
                 family = binomial, data = dm,
                 REML = FALSE,
                 control = glmmTMBControl(parallel = 15)
)

summary(m_fn3)

MuMIn::model.sel(m_fn, m_fn2, m_fn3)



# look at plot
e1 = allEffects(m_fn, xlevels = 1000)$"scale(datetime_rel_pair)" |>
  data.frame() |>
  setDT()

e2 = allEffects(m_fn2, xlevels = 1000)$"poly(datetime_rel_pair,2)" |>
  data.frame() |>
  setDT()

e3 = allEffects(m_fn3, xlevels = 1000)$"poly(datetime_rel_pair,2)" |>
  data.frame() |>
  setDT()

e1[, type := 'scaled']
e2[, type := 'poly']
e3[, type := 'poly_nr']

e = rbindlist(list(e1, e2, e3))

# plot 
p = 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[type == 'f_at_nest_prop'], 
               aes(datetime_rel_pair0, prop, group = datetime_rel_pair0),
               lwd = 0.4, outlier.size = 0.7) +
  scale_color_manual(values = c('darkorange', 'darkgreen', 'darkred'), name = '', 
                     labels = c('poly', 'poly_nr', 'scaled')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkred'), name = '', 
                    labels = c('poly', 'poly_nr', 'scaled')) +
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair, color = type), size = 0.8) +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair, fill = type, ymin = lower, ymax = upper), alpha = 0.2) +
  scale_x_continuous(limits = c(-5.5, 5.5), breaks = seq(-5, 5, 1), 
                     labels = c('', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', ''),
                     expand = expansion(add = c(0, 0))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.1, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Probability of mates interacting') +
  xlab('Day relative to clutch initiation (= 0)') +
  ggtitle("")

p

















# both at the nest
m_bn <- glmmTMB(both_at_nest ~ poly(datetime_rel_pair, 2) + 
                  scale(sin_time) + scale(cos_time) +
                  (1 + poly(datetime_rel_pair, 2) | nestID),
                family = binomial, data = dm,
                REML = FALSE,
                control = glmmTMBControl(parallel = 15)
)

summary(m_bn)


MuMIn::model.sel(m_mn, m_mn2)


AIC(m_mn2)




plot(allEffects(m_mn3))





# extract model predictions
e = allEffects(m_mn, xlevels = 1000)$"scale(datetime_rel_pair)" |>
  data.frame() |>
  setDT()


e = allEffects(m_bn, xlevels = 1000)$"scale(datetime_rel_pair)" |>
  data.frame() |>
  setDT()

e = allEffects(m_mn2, xlevels = 1000)$"poly(datetime_rel_pair,2)" |>
  data.frame() |>
  setDT()

# plot 
p = 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  scale_color_manual(values = c('darkorange', 'darkgreen'), name = '', 
                     labels = c('pre split', 'post split')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen'), name = '', 
                    labels = c('pre split', 'post split')) +
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair), size = 0.8) +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper), alpha = 0.2) +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1), 
                     labels = c('', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', ''),
                     expand = expansion(add = c(0, 0))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Probability of mates interacting') +
  xlab('Day relative to clutch initiation (= 0)') +
  ggtitle("")

p

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/Figure_Probability_interacting.tiff', plot = last_plot(),  width = 180, height = 180, units = c('mm'), dpi = 'print')














