#==============================================================================================================
# Reply to Mihai
#==============================================================================================================

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
margin_ = unit(c(4, 4, 4, 4), 'pt')

#--------------------------------------------------------------------------------------------------------------
#' M1 Paired male and female 
#--------------------------------------------------------------------------------------------------------------

### PLOT

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

### MODEL1

# subset data for model
dm = dp[Np >= Np_min & datetime_rel_pair >= -10 & datetime_rel_pair <= 10]

# factor year
dm[, year_ := factor(year_)]

# sin and cos of datetime
dm[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
dm[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]

fm1 <- glmmTMB(interaction ~ poly(datetime_rel_pair, 2) + year_ +
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

# factor year
dmr[, year_ := factor(year_)]

# sin and cos of datetime
dmr[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
dmr[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]

fm2 <- glmmTMB(interaction ~ poly(datetime_rel_pair, 2) + year_ +
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

### plot proportion of time together 
p = 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[Np >= Np_min], 
               aes(datetime_rel_pair0, int_prop, color = type,  
                   group = interaction(type, datetime_rel_pair0)), 
               lwd = 0.4, outlier.size = 0.7) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '', 
                     labels = c('Breeding pair', 'Male-female pair')) +
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair), size = 0.8, color = 'firebrick3') +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper), 
              fill = 'firebrick3', alpha = 0.2) +
  geom_line(data = er, aes(y = fit, x = datetime_rel_pair), size = 0.8, color = 'dodgerblue4') +
  geom_ribbon(data = er, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper), 
              fill = 'dodgerblue4', alpha = 0.2) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)') +
  ggtitle("Model 1 as in Figure 1B")

p


### MODEL1 with separation in initiated yes and no

dm[, initiated := fifelse(datetime_rel_pair < 0, "no", "yes")]


fm1 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm1)




e = allEffects(fm1, xlevels = 100)$"scale(datetime_rel_pair):initiated" |>
  data.frame() |>
  setDT()

# predictions are made for the entire range of the data, subset to the relevant interval
e = e[(initiated == "no" & datetime_rel_pair < 0) | (initiated == "yes" & datetime_rel_pair > 0)]

p = 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[Np >= Np_min], 
               aes(datetime_rel_pair0, int_prop, color = type,  
                   group = interaction(type, datetime_rel_pair0)), 
               lwd = 0.4, outlier.size = 0.7) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '', 
                     labels = c('Breeding pair', 'Male-female pair')) +
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair), size = 0.8, color = 'firebrick3') +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper), 
              fill = 'firebrick3', alpha = 0.2) +
  geom_line(data = er, aes(y = fit, x = datetime_rel_pair), size = 0.8, color = 'dodgerblue4') +
  geom_ribbon(data = er, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper), 
              fill = 'dodgerblue4', alpha = 0.2) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)') +
  ggtitle("Model 1 split in initiated (yes/no)")

p


#--------------------------------------------------------------------------------------------------------------
#' M2 & M3 Focal bird with opposite sex
#--------------------------------------------------------------------------------------------------------------

# M2 = male 
# M3 = female

# MODEL2 - Male interacting with other females
fm2 <- glmmTMB(ID1_any_ep_int ~ poly(datetime_rel_pair, 2) + year_ +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm2)

# predict data
e2 <- allEffects(fm2, xlevels = 100)$"poly(datetime_rel_pair,2)" |>
  data.frame() |>
  setDT()

# MODEL3 - Female interacting with other males
fm3 <- glmmTMB(ID2_any_ep_int ~ poly(datetime_rel_pair, 2) + year_ +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm3)

# predict data
e3 <- allEffects(fm3, xlevels = 100)$"poly(datetime_rel_pair,2)" |>
  data.frame() |>
  setDT()

# Plot for males and females probability to interact with other opposite sex individuals
e2[, type := 'Male']
e3[, type := 'Female']

eb = rbind(e2, e3)

p = 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '', 
                     labels = c('Female', 'Male')) +
  scale_fill_manual(values = c('firebrick3', 'dodgerblue4'), name = '', 
                     labels = c('Female', 'Male')) +
  geom_line(data = eb, aes(y = fit, x = datetime_rel_pair, color = type), size = 0.8) +
  geom_ribbon(data = eb, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper, fill = type), 
              alpha = 0.2) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Probability of extra-pair interactions') +
  xlab('Day relative to clutch initiation (= 0)') +
  ggtitle("Model 2 & 3 EP interactions as tried in draft")

p


### MODEL2 & 3 with separation in initiated yes and no

# MODEL2 - Male interacting with other females
fm2 <- glmmTMB(ID1_any_ep_int ~ scale(datetime_rel_pair) * initiated + year_ +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm2)

e = allEffects(fm2, xlevels = 100)$"scale(datetime_rel_pair):initiated" |>
  data.frame() |>
  setDT()

# predictions are made for the entire range of the data, subset to the relevant interval
e = e[(initiated == "no" & datetime_rel_pair < 0) | (initiated == "yes" & datetime_rel_pair > 0)]

p = 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  scale_color_manual(values = c('darkorange', 'darkgreen'), name = '', 
                     labels = c('pre initiation', 'post initiation')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen'), name = '', 
                    labels = c('pre initiation', 'post initiation')) +
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair, color = initiated), size = 0.8) +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper, fill = initiated), 
              alpha = 0.2) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Probability of extra-pair interactions') +
  xlab('Day relative to clutch initiation (= 0)') +
  ggtitle("Model 2 - Males EP interactions - split in initiated (yes/no)")

p


# MODEL3 - Fale interacting with other males
fm3 <- glmmTMB(ID2_any_ep_int ~ scale(datetime_rel_pair) * initiated + year_ +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm3)

e = allEffects(fm3, xlevels = 100)$"scale(datetime_rel_pair):initiated" |>
  data.frame() |>
  setDT()

# predictions are made for the entire range of the data, subset to the relevant interval
e = e[(initiated == "no" & datetime_rel_pair < 0) | (initiated == "yes" & datetime_rel_pair > 0)]

p = 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  scale_color_manual(values = c('darkorange', 'darkgreen'), name = '', 
                     labels = c('pre initiation', 'post initiation')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen'), name = '', 
                    labels = c('pre initiation', 'post initiation')) +
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair, color = initiated), size = 0.8) +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper, fill = initiated), 
              alpha = 0.2) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Probability of extra-pair interactions') +
  xlab('Day relative to clutch initiation (= 0)') +
  ggtitle("Model 3 - Females EP interactions - split in initiated (yes/no)")

p

















# predict data
e2 <- allEffects(fm2, xlevels = 100)$"poly(datetime_rel_pair,2)" |>
  data.frame() |>
  setDT()

# MODEL3 - Female interacting with other males
fm3 <- glmmTMB(ID2_any_ep_int ~ poly(datetime_rel_pair, 2) + year_ +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm3)

# predict data
e3 <- allEffects(fm3, xlevels = 100)$"poly(datetime_rel_pair,2)" |>
  data.frame() |>
  setDT()

# Plot for males and females probability to interact with other opposite sex individuals
e2[, type := 'Male']
e3[, type := 'Female']

eb = rbind(e2, e3)

p = 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '', 
                     labels = c('Female', 'Male')) +
  scale_fill_manual(values = c('firebrick3', 'dodgerblue4'), name = '', 
                    labels = c('Female', 'Male')) +
  geom_line(data = eb, aes(y = fit, x = datetime_rel_pair, color = type), size = 0.8) +
  geom_ribbon(data = eb, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper, fill = type), 
              alpha = 0.2) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Probability of extra-pair interactions') +
  xlab('Day relative to clutch initiation (= 0)')

p





















dm[, interaction := factor(interaction)]






























e = allEffects(fm1, xlevels = 100)$"poly(datetime_rel_pair,2):interaction" |>
  data.frame() |>
  setDT()

# predictions are made for the entire range of the data, subset to the relevant interval
e = e[(interaction == FALSE & datetime_rel_pair < 0) | (interaction == TRUE & datetime_rel_pair > 0)]


ggplot(e, aes(y = fit, x = datetime_rel_pair, color = interaction, fill = interaction)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  ylab("Probability of interaction") +
  xlab("Date [0 = nest initiation date]") +
  ggtitle("Breeding pairs")


fm1 <- glmmTMB(ID2_any_ep_int ~ poly(datetime_rel_pair, 2) * interaction + year_ +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm1)
plot(allEffects(fm1))

# predict data
e = allEffects(fm1, xlevels = 100)$"poly(datetime_rel_pair,2):interaction" |>
  data.frame() |>
  setDT()


ggplot(e, aes(y = fit, x = datetime_rel_pair, color = interaction, fill = interaction)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  ylab("Probability of interaction") +
  xlab("Date [0 = nest initiation date]") +
  ggtitle("Breeding pairs")




























ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '', 
                     labels = c('Breeding pair', 'Male-female pair')) +
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair), size = 0.8, color = 'firebrick3') +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper), 
              fill = 'firebrick3', alpha = 0.2) 


# plot data
dm[, ID1_any_ep_int_binary := ifelse(ID1_any_ep_int == TRUE, 1, 0)]
dm[, ID2_any_ep_int_binary := ifelse(ID2_any_ep_int == TRUE, 1, 0)]

# male
pa = 
  ggplot(data = dm) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  geom_smooth(aes(datetime_rel_pair, ID1_any_ep_int_binary, group = interaction, color = interaction, fill = interaction), alpha = 0.2) +
  geom_smooth(aes(datetime_rel_pair, ID1_any_ep_int_binary), alpha = 0.2, color = 'black', fill = 'black') +
  scale_color_manual(values = c('dodgerblue4', 'firebrick3'), name = '', labels = c('without mate', 'with mate')) +
  scale_fill_manual(values = c('dodgerblue4', 'firebrick3'), name = '', labels = c('without mate', 'with mate')) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.85, 0.9), legend.background = element_blank(), plot.margin = unit(c(15, 4, 2, 0), 'pt')) +
  ylab('Male probability of extra-pair interaction') +
  xlab('')



# MODEL2 with separation in initiated yes and no
fm1 <- glmmTMB(ID1_any_ep_int ~ scale(datetime_rel_pair) * initiated + year_ +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm1)
plot(allEffects(fm1))


e = allEffects(fm1, xlevels = 100)$"scale(datetime_rel_pair):initiated" |>
  data.frame() |>
  setDT()

# predictions are made for the entire range of the data, subset to the relevant interval
e = e[(initiated == "no" & datetime_rel_pair < 0) | (initiated == "yes" & datetime_rel_pair > 0)]


ggplot(e, aes(y = fit, x = datetime_rel_pair, color = initiated, fill = initiated)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  ylab("Probability of interaction") +
  xlab("Date [0 = nest initiation date]") +
  ggtitle("Breeding pairs")






