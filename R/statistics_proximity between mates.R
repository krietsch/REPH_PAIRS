#==============================================================================================================
# Proximity between mates in relation to breeding phenology
#==============================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'foreach', 'knitr', 
          'stringr', 'doFuture', 'patchwork', 'activity', 'glmmTMB', 'effects'), 
        require, character.only = TRUE)

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/Reply_to_Mihai.R', output_dir = './OUTPUTS/R_COMPILED')

# Data
dID = fread('./DATA/NANO_TAGS_UNIQUE_BY_DAY.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dp  = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dr  = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS_RANDOM.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table

# subset data for models
dm = dp[Np >= Np_min & datetime_rel_pair >= -5 & datetime_rel_pair <= 5]

# factor year
dm[, year_ := factor(year_)]

# sin and cos of datetime
dm[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
dm[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]

# assign categories relative to clutch initiation
dm[, initiated_minus3 := fifelse(datetime_rel_pair < -3, "no", "yes")]
dm[, initiated_minus2 := fifelse(datetime_rel_pair < -2, "no", "yes")]
dm[, initiated_minus1 := fifelse(datetime_rel_pair < -1, "no", "yes")]
dm[, initiated        := fifelse(datetime_rel_pair < 0, "no", "yes")]
dm[, initiated_plus1  := fifelse(datetime_rel_pair < 1, "no", "yes")]
dm[, initiated_plus2  := fifelse(datetime_rel_pair < 2, "no", "yes")]
dm[, initiated_plus3  := fifelse(datetime_rel_pair < 3, "no", "yes")]

# plot settings
margin_ = unit(c(4, 4, 4, 4), 'pt')

#--------------------------------------------------------------------------------------------------------------
#' # Model probability of close proximity for breeding pairs 
#--------------------------------------------------------------------------------------------------------------

#' ### model selection

# three days before clutch initiation
m_3 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_minus3 +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(m_3)

# two days before clutch initiation
m_2 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_minus2 +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

# one day before clutch initiation
m_1 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_minus1 +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(m_1)

# day with clutch initiation
m0 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated +
                scale(sin_time) + scale(cos_time) +
                (1 + poly(datetime_rel_pair, 2) | pairID),
              family = binomial, data = dm,
              REML = FALSE,
              control = glmmTMBControl(parallel = 15)
)

summary(m0)

# one day after clutch initiation
m1 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_plus1 +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(m1)

# two days after clutch initiation
m2 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_plus2 +
                scale(sin_time) + scale(cos_time) +
                (1 + poly(datetime_rel_pair, 2) | pairID),
              family = binomial, data = dm,
              REML = FALSE,
              control = glmmTMBControl(parallel = 15)
)

summary(m2)

# three days after clutch initiation
m3 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_plus3 +
                scale(sin_time) + scale(cos_time) +
                (1 + poly(datetime_rel_pair, 2) | pairID),
              family = binomial, data = dm,
              REML = FALSE,
              control = glmmTMBControl(parallel = 15)
)

summary(m3)


# compare models
MuMIn::model.sel(m_3, m_2, m_1, m0, m1, m2, m3)


#' ### refit best fitting model with REML = TRUE
m_2r <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_minus2 +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)



#' ### plot model with raw data

# Proportion of time together breeders
dps = dp[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dps, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]

# extract model predictions
e = allEffects(m_2r, xlevels = 1000)$"scale(datetime_rel_pair):initiated_minus2" |>
  data.frame() |>
  setDT()

# predictions are made for the entire range of the data, subset to the relevant interval
e = e[(initiated_minus2 == "no" & datetime_rel_pair < -2) | (initiated_minus2 == "yes" & datetime_rel_pair > -2)]

require(ggnewscale)

# plot 
p = 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  geom_path(data = du, aes(y = int_prop, x = datetime_rel_pair0, group = nestID, color = Np), alpha = 0.5) +
  viridis::scale_color_viridis(direction = -1, name = 'N positions') +
  
  new_scale('color') +
  scale_color_manual(values = c('darkorange', 'darkgreen'), name = '', 
                     labels = c('pre initiation', 'post initiation')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen'), name = '', 
                    labels = c('pre initiation', 'post initiation')) +
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair, color = initiated_minus2), size = 0.8) +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper, fill = initiated_minus2), alpha = 0.2) +
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
  ggtitle("")

p




du[int_prop > 0.6 & datetime_rel_pair0 == 5]
