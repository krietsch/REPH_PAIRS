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

# Threshold to exclude data
Np_min = 0
# Np_min = 0.25
# Np_min = 0.5

# subset data for models
dm = dp[Np >= Np_min & datetime_rel_pair >= -10 & datetime_rel_pair <= 10]

# factor year
dm[, year_ := factor(year_)]

# sin and cos of datetime
dm[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
dm[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]

dm[, initiated := fifelse(datetime_rel_pair < 0, "no", "yes")]

# plot settings
margin_ = unit(c(4, 4, 4, 4), 'pt')


#--------------------------------------------------------------------------------------------------------------
#' # M1 Paired male and female 
#--------------------------------------------------------------------------------------------------------------


m1 <- glmmTMB(interaction ~ poly(datetime_rel_pair) * initiated +
                scale(sin_time) + scale(cos_time) +
                (1 + poly(datetime_rel_pair, 2) | pairID),
              family = binomial, data = dm,
              REML = FALSE,
              control = glmmTMBControl(parallel = 15)
)

summary(m1)




e = allEffects(m1, xlevels = 100)$"poly(datetime_rel_pair):initiated" |>
  data.frame() |>
  setDT()

# predictions are made for the entire range of the data, subset to the relevant interval
e = e[(initiated == "no" & datetime_rel_pair < 0) | (initiated == "yes" & datetime_rel_pair > 0)]


du[, initiated := fifelse(datetime_rel_pair0 < 0, "no", "yes")]

p = 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  scale_color_manual(values = c('darkorange', 'darkgreen'), name = '', 
                     labels = c('pre initiation', 'post initiation')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen'), name = '', 
                    labels = c('pre initiation', 'post initiation')) +
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair, color = initiated), size = 0.8) +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper, fill = initiated), alpha = 0.2) +
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
