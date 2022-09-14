#==============================================================================================================
# Extrapair interactions and paternity
#==============================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'foreach', 'knitr',  'stringr', 
          'doFuture', 'patchwork', 'activity', 'glmmTMB', 'effects', 'performance', 'broomExtra',
          'flextable', 'officer', 'dplyr'), 
        require, character.only = TRUE)

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/statistics_extrapair interactions and paternity.R', output_dir = './OUTPUTS/R_COMPILED')

# Data
dID = fread('./DATA/NANO_TAGS_UNIQUE_BY_DAY.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dp  = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table

# subset data for models
dm = dp[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 5]

# sin and cos of datetime
dm[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
dm[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]

#--------------------------------------------------------------------------------------------------------------
#' Mate guarding intensity and probability of extra-pair interactions
#--------------------------------------------------------------------------------------------------------------

# model
fm1 <- glmmTMB(ID1_any_ep_int ~ interaction + poly(datetime_rel_pair, 2) +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm1)
plot(allEffects(fm1))

# predict data
e <- allEffects(fm1, xlevels = 100)$"poly(datetime_rel_pair,2)" |>
  data.frame() |>
  setDT()

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

# female
pb = 
  ggplot(data = dm) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  geom_smooth(aes(datetime_rel_pair, ID2_any_ep_int_binary, group = interaction, color = interaction, fill = interaction), alpha = 0.2) +
  geom_smooth(aes(datetime_rel_pair, ID2_any_ep_int_binary), alpha = 0.2, color = 'black', fill = 'black') +
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
  theme(legend.position = c(111, 111), legend.background = element_blank(), plot.margin = unit(c(15, 4, 2, 0), 'pt')) +
  ylab('Female probability of extra-pair interaction') +
  xlab('Day relative to clutch initiation (= 0)')

# merge plots
pa + pb +
  plot_layout(nrow = 2) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = 'A')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/Probability_of_EP_interactions.tiff', plot = last_plot(),  width = 180, height = 200, units = c('mm'), dpi = 'print')






# model
fm1 <- glmmTMB(ID1_any_ep_int ~ interaction +
                 scale(sin_time) + scale(cos_time) +
                 (1 | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm1)
plot(allEffects(fm1))



