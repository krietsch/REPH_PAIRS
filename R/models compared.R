

sapply(c(
  "data.table", 
  "glue", 
  "lubridate",
  "glmmTMB", 
  "ggplot2",
  "DHARMa",
  "performance",
  "effects"
),
require,
character.only = TRUE
)

dt2hh <- function(x) {
  h <- as.POSIXlt(x)
  h$hour + h$min / 60 + h$sec / 3600
}

hh2rad <- function(x) {
  x * pi / 12
}

# DATA
dp <- fread( here::here("DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt"),
             nThread = 20, tz = "UTC")
setorder(dp, pairID, datetime_1)

dp[, datetime_round := round_date(datetime_1, unit = "3 hours")]

dp[, period := fcase(
  datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, "[-5,-1]", 
  datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3,   "[0,3]", 
  datetime_rel_pair0 >= 4 & datetime_rel_pair0 <= 10,  "[4,10]"
)]

X = dp[!is.na(period), .(prop_interactions = mean(interaction), 
                         datetime_rel_pair  = mean(datetime_rel_pair), 
                         datetime_rel_pair0 = min(datetime_rel_pair0),
                         initiation_rel = mean(initiation_rel)
                         
), 
by = .(nestID, year_, date_,period, datetime_round)]

X[, HH := dt2hh(datetime_round)]

# beta models only accept proportion in the (0,1) interval.
X[prop_interactions == 1, prop_interactions := 0.9999]
X[prop_interactions == 0, prop_interactions := 0.0001]

# mean by nestID for initiation_rel plot
xp = dp[, .(prop_interactions = mean(interaction), .N), .(nestID, initiation_rel0, period)]



dx <- X[period == "[-5,-1]"]
setorder(dx, nestID, datetime_rel_pair)
dx[, time := 1:.N, .(date_, nestID)]

fm <- glmmTMB(
  prop_interactions ~
    sin(hh2rad(HH)) + 
    cos(hh2rad(HH)) + 
    poly(initiation_rel, 2) + 
    poly(datetime_rel_pair, 2) +
    (1 + datetime_rel_pair + I(datetime_rel_pair^2) | nestID) 
  ,
  family = beta_family(link="logit"), 
  data = dx,
  control = glmmTMBControl(parallel = 30)
)

summary(fm)



res <-simulateResiduals(fm, plot = T)
testDispersion(res)



EE <- allEffects(fm, xlevels = 100)

e <- EE$"poly(datetime_rel_pair,2)"
ed <- e |>
  data.frame() |>
  setDT()

ggplot(ed, aes(y = fit, x = datetime_rel_pair)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) + 
  geom_line(linewidth = 2) 

EE <- allEffects(fm, xlevels = 100)

e <- EE$"poly(initiation_rel,2)"
ed <- e |>
  data.frame() |>
  setDT()

ggplot(ed, aes(y = fit, x = initiation_rel)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) + 
  geom_line(linewidth = 2) + 
  geom_point(data = xp[period == "[-5,-1]"], aes(x = initiation_rel0, y = prop_interactions, size = N), shape = 21) 







# extract effect from model for plot
e = effect("poly(initiation_rel,2)", fm, xlevels = 100) |>
  data.frame() |>
  setDT()



p1 = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day -5 to -1 (3 h mean beta)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.1), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

p1





p4 = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day -5 to -1 (3 h mean beta)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = xp[period == "[-5,-1]", .(prop_interactions = median(prop_interactions)), initiation_rel0], 
             aes(y = prop_interactions, x = initiation_rel0), shape = 21, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.1), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

p4








# mean for each day
dpm = dp[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0, initiation_rel, N)]

dpm[, interaction_per_day := N_int / N]


# subset data before clutch initiation
dx = dpm[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
# dx = dp[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]


setorder(dx, pairID, nestID, datetime_rel_pair0)
dx[, time := 1:.N, .(datetime_rel_pair0, nestID)]



# model with time but without autocorrelation correction
m <- glmmTMB(interaction_per_day ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) +
               (datetime_rel_pair0 | nestID),
             family = gaussian, data = dx, REML = FALSE,
             control = glmmTMBControl(parallel = 15)
)


# plot(allEffects(m))
summary(m)


res <-simulateResiduals(m, plot = T)
testDispersion(res)


# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()



p2 = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day -5 to -1 (fitted on mean per day gaussian)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.1), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

p2





p5 = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day -5 to -1 (fitted on mean per day gaussian)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = xp[period == "[-5,-1]", .(prop_interactions = median(prop_interactions)), initiation_rel0], 
             aes(y = prop_interactions, x = initiation_rel0), shape = 21, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.1), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

p5







# beta models only accept proportion in the (0,1) interval.
dx[interaction_per_day == 1, interaction_per_day := 0.9999]
dx[interaction_per_day == 0, interaction_per_day := 0.0001]


# model with time but without autocorrelation correction
m <- glmmTMB(interaction_per_day ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) +
               (datetime_rel_pair0 | nestID),
             family = beta_family(link="logit"), data = dx, REML = FALSE,
             control = glmmTMBControl(parallel = 15)
)


# plot(allEffects(m))
summary(m)


res <-simulateResiduals(m, plot = T)
testDispersion(res)

# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()



p3 = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day -5 to -1 (fitted on mean per day beta)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.1), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

p3



p6 = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day -5 to -1 (fitted on mean per day beta)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = xp[period == "[-5,-1]", .(prop_interactions = median(prop_interactions)), initiation_rel0], 
             aes(y = prop_interactions, x = initiation_rel0), shape = 21, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.1), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

p6



ggplot(ed, aes(y = fit, x = initiation_rel)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) + 
  geom_line(linewidth = 2) + 
  geom_point(data = xp[period == "[-5,-1]", .(prop_interactions = median(prop_interactions)), initiation_rel0], aes(y = prop_interactions, x = initiation_rel0), shape = 21) 




# merge plots
p1 + p2 + p3 + p2 +
  plot_layout(nrow = 2) +
  plot_annotation(tag_levels = 'a')


# merge plots
p4 + p5 + p6 + p5 +
  plot_layout(nrow = 2) +
  plot_annotation(tag_levels = 'a')




