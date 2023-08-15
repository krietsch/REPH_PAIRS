#' ---
#' title: Figures and Statistic
#' subtitle: 
#' author: Johannes Krietsch
#' output:
#'    html_document:
#'      toc: true
#'      highlight: tango
#' ---

#==============================================================================================================
#' Data and code from "Mutual mate guarding and limited sexual conflict in a sex-role reversed shorebird"
#' Contributor: Johannes Krietsch  
#' 📍 This script runs relative to the project's root directory and describes how I created all figures 
#' (stored in ./OUTPUTS/FIGURES) and performed all statistical analysis (summary tables stored in./OUTPUTS/ESM).
#==============================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'ggplot2', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr', 
          'stringr', 'ggnewscale', 'doFuture', 'patchwork', 'activity', 'glmmTMB', 'effects', 'broomExtra',
          'flextable', 'officer', 'dplyr', 'performance', 'ggh4x', 'DHARMa'), 
        require, character.only = TRUE)

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/7_figures_and_statistic.R', output_dir = './OUTPUTS/R_COMPILED')

# Data
dID = fread('./DATA/NANO_TAGS_UNIQUE_BY_DAY.txt', sep = '\t', header = TRUE) %>% data.table
dp  = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', sep = '\t', header = TRUE) %>% data.table
dr  = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS_RANDOM.txt', sep = '\t', header = TRUE) %>% data.table
dn  = fread('./DATA/NESTS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
st_transform_DT(dn) # change projection
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), 
                                format = '%m-%d %H:%M:%S', tz = 'UTC')]
dn = dn[year_ > 2017]

# plot settings
margin_ = unit(c(2, 2, 2, 2), 'pt')
margin_top = unit(c(2, 2, 6, 2), 'pt')
sample_size_label = 2.5
egg_laying_color = 'grey85'

male_col = 'steelblue4'
female_col = 'indianred3'# 'firebrick3'
col1 = '#7aa048' # '#7aa048'
col2 = '#E69F00' 

# assign periods
dp[, period := fcase(
  date_rel_pair >= -5 & date_rel_pair <= -1, "[-5,-1]", 
  date_rel_pair >= 0 & date_rel_pair <= 3,   "[0,3]", 
  date_rel_pair >= 4 & date_rel_pair <= 10,  "[4,10]"
)]

# set order
setorder(dp, pairID, nestID, datetime_1)

# subset data 10 days around clutch initiation
dm = dp[date_rel_pair >= -10 & date_rel_pair <= 10]
dmr = dr[date_rel_pair >= -10 & date_rel_pair <= 10]

# sample size
dm[, .N, pairID] |> nrow()
dm[, .N, nestID] |> nrow()

# mean for each day
dpmi = dp[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, date_rel_pair)]
dpm = unique(dp, by = c('pairID', 'nestID', 'date_rel_pair', 'initiation_rel', 'N', 'year_'))
dpm = merge(dpm, dpmi, by = c('pairID', 'nestID', 'date_rel_pair'), all.x = TRUE)
dpm[is.na(N_int), N_int := 0]
dpm[, interaction_per_day := N_int / N]

dpm[, period := fcase(
  date_rel_pair >= -5 & date_rel_pair <= -1, "[-5,-1]", 
  date_rel_pair >= 0 & date_rel_pair <= 3,   "[0,3]", 
  date_rel_pair >= 4 & date_rel_pair <= 10,  "[4,10]"
)]

setorder(dpm, pairID, nestID, date_rel_pair)

# random pairs
dr[, N := .N, by = .(pairID, nestID, date_rel_pair)]
drmi = dr[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, date_rel_pair)]
drm = unique(dr, by = c('pairID', 'nestID', 'date_rel_pair', 'initiation_rel', 'N', 'year_'))
drm = merge(drm, drmi, by = c('pairID', 'nestID', 'date_rel_pair'), all.x = TRUE)
drm[is.na(N_int), N_int := 0]
drm[, interaction_per_day := N_int / N]

drm[, period := fcase(
  date_rel_pair >= -5 & date_rel_pair <= -1, "[-5,-1]", 
  date_rel_pair >= 0 & date_rel_pair <= 3,   "[0,3]", 
  date_rel_pair >= 4 & date_rel_pair <= 10,  "[4,10]"
)]

setorder(drm, pairID, nestID, date_rel_pair)

# nest method
dmn = unique(dm, by = 'nestID')
xn = dmn$nestID
dns = dn[nestID %in% xn]
dns = dns[, .N, by = initiation_method]
dns[, N_percentage_total := round(N / nrow(dmn) * 100, 0)]

# start word file for ESM
ESM = read_docx()

# parameter names 
pn = fread("parname;                                                          parameter
            (Intercept);                                                      Intercept 
            sin(hh2rad(HH));                                                  Sinus time
            cos(hh2rad(HH));                                                  Cosinus time
            anyEPYTRUE;                                                       EPP (yes)
            sexM;                                                             Sex (male)
            initiation_rel;                                                   Clutch initiation date
            poly(initiation_rel, 2)1;                                         Clutch initiation date (linear)
            poly(initiation_rel, 2)2;                                         Clutch initiation date (quadratic)
            date_rel_pair;                                                    Day relative to clutch initiation 
            poly(datetime_rel_pair, 2)1;                                      Day relative to clutch initiation (linear)
            poly(datetime_rel_pair, 2)2;                                      Day relative to clutch initiation (quadratic)
            poly(date_rel_pair, 2)1;                                          Day relative to clutch initiation (linear)
            poly(date_rel_pair, 2)2;                                          Day relative to clutch initiation (quadratic)
            year_2019;                                                        Year (2019)
            f_polyandrous_firstTRUE;                                          First clutch of polyandrous female (yes)
            typerandomization;                                                Data type (random pairs)
            date_rel_pair:typerandomization;                                  Day relative to clutch initiation:data type (random pairs)
            poly(datetime_rel_pair0, 2)1:typerandomization;                   Day relative to clutch initiation (linear):data type (random pairs)
            poly(datetime_rel_pair0, 2)2:typerandomization;                   Day relative to clutch initiation (quadratic):data type (random pairs)
            typerandomization:initiation_rel;                                 Clutch initiation date:data type (random pairs)
            typerandomization:poly(initiation_rel, 2)1;                       Clutch initiation date (linear):data type (random pairs)
            typerandomization:poly(initiation_rel, 2)2;                       Clutch initiation date (quadratic):data type (random pairs)
            sd__(Intercept);                                                  Random intercept
            r2marg;                                                           R² marginal
            r2cond;                                                           R² conditional
            
", sep = ';')

#--------------------------------------------------------------------------------------------------------------
#' # Data available relative to clutch initiation
#--------------------------------------------------------------------------------------------------------------

dIDs = unique(dID[date_rel_pair >= -10 & date_rel_pair <= 10], 
              by = c('sex', 'nestID', 'date_rel_pair'))
dIDs = dIDs[, .N, by = .(sex, date_rel_pair, both_tagged_overlapping)]
dIDs

# pairwise sample size
du = unique(dp, by = c('pairID', 'nestID', 'date_rel_pair'))
dss = unique(du[date_rel_pair >= -10 & date_rel_pair <= 10], 
             by = c('nestID', 'date_rel_pair'))
dss = dss[, .N, by = date_rel_pair]
dss

# samply size pairs and nests
unique(du, by = 'nestID') |> nrow()
unique(du, by = 'pairID') |> nrow()
unique(du, by = 'ID1') |> nrow()
unique(du, by = 'ID2') |> nrow()

# plot data available
dIDs[, sex := factor(sex, levels = c('F', 'M', 'pair'))]

# plot by nest
dp[, datetime_rel_pair_min := min(datetime_rel_pair), by = nestID]

du = unique(dp, by = c('nestID'))
setorder(du, by = datetime_rel_pair_min)
dp[, nestID := factor(nestID, levels = c(du$nestID))]

# last interaction
dp[interaction == TRUE, max(datetime_rel_pair)]

ggplot(data = dp) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = -Inf, ymax = Inf), fill = egg_laying_color) +
  geom_tile(aes(datetime_rel_pair, nestID, fill = interaction), width = 0.5, show.legend = TRUE) +
  scale_fill_manual(values = c('TRUE' = col1, 'FALSE' = col2), name = '', 
                    labels = c('Not together', 'Together'), drop = FALSE) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 0.2) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 0.2) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Nest') +
  scale_x_continuous(limits = c(-12, 19), breaks = seq(-12, 19, 1), expand = expansion(add = c(0.2, 0.2))) +
  theme_classic(base_size = 10) + 
  theme(legend.position = c(0.07, 0.95), legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank())

# ggsave('./OUTPUTS/FIGURES/MG_over_season_eachID.tiff', plot = last_plot(),  width = 238, height = 177, 
#        units = c('mm'), dpi = 'print')

dp[, nestID := as.character(nestID)]

#--------------------------------------------------------------------------------------------------------------
#' # Mate guarding intensity in relation to daily patterns
#--------------------------------------------------------------------------------------------------------------

# Function to deal with time
dt2hh <- function(x) {
  h <- as.POSIXlt(x)
  h$hour + h$min / 60 + h$sec / 3600
}

hh2rad <- function(x) {
  x * pi / 12
}

dm[, HH := dt2hh(datetime_1)]


### before clutch initiation
dx = dm[period == "[-5,-1]"]

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations


# model
m <- glmmTMB(interaction ~ sin(hh2rad(HH)) + cos(hh2rad(HH)) + 
               poly(initiation_rel, 2) + poly(datetime_rel_pair, 2) +
               (datetime_rel_pair | nestID),
  family = binomial(link = "logit"), 
  data = dx,
  control = glmmTMBControl(parallel = 15)
)

plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res)
acf(resid(m), type = 'partial') 


# check effect of time of the day
e = effect("sin(hh2rad(HH))", m, xlevels = 100) |>
  data.frame() |>
  setDT()


ggplot(e, aes(y = fit, x = HH)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) + 
  geom_line(linewidth = 1) +
  theme_classic(base_size = 10) +
  xlab("Hour") + 
  ylab("Proportion of time together") 

# check effect size and when min and max
e[, min(fit)]
e[, max(fit)]
(e[, max(fit)] - e[, min(fit)]) * 100 # not biological relevant

e[fit == min(fit), .(HH)]
e[fit == max(fit), .(HH)]


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 

setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)] 
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S1. GLMM together and time -5 to -1')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')



### during egg-laying
dx = dm[period == "[0,3]"]

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# model
m <- glmmTMB(interaction ~ sin(hh2rad(HH)) + cos(hh2rad(HH)) + 
               poly(initiation_rel, 2) + poly(datetime_rel_pair, 2) +
               (datetime_rel_pair | nestID),
             family = binomial(link = "logit"), 
             data = dx,
             control = glmmTMBControl(parallel = 15)
)

plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial') 


# check effect of time of the day
e = effect("sin(hh2rad(HH))", m, xlevels = 100) |>
  data.frame() |>
  setDT()


ggplot(e, aes(y = fit, x = HH)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) + 
  geom_line(linewidth = 1) +
  theme_classic(base_size = 10) +
  xlab("Hour") + 
  ylab("Proportion of time together") 

# check effect size and when min and max
e[, min(fit)] * 100
e[, max(fit)] * 100
(e[, max(fit)] - e[, min(fit)]) * 100 # biological relevant

e[fit == min(fit), .(HH, se)]
e[fit == max(fit), .(HH, se)]

# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 

setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)] 
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S2. GLMM together and time 0 to 3')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

#--------------------------------------------------------------------------------------------------------------
#' # Mate guarding intensity in relation year
#--------------------------------------------------------------------------------------------------------------

### before clutch initiation
dx = dpm[period == "[-5,-1]"]

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# by year
dxu = unique(dx, by = c('nestID', 'year_')) 
dxu[, .N, by = year_]
dx[, .N, by = year_]

# beta models only accept proportion in the (0,1) interval
dx[interaction_per_day == 1, interaction_per_day := 0.9999]
dx[interaction_per_day == 0, interaction_per_day := 0.0001]

dx[, year_ := as.character(year_)]

# model
m <- glmmTMB(interaction_per_day ~ poly(date_rel_pair, 2) + poly(initiation_rel, 2) + year_ + 
               (date_rel_pair | nestID),
             family =  beta_family(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S3. GLMM together and year -5 to -1')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# check effect of time of the day
e = effect("year_", m, xlevels = 2) |>
  data.frame() |>
  setDT()


### during egg-laying
dx = dpm[period == "[0,3]"]

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# by year
dxu = unique(dx, by = c('nestID', 'year_')) 
dxu[, .N, by = year_]
dx[, .N, by = year_]

# beta models only accept proportion in the (0,1) interval
dx[interaction_per_day == 1, interaction_per_day := 0.9999]
dx[interaction_per_day == 0, interaction_per_day := 0.0001]

dx[, year_ := as.character(year_)]

# model
m <- glmmTMB(interaction_per_day ~ date_rel_pair + poly(initiation_rel, 2) + year_ + 
               (date_rel_pair | nestID),
             family =  beta_family(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S4. GLMM together and year 0 to 3')) |>  body_add_par('') |>
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# Plot time together breeding pairs split by year

# pairwise sample size 2018
dus = unique(dp, by = c('pairID', 'nestID', 'date_rel_pair'))
dss_2018 = unique(dus[year_ == 2018 & date_rel_pair >= -10 & date_rel_pair <= 10], 
                  by = c('nestID', 'date_rel_pair'))
dss_2018 = dss_2018[, .N, by = date_rel_pair]

# pairwise sample size 2019
dus = unique(dp, by = c('pairID', 'nestID', 'date_rel_pair'))
dss_2019 = unique(dus[year_ == 2019 & date_rel_pair >= -10 & date_rel_pair <= 10], 
                  by = c('nestID', 'date_rel_pair'))
dss_2019 = dss_2019[, .N, by = date_rel_pair]

du[, year_ := as.character(year_)]


# merge 
dss = merge(dss_2018[, .(N_2018 = N, date_rel_pair)], 
            dss_2019[, .(N_2019 = N, date_rel_pair)], by = 'date_rel_pair', all = TRUE)
dss[is.na(N_2018), N_2018 := 0]
dss[, N_year_label := paste0(N_2018, '/', N_2019)]

dpm[, year_cha := as.character(year_)]


p1 = 
  ggplot() +
  geom_text(data = dss, aes(date_rel_pair, Inf, label = N_year_label), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = dpm, 
               aes(date_rel_pair, interaction_per_day, group = interaction(date_rel_pair, year_cha), 
                   color = year_cha),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0, 
               position = position_dodge2(preserve = "single")) +
  geom_point(data = dpm, 
             aes(date_rel_pair, interaction_per_day, group = interaction(date_rel_pair, year_cha), 
                 color = year_cha), # shape = data_quality
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c(col1, col2), name = '', 
                     labels = c('2018', '2019'), drop = FALSE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.93, 0.85), legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)')

p1


# clutch initiation distribution
di = dn[!is.na(year_) & data_type == 'study_site', .(initiation_mean = mean(initiation, na.rm = TRUE)), 
        by = year_]

dn = merge(dn, di, by = 'year_', all.x = TRUE)
dn[, initiation_rel_ := difftime(initiation, initiation_mean, units = 'days') %>% as.numeric %>% round(., 0)]

dns = dn[data_type == 'study_site' & !is.na(initiation_rel_)]
dns[, year_ := as.character(year_)]

# Sample size
dss = dns[, .(median = median(initiation_rel_), q25 = quantile(initiation_rel_, probs = c(0.25)),
              q75 = quantile(initiation_rel_, probs = c(0.75)), .N, max = max(initiation_rel_)), by = year_]
dss[, year_ := as.character(year_)]

# x axis ticks
x = data.table(seq1 = seq(-16, 16, 1))
y = data.table(seq1 = seq(-16, 16, 2), seq2 = as.character(seq(-16, 16, 2)))
xy = merge(x, y, by = 'seq1', all.x = TRUE)
xy[is.na(seq2), seq2 := '']

p2 =
  ggplot(data = dns) +
  geom_violin(aes(initiation_rel_, year_, color = year_, fill = year_), alpha = 0.2, show.legend = FALSE) +
  geom_point(data = dss, aes(median, year_, color = year_), size = 1.5) +
  geom_linerange(data = dss, aes(y = year_, xmin = q75, xmax = q25, color = year_), size = 0.3) +
  
  geom_text(data = dss, aes(Inf, year_, label = N), hjust = 1, size = sample_size_label) +
  scale_color_manual(values = c(col1, col2), name = '', 
                     labels = c('2018', '2019'), drop = FALSE) +
  scale_fill_manual(values = c(col1, col2), name = '', 
                    labels = c('2018', '2019'), drop = FALSE) +
  scale_x_continuous(limits = c(-16, 16), breaks = seq(-16, 16, 1), 
                     labels = xy$seq2,
                     expand = expansion(add = c(0.5, 0.5))) +
  ylab('Year') + xlab('Clutch initiation date (standardized)') +
  theme_classic(base_size = 10) +
  theme(legend.position = "none")
p2



# merge plots
p1 + p2 + 
  plot_layout(nrow = 2, heights = c(3, 1)) +
  plot_annotation(tag_levels = 'a')


# ggsave('./OUTPUTS/FIGURES/prop_time_together_season_year.tiff', plot = last_plot(),  width = 177, height = 120, 
#        units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' # Mate guarding intensity in relation to breeding state 
#--------------------------------------------------------------------------------------------------------------

# plot time together

# merge data
du = rbindlist(list(dpm[, .(pairID, nestID, year_, date_rel_pair, prop = interaction_per_day, 
                            type = 'm_f_together')],
                    drm[, .(pairID, nestID, year_, date_rel_pair, prop = interaction_per_day, 
                            type = 'm_f_together_randomized')]
))


# pairwise sample size
ds = unique(dpm, by = c('pairID', 'nestID', 'date_rel_pair'))
dss = unique(ds[date_rel_pair >= -10 & date_rel_pair <= 10], 
             by = c('nestID', 'date_rel_pair'))
dss = dss[, .N, by = date_rel_pair]
dss

# Plot time together breeding pairs vs. randomized pairs
pa = 
  ggplot() +
  geom_text(data = dss, aes(date_rel_pair, Inf, label = N), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = du, 
               aes(date_rel_pair, prop, group = interaction(date_rel_pair, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du, 
             aes(date_rel_pair, prop, group = interaction(date_rel_pair, type), color = type),
             position=position_jitterdodge(), size = 0.2) +
  # scale_shape_manual(values=c(20, 19)) +
  scale_color_manual(values = c(col1, col2), name = '', 
                     labels = c('Breeding pair', 'Random pair'), drop = FALSE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.9, 0.85), legend.background = element_blank(), plot.margin = margin_top, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)')

pa


### statistic

### before clutch initiation
dx = dpm[period == "[-5,-1]"]

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# beta models only accept proportion in the (0,1) interval
dx[interaction_per_day == 1, interaction_per_day := 0.9999]
dx[interaction_per_day == 0, interaction_per_day := 0.0001]


# model
m <- glmmTMB(interaction_per_day ~ poly(date_rel_pair, 2) + poly(initiation_rel, 2) + 
               (date_rel_pair | nestID),
             family =  beta_family(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S5. GLMM together -5 to -1')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# descriptive part
x = effect("poly(initiation_rel,2)", m, xlevels = 19) |>
  data.frame() |>
  setDT() |> 
  print()

x[fit > 0.90] 

# before and after peak
x[initiation_rel < -1, .(fit = mean(fit), se = mean(se))] * 100
x[initiation_rel > 7, .(fit = mean(fit), se = mean(se))] * 100


x = effect("poly(date_rel_pair,2)", m, xlevels = 5) |>
  data.frame() |>
  setDT() |> 
  print()

x[, .(fit = mean(fit), se = mean(se))] * 100

# pairs with more than 95% together
x[fit > 0.90] 

dpm[date_rel_pair == -2 & interaction_per_day > 0.90] |> nrow()
dpm[date_rel_pair == -2] |> nrow()


# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dm[period == "[-5,-1]"]
dms = dms[, N_ini := .N, by = .(pairID, nestID)]
du = unique(dms, by = c('pairID', 'nestID', 'initiation_rel'))
du = du[!is.na(N_ini)]
du[, .(min(N_ini), max(N_ini))] # check min and max
du[, .(min(initiation_rel), max(initiation_rel))] # check min and max

dms = dms[interaction == TRUE & period == "[-5,-1]", .(N_int = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N_ini]
d0 = copy(du)

# point sizes range
du[, .(min(N_ini), max(N_ini))]

pb = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day -5 to -1'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1, color = col1) +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = col1) +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, 
              fill = col1) +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

pb


### during egg-laying
dx = dpm[period == "[0,3]"]

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# beta models only accept proportion in the (0,1) interval
dx[interaction_per_day == 1, interaction_per_day := 0.9999]
dx[interaction_per_day == 0, interaction_per_day := 0.0001]

dx[, year_ := as.character(year_)]

# model
m <- glmmTMB(interaction_per_day ~ date_rel_pair + poly(initiation_rel, 2) + 
               (date_rel_pair | nestID),
             family =  beta_family(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S6. GLMM together and year 0 to 3')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# descriptive part
effect("date_rel_pair", m, xlevels = 4) |>
  data.frame() |>
  setDT() |> 
  print() * 100

x = effect("poly(initiation_rel,2)", m, xlevels = 22) |>
  data.frame() |>
  setDT() |> 
  print() 

# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()

# data for points 
dms = dm[period == "[0,3]"]
dms = dms[, N_ini := .N, by = .(pairID, nestID)]
du = unique(dms, by = c('pairID', 'nestID', 'initiation_rel'))
du = du[!is.na(N_ini)]
du[, .(min(N_ini), max(N_ini))] # check min and max
du[, .(min(initiation_rel), max(initiation_rel))] # check min and max

dms = dms[interaction == TRUE & period == "[0,3]", .(N_int = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N_ini]
d0 = copy(du)

# point sizes range
du[, .(min(N_ini), max(N_ini))]

pc = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day 0 to 3'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1, color = col1) +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = col1) +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, 
              fill = col1) +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.4, 0.4))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('') +
  xlab('Clutch initiation date (standardized)')

pc


# merge plots
pa + pb + pc +
  plot_layout(design = "
  11
  23
") +
  plot_annotation(tag_levels = 'a')

# ggsave('./OUTPUTS/FIGURES/male_female_together.tiff', plot = last_plot(),  width = 177, height = 177, 
#        units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' # Mate guarding intensity in relation to breeding state breeders vs. random pairs
#--------------------------------------------------------------------------------------------------------------


# Statistic together breeding pairs vs. random pairs

# assign random pairs nestID as pairID
dmr[, nestID := pairID]

# merge data
dprm = rbindlist(list(dpm[, .(pairID, nestID, ID1, ID2, year_, date_rel_pair, initiation_rel, interaction_per_day, 
                              period, type = 'breeder')],
                      drm[, .(pairID, nestID, ID1, ID2, year_, date_rel_pair, initiation_rel, interaction_per_day, 
                              period, type = 'randomization')]
))


### before clutch initiation
dx = dprm[period == "[-5,-1]"]

# sample size
unique(dx$pairID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# by type
dxu = unique(dx, by = c('nestID', 'type')) 
dxu[, .N, by = type]
dx[, .N, by = type]

dxu = unique(dx, by = c('pairID', 'type')) 
dxu[, .N, by = type]
dx[, .N, by = type]

# beta models only accept proportion in the (0,1) interval
dx[interaction_per_day == 1, interaction_per_day := 0.9999]
dx[interaction_per_day == 0, interaction_per_day := 0.0001]

# model
m <- glmmTMB(interaction_per_day ~ poly(date_rel_pair, 2) * type + poly(initiation_rel, 2) * type + 
               (date_rel_pair | nestID),
             family =  beta_family(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S7. GLMM together vs. randomized -5 to -1')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# descriptive part
effect("type", m, xlevels = 2) |>
  data.frame() |>
  setDT() |> 
  print() * 100

### during egg-laying
dx = dprm[period == "[0,3]"]

# sample size
unique(dx$pairID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# by type
dxu = unique(dx, by = c('nestID', 'type')) 
dxu[, .N, by = type]
dx[, .N, by = type]

dxu = unique(dx, by = c('pairID', 'type')) 
dxu[, .N, by = type]
dx[, .N, by = type]

# beta models only accept proportion in the (0,1) interval
dx[interaction_per_day == 1, interaction_per_day := 0.9999]
dx[interaction_per_day == 0, interaction_per_day := 0.0001]

# model
m <- glmmTMB(interaction_per_day ~ date_rel_pair * type + poly(initiation_rel, 2) * type + 
               (date_rel_pair | nestID),
             family =  beta_family(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S8. GLMM together vs. randomized 0 to 3')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# descriptive part
effect("type", m, xlevels = 2) |>
  data.frame() |>
  setDT() |> 
  print() * 100

### after egg-laying
dx = dprm[period == "[4,10]"]

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# by type
dxu = unique(dx, by = c('nestID', 'type')) 
dxu[, .N, by = type]
dx[, .N, by = type]

dxu = unique(dx, by = c('pairID', 'type')) 
dxu[, .N, by = type]
dx[, .N, by = type]

# beta models only accept proportion in the (0,1) interval
dx[interaction_per_day == 1, interaction_per_day := 0.9999]
dx[interaction_per_day == 0, interaction_per_day := 0.0001]

# model
m <- glmmTMB(interaction_per_day ~ date_rel_pair * type + initiation_rel * type + 
               (date_rel_pair | nestID),
             family =  beta_family(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S9. GLMM together vs. randomized 4 to 10')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# descriptive part
effect("type", m, xlevels = 2) |>
  data.frame() |>
  setDT() |> 
  print() * 100

#--------------------------------------------------------------------------------------------------------------
#' # Female moves away
#--------------------------------------------------------------------------------------------------------------

# Proportion of split events
dms = dm[split == TRUE, .(N_split = .N), by = .(pairID, nestID, date_rel_pair)]
du = unique(dm[split == TRUE], by = c('pairID', 'nestID', 'date_rel_pair'))
du = merge(du, dms, by = c('pairID', 'nestID', 'date_rel_pair'), all.x = TRUE)
du[is.na(N_split), N_split := 0]
du[, split_prop := N_split / N]
d1 = copy(du)

dm = merge(dm, du[, .(pairID, nestID, date_rel_pair, N_split)], by = c('pairID', 'nestID', 'date_rel_pair'),
           all.x = TRUE)

# Times females split
dms = dm[split == TRUE & IDsplitting == 'ID2', .(N_f_split = .N), by = .(pairID, nestID, date_rel_pair)]
du = unique(dm[split == TRUE], by = c('pairID', 'nestID', 'date_rel_pair'))
du = merge(du, dms, by = c('pairID', 'nestID', 'date_rel_pair'), all.x = TRUE)
du[is.na(N_f_split), N_f_split := 0]
du[, f_split_prop := N_f_split /N_split]

# for plot later with EPY
dusm = copy(du)

# pairwise sample size
dus = unique(dp[split == TRUE], by = c('pairID', 'nestID', 'date_rel_pair'))
dss = unique(dus[date_rel_pair >= -10 & date_rel_pair <= 10], 
             by = c('nestID', 'date_rel_pair'))
dss = dss[, .N, by = date_rel_pair]
dss



# plot splits females 
pa = 
  ggplot() +
  geom_text(data = dss, aes(date_rel_pair, Inf, label = N), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = du, 
               aes(date_rel_pair, f_split_prop, group = interaction(date_rel_pair)), colour = col1,
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_jitter(data = du, aes(date_rel_pair, f_split_prop), colour = col1, size = 0.5) + 
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.15), legend.background = element_blank(), plot.margin = margin_top) +
  ylab('Proportion female moves away') +
  xlab('Day relative to clutch initiation (= 0)')

pa


### Models 

# Statistic females moving away

### before clutch initiation
dx = dp[split == TRUE & period == "[-5,-1]"]
dx[, IDsplitting := ifelse(IDsplitting == 'ID1', 0, 1)] # males = 0

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# model
m <- glmmTMB(IDsplitting ~ date_rel_pair + initiation_rel + 
               (date_rel_pair | nestID),
             family =  binomial(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')

# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S10. GLMM female moving away -5 to -1')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# descriptive part
x = effect("date_rel_pair", m, xlevels = 4) |>
  data.frame() |>
  setDT() |> 
  print() 

x[, .(fit = mean(fit), se = mean(se))] * 100

x = effect("initiation_rel", m, xlevels = 16) |>
  data.frame() |>
  setDT() |> 
  print() 

# before and after peak
x[initiation_rel <= -1, .(fit = mean(fit), se = mean(se))] * 100
x[initiation_rel >= 0, .(fit = mean(fit), se = mean(se))] * 100

# extract effect from model for plot
e = effect("initiation_rel", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dm[split == TRUE & period == "[-5,-1]"]
dms[, N_splits_season := .N, by = .(pairID, nestID, initiation_rel)]
du = unique(dms[!is.na(N_splits_season)], by = c('pairID', 'nestID', 'initiation_rel'))
du[, .(min(N_splits_season), max(N_splits_season))] # check min and max
du[, .(min(initiation_rel), max(initiation_rel))] # check min and max


# Proportion of split events
dms = dm[split == TRUE & IDsplitting == 'ID2' & period == "[-5,-1]", 
         .(N_split_female = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_split_female), N_split_female := 0]
du[, split_prop := N_split_female / N_splits_season]
d1 = copy(du)

# point sizes range
du[, .(min(N_splits_season), max(N_splits_season))]

pb = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day -5 to -1'), vjust = 1, hjust = 0, size = 3.3) +
  geom_hline(yintercept = 0.5, color = 'black', linetype = 'dashed') +
  geom_point(data = du, aes(initiation_rel, split_prop, size = N_splits_season), shape = 1, 
             color = col1) +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = col1) +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, 
              fill = col1) +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.4, 0.4))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(10, 20, 30)) +
  theme_classic(base_size = 10) +
  theme(legend.position = 'none', legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion female moves away') +
  xlab('Clutch initiation date (standardized)')

pb



### during egg-laying
dx = dp[split == TRUE & period == "[0,3]"]
dx[, IDsplitting := ifelse(IDsplitting == 'ID1', 0, 1)] # males = 0

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# model
m <- glmmTMB(IDsplitting ~ date_rel_pair + initiation_rel + 
               (date_rel_pair | nestID),
             family =  binomial(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')

# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S11. GLMM female moving away 0 to 3')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# descriptive part
x = effect("initiation_rel", m, xlevels = 16) |>
  data.frame() |>
  setDT() |> 
  print() 

# before and after peak
x[initiation_rel <= -1, .(fit = mean(fit), se = mean(se))] * 100
x[initiation_rel >= 0, .(fit = mean(fit), se = mean(se))] * 100


x = effect("date_rel_pair", m, xlevels = 4) |>
  data.frame() |>
  setDT() |> 
  print() 

x[, .(fit = mean(fit), se = mean(se))] * 100


# extract effect from model for plot
e = effect("initiation_rel", m, xlevels = 100) |>
  data.frame() |>
  setDT()

# data for points 
dms = dm[split == TRUE & period == "[0,3]"]
dms[, N_splits_season := .N, by = .(pairID, nestID, initiation_rel)]
du = unique(dms[!is.na(N_splits_season)], by = c('pairID', 'nestID', 'initiation_rel'))
du[, .(min(N_splits_season), max(N_splits_season))] # check min and max
du[, .(min(initiation_rel), max(initiation_rel))] # check min and max


# Proportion of split events
dms = dm[split == TRUE & IDsplitting == 'ID2' & period == "[0,3]", 
         .(N_split_female = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_split_female), N_split_female := 0]
du[, split_prop := N_split_female / N_splits_season]
d1 = copy(du)

# point sizes range
du[, .(min(N_splits_season), max(N_splits_season))]


pc = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day 0 to 3'), vjust = 1, hjust = 0, size = 3.3) +
  geom_hline(yintercept = 0.5, color = 'black', linetype = 'dashed') +
  geom_point(data = du, aes(initiation_rel, split_prop, size = N_splits_season), shape = 1, 
             color = col1) +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = col1) +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, 
              fill = col1) +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.4, 0.4))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(10, 20, 30)) +
  theme_classic(base_size = 10) +
  theme(legend.position = 'none', legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('') +
  xlab('Clutch initiation date (standardized)')

pc


# merge plots
pa + pb + pc +
  plot_layout(design = "
  11
  23
") +
  plot_annotation(tag_levels = 'a')

# ggsave('./OUTPUTS/FIGURES/female_moving_away.tiff', plot = last_plot(),  width = 177, height = 177, 
#        units = c('mm'), dpi = 'print')

#--------------------------------------------------------------------------------------------------------------
#' # Number of separating flights 
#--------------------------------------------------------------------------------------------------------------

# subset data
dms = unique(dm, by = c('pairID', 'nestID', 'date_rel_pair'))
dms[is.na(N_split),  N_split := 0]

# descriptive statistic 
dms[period == "[-5,-1]", .(mean = mean(N_split), min = min(N_split), max = max(N_split))]
dms[period == "[0,3]", .(mean = mean(N_split), min = min(N_split),max = max(N_split))]

# pairwise sample size
dus = unique(dp, by = c('pairID', 'nestID', 'date_rel_pair'))
dss = unique(dus[date_rel_pair >= -10 & date_rel_pair <= 10], 
             by = c('nestID', 'date_rel_pair'))
dss = dss[, .N, by = date_rel_pair]
dss


ggplot() +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.3, ymax = 16.1), fill = egg_laying_color) +
  geom_text(data = dss, aes(date_rel_pair, Inf, label = N), vjust = 1, size = sample_size_label) +
  geom_boxplot(data = dms, 
               aes(date_rel_pair, N_split, group = interaction(date_rel_pair)),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0, color = col1) +
  geom_point(data = dms, 
             aes(date_rel_pair, N_split, group = interaction(date_rel_pair)), color = col1, 
             position=position_jitter(height = 0), size = 0.2) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.3, 16.7), breaks = seq(0, 16, 1),
                     labels = c('0', '', '2', '', '4', '', '6', '', '8', '', '10', '', '12', '', '14', '', '16'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.87, 0.94), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Number of separating flights') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/male_female_split_events_number.tiff', plot = last_plot(),  width = 177, height = 89, 
#        units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' # Distance moved away by sex
#--------------------------------------------------------------------------------------------------------------

# moved away
dms = dm[split == TRUE]

# merge male and female data for plot
dms_m = dms[IDsplitting == 'ID1', .(pairID, nestID, ID1, ID2, date_rel_pair, period, initiation_rel, sex = sex1, 
                                     split_distance = distance1_before, stay_distance = distance2_before)]
dms_f = dms[IDsplitting == 'ID2', .(pairID, nestID, ID1, ID2, date_rel_pair, period, initiation_rel, sex = sex2, 
                                     split_distance = distance2_before, stay_distance = distance1_before)]

dms = rbindlist(list(dms_m, dms_f))

# how often did the other ID stay within 30 m?
ggplot(data = dms) +
  geom_histogram(aes(x = stay_distance)) +
  theme_classic(base_size = 10)
  
dms[, median(stay_distance)]
dms[stay_distance < 30] |> nrow() / nrow(dms) * 100

# how much was the differences when the other ID moved too?
dms[, delta_split := split_distance - stay_distance]

dms[stay_distance > 30, median(delta_split)]
dms[stay_distance > 30, min(delta_split)]
dms[stay_distance > 30, max(delta_split)]

ggplot(data = dms[stay_distance > 30]) +
  geom_histogram(aes(x = delta_split)) +
  theme_classic(base_size = 10)


# pairwise sample size
du = unique(dm[split == TRUE], by = c('pairID', 'nestID', 'date_rel_pair'))
dss_m = unique(du[IDsplitting == 'ID1'], by = c('nestID', 'date_rel_pair'))
dss_m = dss_m[, .N, by = date_rel_pair]

# pairwise sample size
dss_f = unique(du[IDsplitting == 'ID2'], by = c('nestID', 'date_rel_pair'))
dss_f = dss_f[, .N, by = date_rel_pair]

# merge 
dss = merge(dss_m[, .(N_m = N, date_rel_pair)], dss_f[, .(N_f = N, date_rel_pair)], by = 'date_rel_pair', 
            all.x = TRUE)
dss[, N_label := paste0(N_f, '/', N_m)]


# adjust distance above 1000 m
dms[, split_distance1000 := split_distance]
dms[split_distance > 1000, split_distance1000 := 1000]

ggplot() +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1005), fill = egg_laying_color) +
  geom_text(data = dss, aes(date_rel_pair, Inf, label = N_label), vjust = 1, size = sample_size_label) +
  geom_boxplot(data = dms, 
               aes(date_rel_pair, split_distance1000, group = interaction(date_rel_pair, sex), 
                   color = sex),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = dms, 
             aes(date_rel_pair, split_distance1000, group = interaction(date_rel_pair, sex), 
                 color = sex), position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c(female_col, male_col), name = '',
                     labels = c('Female moves away', 'Male moves away'), drop = FALSE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(expand = expansion(add = c(0, 50))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.87, 0.92), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Distance moved when separated (m)') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/male_female_split_events_distance_moved.tiff', plot = last_plot(),  width = 177, 
#        height = 89, units = c('mm'), dpi = 'print')




# statistic

### before clutch initiation
dx = dms[period == "[-5,-1]"]

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# by sex
dxu = unique(dx, by = c('nestID', 'sex')) 
dxu[, .N, by = sex]
dx[, .N, by = sex]

m <- glmmTMB(split_distance ~ sex + date_rel_pair + initiation_rel + (date_rel_pair | nestID),
               family = gaussian, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')

# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S12. GLMM split distance -5 to -1')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# descriptive part 
x = effect("sex", m, xlevels = 2) |>
  data.frame() |>
  setDT() |> 
  print()

x = effect("date_rel_pair", m, xlevels = 9) |>
  data.frame() |>
  setDT() |> 
  print()



### during egg-laying
dx = dms[period == "[0,3]"]

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# by sex
dxu = unique(dx, by = c('nestID', 'sex')) 
dxu[, .N, by = sex]
dx[, .N, by = sex]


m <- glmmTMB(split_distance ~ sex + date_rel_pair + initiation_rel + (date_rel_pair | nestID),
             family = gaussian, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S13. GLMM split distance 0 to 3')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# descriptive part
x = effect("sex", m, xlevels = 2) |>
  data.frame() |>
  setDT() |> 
  print()

x = effect("date_rel_pair", m, xlevels = 9) |>
  data.frame() |>
  setDT() |> 
  print()

#--------------------------------------------------------------------------------------------------------------
#' # Nest attendance by sex
#--------------------------------------------------------------------------------------------------------------

# assign parameters
dm[, m_at_nest := at_nest1 == TRUE | at_nest2 == TRUE & interaction == TRUE]
dm[, f_at_nest := at_nest2 == TRUE | at_nest1 == TRUE & interaction == TRUE]
dm[, both_at_nest := at_nest1 == TRUE & interaction == TRUE | at_nest2 == TRUE & interaction == TRUE]
dm[, m_alone_at_nest := at_nest1 == TRUE & interaction == FALSE]
dm[, f_alone_at_nest := at_nest2 == TRUE & interaction == FALSE]

# First nest location visit
ds1 = dm[, .(first_pairwise_location = min(datetime_rel_pair)), by = nestID]
ds2 = dm[m_at_nest == TRUE | f_at_nest == TRUE, .(first_nest_visit = min(datetime_rel_pair)), by = nestID]
ds = merge(ds1, ds2, by = 'nestID')

# how many days with data before?
ds[, days_data_before_nest_visit := first_pairwise_location - first_nest_visit]

# subset pairs with at least one day of data before
dss = ds[days_data_before_nest_visit < -1]

# summary (relative to clutch initiation)
dss |> nrow()
dss[, .(mean = mean(first_nest_visit),
        min = min(first_nest_visit),
        max = max(first_nest_visit))]

# Male and female together
dms = dm[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, date_rel_pair)]
du = unique(dm, by = c('pairID', 'nestID', 'date_rel_pair'))
du = merge(du, dms, by = c('pairID', 'nestID', 'date_rel_pair'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]
d0 = copy(du)

# Proportion of time males at nest
dms = dm[m_at_nest == TRUE, .(N_m_at_nest = .N), by = .(pairID, nestID, date_rel_pair)]
du = unique(dm, by = c('pairID', 'nestID', 'date_rel_pair'))
du = merge(du, dms, by = c('pairID', 'nestID', 'date_rel_pair'), all.x = TRUE)
du[is.na(N_m_at_nest), N_m_at_nest := 0]
du[, m_at_nest_prop := N_m_at_nest / N]
d1 = copy(du)

# Proportion of time females at nest
dms = dm[f_at_nest == TRUE, .(N_f_at_nest = .N), by = .(pairID, nestID, date_rel_pair)]
du = unique(dm, by = c('pairID', 'nestID', 'date_rel_pair'))
du = merge(du, dms, by = c('pairID', 'nestID', 'date_rel_pair'), all.x = TRUE)
du[is.na(N_f_at_nest), N_f_at_nest := 0]
du[, f_at_nest_prop := N_f_at_nest / N]
d2 = copy(du)

# Proportion of time both at nest
dms = dm[both_at_nest == TRUE, .(N_both_at_nest = .N), by = .(pairID, nestID, date_rel_pair)]
du = unique(dm, by = c('pairID', 'nestID', 'date_rel_pair'))
du = merge(du, dms, by = c('pairID', 'nestID', 'date_rel_pair'), all.x = TRUE)
du[is.na(N_both_at_nest), N_both_at_nest := 0]
du[, both_at_nest_prop := N_both_at_nest / N]
d3 = copy(du)

# Proportion of time male alone at nest
dms = dm[m_alone_at_nest == TRUE, .(N_m_alone_at_nest = .N), by = .(pairID, nestID, date_rel_pair)]
du = unique(dm, by = c('pairID', 'nestID', 'date_rel_pair'))
du = merge(du, dms, by = c('pairID', 'nestID', 'date_rel_pair'), all.x = TRUE)
du[is.na(N_m_alone_at_nest), N_m_alone_at_nest := 0]
du[, m_alone_at_nest_prop := N_m_alone_at_nest / N]
d4 = copy(du)

# Proportion of time female alone at nest
dms = dm[f_alone_at_nest == TRUE, .(N_f_alone_at_nest = .N), by = .(pairID, nestID, date_rel_pair)]
du = unique(dm, by = c('pairID', 'nestID', 'date_rel_pair'))
du = merge(du, dms, by = c('pairID', 'nestID', 'date_rel_pair'), all.x = TRUE)
du[is.na(N_f_alone_at_nest), N_f_alone_at_nest := 0]
du[, f_alone_at_nest_prop := N_f_alone_at_nest / N]
d5 = copy(du)

# male alone and not at the nest
d6 = merge(d0[, .(pairID, nestID, ID1, ID2, date_rel_pair, int_prop)], 
           d4[, .(pairID, nestID, ID1, ID2, date_rel_pair, m_alone_at_nest_prop)], 
           by = c('pairID', 'nestID', 'ID1', 'ID2','date_rel_pair'), all.x = TRUE)
d6[, m_alone_prop := 1 - c(int_prop + m_alone_at_nest_prop)]

d6[, total := m_alone_prop + int_prop + m_alone_at_nest_prop]

# female alone and not at the nest
d7 = merge(d0[, .(pairID, nestID, ID1, ID2, date_rel_pair, int_prop)], 
           d5[, .(pairID, nestID, ID1, ID2, date_rel_pair, f_alone_at_nest_prop)], 
           by = c('pairID', 'nestID', 'ID1', 'ID2', 'date_rel_pair'), all.x = TRUE)
d7[, f_alone_prop := 1 - c(int_prop + f_alone_at_nest_prop)]

d7[, total := f_alone_prop + int_prop + f_alone_at_nest_prop]



# merge data
du = rbindlist(list(d0[, .(pairID, nestID, ID1, ID2, date_rel_pair, prop = int_prop, type = 'm_f_together')],
                    d1[, .(pairID, nestID, ID1, ID2, date_rel_pair, prop = m_at_nest_prop, type = 'm_at_nest_prop')],
                    d2[, .(pairID, nestID, ID1, ID2, date_rel_pair, prop = f_at_nest_prop, type = 'f_at_nest_prop')],
                    d3[, .(pairID, nestID, ID1, ID2, date_rel_pair, prop = both_at_nest_prop, type = 'both_at_nest_prop')],
                    d4[, .(pairID, nestID, ID1, ID2, date_rel_pair, prop = m_alone_at_nest_prop, type = 'm_alone_at_nest_prop')],
                    d5[, .(pairID, nestID, ID1, ID2, date_rel_pair, prop = f_alone_at_nest_prop, type = 'f_alone_at_nest_prop')],
                    d6[, .(pairID, nestID, ID1, ID2, date_rel_pair, prop = m_alone_prop, type = 'm_alone_prop')],
                    d7[, .(pairID, nestID, ID1, ID2, date_rel_pair, prop = f_alone_prop, type = 'f_alone_prop')]
))



# descriptive statistic

# day before clutch initiation
du[type == 'f_at_nest_prop' & date_rel_pair == -1, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_at_nest_prop' & date_rel_pair == -1, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100

# at clutch initiation
du[type == 'f_at_nest_prop' & date_rel_pair == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_at_nest_prop' & date_rel_pair == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100

# at clutch last day of egg laying
du[type == 'f_at_nest_prop' & date_rel_pair == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_at_nest_prop' & date_rel_pair == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100


## without partner at nest
# clutch initiation 
du[type == 'f_alone_at_nest_prop' & date_rel_pair == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_alone_at_nest_prop' & date_rel_pair == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100


# at clutch last day of egg laying
du[type == 'f_alone_at_nest_prop' & date_rel_pair == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_alone_at_nest_prop' & date_rel_pair == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100


### without partner away from nest
# clutch initiation 
du[type == 'f_alone_prop' & date_rel_pair == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_alone_prop' & date_rel_pair == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100


# at clutch last day of egg laying
du[type == 'f_alone_prop' & date_rel_pair == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_alone_prop' & date_rel_pair == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100




# pairwise sample size
dus = unique(dp, by = c('pairID', 'nestID', 'date_rel_pair'))
dss = unique(dus[datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
             by = c('nestID', 'date_rel_pair'))
dss = dss[, .N, by = date_rel_pair]
dss


# order
dus = du[type == 'm_at_nest_prop' | type == 'f_at_nest_prop']
dus[, type := factor(type, levels = c('m_at_nest_prop', 'f_at_nest_prop'))]


# Males and females at the nest
pa = 
ggplot() +
  geom_text(data = dss, aes(date_rel_pair, Inf, label = N), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = dus, 
               aes(date_rel_pair, prop, group = interaction(date_rel_pair, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = dus, 
             aes(date_rel_pair, prop, group = interaction(date_rel_pair, type), color = type), 
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c(female_col, male_col), name = '', 
                     labels = c('Male', 'Female'), drop = FALSE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.07, 0.9), legend.background = element_blank(), plot.margin = margin_top) +
  ylab('Proportion of time at nest') +
  xlab('Day relative to clutch initiation (= 0)')

pa


# Males alone and alone at nest
pd = 
  ggplot() +
  geom_text(aes(-5.2, Inf, label = 'Male'), vjust = 1, hjust = 0, size = 3.3) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = du[type == 'm_alone_prop' | type == 'm_alone_at_nest_prop'], 
               aes(date_rel_pair, prop, group = interaction(date_rel_pair, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'm_alone_prop' | type == 'm_alone_at_nest_prop'], 
             aes(date_rel_pair, prop, group = interaction(date_rel_pair, type), color = type), 
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c(male_col, col2), name = '', 
                     labels = c('at nest', 'not at nest'), drop = FALSE) +
  scale_x_continuous(limits = c(-5.4, 5.4), breaks = seq(-5, 5, 1), 
                     labels = c('', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', ''),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.18, 0.9), legend.background = element_blank(), plot.margin = margin_top) +
  ylab('Proportion of time alone') +
  xlab('Day relative to clutch initiation (= 0)')

pd

# Female alone and alone at nest
pe = 
  ggplot() +
  geom_text(aes(-5.2, Inf, label = 'Female'), vjust = 1, hjust = 0, size = 3.3) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = du[type == 'f_alone_prop' | type == 'f_alone_at_nest_prop'], 
               aes(date_rel_pair, prop, group = interaction(date_rel_pair, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'f_alone_prop' | type == 'f_alone_at_nest_prop'], 
             aes(date_rel_pair, prop, group = interaction(date_rel_pair, type), color = type), 
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c(female_col, col2), name = '', 
                     labels = c('at nest', 'not at nest'), drop = FALSE) +
  scale_x_continuous(limits = c(-5.4, 5.4), breaks = seq(-5, 5, 1), 
                     labels = c('', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', ''),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.18, 0.9), legend.background = element_blank(), plot.margin = margin_top) +
  ylab('') +
  xlab('Day relative to clutch initiation (= 0)')

pe


# statistic 

# merge du with clutch initiation date
dni = unique(dp[, .(nestID, initiation_rel)], by = 'nestID')
du = merge(du, dni, by = 'nestID', all.x = TRUE)

### during egg-laying male
dx = du[type == 'm_at_nest_prop' & date_rel_pair >= 0 & date_rel_pair <= 3]

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# beta models only accept proportion in the (0,1) interval
dx[prop == 1, prop := 0.9999]
dx[prop == 0, prop := 0.0001]

# model
m <- glmmTMB(prop ~ date_rel_pair + initiation_rel + 
               (date_rel_pair | nestID),
             family =  beta_family(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')

# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S14. GLMM male at nest 0 to 3')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# descriptive part
x = effect("initiation_rel", m, xlevels = 22) |>
  data.frame() |>
  setDT() |> 
  print() 

# before and after peak
x[initiation_rel <= 0, .(fit = mean(fit), se = mean(se))] * 100
x[initiation_rel >= 0, .(fit = mean(fit), se = mean(se))] * 100


x = effect("date_rel_pair", m, xlevels = 4) |>
  data.frame() |>
  setDT() |> 
  print() 

x[, .(fit = mean(fit), se = mean(se))] * 100


# extract effect from model for plot
e = effect("initiation_rel", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dm[period == "[0,3]"]
dms[, N_nest_season := .N, by = .(pairID, nestID, initiation_rel)]
dus = unique(dms[!is.na(N_nest_season)], by = c('pairID', 'nestID', 'initiation_rel'))
dus[, .(min(N_nest_season), max(N_nest_season))] # check min and max
dus[, .(min(initiation_rel), max(initiation_rel))] # check min and max

dms = dm[m_at_nest == TRUE & period == "[0,3]", 
         .(N_m_at_nest = .N), by = .(pairID, nestID, initiation_rel)]
dus = merge(dus, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
dus[is.na(N_m_at_nest), N_m_at_nest := 0]
dus[, m_at_nest_prop := N_m_at_nest / N_nest_season]
d1 = copy(dus)

# point sizes range
dus[, .(min(N_nest_season), max(N_nest_season))]

pb = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day 0 to 3 (Male)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = dus, aes(initiation_rel, m_at_nest_prop, size = N_nest_season), shape = 1, 
             color = male_col) +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = male_col) +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, 
              fill = male_col) +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.4, 0.4))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = 'none', legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time at nest') +
  xlab('Clutch initiation date (standardized)')

pb



### during egg-laying female
dx = du[type == 'f_at_nest_prop' & date_rel_pair >= 0 & date_rel_pair <= 3]

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# beta models only accept proportion in the (0,1) interval
dx[prop == 1, prop := 0.9999]
dx[prop == 0, prop := 0.0001]

# model
m <- glmmTMB(prop ~ date_rel_pair + poly(initiation_rel, 2) + 
               (date_rel_pair | nestID),
             family =  beta_family(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')

# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S15. GLMM female at nest 0 to 3')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# descriptive part
x = effect("poly(initiation_rel,2)", m, xlevels = 22) |>
  data.frame() |>
  setDT() |> 
  print() 

x[fit > 0.15]

# before and after peak
x[initiation_rel <= 1, .(fit = mean(fit), se = mean(se))] * 100
x[initiation_rel >= 2 & initiation_rel <= 5, .(fit = mean(fit), se = mean(se))] * 100
x[initiation_rel >= 6, .(fit = mean(fit), se = mean(se))] * 100


x = effect("date_rel_pair", m, xlevels = 4) |>
  data.frame() |>
  setDT() |> 
  print() 

x[, .(fit = mean(fit), se = mean(se))] * 100


# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dm[date_rel_pair >= 0 & date_rel_pair <= 3]
dms[, N_nest_season := .N, by = .(pairID, nestID, initiation_rel)]
dus = unique(dms[!is.na(N_nest_season)], by = c('pairID', 'nestID', 'initiation_rel'))
dus[, .(min(N_nest_season), max(N_nest_season))] # check min and max
dus[, .(min(initiation_rel), max(initiation_rel))] # check min and max


dms = dm[f_at_nest == TRUE & date_rel_pair >= 0 & date_rel_pair <= 3, 
         .(N_f_at_nest = .N), by = .(pairID, nestID, initiation_rel)]
dus = merge(dus, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
dus[is.na(N_f_at_nest), N_f_at_nest := 0]
dus[, f_at_nest_prop := N_f_at_nest / N_nest_season]
d1 = copy(du)

# point sizes range
dus[, .(min(N_nest_season), max(N_nest_season))]

pc = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day 0 to 3 (Female)'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = dus, aes(initiation_rel, f_at_nest_prop, size = N_nest_season), shape = 1, 
             color = female_col) +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = female_col) +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, 
              fill = female_col) +
  scale_x_continuous(limits = c(-8, 12), breaks = seq(-8, 12, 1), 
                     labels = c('-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10', '', '12'),
                     expand = expansion(add = c(0.4, 0.4))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0.05, 0.05))) +
  scale_size_area(max_size = 4, breaks=c(100, 300, 500)) +
  theme_classic(base_size = 10) +
  theme(legend.position = 'none', legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('') +
  xlab('Clutch initiation date (standardized)')

pc


# merge plots
pa + pb + pc + pd + pe +
  plot_layout(design = "
  11
  23
  45
") +
  # plot_layout(heights = c(1, 4, 4)) +
  plot_annotation(tag_levels = 'a')

# ggsave('./OUTPUTS/FIGURES/male_female_at_nest.tiff', plot = last_plot(),  width = 177, height = 238,
#        units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' # Mate guarding intensity and extra-pair paternity
#--------------------------------------------------------------------------------------------------------------

# pairwise sample size
du = unique(dp, by = c('pairID', 'nestID', 'date_rel_pair'))
dss = unique(du[anyEPY == FALSE & date_rel_pair >= -10 & date_rel_pair <= 10], 
             by = c('nestID', 'date_rel_pair'))
dss = dss[, .N, by = date_rel_pair]

# pairwise sample size
dss_epy = unique(du[anyEPY == TRUE & date_rel_pair >= -10 & date_rel_pair <= 10], 
             by = c('nestID', 'date_rel_pair'))
dss_epy = dss_epy[, .N, by = date_rel_pair]
dss_epy

# merge 
dss = merge(dss, dss_epy[, .(N_epy = N, date_rel_pair)], by = 'date_rel_pair', all.x = TRUE)
dss[, N_epy_label := paste0(N, '/', N_epy)]

# Proportion of time together breeders
dps = dp[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, date_rel_pair)]
du = unique(dp, by = c('pairID', 'nestID', 'date_rel_pair'))
du = merge(du, dps, by = c('pairID', 'nestID', 'date_rel_pair'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]

# N 
du[, .N, by = pairID] |> nrow()
du[, .N, by = nestID] |> nrow()

du = du[date_rel_pair >= -10 & date_rel_pair <= 10]

# median per day
dmd = du[, .(int_prop_median = median(int_prop)), by = date_rel_pair]

# order
du[, any_EPY_plot := ifelse(anyEPY == TRUE, 'EPY', 'No EPY')]
du[, any_EPY_plot := factor(any_EPY_plot, levels = c('No EPY', 'EPY'))]

### plot proportion of time together 
pa = 
ggplot() +
  geom_text(data = dss, aes(date_rel_pair, Inf, label = N_epy_label), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = du[!is.na(anyEPY)], 
               aes(date_rel_pair, int_prop, group = interaction(date_rel_pair, any_EPY_plot), 
                   color = any_EPY_plot),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[!is.na(anyEPY)], 
             aes(date_rel_pair, int_prop, group = interaction(date_rel_pair, any_EPY_plot), 
                 color = any_EPY_plot), 
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c(col1, col2), name = '', 
                     labels = c('No EPP', 'EPP'), drop = FALSE) +
  scale_x_continuous(limits = c(-5.4, 5.4), breaks = seq(-5, 5, 1), 
                     labels = c('', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', ''),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.18, 0.14), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)')

pa


# descriptive statistic

# fertile period 
du[anyEPY == TRUE & date_rel_pair >= -5 & date_rel_pair <= 2, quantile(int_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[anyEPY == FALSE & date_rel_pair >= -5 & date_rel_pair <= 2, quantile(int_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]

# before clutch initiation period 
du[anyEPY == TRUE & date_rel_pair >= -5 & date_rel_pair <= -1, quantile(int_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[anyEPY == FALSE & date_rel_pair >= -5 & date_rel_pair <= -1, quantile(int_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]

# after clutch initiation period 
du[anyEPY == TRUE & date_rel_pair >= 0 & date_rel_pair <= 3, quantile(int_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[anyEPY == FALSE & date_rel_pair >= 0 & date_rel_pair <= 3, quantile(int_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]


### before clutch initiation
dx = du[!is.na(anyEPY) & period == "[-5,-1]"]
dx[, prop := int_prop]

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# by anyEPY
dxu = unique(dx, by = c('nestID', 'anyEPY')) 
dxu[, .N, by = anyEPY]
dx[, .N, by = anyEPY]

# beta models only accept proportion in the (0,1) interval
dx[prop == 1, prop := 0.9999]
dx[prop == 0, prop := 0.0001]

# model
m <- glmmTMB(prop ~ poly(date_rel_pair, 2) + poly(initiation_rel, 2) + anyEPY + 
               (date_rel_pair | nestID),
             family =  beta_family(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S16. GLMM together and EPY -5 to -1')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# extract effect from model for plot
e1 = effect("anyEPY", m, xlevels = 2) |>
  data.frame() |>
  setDT()



### during egg-laying fertile period
dx = du[!is.na(anyEPY) & date_rel_pair >= 0 & date_rel_pair <= 2]
dx[, prop := int_prop]

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# by anyEPY
dxu = unique(dx, by = c('nestID', 'anyEPY')) 
dxu[, .N, by = anyEPY]
dx[, .N, by = anyEPY]

# beta models only accept proportion in the (0,1) interval
dx[prop == 1, prop := 0.9999]
dx[prop == 0, prop := 0.0001]

# model
m <- glmmTMB(prop ~ date_rel_pair + poly(initiation_rel, 2) + anyEPY + 
               (date_rel_pair | nestID),
             family =  beta_family(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')

# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S17. GLMM together and EPY 0 to 2')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# extract effect from model for plot
e2 = effect("anyEPY", m, xlevels = 2) |>
  data.frame() |>
  setDT()


# plot EPY effect

# merge data
e1[, type := '-5 to -1']
e2[, type := '0 to 2']

e = rbind(e1, e2)

# pairwise sample size
du = unique(dp, by = c('pairID', 'nestID', 'date_rel_pair'))
du = du[date_rel_pair >= -5 & date_rel_pair <= 2]
du[, type := ifelse(date_rel_pair >= 0, '0 to 2', '-5 to -1')]
dsss = unique(du[anyEPY == FALSE], by = c('nestID', 'type'))
dsss = dsss[, .N, by = type]

dsss_epy = unique(du[anyEPY == TRUE], by = c('nestID', 'type'))
dsss_epy = dsss_epy[, .N, by = type]

# merge 
dsss = merge(dsss, dsss_epy[, .(N_epy = N, type)], by = 'type', all.x = TRUE)
dsss[, N_epy_label := paste0(N, '/', N_epy)]

# p values
dsp = data.table(type = c( '-5 to -1', '0 to 2'),
                 p_value = c('p = 0.61', 'p = 0.22'))


p1 =
  ggplot() +
  geom_point(data = e, aes(type, fit, group = interaction(anyEPY, type), color = anyEPY), 
             position = position_dodge(width = 0.5)) +
  geom_linerange(data = e, aes(x = type, ymin = upper, ymax = lower, color = anyEPY), size = 0.3, 
                 position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c(col1, col2), name = '', 
                       labels = c('No EPP', 'EPP'), drop = FALSE) +
  geom_text(data = dsss, aes(type, Inf, label = N_epy_label), vjust = 1, size = sample_size_label) +
  geom_text(data = dsp, aes(type, c(0.8, 0.55), label = p_value), vjust = 1, size = sample_size_label) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                   labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                   expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.3, 0.15), legend.background = element_blank(), 
        plot.margin = unit(c(2, 2, 0, 2), 'pt'), axis.title.x = element_blank()) +
  ylab('Proportion of time together') +
  xlab('')

p1


#--------------------------------------------------------------------------------------------------------------
#' # Split events and extra-pair paternity
#--------------------------------------------------------------------------------------------------------------

# pairwise sample size
du = unique(dp[split == TRUE], by = c('pairID', 'nestID', 'date_rel_pair'))
dss = unique(du[anyEPY == FALSE & date_rel_pair >= -10 & date_rel_pair <= 10], 
             by = c('nestID', 'date_rel_pair'))
dss = dss[, .N, by = date_rel_pair]

# pairwise sample size
dss_epy = unique(du[anyEPY == TRUE & date_rel_pair >= -10 & date_rel_pair <= 10], 
                 by = c('nestID', 'date_rel_pair'))
dss_epy = dss_epy[, .N, by = date_rel_pair]
dss_epy

# merge 
dss = merge(dss, dss_epy[, .(N_epy = N, date_rel_pair)], by = 'date_rel_pair', all.x = TRUE)
dss[, N_epy_label := paste0(N, '/', N_epy)]

# plot splits and merges females 
dus = dusm[!is.na(anyEPY)]

# order
dus[, any_EPY_plot := ifelse(anyEPY == TRUE, 'EPY', 'No EPY')]
dus[, any_EPY_plot := factor(any_EPY_plot, levels = c('No EPY', 'EPY'))]


pb = 
ggplot() +
  geom_text(data = dss, aes(date_rel_pair, Inf, label = N_epy_label), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = dus, 
               aes(date_rel_pair, f_split_prop, group = interaction(date_rel_pair, any_EPY_plot), 
                   color = any_EPY_plot),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = dus, 
             aes(date_rel_pair, f_split_prop, group = interaction(date_rel_pair, any_EPY_plot), 
                 color = any_EPY_plot), 
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c(col1, col2), name = '', 
                     labels = c('No EPP', 'EPP'), drop = FALSE) +
  scale_x_continuous(limits = c(-5.4, 5.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(10.86, 10.12), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion female moves away') +
  xlab('Day relative to clutch initiation (= 0)')


# merge plots
pa + pb + 
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a')

# ggsave('./OUTPUTS/FIGURES/male_female_together_epy.tiff', plot = last_plot(),  width = 177, height = 89, 
#        units = c('mm'), dpi = 'print')



# descriptive statistic

# movements away fertile period 
dus[anyEPY == TRUE & date_rel_pair >= -5 & date_rel_pair <= 2, quantile(f_split_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dus[anyEPY == TRUE & date_rel_pair >= -5 & date_rel_pair <= 2, mean(f_split_prop, na.rm = TRUE)]

dus[anyEPY == FALSE & date_rel_pair >= -5 & date_rel_pair <= 2, quantile(f_split_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dus[anyEPY == FALSE & date_rel_pair >= -5 & date_rel_pair <= 2, mean(f_split_prop, na.rm = TRUE)]

# movements before clutch initiation
dus[anyEPY == TRUE & date_rel_pair >= -5 & date_rel_pair <= -1, quantile(f_split_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dus[anyEPY == TRUE & date_rel_pair >= -5 & date_rel_pair <= -1, mean(f_split_prop, na.rm = TRUE)]

dus[anyEPY == FALSE & date_rel_pair >= -5 & date_rel_pair <= -1, quantile(f_split_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dus[anyEPY == FALSE & date_rel_pair >= -5 & date_rel_pair <= -1, mean(f_split_prop, na.rm = TRUE)]

# movements away after clutch initiation
dus[anyEPY == TRUE & date_rel_pair >= 0 & date_rel_pair <= 3, quantile(f_split_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dus[anyEPY == TRUE & date_rel_pair >= 0 & date_rel_pair <= 3, mean(f_split_prop, na.rm = TRUE)]

dus[anyEPY == FALSE & date_rel_pair >= -5 & date_rel_pair <= 3, quantile(f_split_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dus[anyEPY == FALSE & date_rel_pair >= -5 & date_rel_pair <= 3, mean(f_split_prop, na.rm = TRUE)]

### Models 

# Statistic females moving away

### before clutch initiation
dx = dp[!is.na(anyEPY) & split == TRUE & period == "[-5,-1]"]
dx[, IDsplitting := ifelse(IDsplitting == 'ID1', 0, 1)] # males = 0

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# by anyEPY
dxu = unique(dx, by = c('nestID', 'anyEPY')) 
dxu[, .N, by = anyEPY]
dx[, .N, by = anyEPY]

# model
m <- glmmTMB(IDsplitting ~ date_rel_pair + initiation_rel + anyEPY +
               (date_rel_pair | nestID),
             family =  binomial(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S18. GLMM female moves away and EPY -5 to -1')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# extract effect from model for plot
e1 = effect("anyEPY", m, xlevels = 2) |>
  data.frame() |>
  setDT()


### during egg-laying
dx = dp[!is.na(anyEPY) & split == TRUE & date_rel_pair >= 0 & date_rel_pair <= 2]
dx[, IDsplitting := ifelse(IDsplitting == 'ID1', 0, 1)] # males = 0

# sample size
unique(dx$nestID) |> length() # N nests
unique(dx$ID1) |> length() # N males
unique(dx$ID2) |> length() # N females
dx |> nrow() # N observations

# by anyEPY
dxu = unique(dx, by = c('nestID', 'anyEPY')) 
dxu[, .N, by = anyEPY]
dx[, .N, by = anyEPY]

# model
m <- glmmTMB(IDsplitting ~ date_rel_pair + initiation_rel + anyEPY +
               (date_rel_pair | nestID),
             family =  binomial(link = "logit"), data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

res <-simulateResiduals(m, plot = T)
testDispersion(res) 
acf(resid(m), type = 'partial')


# create clean summary table
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(Parameter = parameter, Estimate = estimate, SE = std.error, Statistic = statistic, p = p.value)]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S19. GLMM female moves away and EPY 0 to 2')) |>  body_add_par('') |> 
  body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# extract effect from model for plot
e2 = effect("anyEPY", m, xlevels = 2) |>
  data.frame() |>
  setDT()



# plot EPY effect

# merge data
e1[, type := '-5 to -1']
e2[, type := '0 to 2']

e = rbind(e1, e2)

# pairwise sample size
du = unique(dp[split == TRUE], by = c('pairID', 'nestID', 'date_rel_pair'))
du = du[date_rel_pair >= -5 & date_rel_pair <= 2]
du[, type := ifelse(date_rel_pair >= 0, '0 to 2', '-5 to -1')]
dsss = unique(du[anyEPY == FALSE], by = c('nestID', 'type'))
dsss = dsss[, .N, by = type]

dsss_epy = unique(du[anyEPY == TRUE], by = c('nestID', 'type'))
dsss_epy = dsss_epy[, .N, by = type]

# merge 
dsss = merge(dsss, dsss_epy[, .(N_epy = N, type)], by = 'type', all.x = TRUE)
dsss[, N_epy_label := paste0(N, '/', N_epy)]

# p values
dsp = data.table(type = c( '-5 to -1', '0 to 2'),
                 p_value = c('p = 0.13', 'p = 0.72'))

p2 =
ggplot() +
  geom_hline(yintercept = 0.5, color = 'black', linetype = 'dashed') +
  geom_point(data = e, aes(type, fit, group = interaction(anyEPY, type), color = anyEPY), 
             position = position_dodge(width = 0.5)) +
  geom_linerange(data = e, aes(x = type, ymin = upper, ymax = lower, color = anyEPY), size = 0.3, 
                 position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c(col1, col2), name = '', 
                     labels = c('No EPP', 'EPP'), drop = FALSE) +
  geom_text(data = dsss, aes(type, Inf, label = N_epy_label), vjust = 1, size = sample_size_label) +
  geom_text(data = dsp, aes(type, c(0.3, 0.35), label = p_value), vjust = 1, size = sample_size_label) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1),
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = 'none', legend.background = element_blank(), 
        plot.margin = unit(c(2, 2, 0, 2), 'pt'), axis.title.x = element_blank()) +
  ylab('Proportion female moves away') +
  xlab('Day relative to clutch initiation (= 0)')

p2


# merge plots
library(grid)
library(gridExtra)

p <- p1 + p2 + 
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a')

gt = patchworkGrob(p)
g = arrangeGrob(gt, bottom = textGrob('          Day relative to clutch initiation (= 0)', gp = gpar(fontsize = 10)))


# ggsave('./OUTPUTS/FIGURES/male_female_together_female_moving_epy.tiff', plot = g,  width = 89, height = 89, 
#        units = c('mm'), dpi = 'print')



#--------------------------------------------------------------------------------------------------------------
#' # Mate guarding intensity of polyandrous females
#--------------------------------------------------------------------------------------------------------------

# Proportion of time together breeders
dps = dp[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, date_)]
du = unique(dp, by = c('pairID', 'nestID', 'date_'))
du = merge(du, dps, by = c('pairID', 'nestID', 'date_'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]

# how many nests with polyandrous first clutch 
du[f_polyandrous_first == TRUE, .N, by = .(nestID)]
du[, f_polyandrous_first_plot := ifelse(f_polyandrous_first == TRUE, '1st mate (polyandrous)', 'Other')]

# point sizes range
du[, .(min(N), max(N))]


# polyandrous females single plots

p1 = 
  ggplot() +
  geom_rect(aes(xmin = as.Date('2019-06-11'), xmax = as.Date('2019-06-14'), ymin = -0.07, ymax = 1.07), 
            fill = egg_laying_color) +
  geom_rect(aes(xmin = as.Date('2019-06-20'), xmax = as.Date('2019-06-23'), ymin = -0.07, ymax = 1.07), 
            fill = egg_laying_color) +
  geom_path(data = du[ID2 == 273145121], aes(date_, int_prop, group = nestID, 
                                             color = f_polyandrous_first_plot), linewidth = 1) +
  scale_color_manual(values = c(col1, col2), name = '',
                     labels = c(bquote("1"^st~ mate), bquote("2"^nd~ mate))) +
  geom_point(data = du[ID2 == 273145121], aes(date_, int_prop, color = f_polyandrous_first_plot, size = N)) +
  scale_size_area(max_size = 4, breaks=c(10, 50, 100)) +
  scale_y_continuous(limits = c(-0.07, 1.07), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  scale_x_date(date_breaks = '2 day', date_labels = '%d',  #  %b to add month
               limits = c(as.Date('2019-06-11'), as.Date('2019-07-01')), guide = "axis_minor" ) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.9, 0.88), legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank(), ggh4x.axis.ticks.length.minor = rel(1)) +
  guides(size = "none") +
  ylab('') +
  xlab('')

p1


p2 = 
  ggplot() +
  geom_rect(aes(xmin = as.Date('2019-06-09'), xmax = as.Date('2019-06-12'), ymin = -0.07, ymax = 1.07), 
            fill = egg_laying_color) +
  geom_rect(aes(xmin = as.Date('2019-06-18'), xmax = as.Date('2019-06-21'), ymin = -0.07, ymax = 1.07), 
            fill = egg_laying_color) +
  geom_path(data = du[ID2 == 270170935 & nestID == 'R405_19' | nestID == 'R406_19'], 
            aes(date_, int_prop, group = nestID, color = f_polyandrous_first_plot), linewidth = 1) +
  scale_color_manual(values = c(col1, col2), name = '',
                     labels = c('1st mate', '2nd mate')) +
  geom_point(data = du[ID2 == 270170935 & nestID == 'R405_19' | nestID == 'R406_19'], 
             aes(date_, int_prop, color = f_polyandrous_first_plot, size = N)) +
  scale_size_area(max_size = 4, breaks=c(10, 50, 100)) +
  scale_y_continuous(limits = c(-0.07, 1.07), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  scale_x_date(date_breaks = '2 day', date_labels = '%d',guide = "axis_minor") +
  theme_classic(base_size = 10) +
  theme(legend.position = 'none', legend.background = element_blank(), plot.margin = margin_) +
  ylab('') +
  xlab('')

p2

p3 = 
  ggplot() +
  geom_rect(aes(xmin = as.Date('2019-06-11'), xmax = as.Date('2019-06-13'), ymin = -0.07, ymax = 1.07), 
            fill = egg_laying_color) +
  geom_rect(aes(xmin = as.Date('2019-06-15'), xmax = as.Date('2019-06-16'), ymin = -0.07, ymax = 1.07), 
            fill = egg_laying_color) +
  geom_path(data = du[ID2 == 273145036], 
            aes(date_, int_prop, group = nestID, color = f_polyandrous_first_plot), linewidth = 1) +
  scale_color_manual(values = c(col1, col2), name = '',
                     labels = c('1st mate', '2nd mate')) +
  geom_point(data = du[ID2 == 273145036], aes(date_, int_prop, color = f_polyandrous_first_plot, size = N)) +
  scale_size_area(max_size = 4, breaks=c(10, 50, 100)) +
  scale_y_continuous(limits = c(-0.07, 1.07), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  scale_x_date(date_breaks = '2 day', date_labels = '%d', guide = "axis_minor") +
  theme_classic(base_size = 10) +
  theme(legend.position = 'none', legend.background = element_blank(), plot.margin = margin_) +
  ylab('') +
  xlab('')
p3

p4 = 
  ggplot() +
  geom_rect(aes(xmin = as.Date('2019-06-06'), xmax = as.Date('2019-06-09'), ymin = -0.07, ymax = 1.07), 
            fill = egg_laying_color) +
  geom_rect(aes(xmin = as.Date('2019-06-15'), xmax = as.Date('2019-06-18'), ymin = -0.07, ymax = 1.07), 
            fill = egg_laying_color) +
  geom_path(data = du[ID2 == 273145109], 
            aes(date_, int_prop, group = nestID, color = f_polyandrous_first_plot), linewidth = 1) +
  scale_color_manual(values = c(col1, col2), name = '',
                     labels = c('1st mate', '2nd mate')) +
  geom_point(data = du[ID2 == 273145109], aes(date_, int_prop, color = f_polyandrous_first_plot, size = N)) +
  scale_size_area(max_size = 4, breaks=c(10, 50, 100)) +
  scale_y_continuous(limits = c(-0.07, 1.07), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  scale_x_date(date_breaks = '2 day', date_labels = '%d', guide = "axis_minor") +
  theme_classic(base_size = 10) +
  theme(legend.position = 'none', legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time together') +
  xlab('Date (June)')

p4


# merge plots
p1 + p2 + p3 + p4 +
  plot_layout(nrow = 4, ncol = 1) +
  plot_annotation(tag_levels = 'a')

# ggsave('./OUTPUTS/FIGURES/MG_over_season_polyandrous_4_females.tiff', plot = last_plot(),  width = 129, 
#        height = 200, units = c('mm'), dpi = 'print')



# save word file
# print(ESM, target = "./OUTPUTS/ESM/ESM_REPH_PAIRS.docx")


#--------------------------------------------------------------------------------------------------------------
#' # Behavioural observations
#--------------------------------------------------------------------------------------------------------------

# Data
d = fread('./DATA/OBSERVATIONS.txt', sep = '\t', header = TRUE) %>% data.table

### Following flights

# Total observations
unique(d, by = 'obs_id') |> nrow()

# How many ID at observations?
d[, N_ind_int := .N, by = obs_id]

# How many observations with at least two IDs and a flight?
d[, any_flights := any(flight %like% 'F'), by = obs_id]

# Total with at least 2 IDs
unique(d[N_ind_int > 1], by = 'obs_id') |> nrow()

# Total with a flight initiator scored
d[, any_F1 := any(flight == 'F1'), by = obs_id]
d[, any_F0 := any(flight == 'F0'), by = obs_id]

# subset relevant data
ds = d[!is.na(sex) & N_ind_int > 1 & any_F1 == TRUE]

# Flight imitator scored
ds[flight == 'F1'] |> nrow()

# Female initiating
ds[sex == 'F' & flight == 'F1'] |> nrow()

# Any male following?
ds[, any_male_F0 := any(flight == 'F0' & sex == 'M'), by = obs_id]
unique(ds[N_ind_int > 1 & sex == 'F' & flight == 'F1' & any_male_F0 == TRUE], by = 'obs_id') |> nrow()

# Male initiating
ds[sex == 'M' & flight == 'F1'] |> nrow()

# Any female following?
ds[, any_female_F0 := any(flight == 'F0' & sex == 'F'), by = obs_id]
unique(ds[N_ind_int > 1 & sex == 'M' & flight == 'F1' & any_female_F0 == TRUE], by = 'obs_id') |> nrow()

# Summary  
round(88/153 * 100, 0) # percent of females initiating when scored
round(72/88 * 100, 0)  # percent male following 

round(65/153 * 100, 0) # percent of males initiating when scored
round(49/65 * 100, 0)  # percent female following 



### Aggressive interactions 

# How many observations with at least two IDs and a aggression?
d[, any_aggres := any(!is.na(aggres)), by = obs_id]

# Total with at least 2 IDs
unique(d[N_ind_int > 1], by = 'obs_id') |> nrow()

# Total with a aggression
unique(d[N_ind_int > 1 & any_aggres == TRUE], by = 'obs_id') |> nrow()
148/2876 * 100
unique(d[N_ind_int > 2 & any_aggres == TRUE], by = 'obs_id') |> nrow()
72/2876 * 100

# Total with a aggression initiator scored
d[, any_1_obs_id := any(aggres %like% '1'), by = obs_id]
d[, any_0_obs_id := any(aggres %like% '0'), by = obs_id]

# for each aggression
d[, any_1 := any(aggres %like% '1'), by = 1:nrow(d)]
d[, any_0 := any(aggres %like% '0'), by = 1:nrow(d)]

# both sexes aggressive in one observation
d[, pair_1 := any(any_1 == TRUE & sex == 'F') & any(any_1 == TRUE & sex == 'M'), by = obs_id]

# Which sex was target?
d[, any_female_0 := any(aggres %like% '0' & sex == 'F'), by = obs_id]
d[, any_male_0 := any(aggres %like% '0' & sex == 'M'), by = obs_id]

# subset relevant data
ds = d[!is.na(sex) & N_ind_int > 1 & any_1_obs_id == TRUE]

# Aggression imitator scored
ds[any_1 == TRUE] |> nrow()

# Male initiating
ds[sex == 'M' & any_1 == TRUE] |> nrow()

# Female initiating
ds[sex == 'F' & any_1 == TRUE] |> nrow()

# Pair initiating
ds[pair_1 == TRUE & any_1 == TRUE] |> nrow()

# Direction of aggression
ds[sex == 'M' & any_1 == TRUE & any_female_0 == TRUE] |> nrow()
ds[sex == 'M' & any_1 == TRUE & any_male_0 == TRUE] |> nrow()

ds[sex == 'F' & any_1 == TRUE & any_female_0 == TRUE] |> nrow()
ds[sex == 'F' & any_1 == TRUE & any_male_0 == TRUE] |> nrow()

# Summary 
round(62/144 * 100, 0) # male initiated
round(40/61 * 100, 0) # percent of male aggression against females
round(25/61 * 100, 0) # percent of male aggression against males

round(82/144 * 100, 0) # female initiated
round(46/80 * 100, 0) # percent of female aggression against females
round(34/80 * 100, 0) # percent of female aggression against males

round(9/144 * 100, 0) # pair initiated


### Copulations

# subset data
ds = d[!is.na(cop)]

# initiators
ds[, initiator := like(cop, '1')]
ds[, .(cop, initiator)]

# N observations 
ds |> nrow()
ds[initiator == TRUE] |> nrow()
ds[sex == 'M' & initiator == TRUE] |> nrow()
ds[sex == 'F' & initiator == TRUE] |> nrow()

# Summary
round(82/113 * 100, 0) # Male initiated copulation
round(31/113 * 100, 0) # Female initiated copulation



# version information
sessionInfo()
