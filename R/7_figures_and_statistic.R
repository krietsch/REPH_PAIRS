#==============================================================================================================
# Figures & Statistics
#==============================================================================================================

# Summary
# 

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr', 
          'stringr', 'ggnewscale', 'doFuture', 'patchwork', 'activity', 'glmmTMB', 'effects', 'broomExtra',
          'flextable', 'officer', 'dplyr', 'performance'), 
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
margin_ = unit(c(2, 2, 2, 2), 'pt')
margin_top = unit(c(2, 2, 6, 2), 'pt')
sample_size_label = 2.5
egg_laying_color = 'grey85'

# subset data 10 days around clutch initiation
dm = dp[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10]
dmr = dr[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10]

# sample size
dm[, .N, pairID] |> nrow()
dm[, .N, nestID] |> nrow()

# start word file for ESM
ESM = read_docx()

# parameter names 
pn = fread("parname;                                                          parameter
            (Intercept);                                                      intercept  
            any_EPYTRUE;                                                      any EPY (yes)
            sexM;                                                             sex (male)
            scale(initiation_rel);                                            clutch initiation relative to season
            poly(initiation_rel, 2)1;                                         clutch initiation relative to season (linear)
            poly(initiation_rel, 2)2;                                         clutch initiation relative to season (quadratic)
            scale(datetime_rel_pair0);                                        clutch initiation relative to first egg
            poly(datetime_rel_pair0, 2)1;                                     clutch initiation relative to first egg (linear)
            poly(datetime_rel_pair0, 2)2;                                     clutch initiation relative to first egg (quadratic)
            year_2019;                                                        year (2019)
            f_polyandrous_firstTRUE;                                          first clutch of polyandrous female (yes)
            typerandomization;                                                data type (random pairs)
            sd__(Intercept);                                                  random intercept
            r2marg;                                                           R² marginal
            r2cond;                                                           R² conditional
            
", sep = ';')

#--------------------------------------------------------------------------------------------------------------
#' Data available relative to clutch initiation
#--------------------------------------------------------------------------------------------------------------

dIDs = unique(dID[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
              by = c('sex', 'nestID', 'datetime_rel_pair0'))
dIDs = dIDs[, .N, by = .(sex, datetime_rel_pair0, both_tagged_overlapping)]
dIDs

# pairwise sample size
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss = unique(du[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
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

# last interaction
dp[interaction == TRUE, max(datetime_rel_pair)]

ggplot(data = dp) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = -Inf, ymax = Inf), fill = egg_laying_color) +
  geom_tile(aes(datetime_rel_pair, nestID, fill = interaction), width = 0.5, show.legend = TRUE) +
  scale_fill_manual(values = c('TRUE' = 'steelblue4', 'FALSE' = 'darkorange'), name = '', 
                    labels = c('Together', 'Not together'), drop = FALSE) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 0.2) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 0.2) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Nest') +
  scale_x_continuous(limits = c(-12, 19), breaks = seq(-12, 19, 1), expand = expansion(add = c(0.2, 0.2))) +
  theme_classic(base_size = 10) + 
  theme(legend.position = c(0.07, 0.95), legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank())

# ggsave('./OUTPUTS/FIGURES/MG_over_season_eachID.tiff', plot = last_plot(),  width = 238, height = 177, units = c('mm'), dpi = 'print')

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


# Male and female together randomized
dms = dmr[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dmr, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]
d1 = copy(du)


# merge data
du = rbindlist(list(d0[, .(pairID, nestID, year_, datetime_rel_pair0, prop = int_prop, Np, type = 'm_f_together')],
                    d1[, .(pairID, nestID, year_, datetime_rel_pair0, prop = int_prop, Np, type = 'm_f_together_randomized')]
                   ))


# Plot time together breeding pairs vs. randomized pairs
pa = 
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = du, 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du, 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), # shape = data_quality
             position=position_jitterdodge(), size = 0.2) +
  # scale_shape_manual(values=c(20, 19)) +
  scale_color_manual(values = c('steelblue4', 'darkorange'), name = '', 
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

# Plot time together breeding pairs split by year

# pairwise sample size 2018
dus = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss_2018 = unique(dus[year_ == 2018 & datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss_2018 = dss_2018[, .N, by = datetime_rel_pair0]

# pairwise sample size 2019
dus = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss_2019 = unique(dus[year_ == 2019 & datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
                 by = c('nestID', 'datetime_rel_pair0'))
dss_2019 = dss_2019[, .N, by = datetime_rel_pair0]

du[, year_ := as.character(year_)]


# merge 
dss = merge(dss_2018[, .(N_2018 = N, datetime_rel_pair0)], 
            dss_2019[, .(N_2019 = N, datetime_rel_pair0)], by = 'datetime_rel_pair0', all = TRUE)
dss[is.na(N_2018), N_2018 := 0]
dss[, N_year_label := paste0(N_2018, '/', N_2019)]

p1 = 
  ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N_year_label), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = du[type == 'm_f_together'], 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, year_), color = year_),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0, position = position_dodge2(preserve = "single")) +
  geom_point(data = du[type == 'm_f_together'], 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, year_), color = year_), # shape = data_quality
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('steelblue4', 'darkorange'), name = '', 
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
di = dn[!is.na(year_) & plot == 'NARL', .(initiation_mean = mean(initiation, na.rm = TRUE)), by = year_]

dn = merge(dn, di, by = 'year_', all.x = TRUE)
dn[, initiation_rel_ := difftime(initiation, initiation_mean, units = 'days') %>% as.numeric %>% round(., 0)]

dns = dn[plot == 'NARL' & !is.na(initiation_rel_)]
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
  scale_color_manual(values = c('steelblue4', 'darkorange'), name = '', 
                     labels = c('2018', '2019'), drop = FALSE) +
  scale_fill_manual(values = c('steelblue4', 'darkorange'), name = '', 
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


# ggsave('./OUTPUTS/FIGURES/prop_time_together_season_year.tiff', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')


### Models 

# Statistic together differences between years

# before clutch initiation
dx = dm[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
dx[, year_ := as.character(year_)]

m <- glmmTMB(interaction ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) + year_ + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S1. GLMM together and year -5 to -1')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# during egg-laying
dx = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dx[, year_ := as.character(year_)]

m <- glmmTMB(interaction ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) + year_ + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S2. GLMM together and year 0 to 3')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# Statistic together

# before clutch initiation
dx = dm[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
dx[, year_ := as.character(year_)]

m <- glmmTMB(interaction ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
# y[parameter %in% c('intercept', 'relative day', 'split (after)'), p := NA]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S3. GLMM together -5 to -1')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# descriptive part
x = effect("poly(initiation_rel,2)", m, xlevels = 20) |>
  data.frame() |>
  setDT() |> 
  print()

# more than 95% together 
x[fit > 0.95] 

# before and after peak
x[initiation_rel < -1, .(fit = mean(fit), se = mean(se))] * 100
x[initiation_rel > 8, .(fit = mean(fit), se = mean(se))] * 100


x = effect("poly(datetime_rel_pair0,2)", m, xlevels = 5) |>
  data.frame() |>
  setDT() |> 
  print()

x[, .(fit = mean(fit), se = mean(se))] * 100

# pairs with more than 95% together
d0[datetime_rel_pair0 == -2 & int_prop > 0.95] |> nrow()



# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dm[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
dms = dms[, N_ini := .N, by = .(pairID, nestID)]
du = unique(dms, by = c('pairID', 'nestID', 'initiation_rel'))
du = du[!is.na(N_ini)]
du[, .(min(N_ini), max(N_ini))] # check min and max
du[, .(min(initiation_rel), max(initiation_rel))] # check min and max

dms = dms[interaction == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, .(N_int = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N_ini]
d0 = copy(du)

# point sizes range
du[, .(min(N_ini), max(N_ini))]

pb = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day -5 to -1'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
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


# during egg-laying
dx = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dx[, year_ := as.character(year_)]

m <- glmmTMB(interaction ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S4. GLMM together 0 to 3')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# descriptive part
effect("poly(datetime_rel_pair0,2)", m, xlevels = 4) |>
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
dms = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dms = dms[, N_ini := .N, by = .(pairID, nestID)]
du = unique(dms, by = c('pairID', 'nestID', 'initiation_rel'))
du = du[!is.na(N_ini)]
du[, .(min(N_ini), max(N_ini))] # check min and max
du[, .(min(initiation_rel), max(initiation_rel))] # check min and max

dms = dm[interaction == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, .(N_int = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N_ini]
d0 = copy(du)

# point sizes range
du[, .(min(N_ini), max(N_ini))]

pc = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day 0 to 3'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
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

# ggsave('./OUTPUTS/FIGURES/male_female_together.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')



# Statistic together breeding pairs vs. random pairs

# assign random pairs nestID as pairID
dmr[, nestID := pairID]

# merge data
dmx = rbindlist(list(dm[, .(pairID, nestID, interaction, initiation_rel, datetime_rel_pair0, type)],
                     dmr[, .(pairID, nestID, interaction, initiation_rel, datetime_rel_pair0, type)]))


# model before clutch initiation
dx = dmx[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 3]


m <- glmmTMB(interaction ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) + type + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)


# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table()


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S5. GLMM together vs. randomized -5 to -1')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# extract effect from model for plot
effect("type", m, xlevels = 2) |>
  data.frame() |>
  setDT() |> 
  print()


# model during laying
dx = dmx[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]


m <- glmmTMB(interaction ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) + type + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)


# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table()


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S6. GLMM together vs. randomized 0 to 3')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# extract effect from model for plot
effect("type", m, xlevels = 2) |>
  data.frame() |>
  setDT() |> 
  print()


# model after laying
dx = dmx[datetime_rel_pair0 >= 4 & datetime_rel_pair0 <= 10]


m <- glmmTMB(interaction ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) + type + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)


# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table()


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S7. GLMM together vs. randomized 4 to 10')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# extract effect from model for plot
effect("type", m, xlevels = 2) |>
  data.frame() |>
  setDT() |> 
  print()


#--------------------------------------------------------------------------------------------------------------
#' Female moves away
#--------------------------------------------------------------------------------------------------------------

# define who flies away or joins  

# subset data
dm = dp[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10]

# check sex
dp[, .N, .(sex1)]
dp[, .N, .(sex2)]

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
du = unique(dm[split == TRUE], by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_split), N_split := 0]
du[, split_prop := N_split / N]
d1 = copy(du)

dm = merge(dm, du[, .(pairID, nestID, datetime_rel_pair0, N_split)], by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)


# Times male split
dms = dm[split == TRUE & ID_splitting == 'ID1', .(N_m_split = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm[split == TRUE], by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_m_split), N_m_split := 0]
du[, m_split_prop := N_m_split / N_split]
d2 = copy(du)

dm = merge(dm, du[, .(pairID, nestID, datetime_rel_pair0, N_m_split)], by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)


# Times females split
dms = dm[split == TRUE & ID_splitting == 'ID2', .(N_f_split = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm[split == TRUE], by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_f_split), N_f_split := 0]
du[, f_split_prop := N_f_split /N_split]
d3 = copy(du)

dm = merge(dm, du[, .(pairID, nestID, datetime_rel_pair0, N_f_split)], by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)


# Proportion of merge events
dms = dm[merge == TRUE, .(N_merge = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm[split == TRUE], by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_merge), N_merge := 0]
du[, merge_prop := N_merge / N]
d4 = copy(du)

dm = merge(dm, du[, .(pairID, nestID, datetime_rel_pair0, N_merge)], by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)


# Times males merge
dms = dm[merge == TRUE & ID_merging == 'ID1', .(N_m_merge = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm[split == TRUE], by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_m_merge), N_m_merge := 0]
du[, m_merge_prop := N_m_merge /N_merge]
d5 = copy(du)

# Times females merge
dms = dm[merge == TRUE & ID_merging == 'ID2', .(N_f_merge = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm[split == TRUE], by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_f_merge), N_f_merge := 0]
du[, f_merge_prop := N_f_merge /N_merge]
d6 = copy(du)



# merge data
du = rbindlist(list(d0[, .(pairID, nestID, datetime_rel_pair0, prop = int_prop, type = 'm_f_together')],
                    d1[, .(pairID, nestID, datetime_rel_pair0, prop = split_prop, type = 'split_prop')],
                    d2[, .(pairID, nestID, datetime_rel_pair0, prop = m_split_prop, type = 'm_split_prop')],
                    d3[, .(pairID, nestID, datetime_rel_pair0, prop = f_split_prop, type = 'f_split_prop')],
                    d4[, .(pairID, nestID, datetime_rel_pair0, prop = merge_prop, type = 'merge_prop')],
                    d5[, .(pairID, nestID, datetime_rel_pair0, prop = m_merge_prop, type = 'm_merge_prop')],
                    d6[, .(pairID, nestID, datetime_rel_pair0, prop = f_merge_prop, type = 'f_merge_prop')]
))


# merge with EPY data
duu = unique(dm, by = c('nestID'))
dusm = merge(du, duu[, .(nestID, any_EPY)], by = 'nestID', all.x = TRUE)

# proportion of all observations
d0 = data.table(datetime_rel_pair0 = seq(-10, 10, 1))
d1 = dm[split == TRUE, .(N_split = .N), by = .(datetime_rel_pair0)]
d2 = dm[split == TRUE & ID_splitting == 'ID2', .(N_f_split = .N), by = .(datetime_rel_pair0)]
d3 = dm[merge == TRUE, .(N_merge = .N), by = .(datetime_rel_pair0)]
d4 = dm[merge == TRUE & ID_merging == 'ID2', .(N_f_merge = .N), by = .(datetime_rel_pair0)]

d0 = merge(d0, d1, by = 'datetime_rel_pair0', all.x = TRUE)
d0 = merge(d0, d2, by = 'datetime_rel_pair0', all.x = TRUE)
d0 = merge(d0, d3, by = 'datetime_rel_pair0', all.x = TRUE)
d0 = merge(d0, d4, by = 'datetime_rel_pair0', all.x = TRUE)

d0[, f_split_prop := N_f_split / N_split]
d0[is.na(f_split_prop), f_split_prop := 0]
d0[, f_merge_prop := N_f_merge / N_merge]
d0[is.na(f_merge_prop), f_merge_prop := 0]

dua = rbindlist(list(d0[, .(datetime_rel_pair0, prop = f_split_prop, type = 'f_split_prop')],
                     d0[, .(datetime_rel_pair0, prop = f_merge_prop, type = 'f_merge_prop')]
                     ))



# pairwise sample size
dus = unique(dp[split == TRUE], by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss = unique(dus[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss = dss[, .N, by = datetime_rel_pair0]
dss



# plot splits females 
dus = du[type == 'f_split_prop']

pa = 
  ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = dus, 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0)), colour = 'steelblue4',
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_jitter(data = dus, aes(datetime_rel_pair0, prop), colour = 'steelblue4', size = 0.5) + 
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

# before clutch initiation
dx = dm[split == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
dx[, early := ifelse(initiation_rel <= -2,  TRUE, FALSE)]
dx[, ID_splitting := ifelse(ID_splitting == 'ID1', 0, 1)] # males = 0

m <- glmmTMB(ID_splitting ~ scale(datetime_rel_pair0) + scale(initiation_rel) + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)


# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S8. GLMM female moving away -5 to -1')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# descriptive part
x = effect("scale(datetime_rel_pair0)", m, xlevels = 4) |>
  data.frame() |>
  setDT() |> 
  print() 

x[, .(fit = mean(fit), se = mean(se))] * 100

x = effect("scale(initiation_rel)", m, xlevels = 16) |>
  data.frame() |>
  setDT() |> 
  print() 

# before and after peak
x[initiation_rel <= -2, .(fit = mean(fit), se = mean(se))] * 100
x[initiation_rel >= -1, .(fit = mean(fit), se = mean(se))] * 100

# extract effect from model for plot
e = effect("scale(initiation_rel)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dm[split == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
dms[, N_splits_season := .N, by = .(pairID, nestID, initiation_rel)]
du = unique(dms[!is.na(N_splits_season)], by = c('pairID', 'nestID', 'initiation_rel'))
du[, .(min(N_splits_season), max(N_splits_season))] # check min and max
du[, .(min(initiation_rel), max(initiation_rel))] # check min and max


# Proportion of split events
dms = dm[split == TRUE & ID_splitting == 'ID2' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, 
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
  geom_point(data = du, aes(initiation_rel, split_prop, size = N_splits_season), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
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



#  during egg-laying
dx = dm[split == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dx[, early := ifelse(initiation_rel <= -2,  TRUE, FALSE)]
dx[, ID_splitting := ifelse(ID_splitting == 'ID1', 0, 1)] # males = 0

m <- glmmTMB(ID_splitting ~ scale(datetime_rel_pair0) + scale(initiation_rel) + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)



# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S9. GLMM female moving away 0 to 3')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# descriptive part
x = effect("scale(initiation_rel)", m, xlevels = 16) |>
  data.frame() |>
  setDT() |> 
  print() 

# before and after peak
x[initiation_rel <= -2, .(fit = mean(fit), se = mean(se))] * 100
x[initiation_rel >= -1, .(fit = mean(fit), se = mean(se))] * 100


x = effect("scale(datetime_rel_pair0)", m, xlevels = 4) |>
  data.frame() |>
  setDT() |> 
  print() 

x[, .(fit = mean(fit), se = mean(se))] * 100


# extract effect from model for plot
e = effect("scale(initiation_rel)", m, xlevels = 100) |>
  data.frame() |>
  setDT()

# data for points 
dms = dm[split == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dms[, N_splits_season := .N, by = .(pairID, nestID, initiation_rel)]
du = unique(dms[!is.na(N_splits_season)], by = c('pairID', 'nestID', 'initiation_rel'))
du[, .(min(N_splits_season), max(N_splits_season))] # check min and max
du[, .(min(initiation_rel), max(initiation_rel))] # check min and max


# Proportion of split events
dms = dm[split == TRUE & ID_splitting == 'ID2' & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, 
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
  geom_point(data = du, aes(initiation_rel, split_prop, size = N_splits_season), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
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

# ggsave('./OUTPUTS/FIGURES/female_moving_away.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')

#--------------------------------------------------------------------------------------------------------------
#' Number moved away by sex
#--------------------------------------------------------------------------------------------------------------

# subset data
dms = unique(dm, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dms[is.na(N_splits),  N_splits := 0]
dms[is.na(N_m_split), N_m_split := 0]
dms[is.na(N_f_split), N_f_split := 0]

# pairwise sample size
dus = unique(dp[split == TRUE], by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss = unique(dus[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss = dss[, .N, by = datetime_rel_pair0]
dss


ggplot() +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.3, ymax = 16.1), fill = egg_laying_color) +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = sample_size_label) +
  geom_boxplot(data = dms, 
               aes(datetime_rel_pair0, N_splits, group = interaction(datetime_rel_pair0)),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0, color = 'steelblue4') +
  geom_point(data = dms, 
             aes(datetime_rel_pair0, N_splits, group = interaction(datetime_rel_pair0)), color = 'steelblue4', 
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
  ylab('Number of separation movements') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/male_female_split_events_number.tiff', plot = last_plot(),  width = 177, height = 89, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' Distance moved away by sex
#--------------------------------------------------------------------------------------------------------------

# moved away
dms = dm[split == TRUE]

# merge male and female data for plot
dms_m = dms[ID_splitting == 'ID1', .(pairID, nestID, datetime_rel_pair0, initiation_rel, sex = sex1, 
                                     split_distance = distance1_before)]
dms_f = dms[ID_splitting == 'ID2', .(pairID, nestID, datetime_rel_pair0, initiation_rel, sex = sex2, 
                                     split_distance = distance2_before)]

dms = rbindlist(list(dms_m, dms_f))



# pairwise sample size
du = unique(dm[split == TRUE], by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss_m = unique(du[ID_splitting == 'ID1' & datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss_m = dss_m[, .N, by = datetime_rel_pair0]

# pairwise sample size
dss_f = unique(du[ID_splitting == 'ID2' & datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
               by = c('nestID', 'datetime_rel_pair0'))
dss_f = dss_f[, .N, by = datetime_rel_pair0]

# merge 
dss = merge(dss_m[, .(N_m = N, datetime_rel_pair0)], dss_f[, .(N_f = N, datetime_rel_pair0)], by = 'datetime_rel_pair0', all.x = TRUE)
dss[, N_label := paste0(N_f, '/', N_m)]


# adjust distance above 1000 m
dms[, split_distance1000 := split_distance]
dms[split_distance > 1000, split_distance1000 := 1000]

ggplot() +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1005), fill = egg_laying_color) +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N_label), vjust = 1, size = sample_size_label) +
  geom_boxplot(data = dms, 
               aes(datetime_rel_pair0, split_distance1000, group = interaction(datetime_rel_pair0, sex), 
                   color = sex),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = dms, 
             aes(datetime_rel_pair0, split_distance1000, group = interaction(datetime_rel_pair0, sex), 
                 color = sex), position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('firebrick3', 'steelblue4'), name = '',
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


# ggsave('./OUTPUTS/FIGURES/male_female_split_events_distance_moved.tiff', plot = last_plot(),  width = 177, height = 89, units = c('mm'), dpi = 'print')

# model before clutch initiation
dx = dms[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]

m <- glmmTMB(split_distance ~ sex + scale(datetime_rel_pair0) + scale(initiation_rel) + (datetime_rel_pair0 | nestID),
               family = gaussian, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)


# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S10. GLMM split distance -5 to -1')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

x = effect("sex", m, xlevels = 2) |>
  data.frame() |>
  setDT() |> 
  print()

x = effect("scale(datetime_rel_pair0)", m, xlevels = 9) |>
  data.frame() |>
  setDT() |> 
  print()


# model during clutch initiation
dx = dms[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]

m <- glmmTMB(split_distance ~ sex + scale(datetime_rel_pair0) + scale(initiation_rel) + (datetime_rel_pair0 | nestID),
             family = gaussian, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)


# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S10. GLMM split distance 0 to 3')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

x = effect("sex", m, xlevels = 2) |>
  data.frame() |>
  setDT() |> 
  print()

x = effect("scale(datetime_rel_pair0)", m, xlevels = 9) |>
  data.frame() |>
  setDT() |> 
  print()

# before and after clutch initiation
x[datetime_rel_pair0 <= -1, .(fit = mean(fit), se = mean(se))]
x[datetime_rel_pair0 >= 0, .(fit = mean(fit), se = mean(se))]


# before and after clutch initiation
x[datetime_rel_pair0 <= -1, .(fit = mean(fit), se = mean(se))]
x[datetime_rel_pair0 >= 0, .(fit = mean(fit), se = mean(se))]


#--------------------------------------------------------------------------------------------------------------
#' Nest attendance by sex
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



# descriptive statistic

# day before clutch initiation
du[type == 'f_at_nest_prop' & datetime_rel_pair0 == -1, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_at_nest_prop' & datetime_rel_pair0 == -1, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100

# at clutch initiation
du[type == 'f_at_nest_prop' & datetime_rel_pair0 == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_at_nest_prop' & datetime_rel_pair0 == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100

# at clutch last day of egg laying
du[type == 'f_at_nest_prop' & datetime_rel_pair0 == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_at_nest_prop' & datetime_rel_pair0 == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100


## without partner at nest
# clutch initiation 
du[type == 'f_alone_at_nest_prop' & datetime_rel_pair0 == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_alone_at_nest_prop' & datetime_rel_pair0 == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100


# at clutch last day of egg laying
du[type == 'f_alone_at_nest_prop' & datetime_rel_pair0 == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_alone_at_nest_prop' & datetime_rel_pair0 == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100


### without partner away from nest
# clutch initiation 
du[type == 'f_alone_prop' & datetime_rel_pair0 == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_alone_prop' & datetime_rel_pair0 == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100


# at clutch last day of egg laying
du[type == 'f_alone_prop' & datetime_rel_pair0 == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100
du[type == 'm_alone_prop' & datetime_rel_pair0 == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)] * 100




# pairwise sample size
dus = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss = unique(dus[datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss = dss[, .N, by = datetime_rel_pair0]
dss


# order
dus = du[type == 'm_at_nest_prop' | type == 'f_at_nest_prop']
dus[, type := factor(type, levels = c('m_at_nest_prop', 'f_at_nest_prop'))]


# Males and females at the nest
pa = 
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = dus, 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = dus, 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('steelblue4', 'firebrick3'), name = '', 
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
pb = 
  ggplot() +
  geom_text(aes(-5.2, Inf, label = 'Male'), vjust = 1, hjust = 0, size = 3.3) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = du[type == 'm_alone_prop' | type == 'm_alone_at_nest_prop'], 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'm_alone_prop' | type == 'm_alone_at_nest_prop'], 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('steelblue4', 'darkorange'), name = '', 
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

pb

# Female alone and alone at nest
pc = 
ggplot() +
  geom_text(aes(-5.2, Inf, label = 'Female'), vjust = 1, hjust = 0, size = 3.3) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = du[type == 'f_alone_prop' | type == 'f_alone_at_nest_prop'], 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'f_alone_prop' | type == 'f_alone_at_nest_prop'], 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('firebrick3', 'darkorange'), name = '', 
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

pc


# statistic 

# during egg-laying male
dx = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
m <- glmmTMB(m_at_nest ~ poly(initiation_rel, 2) + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S11. GLMM male at nest 0 to 3')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# descriptive part
x = effect("poly(initiation_rel,2)", m, xlevels = 22) |>
  data.frame() |>
  setDT() |> 
  print() 

# before and after peak
x[initiation_rel <= 0, .(fit = mean(fit), se = mean(se))] * 100
x[initiation_rel >= 0, .(fit = mean(fit), se = mean(se))] * 100


x = effect("scale(datetime_rel_pair0)", m, xlevels = 4) |>
  data.frame() |>
  setDT() |> 
  print() 

x[, .(fit = mean(fit), se = mean(se))] * 100


# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dms[, N_nest_season := .N, by = .(pairID, nestID, initiation_rel)]
du = unique(dms[!is.na(N_nest_season)], by = c('pairID', 'nestID', 'initiation_rel'))
du[, .(min(N_nest_season), max(N_nest_season))] # check min and max
du[, .(min(initiation_rel), max(initiation_rel))] # check min and max

dms = dm[m_at_nest == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, 
         .(N_m_at_nest = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_m_at_nest), N_m_at_nest := 0]
du[, m_at_nest_prop := N_m_at_nest / N_nest_season]
d1 = copy(du)

# point sizes range
du[, .(min(N_nest_season), max(N_nest_season))]

pd = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day 0 to 3'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, m_at_nest_prop, size = N_nest_season), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
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

pd



# during egg-laying female
dx = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
m <- glmmTMB(f_at_nest ~ poly(initiation_rel, 2) + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)

# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S12. GLMM female at nest 0 to 3')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# descriptive part
x = effect("scale(initiation_rel)", m, xlevels = 22) |>
  data.frame() |>
  setDT() |> 
  print() 

x[fit > 0.15]

# before and after peak
x[initiation_rel <= 1, .(fit = mean(fit), se = mean(se))] * 100
x[initiation_rel >= 2 & initiation_rel <= 5, .(fit = mean(fit), se = mean(se))] * 100
x[initiation_rel >= 6, .(fit = mean(fit), se = mean(se))] * 100


x = effect("scale(datetime_rel_pair0)", m, xlevels = 4) |>
  data.frame() |>
  setDT() |> 
  print() 

x[, .(fit = mean(fit), se = mean(se))] * 100


# extract effect from model for plot
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dms[, N_nest_season := .N, by = .(pairID, nestID, initiation_rel)]
du = unique(dms[!is.na(N_nest_season)], by = c('pairID', 'nestID', 'initiation_rel'))
du[, .(min(N_nest_season), max(N_nest_season))] # check min and max
du[, .(min(initiation_rel), max(initiation_rel))] # check min and max


dms = dm[f_at_nest == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, 
         .(N_f_at_nest = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_f_at_nest), N_f_at_nest := 0]
du[, f_at_nest_prop := N_f_at_nest / N_nest_season]
d1 = copy(du)

# point sizes range
du[, .(min(N_nest_season), max(N_nest_season))]

pe = 
  ggplot() +
  geom_text(aes(-7.8, Inf, label = 'Day 0 to 3'), vjust = 1, hjust = 0, size = 3.3) +
  geom_point(data = du, aes(initiation_rel, f_at_nest_prop, size = N_nest_season), shape = 1, color = 'firebrick3') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'firebrick3') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'firebrick3') +
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

pe





# merge plots
pa + pb + pc + pd + pe +
  plot_layout(design = "
  11
  45
  23
") +
  # plot_layout(heights = c(1, 4, 4)) +
  plot_annotation(tag_levels = 'a')

# ggsave('./OUTPUTS/FIGURES/male_female_at_nest.tiff', plot = last_plot(),  width = 177, height = 238, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' Mate guarding effect on extra-pair paternity
#--------------------------------------------------------------------------------------------------------------

# pairwise sample size
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss = unique(du[any_EPY == FALSE & datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss = dss[, .N, by = datetime_rel_pair0]

# pairwise sample size
dss_epy = unique(du[any_EPY == TRUE & datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss_epy = dss_epy[, .N, by = datetime_rel_pair0]
dss_epy

# merge 
dss = merge(dss, dss_epy[, .(N_epy = N, datetime_rel_pair0)], by = 'datetime_rel_pair0', all.x = TRUE)
dss[, N_epy_label := paste0(N, '/', N_epy)]

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
du[, any_EPY_plot := factor(any_EPY_plot, levels = c('No EPY', 'EPY'))]

### plot proportion of time together 
pa = 
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N_epy_label), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = du[!is.na(any_EPY)], 
               aes(datetime_rel_pair0, int_prop, group = interaction(datetime_rel_pair0, any_EPY_plot), color = any_EPY_plot),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[!is.na(any_EPY)], 
             aes(datetime_rel_pair0, int_prop, group = interaction(datetime_rel_pair0, any_EPY_plot), color = any_EPY_plot), 
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('steelblue4', 'darkorange'), name = '', 
                     labels = c('No EPY', 'EPY'), drop = FALSE) +
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
du[any_EPY == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2, quantile(int_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[any_EPY == FALSE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2, quantile(int_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]

# before clutch initiation period 
du[any_EPY == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, quantile(int_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[any_EPY == FALSE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, quantile(int_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]

# after clutch initiation period 
du[any_EPY == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, quantile(int_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[any_EPY == FALSE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, quantile(int_prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]



# before clutch initiation
dx = dm[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]


m <- glmmTMB(interaction ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) + any_EPY + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)


# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S13. GLMM together and EPY -5 to -1')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# extract effect from model for plot
e1 = effect("any_EPY", m, xlevels = 2) |>
  data.frame() |>
  setDT()


# model before clutch initiation
dx = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 2]

m <- glmmTMB(interaction ~ scale(datetime_rel_pair0) + poly(initiation_rel, 2) + any_EPY + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)


# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S14. GLMM together and EPY 0 to 2')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# extract effect from model for plot
e2 = effect("any_EPY", m, xlevels = 2) |>
  data.frame() |>
  setDT()


# plot EPY effect

# merge data
e1[, type := '-5 to -1']
e2[, type := '0 to 2']

e = rbind(e1, e2)

# pairwise sample size
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = du[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2]
du[, type := ifelse(datetime_rel_pair0 >= 0, '0 to 2', '-5 to -1')]
dsss = unique(du[any_EPY == FALSE], by = c('nestID', 'type'))
dsss = dsss[, .N, by = type]

dsss_epy = unique(du[any_EPY == TRUE], by = c('nestID', 'type'))
dsss_epy = dsss_epy[, .N, by = type]

# merge 
dsss = merge(dsss, dsss_epy[, .(N_epy = N, type)], by = 'type', all.x = TRUE)
dsss[, N_epy_label := paste0(N, '/', N_epy)]


p1 =
  ggplot() +
  geom_point(data = e, aes(type, fit, group = interaction(any_EPY, type), color = any_EPY), position = position_dodge(width = 0.5)) +
  geom_linerange(data = e, aes(x = type, ymin = upper, ymax = lower, color = any_EPY), size = 0.3, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c('steelblue4', 'darkorange'), name = '', 
                       labels = c('No EPY', 'EPY'), drop = FALSE) +
  geom_text(data = dsss, aes(type, Inf, label = N_epy_label), vjust = 1, size = sample_size_label) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                   labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                   expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.3, 0.15), legend.background = element_blank(), plot.margin = unit(c(2, 2, 0, 2), 'pt'), axis.title.x = element_blank()) +
  ylab('Proportion of time together') +
  xlab('')

p1


#--------------------------------------------------------------------------------------------------------------
#' Split events and extra-pair paternity
#--------------------------------------------------------------------------------------------------------------

# pairwise sample size
du = unique(dp[split == TRUE], by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss = unique(du[any_EPY == FALSE & datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss = dss[, .N, by = datetime_rel_pair0]

# pairwise sample size
dss_epy = unique(du[any_EPY == TRUE & datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10], 
                 by = c('nestID', 'datetime_rel_pair0'))
dss_epy = dss_epy[, .N, by = datetime_rel_pair0]
dss_epy

# merge 
dss = merge(dss, dss_epy[, .(N_epy = N, datetime_rel_pair0)], by = 'datetime_rel_pair0', all.x = TRUE)
dss[, N_epy_label := paste0(N, '/', N_epy)]


# data from split events 
dusm

# plot splits and merges females 
dus = dusm[!is.na(any_EPY) & type == 'f_split_prop']

# order
dus[, any_EPY_plot := ifelse(any_EPY == TRUE, 'EPY', 'No EPY')]
dus[, any_EPY_plot := factor(any_EPY_plot, levels = c('No EPY', 'EPY'))]



pb = 
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N_epy_label), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = egg_laying_color) +
  geom_boxplot(data = dus, 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, any_EPY_plot), color = any_EPY_plot),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = dus, 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, any_EPY_plot), color = any_EPY_plot), 
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('steelblue4', 'darkorange'), name = '', 
                     labels = c('No EPY', 'EPY'), drop = FALSE) +
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

# ggsave('./OUTPUTS/FIGURES/male_female_together_epy.tiff', plot = last_plot(),  width = 177, height = 89, units = c('mm'), dpi = 'print')




# descriptive statistic

# movements away fertile period 
dus[any_EPY == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dus[any_EPY == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2, mean(prop, na.rm = TRUE)]

dus[any_EPY == FALSE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dus[any_EPY == FALSE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2, mean(prop, na.rm = TRUE)]

# movements before clutch initiation
dus[any_EPY == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dus[any_EPY == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, mean(prop, na.rm = TRUE)]

dus[any_EPY == FALSE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dus[any_EPY == FALSE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, mean(prop, na.rm = TRUE)]

# movements away after clutch initiation
dus[any_EPY == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dus[any_EPY == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, mean(prop, na.rm = TRUE)]

dus[any_EPY == FALSE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dus[any_EPY == FALSE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 3, mean(prop, na.rm = TRUE)]



# before clutch initiation
dx = dm[split == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
dx[, ID_splitting := ifelse(ID_splitting == 'ID1', 0, 1)]

m <- glmmTMB(ID_splitting ~ scale(datetime_rel_pair0) + scale(initiation_rel) + any_EPY + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)


# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S15. GLMM female moves away and EPY -5 to -1')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# extract effect from model for plot
e1 = effect("any_EPY", m, xlevels = 2) |>
  data.frame() |>
  setDT()


# during clutch initiation (fertile period)
dx = dm[split == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 2]
dx[, ID_splitting := ifelse(ID_splitting == 'ID1', 0, 1)] # males = 0

m <- glmmTMB(ID_splitting ~ scale(datetime_rel_pair0) + scale(initiation_rel) + any_EPY + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)


# create clean summary table -----
y = tidy(m) |> data.table()
x = r2(m) |> data.table() 


setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word -----
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S16. GLMM female moves away and EPY 0 to 2')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# extract effect from model for plot
e2 = effect("any_EPY", m, xlevels = 2) |>
  data.frame() |>
  setDT()



# plot EPY effect

# merge data
e1[, type := '-5 to -1']
e2[, type := '0 to 2']

e = rbind(e1, e2)

# pairwise sample size
du = unique(dp[split == TRUE], by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = du[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2]
du[, type := ifelse(datetime_rel_pair0 >= 0, '0 to 2', '-5 to -1')]
dsss = unique(du[any_EPY == FALSE], by = c('nestID', 'type'))
dsss = dsss[, .N, by = type]

dsss_epy = unique(du[any_EPY == TRUE], by = c('nestID', 'type'))
dsss_epy = dsss_epy[, .N, by = type]

# merge 
dsss = merge(dsss, dsss_epy[, .(N_epy = N, type)], by = 'type', all.x = TRUE)
dsss[, N_epy_label := paste0(N, '/', N_epy)]


p2 =
ggplot() +
  geom_hline(yintercept = 0.5, color = 'black', linetype = 'dashed') +
  geom_point(data = e, aes(type, fit, group = interaction(any_EPY, type), color = any_EPY), position = position_dodge(width = 0.5)) +
  geom_linerange(data = e, aes(x = type, ymin = upper, ymax = lower, color = any_EPY), size = 0.3, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c('steelblue4', 'darkorange'), name = '', 
                     labels = c('No EPY', 'EPY'), drop = FALSE) +
  geom_text(data = dsss, aes(type, Inf, label = N_epy_label), vjust = 1, size = sample_size_label) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1),
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = 'none', legend.background = element_blank(), plot.margin = unit(c(2, 2, 0, 2), 'pt'), axis.title.x = element_blank()) +
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


# ggsave('./OUTPUTS/FIGURES/male_female_together_female_moving_epy.tiff', plot = g,  width = 89, height = 89, units = c('mm'), dpi = 'print')



#--------------------------------------------------------------------------------------------------------------
#' Mate guarding intensity of polyandrous females
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
  geom_rect(aes(xmin = as.Date('2019-06-11'), xmax = as.Date('2019-06-14'), ymin = -0.07, ymax = 1.07), fill = egg_laying_color) +
  geom_rect(aes(xmin = as.Date('2019-06-20'), xmax = as.Date('2019-06-23'), ymin = -0.07, ymax = 1.07), fill = egg_laying_color) +
  geom_path(data = du[ID2 == 273145121], aes(date_, int_prop, group = nestID, color = f_polyandrous_first_plot), size = 1) +
  scale_color_manual(values = c('steelblue4', 'darkorange'), name = '',
                     labels = c(bquote("1"^st~ mate), bquote("2"^nd~ mate))) +
  geom_point(data = du[ID2 == 273145121], aes(date_, int_prop, color = f_polyandrous_first_plot, size = N)) +
  scale_size_area(max_size = 4, breaks=c(10, 50, 100)) +
  scale_y_continuous(limits = c(-0.07, 1.07), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  scale_x_date(date_breaks = '3 day', date_labels = '%d %b', limits = c(as.Date('2019-06-11'), as.Date('2019-07-01'))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.9, 0.88), legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  guides(size = "none") +
  ylab('Proportion of time together') +
  xlab('')

p1


p2 = 
  ggplot() +
  geom_rect(aes(xmin = as.Date('2019-06-09'), xmax = as.Date('2019-06-12'), ymin = -0.07, ymax = 1.07), fill = egg_laying_color) +
  geom_rect(aes(xmin = as.Date('2019-06-18'), xmax = as.Date('2019-06-21'), ymin = -0.07, ymax = 1.07), fill = egg_laying_color) +
  geom_path(data = du[ID2 == 270170935 & nestID == 'R405_19' | nestID == 'R406_19'], aes(date_, int_prop, group = nestID, color = f_polyandrous_first_plot), size = 1) +
  scale_color_manual(values = c('steelblue4', 'darkorange'), name = '',
                     labels = c('1st mate', '2nd mate')) +
  geom_point(data = du[ID2 == 270170935 & nestID == 'R405_19' | nestID == 'R406_19'], aes(date_, int_prop, color = f_polyandrous_first_plot, size = N)) +
  scale_size_area(max_size = 4, breaks=c(10, 50, 100)) +
  scale_y_continuous(limits = c(-0.07, 1.07), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  scale_x_date(date_breaks = '3 day', date_labels = '%d %b') +
  theme_classic(base_size = 10) +
  theme(legend.position = 'none', legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time together') +
  xlab('')

p2

p3 = 
  ggplot() +
  geom_rect(aes(xmin = as.Date('2019-06-11'), xmax = as.Date('2019-06-13'), ymin = -0.07, ymax = 1.07), fill = egg_laying_color) +
  geom_rect(aes(xmin = as.Date('2019-06-15'), xmax = as.Date('2019-06-16'), ymin = -0.07, ymax = 1.07), fill = egg_laying_color) +
  geom_path(data = du[ID2 == 273145036], aes(date_, int_prop, group = nestID, color = f_polyandrous_first_plot), size = 1) +
  scale_color_manual(values = c('steelblue4', 'darkorange'), name = '',
                     labels = c('1st mate', '2nd mate')) +
  geom_point(data = du[ID2 == 273145036], aes(date_, int_prop, color = f_polyandrous_first_plot, size = N)) +
  scale_size_area(max_size = 4, breaks=c(10, 50, 100)) +
  scale_y_continuous(limits = c(-0.07, 1.07), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  scale_x_date(date_breaks = '3 day', date_labels = '%d %b') +
  theme_classic(base_size = 10) +
  theme(legend.position = 'none', legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time together') +
  xlab('')
p3

p4 = 
  ggplot() +
  geom_rect(aes(xmin = as.Date('2019-06-06'), xmax = as.Date('2019-06-09'), ymin = -0.07, ymax = 1.07), fill = egg_laying_color) +
  geom_rect(aes(xmin = as.Date('2019-06-15'), xmax = as.Date('2019-06-18'), ymin = -0.07, ymax = 1.07), fill = egg_laying_color) +
  geom_path(data = du[ID2 == 273145109], aes(date_, int_prop, group = nestID, color = f_polyandrous_first_plot), size = 1) +
  scale_color_manual(values = c('steelblue4', 'darkorange'), name = '',
                     labels = c('1st mate', '2nd mate')) +
  geom_point(data = du[ID2 == 273145109], aes(date_, int_prop, color = f_polyandrous_first_plot, size = N)) +
  scale_size_area(max_size = 4, breaks=c(10, 50, 100)) +
  scale_y_continuous(limits = c(-0.07, 1.07), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  scale_x_date(date_breaks = '3 day', date_labels = '%d %b') +
  theme_classic(base_size = 10) +
  theme(legend.position = 'none', legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time together') +
  xlab('Date')

p4



p1 

# ggsave('./OUTPUTS/FIGURES/MG_over_season_polyandrous_female_273145121.tiff', plot = last_plot(),  width = 177, height = 60, units = c('mm'), dpi = 'print')


# merge plots
p1 + p2 + p3 + p4 +
  plot_layout(nrow = 4, ncol = 1) +
  plot_annotation(tag_levels = 'a')

# ggsave('./OUTPUTS/FIGURES/MG_over_season_polyandrous_4_females.tiff', plot = last_plot(),  width = 177, height = 238, units = c('mm'), dpi = 'print')










# save word file
print(ESM, target = "./OUTPUTS/ESM/ESM_REPH_PAIRS.docx")




