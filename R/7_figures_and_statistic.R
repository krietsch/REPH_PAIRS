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
# margin_ = unit(c(0, 4, 0, 0), 'pt')
margin_ = unit(c(2, 2, 2, 2), 'pt')
sample_size_label = 2.5



# nest data
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y, nest_state_date)]
dnID = unique(dnID, by = 'nestID')

# as integer
dnID[, male_id := as.integer(male_id)]
dnID[, female_id := as.integer(female_id)]

# assign clutch order
setorder(dnID, male_id, initiation)
dnID[!is.na(male_id) & !is.na(female_id), clutch_together := seq_len(.N), by = .(year_, male_id, female_id)]
dnID[!is.na(male_id), male_clutch     := seq_len(.N), by = .(year_, male_id)]
dnID[!is.na(female_id), female_clutch := seq_len(.N), by = .(year_, female_id)]



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

# renesting females
# first nests of renesting females
x1 = c("R201_19", "R402_19", "R406_19", "R222_19", "R502_19", "R905_19")
dp[nestID %in% x1, f_renesting_first := TRUE]

# second nests of renesting females
x2 = c("R232_19",  "R803_19",  "R320_19",  "R1101_19", "R218_19", "R913_19")
dp[nestID %in% x2, f_renesting_second := TRUE]

# season mean 
di = dn[!is.na(year_) & plot == 'NARL', .(initiation_mean = mean(initiation, na.rm = TRUE)), by = year_]

# quantiles 
dn = merge(dn, di, by = 'year_', all.x = TRUE)
dn[, initiation_rel_ := difftime(initiation, initiation_mean, units = 'days') %>% as.numeric %>% round(., 0)]

quantile(dn[!is.na(initiation_rel_)]$initiation_rel_,  probs = c(33, 66)/100)


dp = merge(dp, di, by = 'year_', all.x = TRUE)

# merge with nests
dp = merge(dp, dnID[, .(male_id, female_id, year_, nestID, initiation)], by.x = c('ID1', 'ID2', 'year_', 'nestID'), 
           by.y = c('male_id', 'female_id', 'year_', 'nestID'), all.x = TRUE)

# relative initiation date
dp[, initiation_rel := difftime(initiation, initiation_mean, units = 'days') %>% as.numeric %>% round(., 0)]

# early and late clutches?
dp[initiation_rel < -2, initiation_type := 'early']
dp[initiation_rel > 1, initiation_type := 'late']
dp[!is.na(initiation) & is.na(initiation_type), initiation_type := 'peak']

# datetime relative to nest initiation date
dp[, datetime_rel_initiation := difftime(datetime_1, initiation, units = 'days') %>% as.numeric()]

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
            scale(datetime_rel_pair0);                                        relative day (scaled)
            any_EPYTRUE;                                                      any EPY (yes)
            sexM;                                                             sex (male)
            poly(initiation_rel, 2)1;                                         clutch initiation relative to season (linear)
            poly(initiation_rel, 2)2;                                         clutch initiation relative to season (quadratic)
            poly(datetime_rel_pair0, 2)1;                                     clutch initiation relative to first egg (linear)
            poly(datetime_rel_pair0, 2)2;                                     clutch initiation relative to first egg (quadratic)
            f_polyandrous_firstTRUE;                                          first clutch of polyandrous female (yes)
            earlyTRUE;                                                        early initiation (yes)
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
dss = unique(du[datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
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
pa = 
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du, 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du, 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), # shape = data_quality
             position=position_jitterdodge(), size = 0.2) +
  # scale_shape_manual(values=c(20, 19)) +
  scale_color_manual(values = c('black', 'yellowgreen'), name = '', 
                     labels = c('breeding pair', 'random pair'), drop = FALSE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.9, 0.8), legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)')

pa

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/prop_time_together_season_null_model.tiff', plot = last_plot(),  width = 177, height = 89, units = c('mm'), dpi = 'print')

# random pairs
d1[, quantile(int_prop, c(0.5, 0.25, 0.75))] 
d1[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 3, quantile(int_prop, c(0.5, 0.25, 0.75))] 

d1[datetime_rel_pair0 == 4, quantile(int_prop, c(0.5, 0.25, 0.75))] 

d1[datetime_rel_pair0 >= 5, quantile(int_prop, c(0.5, 0.25, 0.75))] 

# some descriptive statistic
d0[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, quantile(int_prop, c(0.5, 0.25, 0.75))] 
d0[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 3, quantile(int_prop, c(0.5, 0.25, 0.75))] 

d0[datetime_rel_pair0 == -5, quantile(int_prop, c(0.5, 0.25, 0.75))] 
d0[datetime_rel_pair0 == -2, quantile(int_prop, c(0.5, 0.25, 0.75))] 
d0[datetime_rel_pair0 == -2] |> nrow()
d0[datetime_rel_pair0 == -2 & int_prop > 0.95] |> nrow()
d0[datetime_rel_pair0 == -1, quantile(int_prop, c(0.5, 0.25, 0.75))] 
d0[datetime_rel_pair0 == 3, quantile(int_prop, c(0.5, 0.25, 0.75))] 

d0[datetime_rel_pair0 == 4, quantile(int_prop, c(0.5, 0.25, 0.75))] 

d0[datetime_rel_pair0 >= 5, quantile(int_prop, c(0.5, 0.25, 0.75))] 

d1[, .N, .(datetime_rel_pair0, nestID)]


### Models 


# 
# 
# # test differences between breeding pairs and random pairs
# 
# # assign random pairs nestID as pairID
# dmr[, nestID := pairID]
# 
# 
# # merge data
# dmx = rbindlist(list(dm[, .(pairID, nestID, interaction, initiation_rel, datetime_rel_pair0, type)],
#                      dmr[, .(pairID, nestID, interaction, initiation_rel, datetime_rel_pair0, type)]
# ))
# 
# 
# # model fertile period
# dx = dmx[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 3]
# 
# 
# fm1 <- glmmTMB(interaction ~ type + poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) + (datetime_rel_pair0 | nestID),
#                family = binomial, data = dx, REML = TRUE,
#                control = glmmTMBControl(parallel = 15)
# )
# 
# 
# plot(allEffects(fm1))
# summary(fm1)
# 
# 
# 
# # create clean summary table 
# y = tidy(fm1) |> data.table()
# x = r2(fm1) |> data.table() 
# 
# 
# setnames(x, c('estimate'))
# x[, estimate := as.numeric(estimate)]
# x[, term :=  c('r2cond', 'r2marg')]
# y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
# y[, row_order := rownames(y) |> as.numeric()]
# y = merge(y, pn, by.x = 'term', by.y = 'parname')
# setorder(y, row_order)
# y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
# # y[parameter %in% c('intercept', 'relative day', 'split (after)'), p := NA]
# y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 
# 
# # save table in word
# ft = flextable(y) |> autofit()
# ft = bold(ft, bold = TRUE, part = "header")
# ESM = ESM |> body_add_par(paste0('Table XXX. GLMM together vs randomized initiation -5 to 3')) |>  body_add_par('') |> body_add_flextable(ft)
# ESM = ESM |> body_add_break(pos = 'after')
# 
# 
# 
# 
# # model after laying
# dx = dmx[datetime_rel_pair0 >= 5 & datetime_rel_pair0 <= 10]
# 
# 
# fm1 <- glmmTMB(interaction ~ type + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
#                family = binomial, data = dx, REML = TRUE,
#                control = glmmTMBControl(parallel = 15)
# )
# 
# 
# plot(allEffects(fm1))
# summary(fm1)
# 
# 
# # create clean summary table 
# y = tidy(fm1) |> data.table()
# x = r2(fm1) |> data.table() 
# 
# 
# setnames(x, c('estimate'))
# x[, estimate := as.numeric(estimate)]
# x[, term :=  c('r2cond', 'r2marg')]
# y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
# y[, row_order := rownames(y) |> as.numeric()]
# y = merge(y, pn, by.x = 'term', by.y = 'parname')
# setorder(y, row_order)
# y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
# # y[parameter %in% c('intercept', 'relative day', 'split (after)'), p := NA]
# y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 
# 
# # save table in word
# ft = flextable(y) |> autofit()
# ft = bold(ft, bold = TRUE, part = "header")
# ESM = ESM |> body_add_par(paste0('Table XXX. GLMM together vs randomized initiation 5 to 10')) |>  body_add_par('') |> body_add_flextable(ft)
# ESM = ESM |> body_add_break(pos = 'after')
# 





# before clutch initiation
dx = dm[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]

m <- glmmTMB(interaction ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)



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
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table XXX. GLMM together initiation -5 to -1')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# plot for season effect on time spent together

# extract effect from model
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dm[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, N_ini := .N, by = .(pairID, nestID)]
du = unique(dms, by = c('pairID', 'nestID', 'initiation_rel'))
du = du[!is.na(N_ini)]

dms = dms[interaction == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, .(N_int = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
# du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N_ini]
d0 = copy(du)


d0[initiation_rel < -6]


dm[nestID == 'R901_19']

pb = 
ggplot() +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1) +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8) +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2) +
  scale_x_continuous(expand = expansion(add = c(0.1, 0.1))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.01))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.1, 0.1), legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')

pb


# during egg laying
dx = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]

m <- glmmTMB(interaction ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)




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
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table XXX. GLMM together during laying 0 to 3')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# plot for season effect on time spent together

# extract effect from model
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, N_ini := .N, by = .(pairID, nestID)]
du = unique(dm, by = c('pairID', 'nestID', 'initiation_rel'))
du = du[!is.na(N_ini)]

dms = dm[interaction == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, .(N_int = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N_ini]
d0 = copy(du)


pc = 
  ggplot() +
  geom_point(data = du, aes(initiation_rel, int_prop, size = N_ini), shape = 1) +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8) +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2) +
  scale_x_continuous(expand = expansion(add = c(0.1, 0.1))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.01))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.1, 0.1), legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time together') +
  xlab('Clutch initiation date (standardized)')



# merge plots
pa + pb + pc +
  plot_layout(design = "
  11
  23
") +
  plot_annotation(tag_levels = 'a')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_together.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')




#--------------------------------------------------------------------------------------------------------------
#' Split and merge events
#--------------------------------------------------------------------------------------------------------------

# define who flies away or joins  


# subset data
dm = dp[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10]
# dm = dp[Np > 0.75 & datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10]

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

# Times females split
dms = dm[split == TRUE & ID_splitting == 'ID2', .(N_f_split = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dm[split == TRUE], by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_f_split), N_f_split := 0]
du[, f_split_prop := N_f_split /N_split]
d3 = copy(du)


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


# plot splits females 
dus = du[type == 'f_split_prop']

pa = 
  ggplot() +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = dus, 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0)), colour = 'black',
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_jitter(data = dus, aes(datetime_rel_pair0, prop), colour = 'black', size = 0.5) + 
  # geom_hline(yintercept = 0.5, color = 'black', linetype = 'dashed') +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.15), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of female moves away') +
  xlab('Day relative to clutch initiation (= 0)')

pa
# 
# 
# # plot merges females 
# dus = du[type == 'f_merge_prop']
# 
# 
# pc = 
#   ggplot() +
#   geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
#   geom_boxplot(data = dus, 
#                aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0)), colour = 'yellowgreen',
#                lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
#   geom_jitter(data = dus, aes(datetime_rel_pair0, prop), colour = 'yellowgreen', size = 0.5) + 
#   scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
#                      labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
#                                 '', '2', '', '4', '', '6', '', '8', '', '10'),
#                      expand = expansion(add = c(0.2, 0.2))) +
#   scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
#                      labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
#                      expand = expansion(add = c(0, 0))) +
#   theme_classic(base_size = 11) +
#   theme(legend.position = c(0.9, 0.15), legend.background = element_blank(), plot.margin = margin_) +
#   ylab('Proportion of moves towards by female') +
#   xlab('Day relative to clutch initiation (= 0)')
# 
# pc
# 


# merge plots
pa + pb + pc +
  plot_layout(design = "
  11
  23
") +
  plot_annotation(tag_levels = 'a')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_together_split_merge.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')


# descriptive statistic

# movements away
du[type == 'f_split_prop' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[type == 'f_split_prop' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, mean(prop, na.rm = TRUE)]


du[type == 'f_merge_prop' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[type == 'f_merge_prop' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, mean(prop, na.rm = TRUE)]


# movements together
du[type == 'f_split_prop' & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[type == 'f_split_prop' & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, mean(prop, na.rm = TRUE)]

du[type == 'f_merge_prop' & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[type == 'f_merge_prop' & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, mean(prop, na.rm = TRUE)]



# model before clutch initiation
dx = dm[split == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
dx[, early := ifelse(initiation_rel <= -2,  TRUE, FALSE)]

dx[, ID_splitting := ifelse(ID_splitting == 'ID1', 0, 1)] # males = 0

m <- glmmTMB(ID_splitting ~ scale(datetime_rel_pair0) + scale(initiation_rel) + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)



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
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
# y[parameter %in% c('intercept', 'relative day', 'split (after)'), p := NA]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S1. GLMM split by sex before clutch initiation -5 to -1')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')





# extract effect from model
e = effect("scale(initiation_rel)", m, xlevels = 100) |>
  data.frame() |>
  setDT()





# data for points 
dms = dm[split == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
dms[, N_splits_season := .N, by = .(pairID, nestID, initiation_rel)]
du = unique(dms[!is.na(N_splits_season)], by = c('pairID', 'nestID', 'initiation_rel'))

# Proportion of split events
dms = dm[split == TRUE & ID_splitting == 'ID2' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, 
         .(N_split_female = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_split_female), N_split_female := 0]
du[, split_prop := N_split_female / N_splits_season]
d1 = copy(du)

du[split_prop == 0]


pb = 
  ggplot() +
  geom_hline(yintercept = 0.5, color = 'black', linetype = 'dashed') +
  geom_point(data = du, aes(initiation_rel, split_prop, size = N_splits_season), shape = 1) +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8) +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2) +
  scale_x_continuous(expand = expansion(add = c(0.1, 0.1))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.01))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.1, 0.1), legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of female moves away (day -5 to -1)') +
  xlab('Clutch initiation date (standardized)')

pb










# model during egg-laying
dx = dm[split == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dx[, early := ifelse(initiation_rel <= -2,  TRUE, FALSE)]

dx[, ID_splitting := ifelse(ID_splitting == 'ID1', 0, 1)] # males = 0

m <- glmmTMB(ID_splitting ~ scale(datetime_rel_pair0) + scale(initiation_rel) + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = TRUE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)



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
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
# y[parameter %in% c('intercept', 'relative day', 'split (after)'), p := NA]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S2. GLMM split by sex after clutch initiation 0 to 3')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')






# extract effect from model
e = effect("scale(initiation_rel)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dm[split == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dms[, N_splits_season := .N, by = .(pairID, nestID, initiation_rel)]
du = unique(dms[!is.na(N_splits_season)], by = c('pairID', 'nestID', 'initiation_rel'))

# Proportion of split events
dms = dm[split == TRUE & ID_splitting == 'ID2' & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, 
         .(N_split_female = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_split_female), N_split_female := 0]
du[, split_prop := N_split_female / N_splits_season]
d1 = copy(du)



pc = 
  ggplot() +
  geom_hline(yintercept = 0.5, color = 'black', linetype = 'dashed') +
  geom_point(data = du, aes(initiation_rel, split_prop, size = N_splits_season), shape = 1) +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8) +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2) +
  scale_x_continuous(expand = expansion(add = c(0.1, 0.1))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.01))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.1, 0.1), legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of female moves away (day 0 to 3)') +
  xlab('Clutch initiation date (standardized)')

pc






# merge plots
pa + pb + pc +
  plot_layout(design = "
  11
  23
") +
  plot_annotation(tag_levels = 'a')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/female_moving_away.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')






### merging

# model before clutch initiation
dx = dm[merge == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]

dx[, ID_merging_ := ifelse(ID_merging == 'ID1', 0, 1)] # males = 0

fm1 <- glmmTMB(ID_merging_ ~ scale(datetime_rel_pair0) + scale(initiation_rel) + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)




# model after clutch initiation
dx = dm[merge == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]

dx[, ID_merging_ := ifelse(ID_merging == 'ID1', 0, 1)]

fm1 <- glmmTMB(ID_merging_ ~ scale(datetime_rel_pair0) + scale(initiation_rel) + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)


#--------------------------------------------------------------------------------------------------------------
#' Distance moved away or towards
#--------------------------------------------------------------------------------------------------------------

# moved away
dms = dm[split == TRUE]

# merge male and female data for plot
dms_m = dms[ID_splitting == 'ID1', .(pairID, nestID, datetime_rel_pair0, sex = sex1, split_distance = distance1_before)]
dms_f = dms[ID_splitting == 'ID2', .(pairID, nestID, datetime_rel_pair0, sex = sex2, split_distance = distance2_before)]

dms = rbindlist(list(dms_m, dms_f))

# adjust distance above 1000 m
dms[, split_distance1000 := split_distance]
dms[split_distance > 1000, split_distance1000 := 1000]

pa = 
ggplot() +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1000), fill = 'grey90') +
  geom_boxplot(data = dms, 
               aes(datetime_rel_pair0, split_distance1000, group = interaction(datetime_rel_pair0, sex), color = sex),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = dms, 
             aes(datetime_rel_pair0, split_distance1000, group = interaction(datetime_rel_pair0, sex), color = sex), position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '',
                     labels = c('Female moves away', 'Male moves away'), drop = FALSE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(expand = expansion(add = c(0, 5))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.87, 0.94), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Distance moved when split (m)') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_split_events_distance_moved.tiff', plot = last_plot(),  width = 177, height = 89, units = c('mm'), dpi = 'print')

# before and during laying
dms[sex == 'F' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 3, quantile(split_distance, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dms[sex == 'M' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 3, quantile(split_distance, c(0.5, 0.25, 0.75), na.rm = TRUE)]

dms[sex == 'F' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 3, mean(split_distance, na.rm = TRUE)]
dms[sex == 'M' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 3, mean(split_distance, na.rm = TRUE)]



# model before clutch initiation
dx = dms[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2]

fm1 <- glmmTMB(split_distance ~ sex + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = gaussian, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)


# create clean summary table 
y = tidy(fm1) |> data.table()
x = r2(fm1) |> data.table() 


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

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table SXXXX. GLMM split distance by sex before clutch initiation -5 to 2')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')




# moved towards
dms = dm[merge == TRUE]

# merge male and female data for plot
dms_m = dms[ID_merging == 'ID1', .(pairID, nestID, datetime_rel_pair0, sex = sex1, merge_distance = distance1_before)]
dms_f = dms[ID_merging == 'ID2', .(pairID, nestID, datetime_rel_pair0, sex = sex2, merge_distance = distance2_before)]

dms = rbindlist(list(dms_m, dms_f))

# adjust distance above 1000 m
dms[, merge_distance1000 := merge_distance]
dms[merge_distance > 1000, merge_distance1000 := 1000]

pb = 
ggplot() +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1000), fill = 'grey90') +
  geom_boxplot(data = dms, 
               aes(datetime_rel_pair0, merge_distance1000, group = interaction(datetime_rel_pair0, sex), color = sex),
               lwd = 0.4, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = dms, 
             aes(datetime_rel_pair0, merge_distance1000, group = interaction(datetime_rel_pair0, sex), color = sex), position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4'), name = '',
                     labels = c('Female moves towards', 'Male moves towards'), drop = FALSE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(expand = expansion(add = c(0, 5))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.87, 0.94), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Distance moved when merged (m)') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_split_events_distance_moved.tiff', plot = last_plot(),  width = 177, height = 89, units = c('mm'), dpi = 'print')

# before and during laying
dms[sex == 'F' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 3, quantile(merge_distance, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dms[sex == 'M' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 3, quantile(merge_distance, c(0.5, 0.25, 0.75), na.rm = TRUE)]

# before laying 
dms[sex == 'F' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, quantile(merge_distance, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dms[sex == 'M' & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1, quantile(merge_distance, c(0.5, 0.25, 0.75), na.rm = TRUE)]

# during laying
dms[sex == 'F' & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, quantile(merge_distance, c(0.5, 0.25, 0.75), na.rm = TRUE)]
dms[sex == 'M' & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, quantile(merge_distance, c(0.5, 0.25, 0.75), na.rm = TRUE)]


# merge plots
pa + pb + 
  plot_layout(nrow = 2) +
  plot_annotation(tag_levels = 'A')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_split_merge_distances.tiff', plot = last_plot(),  width = 177, height = 220, units = c('mm'), dpi = 'print')


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



# descriptive statistic

# day before clutch initiation
du[type == 'f_at_nest_prop' & datetime_rel_pair0 == -1, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[type == 'm_at_nest_prop' & datetime_rel_pair0 == -1, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]

# at clutch initiation
du[type == 'f_at_nest_prop' & datetime_rel_pair0 == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[type == 'm_at_nest_prop' & datetime_rel_pair0 == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]

# at clutch last day of egg laying
du[type == 'f_at_nest_prop' & datetime_rel_pair0 == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[type == 'm_at_nest_prop' & datetime_rel_pair0 == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]


## without partner at nest
# clutch initiation 
du[type == 'f_alone_at_nest_prop' & datetime_rel_pair0 == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[type == 'm_alone_at_nest_prop' & datetime_rel_pair0 == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]


# at clutch last day of egg laying
du[type == 'f_alone_at_nest_prop' & datetime_rel_pair0 == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[type == 'm_alone_at_nest_prop' & datetime_rel_pair0 == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]


### without partner away from nest
# clutch initiation 
du[type == 'f_alone_prop' & datetime_rel_pair0 == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[type == 'm_alone_prop' & datetime_rel_pair0 == 0, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]


# at clutch last day of egg laying
du[type == 'f_alone_prop' & datetime_rel_pair0 == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]
du[type == 'm_alone_prop' & datetime_rel_pair0 == 3, quantile(prop, c(0.5, 0.25, 0.75), na.rm = TRUE)]



# Males and females at the nest
pa = 
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[type == 'm_at_nest_prop' | type == 'f_at_nest_prop'], 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'm_at_nest_prop' | type == 'f_at_nest_prop'], 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('firebrick3', 'steelblue4'), name = '', 
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
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[type == 'm_alone_prop' | type == 'm_alone_at_nest_prop'], 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'm_alone_prop' | type == 'm_alone_at_nest_prop'], 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('steelblue4', 'steelblue1'), name = '', 
                     labels = c('At nest', 'Not at nest'), drop = FALSE) +
  scale_x_continuous(limits = c(-5.4, 5.4), breaks = seq(-5, 5, 1), 
                     labels = c('', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', ''),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.18, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time alone (male)') +
  xlab('Day relative to clutch initiation (= 0)')

pb

# Female alone and alone at nest
pc = 
ggplot() +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[type == 'f_alone_prop' | type == 'f_alone_at_nest_prop'], 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[type == 'f_alone_prop' | type == 'f_alone_at_nest_prop'], 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, type), color = type), position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('firebrick3', 'salmon'), name = '', 
                     labels = c('At nest', 'Not at nest'), drop = FALSE) +
  scale_x_continuous(limits = c(-5.4, 5.4), breaks = seq(-5, 5, 1), 
                     labels = c('', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', ''),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.18, 0.9), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time alone (female)') +
  xlab('Day relative to clutch initiation (= 0)')

pc


# statistic 

# during egg-laying male
dx = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
m <- glmmTMB(m_at_nest ~ scale(initiation_rel) + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)




# extract effect from model
e = effect("scale(initiation_rel)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dms[, N_nest_season := .N, by = .(pairID, nestID, initiation_rel)]
du = unique(dms[!is.na(N_nest_season)], by = c('pairID', 'nestID', 'initiation_rel'))

dms = dm[m_at_nest == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, 
         .(N_m_at_nest = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_m_at_nest), N_m_at_nest := 0]
du[, m_at_nest_prop := N_m_at_nest / N_nest_season]
d1 = copy(du)

pd = 
  ggplot() +
  geom_point(data = du, aes(initiation_rel, m_at_nest_prop, size = N), shape = 1, color = 'steelblue4') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'steelblue4') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'steelblue4') +
  scale_x_continuous(expand = expansion(add = c(0.1, 0.1))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.01))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.1, 0.1), legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time at nest (day 0 to 3)') +
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

# extract effect from model
e = effect("poly(initiation_rel,2)", m, xlevels = 100) |>
  data.frame() |>
  setDT()


# data for points 
dms = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dms[, N_nest_season := .N, by = .(pairID, nestID, initiation_rel)]
du = unique(dms[!is.na(N_nest_season)], by = c('pairID', 'nestID', 'initiation_rel'))

dms = dm[f_at_nest == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3, 
         .(N_f_at_nest = .N), by = .(pairID, nestID, initiation_rel)]
du = merge(du, dms, by = c('pairID', 'nestID', 'initiation_rel'), all.x = TRUE)
du[is.na(N_f_at_nest), N_f_at_nest := 0]
du[, f_at_nest_prop := N_f_at_nest / N_nest_season]
d1 = copy(du)

pe = 
  ggplot() +
  geom_point(data = du, aes(initiation_rel, f_at_nest_prop, size = N), shape = 1, color = 'firebrick3') +
  geom_line(data = e, aes(y = fit, x = initiation_rel), size = 0.8, color = 'firebrick3') +
  geom_ribbon(data = e, aes(y = fit, x = initiation_rel, ymin = lower, ymax = upper), alpha = 0.2, fill = 'firebrick3') +
  scale_x_continuous(expand = expansion(add = c(0.1, 0.1))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), 
                     labels = c('0.0', '', '0.2', '', '0.4', '', '0.6', '', '0.8', '', '1.0'),
                     expand = expansion(add = c(0, 0.01))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.1, 0.1), legend.background = element_blank(), plot.margin = margin_, 
        legend.spacing.y = unit(-0.2, "cm"), legend.title = element_blank()) +
  ylab('Proportion of time at nest (day 0 to 3)') +
  xlab('Clutch initiation date (standardized)')

pe





# merge plots
pa + pb + pc + pd + pe +
  plot_layout(design = "
  11
  23
  45
") +
  # plot_layout(heights = c(1, 4, 4)) +
  plot_annotation(tag_levels = 'a')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_at_nest.tiff', plot = last_plot(),  width = 177, height = 238, units = c('mm'), dpi = 'print')




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
pa = 
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N_epy_label), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du[!is.na(any_EPY)], 
               aes(datetime_rel_pair0, int_prop, group = interaction(datetime_rel_pair0, any_EPY_plot), color = any_EPY_plot),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du[!is.na(any_EPY)], 
             aes(datetime_rel_pair0, int_prop, group = interaction(datetime_rel_pair0, any_EPY_plot), color = any_EPY_plot), 
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('yellowgreen', 'black'), name = '', 
                     labels = c('EPY', 'No EPY'), drop = FALSE) +
  scale_x_continuous(limits = c(-5.4, 5.4), breaks = seq(-5, 5, 1), 
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



# model fertile period
dx = dm[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2]


fm1 <- glmmTMB(interaction ~ any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)



# create clean summary table 
y = tidy(fm1) |> data.table()
x = r2(fm1) |> data.table() 


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

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S5. GLMM together and EPY fertile period initiation -5 to 2')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# model before clutch initiation
dx = dm[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]

fm1 <- glmmTMB(interaction ~ any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)


# create clean summary table 
y = tidy(fm1) |> data.table()
x = r2(fm1) |> data.table() 


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

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S6. GLMM together and EPY before clutch initiation -5 to -1')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# model during clutch initiation
dx = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 2]

fm1 <- glmmTMB(interaction ~ any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)


# create clean summary table 
y = tidy(fm1) |> data.table()
x = r2(fm1) |> data.table() 


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

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S7. GLMM together and EPY during initiation 0 to 2')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

#--------------------------------------------------------------------------------------------------------------
#' Split events and extra-pair paternity
#--------------------------------------------------------------------------------------------------------------


# data from split events 
dusm

# plot splits and merges females 
dus = dusm[!is.na(any_EPY) & type == 'f_split_prop']

# order
dus[, any_EPY_plot := ifelse(any_EPY == TRUE, 'EPY', 'No EPY')]

pb = 
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N_epy_label), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = dus, 
               aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, any_EPY_plot), color = any_EPY_plot),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = dus, 
             aes(datetime_rel_pair0, prop, group = interaction(datetime_rel_pair0, any_EPY_plot), color = any_EPY_plot), 
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('yellowgreen', 'black'), name = '', 
                     labels = c('EPY', 'No EPY'), drop = FALSE) +
  scale_x_continuous(limits = c(-5.4, 5.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '-8', '', '-6', '', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', '', '6', '', '8', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(10.86, 10.12), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of moves away by female') +
  xlab('Day relative to clutch initiation (= 0)')




# merge plots
pa + pb + 
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_together_epy.tiff', plot = last_plot(),  width = 177, height = 89, units = c('mm'), dpi = 'print')




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



# model fertile period
dx = dm[split == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2]

dx[, ID_splitting := ifelse(ID_splitting == 'ID1', 0, 1)]

fm1 <- glmmTMB(ID_splitting ~ any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)



# create clean summary table 
y = tidy(fm1) |> data.table()
x = r2(fm1) |> data.table() 


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

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S8. GLMM split by sex and EPY fertile period initiation -5 to 2')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# model before clutch initiation
dx = dm[split == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]

dx[, ID_splitting := ifelse(ID_splitting == 'ID1', 0, 1)] # males = 0

fm1 <- glmmTMB(ID_splitting ~ any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)


# create clean summary table 
y = tidy(fm1) |> data.table()
x = r2(fm1) |> data.table() 


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

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S9. GLMM split by sex and EPY before clutch initiation -5 to -1')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# model during initiation
dx = dm[split == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 2]

dx[, ID_splitting := ifelse(ID_splitting == 'ID1', 0, 1)] # males = 0

fm1 <- glmmTMB(ID_splitting ~ any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)


# create clean summary table 
y = tidy(fm1) |> data.table()
x = r2(fm1) |> data.table() 


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

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S10. GLMM split by sex and EPY during initiation 0 to 2')) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

#--------------------------------------------------------------------------------------------------------------
#' Mate guarding intensity in relation polyandry
#--------------------------------------------------------------------------------------------------------------

# pairwise sample size
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss = unique(du[f_polyandrous_first == FALSE & datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
             by = c('nestID', 'datetime_rel_pair0'))
dss = dss[, .N, by = datetime_rel_pair0]

# polyandrous 1st clutch sample size
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
dss_fp = unique(du[f_polyandrous_first == TRUE & datetime_rel_pair >= -10 & datetime_rel_pair <= 10], 
                 by = c('nestID', 'datetime_rel_pair0'))
dss_fp = dss_fp[, .N, by = datetime_rel_pair0]
dss_fp

# merge 
dss = merge(dss, dss_fp[, .(N_epy = N, datetime_rel_pair0)], by = 'datetime_rel_pair0', all.x = TRUE)
dss[, N_fp_label := paste0(N_epy, '/', N)]

# Proportion of time together breeders
dps = dp[interaction == TRUE, .(N_int = .N), by = .(pairID, nestID, datetime_rel_pair0)]
du = unique(dp, by = c('pairID', 'nestID', 'datetime_rel_pair0'))
du = merge(du, dps, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N]


# order
du[, f_polyandrous_first_plot := ifelse(f_polyandrous_first == TRUE, '1st mate (polyandrous)', 'Other')]

### plot proportion of time together polyandrous females
pc = 
ggplot() +
  geom_text(data = dss, aes(datetime_rel_pair0, Inf, label = N_fp_label), vjust = 1, size = sample_size_label) +
  geom_rect(aes(xmin = -0.5, xmax = 3.5, ymin = -0.01, ymax = 1), fill = 'grey90') +
  geom_boxplot(data = du, 
               aes(datetime_rel_pair0, int_prop, group = interaction(datetime_rel_pair0, f_polyandrous_first_plot), color = f_polyandrous_first_plot),
               lwd = 0.3, outlier.size = 0.7, outlier.alpha = 0) +
  geom_point(data = du, 
             aes(datetime_rel_pair0, int_prop, group = interaction(datetime_rel_pair0, f_polyandrous_first_plot), color = f_polyandrous_first_plot), 
             position=position_jitterdodge(), size = 0.2) +
  scale_color_manual(values = c('darkgreen', 'darkorange'), name = '', 
                     drop = FALSE) +
  scale_x_continuous(limits = c(-5.4, 5.4), breaks = seq(-5, 5, 1), 
                     labels = c('', '-4', '', '-2', '', '0', 
                                '', '2', '', '4', ''),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2), 
                     labels = c('0.0', '0.2', '0.4', '0.6', '0.8', '1.0'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.3, 0.14), legend.background = element_blank(), plot.margin = margin_) +
  ylab('Proportion of time together') +
  xlab('Day relative to clutch initiation (= 0)')

pc

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/male_female_together_polyandrous_females.tiff', plot = last_plot(),  width = 89, height = 89, units = c('mm'), dpi = 'print')






# save word file
print(ESM, target = "./OUTPUTS/ESM/ESM_REPH_PAIRS.docx")




