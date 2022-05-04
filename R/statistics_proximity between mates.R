#==============================================================================================================
# Proximity between mates in relation to breeding phenology
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
dm = dp[datetime_rel_pair >= -5 & datetime_rel_pair <= 5]

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

# start word file
ESM = read_docx()

#--------------------------------------------------------------------------------------------------------------
#' # Model probability of close proximity for breeding pairs 
#--------------------------------------------------------------------------------------------------------------

# parameter names 
pn = fread("parname;                                                          parameter
            (Intercept);                                                      intercept  
            scale(datetime_rel_pair);                                         relative day
            initiated_minus3yes;                                              split (after) 
            initiated_minus2yes;                                              split (after)  
            initiated_minus1yes;                                              split (after)  
            initiated_yes;                                                    split (after)
            initiated_plus1yes;                                               split (after) 
            initiated_plus2yes;                                               split (after)  
            initiated_plus3yes;                                               split (after)
            scale(sin_time);                                                  sin time
            scale(cos_time);                                                  cos time
            scale(datetime_rel_pair):initiated_minus3yes;                     relative day x split (after)                       
            scale(datetime_rel_pair):initiated_minus2yes;                     relative day x split (after)                            
            scale(datetime_rel_pair):initiated_minus1yes;                     relative day x split (after)                            
            scale(datetime_rel_pair):initiatedyes;                            relative day x split (after)                      
            scale(datetime_rel_pair):initiated_plus1yes;                      relative day x split (after)                           
            scale(datetime_rel_pair):initiated_plus2yes;                      relative day x split (after)                           
            scale(datetime_rel_pair):initiated_plus3yes;                      relative day x split (after)                           
            sd__(Intercept);                                                  random intercept
            sd__poly(datetime_rel_pair, 2)1;                                  relative day (1st poly)              
            sd__poly(datetime_rel_pair, 2)2;                                  relative day (2nd poly)              
            r2marg;                                                           R² marginal
            r2cond;                                                           R² conditional
            
", sep = ';')

# table caption
tc1 = 'Linear mixed-effect model on the proximity between breeding pairs in relation to the clutch initiation date (= 0) interacting with the split day '
tc2 = '. We included the sin and cos of time to account for variation explained by daily pattern and the pair ID nested within the relative clutch initiation date as random effects.
       All numeric parameters are scaled.'





#' ### model selection ML for AIC comparison and REML for final table

# three days before clutch initiation
m_3ml <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_minus3 +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)


m_3 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_minus3 +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)

summary(m_3)

# create clean summary table 
y = tidy(m_3) |> data.table()
x = r2(m_3) |> data.table() 

setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y[parameter %in% c('intercept', 'relative day', 'split (after)'), p := NA]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S1. ', tc1, '(three days before clutch initiation)', tc2)) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')


# two days before clutch initiation
m_2ml <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_minus2 +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)


m_2 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_minus2 +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)

summary(m_2)


# create clean summary table 
y = tidy(m_2) |> data.table()
x = r2(m_2) |> data.table() 

setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y[parameter %in% c('intercept', 'relative day', 'split (after)'), p := NA]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S2. ', tc1, '(two days before clutch initiation)', tc2)) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# one day before clutch initiation
m_1ml <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_minus1 +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

m_1 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_minus1 +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)

summary(m_1)

# create clean summary table 
y = tidy(m_1) |> data.table()
x = r2(m_1) |> data.table() 

setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y[parameter %in% c('intercept', 'relative day', 'split (after)'), p := NA]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S3. ', tc1, '(one day before clutch initiation)', tc2)) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# day with clutch initiation
m0ml <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated +
                scale(sin_time) + scale(cos_time) +
                (1 + poly(datetime_rel_pair, 2) | pairID),
              family = binomial, data = dm,
              REML = FALSE,
              control = glmmTMBControl(parallel = 15)
)

m0 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated +
                scale(sin_time) + scale(cos_time) +
                (1 + poly(datetime_rel_pair, 2) | pairID),
              family = binomial, data = dm,
              REML = TRUE,
              control = glmmTMBControl(parallel = 15)
)

summary(m0)

# create clean summary table 
y = tidy(m0) |> data.table()
x = r2(m0) |> data.table() 

setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y[parameter %in% c('intercept', 'relative day', 'split (after)'), p := NA]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S4. ', tc1, '(at clutch initiation)', tc2)) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# one day after clutch initiation
m1ml <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_plus1 +
                scale(sin_time) + scale(cos_time) +
                (1 + poly(datetime_rel_pair, 2) | pairID),
              family = binomial, data = dm,
              REML = FALSE,
              control = glmmTMBControl(parallel = 15)
)

m1 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_plus1 +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)

summary(m1)

# create clean summary table 
y = tidy(m1) |> data.table()
x = r2(m1) |> data.table() 

setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y[parameter %in% c('intercept', 'relative day', 'split (after)'), p := NA]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S5. ', tc1, '(one day after clutch initiation)', tc2)) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# two days after clutch initiation
m2ml <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_plus2 +
                scale(sin_time) + scale(cos_time) +
                (1 + poly(datetime_rel_pair, 2) | pairID),
              family = binomial, data = dm,
              REML = FALSE,
              control = glmmTMBControl(parallel = 15)
)

m2 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_plus2 +
                scale(sin_time) + scale(cos_time) +
                (1 + poly(datetime_rel_pair, 2) | pairID),
              family = binomial, data = dm,
              REML = TRUE,
              control = glmmTMBControl(parallel = 15)
)

summary(m2)

# create clean summary table 
y = tidy(m2) |> data.table()
x = r2(m2) |> data.table() 

setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y[parameter %in% c('intercept', 'relative day', 'split (after)'), p := NA]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S6. ', tc1, '(two days after clutch initiation)', tc2)) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# three days after clutch initiation
m3ml <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_plus3 +
                scale(sin_time) + scale(cos_time) +
                (1 + poly(datetime_rel_pair, 2) | pairID),
              family = binomial, data = dm,
              REML = FALSE,
              control = glmmTMBControl(parallel = 15)
)

m3 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_plus3 +
                scale(sin_time) + scale(cos_time) +
                (1 + poly(datetime_rel_pair, 2) | pairID),
              family = binomial, data = dm,
              REML = TRUE,
              control = glmmTMBControl(parallel = 15)
)

summary(m3)

# create clean summary table 
y = tidy(m3) |> data.table()
x = r2(m3) |> data.table() 

setnames(x, c('estimate'))
x[, estimate := as.numeric(estimate)]
x[, term :=  c('r2cond', 'r2marg')]
y = rbindlist(list(y, x), use.names = TRUE, fill = TRUE)
y[, row_order := rownames(y) |> as.numeric()]
y = merge(y, pn, by.x = 'term', by.y = 'parname')
setorder(y, row_order)
y = y[, .(parameter, estimate, s.e. = std.error, statistic, p = p.value)] # subset relevant
y[parameter %in% c('intercept', 'relative day', 'split (after)'), p := NA]
y = y %>% mutate_if(is.numeric, ~round(., 3)) # round all numeric columns 

# save table in word
ft = flextable(y) |> autofit()
ft = bold(ft, bold = TRUE, part = "header")
ESM = ESM |> body_add_par(paste0('Table S7. ', tc1, '(three days after clutch initiation)', tc2)) |>  body_add_par('') |> body_add_flextable(ft)
ESM = ESM |> body_add_break(pos = 'after')

# compare models
x = MuMIn::model.sel(m_3ml, m_2ml, m_1ml, m0ml, m1ml, m2ml, m3ml)
rn = rownames(x)
x = x |> data.table()
x[, model := rn]

dx = x[, .(model, df, AICc, delta, weights)]
dx




# save word file
print(ESM, target = "./OUTPUTS/ESM/ESM_REPH_PAIRS.docx")




#' ### refit best fitting model with REML = TRUE
m_2r <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_minus2 +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = dm,
               REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)

summary(m_2r)


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
  geom_path(data = du, aes(y = int_prop, x = datetime_rel_pair0, group = nestID), alpha = 0.15) +
  # viridis::scale_color_viridis(direction = -1, name = 'N positions') +
  
  # new_scale('color') +
  scale_color_manual(values = c('darkorange', 'darkgreen'), name = '', 
                     labels = c('pre peak', 'post peak')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen'), name = '', 
                    labels = c('pre peak', 'post peak')) +
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







m1 <- glmmTMB(interaction ~ scale(datetime_rel_pair) * initiated_plus1 +
                scale(sin_time) + scale(cos_time) +
                (1 + poly(datetime_rel_pair, 2) | pairID),
              family = binomial, data = dm,
              REML = FALSE,
              control = glmmTMBControl(parallel = 15)
)

summary(m1)

# extract model predictions
e = allEffects(m1, xlevels = 1000)$"scale(datetime_rel_pair):initiated_plus1" |>
  data.frame() |>
  setDT()

# predictions are made for the entire range of the data, subset to the relevant interval
e = e[(initiated_plus1 == "no" & datetime_rel_pair < 1) | (initiated_plus1 == "yes" & datetime_rel_pair > 1)]

require(ggnewscale)

# plot 
p = 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey90') +
  geom_path(data = du, aes(y = int_prop, x = datetime_rel_pair0, group = nestID), alpha = 0.15) +
  # viridis::scale_color_viridis(direction = -1, name = 'N positions') +
  
  # new_scale('color') +
  scale_color_manual(values = c('darkorange', 'darkgreen'), name = '', 
                     labels = c('pre peak', 'post peak')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen'), name = '', 
                    labels = c('pre peak', 'post peak')) +
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair, color = initiated_plus1), size = 0.8) +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair, ymin = lower, ymax = upper, fill = initiated_plus1), alpha = 0.2) +
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
