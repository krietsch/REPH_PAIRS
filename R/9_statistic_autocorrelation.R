
# Packages
sapply( c('data.table', 'ggplot2',  'glmmTMB', 'effects', 'activity', 'nlme', 'knitr'), 
        require, character.only = TRUE)

# Data
dp  = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', sep = '\t', header = TRUE, nThread = 20) |>  data.table()


# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/9_statistic_autocorrelation.R', output_dir = './OUTPUTS/R_COMPILED')

setorder(dp, pairID, nestID, datetime_1)

# time of the day 
dp[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
dp[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]
dp[, timef := factor(round(as.numeric(scale(datetime_1)), 3))]

# subset data before clutch initiation
dx = dp[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
# dx = dp[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dx[, year_ := as.character(year_)]


# model with time but without autocorrelation correction
m <- glmmTMB(interaction ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) + 
               scale(sin_time) + scale(cos_time)  + (datetime_rel_pair0 | nestID),
             family = binomial, data = dx, REML = FALSE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)



# same model with autocorrelation correction
m <- glmmTMB(interaction ~ poly(datetime_rel_pair0, 2) + poly(initiation_rel, 2) + 
               scale(sin_time) + scale(cos_time) + ar1(timef + 0 | nestID),
             family = binomial, data = dx, REML = FALSE,
             control = glmmTMBControl(parallel = 15)
)


plot(allEffects(m))
summary(m)



# similar model with nlme to plot lag of autocorrelation
m = lme(interaction ~ poly(datetime_rel_pair0, 2) +  poly(initiation_rel, 2) + scale(sin_time) + scale(cos_time), 
        random = (~1 + datetime_rel_pair0 | nestID), data = dx)   

plot(ACF(m,resType = "normalized"), alpha = 0.01)


m2 = update(m, correlation = corAR1(  form = ~ 1| nestID) )
plot(ACF(m2, resType = "normalized"), alpha = 0.01)


plot(allEffects(m2))
summary(m2)




# data in random orderto see influence of data structure
set.seed(2023-02-20-13)
dxr = dx[sample(1:nrow(dx)), ]

# order by nestID and day
setorder(dxr, nestID)


mr = lme(interaction ~ poly(datetime_rel_pair0, 2) +  poly(initiation_rel, 2) + scale(sin_time) + scale(cos_time),
        random = (~1 + datetime_rel_pair0 | nestID), data = dxr)

plot(ACF(mr, resType = "normalized"), alpha = 0.01)



