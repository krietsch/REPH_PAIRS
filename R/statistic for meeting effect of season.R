



# TIME TOGETHER


# before clutch initiation
dx = dm[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
fm3 <- glmmTMB(interaction ~ poly(initiation_rel, 2) + any_EPY + poly(datetime_rel_pair0, 2) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm3))
summary(fm3)


# during laying
dx = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
fm3 <- glmmTMB(interaction ~ poly(initiation_rel, 2) + any_EPY + poly(datetime_rel_pair0, 2) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm3))
summary(fm3)



# Fertile period
dx = dm[datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2]
fm3 <- glmmTMB(interaction ~ poly(initiation_rel, 2) + any_EPY + poly(datetime_rel_pair0, 2) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm3))
summary(fm3)



# SPLITTING


# model before clutch initiation
dx = dm[split == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]
dx[, early := ifelse(initiation_rel <= -2,  TRUE, FALSE)]

dx[, ID_splitting := ifelse(ID_splitting == 'ID1', 0, 1)] # males = 0

fm1 <- glmmTMB(ID_splitting ~ scale(initiation_rel) + any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)


# model during egg-laying
dx = dm[split == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
dx[, early := ifelse(initiation_rel <= -2,  TRUE, FALSE)]

dx[, ID_splitting := ifelse(ID_splitting == 'ID1', 0, 1)] # males = 0

fm1 <- glmmTMB(ID_splitting ~ scale(initiation_rel) + any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)



# model fertile period
dx = dm[split == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2]
dx[, early := ifelse(initiation_rel <= -2,  TRUE, FALSE)]

dx[, ID_splitting := ifelse(ID_splitting == 'ID1', 0, 1)] # males = 0

fm1 <- glmmTMB(ID_splitting ~ scale(initiation_rel) + any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)







# MERGING


# model before clutch initiation
dx = dm[merge == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= -1]

dx[, ID_merging := ifelse(ID_merging == 'ID1', 0, 1)] # males = 0

fm1 <- glmmTMB(ID_merging ~ scale(initiation_rel) + any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)


# model during egg-laying
dx = dm[merge == TRUE & datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]

dx[, ID_merging := ifelse(ID_merging == 'ID1', 0, 1)] # males = 0

fm1 <- glmmTMB(ID_merging ~ scale(initiation_rel) + any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)



# model fertile period
dx = dm[merge == TRUE & datetime_rel_pair0 >= -5 & datetime_rel_pair0 <= 2]

dx[, ID_merging := ifelse(ID_merging == 'ID1', 0, 1)] # males = 0

fm1 <- glmmTMB(ID_merging ~ scale(initiation_rel) + any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = TRUE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm1))
summary(fm1)

# TIME AT NEST
 
# during egg-laying male
dx = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
fm3 <- glmmTMB(at_nest1 ~ scale(initiation_rel) + any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm3))
summary(fm3)



# during egg-laying female
dx = dm[datetime_rel_pair0 >= 0 & datetime_rel_pair0 <= 3]
fm3 <- glmmTMB(at_nest2 ~ scale(initiation_rel) + any_EPY + scale(datetime_rel_pair0) + (datetime_rel_pair0 | nestID),
               family = binomial, data = dx, REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)


plot(allEffects(fm3))
summary(fm3)














