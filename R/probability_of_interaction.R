

# Packages, settings
  sapply(c(
    "data.table", "magrittr", "sdb", "stringr", 
    "activity", # for time to radian conversions
    "glmmTMB", # it is faster than lme4 specially for complex models and has parallel support
    "effects",
    "ggplot2"

  ),
  require,
  character.only = TRUE
  )


# DATA
  dp <- fread("./DATA/PAIR_WISE_INTERACTIONS.txt",
    nThread = 20, tz = "UTC",
    select = c("ID1", "ID2", "sex1", "sex2", "datetime_1", "interaction", "bout")
  )

  # when both M & F pairID should always be F-M
  dp = dp[!(sex1 == "F" & sex2 == "M")]
  setorder(dp, datetime_1)
  
  dp[, pairID := paste(ID1, ID2, sep = "_")]
  
  dp[, year_ := year(datetime_1)]
  
  dn <- dbq(q = "SELECT year_, concat_ws('_', male_id, female_id) pairID, initiation 
               FROM REPHatBARROW.NESTS
                WHERE year_ > 2017 AND
                  plot = 'NARL' AND
                  initiation  is not NULL AND 
                  male_id is not NULL AND
                  female_id is not NULL
                ORDER BY 
                  initiation, pairID
                ")
  dn[, initiation := as.POSIXct(initiation, tz = "UTC")]
  dn[, i := 1:.N, by = .(pairID, year_)]
  # remove second clutches
  dn = dn[i == 1][, i := NULL]

  
  dp = merge(dp, dn, by = c("year_", "pairID"), sort = FALSE, all.x = TRUE)

  # population level relative date-time: relative to season initiation mean

  dp <- merge(dp,
    dn[, .(initiation_mean = mean(initiation)), by = year_],
    by = "year_", 
    sort = FALSE
  )

  dp[, datetime_rel_seasonal := difftime(datetime_1, initiation_mean) |> as.numeric() ]

  # individual level relative date-time: relative to pair initiation
  dp[, datetime_rel_pair := difftime(datetime_1, initiation) |> as.numeric() ]

  dp[, Initiated := fifelse(datetime_rel_pair < 0, "_no", "_yes")]


# MODEL 1: probability of interactions for breeding pairs
  x = dp[!is.na(initiation) &
        datetime_rel_pair / 3600 / 24  <= 10 &   # subset to 10 days before nest initiated and 
        datetime_rel_pair / 3600 / 24  >= -10   # 10 days after nest is initiated
        ]

  x[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
  x[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]

  
  fm1 = glmmTMB(interaction ~ scale(datetime_rel_pair) * Initiated + 
                   scale(sin_time) + scale(cos_time) +
                   (1 + poly(datetime_rel_pair, 2) | pairID) ,
                 family = binomial, x, 
                 REML = FALSE,
                 control = glmmTMBControl(parallel = 15)
  )
  
  
  summary(fm1)
  
  fm1b = glmmTMB(interaction ~ factor(year_) + scale(datetime_rel_pair) * Initiated + 
                scale(sin_time) + scale(cos_time) +
    (1 + poly(datetime_rel_pair, 2) | pairID) ,
    family = binomial, x, 
    REML = FALSE,
    control = glmmTMBControl(parallel = 15)
    )


  summary(fm1b)


  fm2 <- glmmTMB(interaction ~ poly(datetime_rel_pair, 2) +
    scale(sin_time) + scale(cos_time) +
    (1 + poly(datetime_rel_pair, 2) | pairID),
  family = binomial, x,
  REML = FALSE,
  control = glmmTMBControl(parallel = 15)
  )

  # TODO fm3 = interaction ~ datetime_rel_to_egg_4
  AIC(fm1, fm2)



  # INTERACTION: date * Initiated extract effects and plot
  e = allEffects(fm1, xlevels = 100)$"scale(datetime_rel_pair):Initiated" |>
    data.frame() |>
    setDT()
  # predictions are made for the entire range of the data, subset to the relevant interval
  e = e[(Initiated == "_no" & datetime_rel_pair < 0) | (Initiated == "_yes" & datetime_rel_pair >0)]


  ggplot(e, aes(y = fit, x = datetime_rel_pair / 3600 / 24, color = Initiated, fill = Initiated)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
    ylab("Probability of interaction") +
    xlab("Date [0 = nest initiation date]") +
    ggtitle("Breeding pairs")
  


  e <- allEffects(fm2, xlevels = 100)$"poly(datetime_rel_pair,2)" |>
    data.frame() |>
    setDT()

  ggplot(e, aes(y = fit, x = datetime_rel_pair / 3600 / 24)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
    ylab("Probability of interaction") +
    xlab("Date [0 = nest initiation date]") +
    ggtitle("Breeding pairs")



# EPP and BLUPS
b = ranef(fm1)$cond$pairID
b$pairID = row.names(b)
setDT(b)
setnames(b, names(b) |>make.names())
# merge the epp data
setorder(b, poly.datetime_rel_pair..2.2)