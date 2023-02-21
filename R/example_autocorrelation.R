

require(glmmTMB)
require(data.table)
require(ggplot2)
require(emmeans)

dt2hh <- function(x) {
  h <- as.POSIXlt(x)
  h$hour + h$min / 60 + h$sec / 3600
}

hh2rad <- function(x) {
  x * pi / 12
}

set.seed(2023-02-20-13)
t0 = "2023-02-20 00:00:00" |> as.POSIXct()
x = data.table(y = c(rbinom(48, 1, 0.01), rbinom(48, 1, 0.2), rbinom(48, 1, 0.95)))
x[, Time := seq(t0, t0 + 3600 * 24, length.out = .N)]
x[, timef := factor(as.numeric(scale(Time)), levels = as.numeric(scale(Time)))]
x[, HH := dt2hh(Time)]
x[, id := 1]

fm1 = glmmTMB(y ~ 1, family = binomial, x)

# plot(ACF(fm1,resType="normalized") ,alpha=0.01)

fm2 = glmmTMB(y ~ 1 + ar1(timef + 0 | id), family = binomial, x)
fm3 = glmmTMB(y ~ 1 + sin(hh2rad(HH)) + cos(hh2rad(HH)), family = binomial, x)
fm4 = glmmTMB(y ~ 1 + sin(hh2rad(HH)) + cos(hh2rad(HH)) + ar1(timef + 0 | id), family = binomial, x)
fm5 = glmmTMB(y ~ 1 + (sin(hh2rad(HH)) | id) + (cos(hh2rad(HH)) | id), family = binomial, x)


o1 = emmeans(fm1, ~1, type = 'response')|> data.frame()
o2 = emmeans(fm2, ~1, type = 'response')|> data.frame()
o3 = emmeans(fm3, ~1, type = 'response')|> data.frame()
o4 = emmeans(fm4, ~1, type = 'response')|> data.frame()
o5 = emmeans(fm5, ~1, type = 'response')|> data.frame()

X = rbindlist(list(o1, o2, o3, o4, o5))
X[, mod := .I|>factor()]

ggplot(X, aes(y = prob, x = mod, color = mod)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), size = 2)

