

include_graphics("images/gam_simpson_fig2.jpg")

include_graphics("images/gam_simpson_fig3.jpg")

isotopes <- readRDS("data/isotope.rds")
head(isotopes)

library("mgcv")
m <- gam(d15N ~ s(Year, k = 15), data = isotopes, method = "REML")
summary(m)

library("visreg")
visreg(m)

library("DHARMa")
simulateResiduals(m, plot = TRUE)

gam.check(m)

testTemporalAutocorrelation(simulateResiduals(m),
                            time = isotopes$Year)

mod <- gamm(d15N ~ s(Year, k = 15), data = isotopes,
            correlation = corCAR1(form = ~ Year), method = "REML")
summary(mod$gam)

mort <- read.csv("data/UN_GDP_infantmortality.csv")
head(mort)

library("MASS")
mort.glm <- glm.nb(infant.mortality ~ gdp, data = mort)
summary(mort.glm)

visreg(mort.glm)

mort$log.gdp <- log(mort$gdp)
mort.glm.log <- glm.nb(infant.mortality ~ log.gdp, data = mort)
summary(mort.glm.log)

visreg(mort.glm.log)

library("mgcv")
mort.gam <- gam(infant.mortality ~ s(log.gdp), family = nb, data = mort)
summary(mort.gam)

visreg(mort.gam)

gam.check(mort.gam)

library("performance")
compare_performance(mort.glm, mort.glm.log, mort.gam)

library("lme4")
data("sleepstudy")
library("ggplot2")
ggplot(sleepstudy) +
  aes(x = Days, y = Reaction) +
  geom_point() +
  facet_wrap(~Subject)

sgamm <- gam(Reaction ~ s(Days, Subject, k = 3, bs = "fs"),
                   data = sleepstudy, method = "REML")
summary(sgamm)
#gam.check(sleep.gamm)
#coef(p)

visreg(sgamm, xvar = "Days", by = "Subject")
