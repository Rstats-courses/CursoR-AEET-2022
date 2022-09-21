
library("knitr")


titanic <- read.csv("data/titanic_long.csv")
head(titanic)

m5 <- lm(survived ~ class, data = titanic)
layout(matrix(1:4, nrow=2))
plot(m5)
dev.off()

hist(resid(m5))

include_graphics("images/modeling_process.png")

table(titanic$class, titanic$survived)

library("dplyr")
titanic %>%
  group_by(class, survived) %>%
  summarise(count = n())

plot(factor(survived) ~ factor(class), data = titanic)

library(ggmosaic)
ggplot(titanic) +
  geom_mosaic(aes(x = product(survived, class))) +
  labs(x = "", y = "Survived")

tit.glm <- glm(survived ~ class,
               data = titanic,
               family = binomial)

tit.glm <- glm(survived ~ class, data = titanic, family = binomial)
summary(tit.glm)

library("effects")
allEffects(tit.glm)

summary(allEffects(tit.glm))

library("modelbased")
estimate_means(tit.glm)

library("modelbased")
estimate_contrasts(tit.glm)

library("performance")
r2(tit.glm)

kable(xtable::xtable(tit.glm), digits = 2)

## library("modelsummary")
## modelsummary(tit.glm)

plot(allEffects(tit.glm))

library(visreg)
visreg(tit.glm, scale = "response", rug = FALSE)

library(ggplot2)
library(sjPlot)
theme_set(theme_minimal(base_size = 16))
sjPlot::plot_model(tit.glm, type = "eff")

library("parameters")
library("see")
plot(parameters(tit.glm), show_intercept = TRUE)

layout(matrix(1:4, nrow = 2))
plot(tit.glm)
dev.off()

binned_residuals(tit.glm)

## predvals <- predict(tit.glm, type="response")
## arm::binnedplot(predvals, titanic$survived - predvals)

library("DHARMa")
simulateResiduals(tit.glm, plot = TRUE)

library("performance")
check_predictions(tit.glm)

## library(bayesplot)
## sims <- simulate(tit.glm, nsim = 100)
## ppc_bars(titanic$survived, yrep = t(as.matrix(sims)))

library("predtools")
titanic$surv.pred <- predict(tit.glm, type = "response")
calibration_plot(data = titanic, obs = "survived", pred = "surv.pred",
                 x_lim = c(0,1), y_lim = c(0,1))

visreg(tit.glm, scale = "response")

plot(factor(survived) ~ as.factor(sex), data = titanic)

tit.sex <- glm(survived ~ sex, data = titanic, family = binomial)
summary(tit.sex)

allEffects(tit.sex)

plot(allEffects(tit.sex))

simulateResiduals(tit.sex, plot = TRUE)

library("dagitty")
g1 <- dagitty("dag {
              Class -> Survival
              Sex -> Survival
              }")
coordinates(g1) <- list(
  x = c(Class = 1, Sex = 1, Survival = 2),
  y = c(Class = 0, Sex = 2, Survival = 1)
)
plot(g1)

table(titanic$class, titanic$survived, titanic$sex)

tit.sex.class.add <- glm(survived ~ class + sex, family = binomial, data = titanic)
summary(tit.sex.class.add)

plot(allEffects(tit.sex.class.add))

tit.sex.class.int <- glm(survived ~ class * sex, family = binomial, data = titanic)
summary(tit.sex.class.int)

allEffects(tit.sex.class.int)

plot(allEffects(tit.sex.class.int))
# visreg(tit.sex.class, by = "sex", xvar = "class")

plot_model(tit.sex.class.int, type = "int")

library("performance")
compare_performance(tit.sex.class.add, tit.sex.class.int)

compare_parameters(tit.sex.class.add, tit.sex.class.int)

## ## Calibration plot
## titanic$surv.pred <- predict(tit.sex.class.int, type = "response")
## calibration_plot(data = titanic, obs = "survived", pred = "surv.pred",
##                  x_lim = c(0,1), y_lim = c(0,1), nTiles = 10)

tit.prop <- read.csv("data/titanic_prop.csv")
head(tit.prop)

prop.glm <- glm(cbind(Yes, No) ~ Class, data = tit.prop, family = binomial)
summary(prop.glm)

allEffects(prop.glm)

allEffects(tit.glm)

#gdp <- read.csv("http://vincentarelbundock.github.io/Rdatasets/csv/car/UN.csv")
gdp <- read.csv("data/UN_GDP_infantmortality.csv")
names(gdp) <- c("country", "mortality", "gdp")
summary(gdp)

plot(mortality ~ gdp, data = gdp, main = "Infant mortality (per 1000 births)")

gdp.glm <- glm(cbind(mortality, 1000 - mortality) ~ gdp,
               data = gdp, family = binomial)
summary(gdp.glm)

allEffects(gdp.glm)

plot(allEffects(gdp.glm))

library(visreg)
visreg(gdp.glm, scale = "response")
points(mortality/1000 ~ gdp, data = gdp)

simulateResiduals(gdp.glm, plot = TRUE)

simres <- simulateResiduals(gdp.glm, refit = TRUE)
testDispersion(simres, plot = FALSE)

gdp.overdisp <- glm(cbind(mortality, 1000 - mortality) ~ gdp,
               data = gdp, family = quasibinomial)
summary(gdp.overdisp)

coef(gdp.overdisp)

coef(gdp.glm)

plot(allEffects(gdp.glm), main = "binomial")

plot(allEffects(gdp.overdisp), main = "quasibinomial")

visreg(gdp.glm, scale = "response", main = "Binomial")
points(mortality/1000 ~ gdp, data = gdp, pch = 20)

visreg(gdp.overdisp, scale = "response", main = "Quasibinomial")
points(mortality/1000 ~ gdp, data = gdp, pch = 20)

visreg(gdp.glm, ylab = "Mortality (logit scale)")

library(ggResidpanel)
resid_panel(gdp.glm)

gdp.na <- na.omit(gdp)
gdp.na$fit <- fitted(gdp.glm)
plot(gdp.na$fit, gdp.na$mortality,
     xlab = "Probability of mortality (predicted)",
     ylab = "Mortality (observed)")

visreg(gdp.overdisp, main = "Mortality ~ GDP", ylab = "Mortality (logit scale)")

gdp.overdisp2 <- glm(cbind(mortality, 1000 - mortality) ~ gdp + I(gdp*gdp),
               data = gdp, family = quasibinomial)
visreg(gdp.overdisp2, main = "Mortality ~ GDP + GDP^2", ylab = "Mortality (logit scale)")

visreg(gdp.overdisp, main = "Mortality ~ GDP", scale = "response", ylab = "Mortality")
points(mortality/1000 ~ gdp, data = gdp, pch = 20)

gdp.overdisp2 <- glm(cbind(mortality, 1000 - mortality) ~ gdp + I(gdp*gdp),
               data = gdp, family = quasibinomial)
visreg(gdp.overdisp2, main = "Mortality ~ GDP + GDP^2", scale = "response", ylab = "Mortality")
points(mortality/1000 ~ gdp, data = gdp, pch = 20)

visreg(gdp.overdisp, main = "Mortality ~ GDP", ylab = "Mortality (logit scale)")

gdp.overdisp2 <- glm(cbind(mortality, 1000 - mortality) ~ log(gdp),
               data = gdp, family = quasibinomial)
visreg(gdp.overdisp2, main = "Mortality ~ log(GDP)", ylab = "Mortality (logit scale)")

visreg(gdp.overdisp, main = "Mortality ~ GDP", scale = "response", ylab = "Mortality")
points(mortality/1000 ~ gdp, data = gdp, pch = 20)

gdp.overdisp2 <- glm(cbind(mortality, 1000 - mortality) ~ log(gdp),
               data = gdp, family = quasibinomial)
visreg(gdp.overdisp2, main = "Mortality ~ log(GDP)", scale = "response", ylab = "Mortality")
points(mortality/1000 ~ gdp, data = gdp, pch = 20)

## ## Trying Poisson
## m <- glm(mortality ~ log(gdp), data = gdp, family = quasipoisson)
## summary(m)
## visreg(m, scale = "response")
## points(mortality ~ gdp, data = gdp)
##
## gdp.na <- na.omit(gdp)
## gdp.na$fit <- fitted(m)
## plot(gdp.na$fit, gdp.na$mortality,
##      xlab = "Probability of mortality (predicted)",
##      ylab = "Mortality (observed)")

soccer <- read.csv("data/soccer.csv")
soccer

soccer.mod <- glm(cbind(Scored, Nshots - Scored) ~ GoalkeeperTeam, data = soccer, family = binomial)
visreg(soccer.mod, scale = "response",
       ylab = "Probability of scoring")

include_graphics("images/tomato.jpg")

seed <- readr::read_csv("data/seedset.csv")
head(seed)
seed$plant <- as.factor(seed$plant)

plot(seeds ~ ovulecnt, data = seed)

plot(seeds ~ pcmass, data = seed)

seedm <- glm(cbind(seeds, ovulecnt - seeds) ~ plant, data = seed, family = binomial)
#summary(seedm)
plot(allEffects(seedm))

seedm <- glm(cbind(seeds, ovulecnt - seeds) ~ plant + pcmass, data = seed, family = binomial)
#summary(seedm)
plot(allEffects(seedm))
