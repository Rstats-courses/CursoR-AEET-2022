
library("knitr")




trees <- read.csv("data/trees.csv")
head(trees)
trees$site <- as.factor(trees$site)

lm.simple <- lm(height ~ dbh, data = trees)
summary(lm.simple)

library(ggplot2)
ggplot(trees) +
  aes(dbh, height) +
  geom_point() +
  geom_smooth(method = "lm", size = 3) +
  labs(x = "DBH (cm)", y = "Height (m)", title = "Single intercept") +
  theme_minimal(base_size = 16)

ggplot(subset(trees, site == 1 | site == 2)) +
  aes(dbh, height, colour = site) +
  geom_point() +
  geom_smooth(method = "lm", size = 3) +
  labs(x = "DBH (cm)", y = "Height (m)",
       title = "Different intercept for each site") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none")

lm.interc <- lm(height ~ factor(site) + dbh, data = trees)
summary(lm.interc)

ggplot(trees) +
  aes(dbh, height) +
  geom_point() +
  geom_smooth(method = "lm", size = 3) +
  labs(x = "DBH (cm)", y = "Height (m)", title = "Single intercept") +
  theme_minimal(base_size = 16)

ggplot(trees) +
  aes(dbh, height, colour = site) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", size = 1.5, se = FALSE) +
  labs(x = "DBH (cm)", y = "Height (m)",
       title = "Different intercept for each site") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none")

library("lme4")
mixed <- lmer(height ~ dbh + (1|site), data = trees)
summary(mixed)

coef(mixed)

library(broom.mixed)
tidy(mixed)

library(effects)
allEffects(mixed)

plot(allEffects(mixed))

library(visreg)
visreg(mixed, xvar = "dbh", by = "site", re.form = NULL)

visreg(mixed, xvar = "dbh", by = "site", re.form = NULL, overlay = TRUE)

library(sjPlot)
theme_set(theme_minimal(base_size = 16))
#sjp.lmer(mixed, type = "ri.slope")
#plot_model(mixed, type = "eff")

sjPlot::plot_model(mixed, type = "re")

## library("merTools")
## shinyMer(mixed)

plot(mixed)

library("performance")
check_model(mixed)

DHARMa::simulateResiduals(mixed, plot = TRUE, re.form = NULL)

check_predictions(mixed)

r2(mixed)

## ## Predicting heights at NEW sites!
## #https://github.com/lme4/lme4/issues/388#issuecomment-231398937
## newtree <- data.frame(dbh = 30, site = as.factor(25))
## p <- bootMer(mixed,
##         function(x) {simulate(x, newdata = newtree, re.form = ~0, allow.new.levels = TRUE)[[1]]},
##         nsim = 100)
## apply(p$t, 2, mean)
## apply(p$t, 2, sd)
## # similar to:
## apply(simulate(mixed, newdata = newtree, re.form = ~0, allow.new.levels = TRUE, nsim = 100), 1, mean)
##

sitedata <- read.csv("data/sitedata.csv")
sitedata

trees.full <- merge(trees, sitedata, by = "site")
head(trees.full)

group.pred <- lmer(height ~ dbh + (1 | site) + temp, data = trees.full)
summary(group.pred)

mean(sitedata$temp)
trees.full$temp.c <- trees.full$temp - 18

group.pred <- lmer(height ~ dbh + (1 | site) + temp.c, data = trees.full)
summary(group.pred)

## shinyMer(group.pred)

plot(coef(mixed)$site[,1], coef(group.pred)$site[,1],
     xlim = c(10, 25), ylim = c(10, 25),
     xlab = "Without group predictor", ylab = "With group predictor",
     main = "Estimated site effects", las = 1)
abline(a = 0, b = 1)

plot(sitedata$temp, coef(group.pred)$site[,1],
     xlab = "Temperature", ylab = "site effect")

mixed.slopes <- lmer(height ~ dbh + (1 + dbh | site), data=trees)

summary(mixed.slopes)

coef(mixed.slopes)

plot_model(mixed.slopes, type = "re")

data("sleepstudy")
library(ggplot2)
ggplot(sleepstudy) +
  aes(x = Days, y = Reaction) +
  geom_point() +
  facet_wrap(~Subject)

sleep <- lmer(Reaction ~ Days + (1+Days|Subject), data = sleepstudy)
summary(sleep)

visreg(sleep, xvar = "Days", by = "Subject", re.form = NULL)

library(mgcv)
sgamm <- mgcv::gam(Reaction ~ s(Days, Subject, k = 3, bs = "fs"),
                   data = sleepstudy, method = "REML")
summary(sgamm)
#gam.check(sleep.gamm)
#coef(p)

visreg(sgamm, xvar = "Days", by = "Subject")

include_graphics("images/gamm_paper.PNG")

plot(dead ~ dbh, data = trees)

plot(factor(dead) ~ dbh, data = trees)

simple.logis <- glm(dead ~ dbh, data = trees, family=binomial)
summary(simple.logis)

logis2 <- glm(dead ~ dbh + factor(site), data = trees, family=binomial)
summary(logis2)

mixed.logis <- glmer(dead ~ dbh + (1|site), data=trees, family = binomial)
summary(mixed.logis)

coef(mixed.logis)

visreg(mixed.logis, xvar = "dbh", by = "site", scale = "response")

plot_model(mixed.logis, type = "eff", show.ci = TRUE)
