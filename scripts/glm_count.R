
library("knitr")


library("ggplot2")

seedl <- read.csv("data/seedlings.csv")
summary(seedl)

table(seedl$count)

hist(seedl$count)

plot(seedl$light, seedl$count, xlab = "Light (GSF)", ylab = "Seedlings")

seedl.glm <- glm(count ~ light,
                 data = seedl,
                 family = poisson)

equatiomatic::extract_eq(seedl.glm)

seedl.glm <- glm(count ~ light, data = seedl, family = poisson)
summary(seedl.glm)

coef(seedl.glm)[1]

exp(coef(seedl.glm)[1])

library(effects)
allEffects(seedl.glm)

## library(arm)
## library(ggplot2)
##
## coefs <- as.data.frame(coef(sim(seedl.glm)))
## names(coefs) <- c("intercept", "slope")
##
## ggplot(coefs) +
##   geom_density(aes(slope), fill = "grey80") +
##   xlim(-0.02, 0.02) +
##   geom_vline(xintercept = 0) +
##   ggtitle("Effect of light on seedling abundance")

library("parameters")
plot(simulate_parameters(seedl.glm)) +
  geom_vline(xintercept = 0) +
  ggtitle("Effect of light on seedling abundance")

#allEffects(seedl.glm)
plot(allEffects(seedl.glm))

library(visreg)
visreg(seedl.glm, scale = "response", ylim = c(0, 7))
points(count ~ light, data = seedl, pch = 20)

library(sjPlot)
library(ggplot2)
theme_set(theme_minimal(base_size = 16))
sjPlot::plot_model(seedl.glm, type = "eff", show.data = TRUE)

library("performance")
r2(seedl.glm)

library("report")
report(seedl.glm)

layout(matrix(1:4, nrow=2))
plot(seedl.glm)
dev.off()

ggResidpanel::resid_panel(seedl.glm)

library("performance")
check_model(seedl.glm)

plot(seedl$light, resid(seedl.glm))

sims <- simulate(seedl.glm, nsim = 100)
yrep <- t(as.matrix(sims))
bayesplot::ppc_rootogram(seedl$count, yrep)

check_predictions(seedl.glm)

library(DHARMa)
simulateResiduals(seedl.glm, plot = TRUE)

include_graphics("images/Gaus-Pois.png")

simres <- simulateResiduals(seedl.glm, refit = TRUE)
testDispersion(simres)

seedl.overdisp <- glm(count ~ light, data = seedl, family = quasipoisson)
summary(seedl.overdisp)

allEffects(seedl.overdisp)
allEffects(seedl.glm)

plot(allEffects(seedl.overdisp))

plot(allEffects(seedl.glm))

library("MASS")
seedl.nb <- glm.nb(count ~ light, data = seedl)
summary(seedl.nb)

compare_models(seedl.glm, seedl.nb)
compare_performance(seedl.glm, seedl.nb)

head(seedl)

seedl.offset <- glm(count ~ light,
                    offset = log(area),
                    data = seedl,
                    family = poisson)

summary(seedl.offset)

exp(coef(seedl.offset)[1])

new.lights <- data.frame(light = c(10, 90))
predict(seedl.glm, newdata = new.lights, type = "response", se.fit = TRUE)
