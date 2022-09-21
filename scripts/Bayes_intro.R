


trees <- read.csv("data/trees.csv")
summary(trees[, 1:3])

plot(trees$dbh, trees$height, pch=20, las=1, cex.lab=1.4, xlab="DBH (cm)", ylab="Height (m)")

simple.lm <- lm(height ~ dbh, data = trees)
summary(simple.lm)

summary(trees$dbh)
trees$dbh.c <- trees$dbh - 25

plot(trees$dbh.c, trees$height, pch=20, las=1, cex.lab=1.4, xlab="DBH (cm)", ylab="Height (m)")
abline(lm(height ~ dbh.c, data=trees), col="red", lwd=3)

library(arm)
simple.lm <- lm(height ~ dbh.c, data = trees)
display(simple.lm)

library("brms")

height.formu <- brmsformula(height ~ dbh.c)

get_prior(height.formu, data = trees)

plot(density(rnorm(1000, 0, 1000)), main="", xlab="Height (m)")

plot(density(rnorm(1000, 2, 0.5)), main="", xlab="Height (m)")

priors <- c(
  set_prior("normal(30, 10)", class = "Intercept"),
  set_prior("normal(0.5, 0.4)", class = "b"),
  set_prior("normal(0, 5)", class = "sigma")
)

plot(density(rnorm(1000, 25, 10)))

plot(density(rnorm(1000, 0.5, 0.5)))

sig <- rnorm(10000, 0, 5)
sigma <- na.omit(ifelse(sig < 0, NA, sig))
hist(sigma, breaks = 30, probability = T)

height.mod <- brm(height.formu,
         data = trees,
         prior = priors,
         sample_prior = "only")

pp_check(height.mod, ndraws = 100)

height.mod <- brm(height.formu,
         data = trees,
         prior = priors)

summary(height.mod)

plot(height.mod)

pp_check(height.mod, ndraws = 100)

## library("shinystan")
## launch_shinystan(height.mod)
