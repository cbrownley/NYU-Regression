library("arm")
library("rstanarm")
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))
library("rosdata")


# Simulate fake data
x <- 1:20
n <- length(x)
a <- 0.2
b <- 0.3
sigma <- 0.5
# set the random seed to get reproducible results
# change the seed to experiment with variation due to random noise
set.seed(2141) 
y <- a + b*x + sigma*rnorm(n)
fake <- data.frame(x, y)


# Fit a classical linear regression
options(show.signif.stars = FALSE, 
        show.coef.Pvalues = FALSE)

fit_fake_ols   <- lm(y ~ x, data = fake)

summary( fit_fake_ols )
arm::display( fit_fake_ols , digits = 2 )

par(mfrow=c(2,2))
plot( fit_fake_ols )
par(mfrow=c(1,1))

names( fit_fake_ols )
fit_fake_ols$fitted.values
fit_fake_ols$residuals
fake$y - fit_fake_ols$fitted.values

fake_res <- summary( fit_fake_ols )
names( fake_res )


# Fit a Bayesian linear regression
fit_fake_bayes <- stan_glm(y ~ 1 + x, data = fake, seed=2141)

print( fit_fake_bayes , digits = 2 )
summary( fit_fake_bayes )
plot( fit_fake_bayes )

# Plot the data and fitted regression line
# par(mar=c(3,3,1,1), mgp=c(1.7,.5,0), tck=-.01)
plot(fake$x, fake$y, bty="l", pch=20, 
     main="Data and fitted regression line", 
     xlab = "x", ylab = "y")
a_hat <- coef(fit_fake_bayes)[1]
b_hat <- coef(fit_fake_bayes)[2]
abline(a_hat, b_hat)
x_bar <- mean(fake$x)
text(x_bar, a_hat + b_hat*x_bar, paste("   y =", round(a_hat, 2), "+", round(b_hat, 2), "* x"), adj=0)



# A single binary predictor

# Fit a classical linear regression
options(show.signif.stars = FALSE, 
        show.coef.Pvalues = FALSE)
fit_ols <- lm(kid_score ~ 1 + mom_hs, data=kidiq)
summary( fit_ols )
arm::display( fit_ols )
par(mfrow=c(2,2))
plot( fit_ols )
par(mfrow=c(1,1))

# Display confidence intervals for the estimates
round( confint( fit_ols , level = .95) , 2)

res <- summary( fit_ols )
names(res)
str(res)
res$coefficients[, "Std. Error"]
res$coefficients[, 2]
( se <- sqrt( diag( vcov( fit_ols ) ) ) )

# ROS pg. 53

# CIs "by-hand"
res <- summary( fit_ols )
names(res)

n <- length(kidiq$kid_score)
# CI for intercept
res$coefficients[1, "Estimate"] + 
  qt(c(0.025,0.975), n-1) * res$coefficients[1, "Std. Error"]
# CI for slope
res$coefficients[2, "Estimate"] + 
  qt(c(0.025,0.975), n-1) * res$coefficients[2, "Std. Error"]



# Fit a Bayesian linear regression
prior_summary( stan_glm(kid_score ~ mom_hs, data=kidiq) )
fit_1 <- stan_glm(kid_score ~ mom_hs, data=kidiq)                               # , refresh = 0
print( fit_1 )
summary( fit_1 )
plot( fit_1 )

# Display the data and fitted regression
jitt <- runif(nrow(kidiq), -.03, .03)
plot(kidiq$mom_hs + jitt, kidiq$kid_score, 
     xlab="Mother completed high school", ylab="Child test score", 
     bty="l", pch=20, xaxt="n", yaxt="n")
axis(1, c(0,1))
axis(2, seq(20, 140, 40))
abline(coef(fit_1), col="steelblue", lwd=2)

# Display uncertainty in the fitted regression
sims_1 <- as.matrix(fit_1)
n_sims_1 <- nrow(sims_1)
subset <- sample(n_sims_1, 10)
for (i in subset){
  abline(sims_1[i,1], sims_1[i,2], col="gray")
}
abline(coef(fit_1)[1], coef(fit_1)[2], col="black")

# Extract posterior draws from the fitted model object
posterior <- as.array(fit_1)
dim(posterior)
dimnames(posterior)
head(posterior)

# Plot posterior draws of the coefficients a, b
sims <- as.matrix(fit_1)
matplot(sims[,1], sims[,2], pch=20, cex=0.5, col="royalblue", xlab="Intercept", ylab="Slope", main="Posterior draws of the regression coefficients a, b")


# Plot posterior draws of the line, y = a + bx
jitt <- runif(nrow(kidiq), -.03, .03)
plot(kidiq$mom_hs + jitt, kidiq$kid_score,
     xlab="Mother completed high school", ylab="Child test score",
     main="Data and range of possible linear fits", 
     bty="l", pch=20, xaxt="n", yaxt="n", 
     mgp=c(2,.5,0), cex.lab=1.3, cex.main=1.3)
sims <- as.matrix(fit_1)
n_sims <- nrow(sims)
for (s in sample(n_sims, 50))
  abline(sims[s,1], sims[s,2], col="gray50", lwd=0.5)
#with(kidiq, points(mom_hs, kid_score, pch=20, cex=1.25, col="royalblue"))


# Extract posterior draws from the fitted model object
posterior <- as.array(fit_1)
dim(posterior)
dimnames(posterior)
head(posterior)


# Means and Standard Deviations             
# Medians and Median Absolute Deviations (MAD)
# Intercept
mean(posterior[ , , "(Intercept)"])
sd(posterior[ , , "(Intercept)"])

median(posterior[ , , "(Intercept)"])
mad(posterior[ , , "(Intercept)"])

# Slope
mean(posterior[ , , "mom_hs"])
sd(posterior[ , , "mom_hs"])    

median(posterior[ , , "mom_hs"])
mad(posterior[ , , "mom_hs"])

# Sigma
mean(posterior[ , , "sigma"])              
sd(posterior[ , , "sigma"])

median(posterior[ , , "sigma"])
mad(posterior[ , , "sigma"])


# Means and Standard Deviations             
# Medians and Median Absolute Deviations (MAD)
# Intercept
mean(posterior[ , , "(Intercept)"])       median(posterior[ , , "(Intercept)"])      
sd(posterior[ , , "(Intercept)"])         mad(posterior[ , , "(Intercept)"])

# Slope
mean(posterior[ , , "mom_hs"])            median(posterior[ , , "mom_hs"])   
sd(posterior[ , , "mom_hs"])              mad(posterior[ , , "mom_hs"])      

# Sigma
mean(posterior[ , , "sigma"])             median(posterior[ , , "sigma"])              
sd(posterior[ , , "sigma"])               mad(posterior[ , , "sigma"])

# rstanarm: Bayesian compatibility intervals                                       for β1:
ci95 <- posterior_interval(fit_1, prob = 0.95)                                  # , pars = "mom_hs"
round(ci95, 2)


# Posterior predictive check
# https://mc-stan.org/bayesplot/articles/graphical-ppcs.html
# In the plot, the dark line is the distribution of the observed outcomes y and 
# each of the 50 lighter lines is the kernel density estimate of one of the 
# replications of y from the posterior predictive distribution
pp_check(fit_1)
yrep <- posterior_predict(fit_1, draws = 500)
ppc_dens_overlay(kidiq$kid_score, yrep[1:50, ])



# A single continuous predictor

# Fit a classical linear regression
options(show.signif.stars = FALSE, 
        show.coef.Pvalues = FALSE)
mean_mom_iq <- mean(kidiq$mom_iq)
mom_iq_centered = kidiq$mom_iq - mean_mom_iq
fit_2 <- lm(kid_score ~ mom_iq_centered, data=kidiq)
summary( fit_2 )
arm::display( fit_2 )
par(mfrow=c(2,2))
plot( fit_2 )
par(mfrow=c(1,1))


# Display the data and fitted regression
# Plot points and regression line (x-axis original units)
labels <-
  c(-40, -20, 0, 20, 40) + mean(kidiq$mom_iq) %>% 
  round(digits = 0)

kidiq %>%
  ggplot(aes(x = mom_iq_centered, y = kid_score)) +
  geom_abline(intercept = coef(fit_2)[1], 
              slope     = coef(fit_2)[2]) +
  geom_point(shape = 1, size = 2) +
  scale_x_continuous("Mother IQ score",
                     breaks = c(-40, -20, 0, 20, 40),
                     labels = labels) +
  labs(y = "Child test score", title = "") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))


# Fit a Bayesian linear regression
mean_mom_iq <- mean(kidiq$mom_iq)
mom_iq_centered <- kidiq$mom_iq - mean_mom_iq
prior_summary( stan_glm(kid_score ~ mom_iq, data=kidiq) )
fit_2 <- stan_glm(kid_score ~ mom_iq, data=kidiq)                               # , refresh = 0
print(fit_2)
summary(fit_2)
plot(fit_2)

# Display the data and fitted regression
plot(kidiq$mom_iq, kidiq$kid_score, 
     xlab="Mother IQ score", ylab="Child test score")
abline(coef(fit_2), col="steelblue", lwd=2)

# Display uncertainty in the fitted regression
sims_2 <- as.matrix(fit_2)
n_sims_2 <- nrow(sims_2)
subset <- sample(n_sims_2, 10)
plot(kidiq$mom_iq, kidiq$kid_score,
     xlab="Mother IQ score", ylab="Child test score")
for (i in subset){
  abline(sims_2[i,1], sims_2[i,2], col="gray")
}
abline(coef(fit_2)[1], coef(fit_2)[2], col="black")

# Posterior predictive check
# https://mc-stan.org/bayesplot/articles/graphical-ppcs.html
pp_check(fit_2)
yrep <- posterior_predict(fit_2, draws = 500)
ppc_dens_overlay(kidiq$kid_score, yrep[1:50, ])



# Two predictors -- model without interaction
# Change mom_iq_centered to mom_iq to create the plots below
# Fit a Bayesian linear regression
prior_summary( stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq) )
fit_3 <- stan_glm(kid_score ~ 1 + mom_hs + mom_iq, data=kidiq)                      # , refresh = 0
print(fit_3)
summary(fit_3)
plot(fit_3)

# Display the data and fitted regression
colors <- ifelse(kidiq$mom_hs==1, "black", "gray")
plot(kidiq$mom_iq, kidiq$kid_score,
     xlab="Mother IQ score", ylab="Child test score", col=colors, pch=20)
b_hat <- coef(fit_3)
abline(b_hat[1] + b_hat[2], b_hat[3], col="black")
abline(b_hat[1], b_hat[3], col="gray")

# Display uncertainty in the fitted regression
# Lines on same plot
sims_3 <- as.matrix(fit_3)
n_sims_3 <- nrow(sims_3)
subset <- sample(n_sims_3, 10)
colors <- ifelse(kidiq$mom_hs==1, "black", "gray")
plot(kidiq$mom_iq, kidiq$kid_score,
     xlab="Mother IQ score", ylab="Child test score", col=colors, pch=20)
for (i in subset){
  abline(sims_3[i,1], sims_3[i,3], col="gray")
  abline(sims_3[i,1] + sims_3[i,2], sims_3[i,3], col="black")
}
abline(coef(fit_3)[1], coef(fit_3)[3], col="gray")
abline(coef(fit_3)[1] + coef(fit_3)[2], coef(fit_3)[3], col="black")

# Lines on two separate plots
par(mfrow=c(1,2))
plot(kidiq$mom_iq, kidiq$kid_score, 
     xlab="Mother IQ score", ylab="Child test score", 
     bty="l", pch=20, xaxt="n", yaxt="n")
axis(1, seq(80, 140, 20))
axis(2, seq(20, 140, 40))
mom_hs_bar <- mean(kidiq$mom_hs)
subset <- sample(n_sims_3, 10)
for (i in subset){
  curve(cbind(1, mom_hs_bar, x) %*% sims_3[i,1:3], lwd=.5,
        col="gray", add=TRUE)
}
curve(cbind(1, mom_hs_bar, x) %*% coef(fit_3), col="black", add=TRUE)
jitt <- runif(nrow(kidiq), -.03, .03)
plot(kidiq$mom_hs + jitt, kidiq$kid_score, 
     xlab="Mother completed high school", ylab="Child test score", 
     bty="l", pch=20, xaxt="n", yaxt="n")
axis(1, c(0,1))
axis(2, seq(20, 140, 40))
mom_iq_bar <- mean(kidiq$mom_iq)
for (i in subset){
  curve(cbind(1, x, mom_iq_bar) %*% sims_3[i,1:3], lwd=.5,
        col="gray", add=TRUE)
}
curve(cbind(1, x, mom_iq_bar) %*% coef(fit_3), col="black", add=TRUE)
par(mfrow=c(1,1))

# Posterior predictive check
# https://mc-stan.org/bayesplot/articles/graphical-ppcs.html
pp_check(fit_3)
yrep <- posterior_predict(fit_3, draws = 500)
ppc_dens_overlay(kidiq$kid_score, yrep[1:50, ])



# Two fitted regression lines -- model with interaction
# Change mom_iq_centered to mom_iq to create the plots below
# Fit a Bayesian linear regression
fit_4 <- stan_glm(kid_score ~ 1 + mom_hs + mom_iq + mom_hs:mom_iq, 
                  data=kidiq)                                                   # , refresh = 0
print(fit_4)
summary(fit_4)
plot(fit_4)

# Display the data and fitted regression
colors <- ifelse(kidiq$mom_hs==1, "black", "gray")
plot(kidiq$mom_iq, kidiq$kid_score,                                             # xlim = c(0,140), ylim = c(-40,140),
     xlab="Mother IQ score", ylab="Child test score", col=colors, pch=20)
b_hat <- coef(fit_4)
abline(b_hat[1] + b_hat[2], b_hat[3] + b_hat[4], col="black")
abline(b_hat[1], b_hat[3], col="gray")

# Display uncertainty in the fitted regression
# Lines on same plot
sims_4 <- as.matrix(fit_4)
n_sims_4 <- nrow(sims_4)
subset <- sample(n_sims_4, 10)
colors <- ifelse(kidiq$mom_hs==1, "black", "gray")
plot(kidiq$mom_iq, kidiq$kid_score,
     xlab="Mother IQ score", ylab="Child test score", col=colors, pch=20)
for (i in subset){
  abline(sims_4[i,1], sims_4[i,3], col="gray")
  abline(sims_4[i,1] + sims_4[i,2], sims_4[i,3] + sims_4[i,4], col="black")
}
abline(coef(fit_4)[1], coef(fit_4)[3], col="gray")
abline(coef(fit_4)[1] + coef(fit_4)[2], coef(fit_4)[3] + coef(fit_4)[4], col="black")

# Lines on two separate plots
par(mfrow=c(1,2))
plot(kidiq$mom_iq, kidiq$kid_score, 
     xlab="Mother IQ score", ylab="Child test score", 
     bty="l", pch=20, xaxt="n", yaxt="n")
axis(1, seq(80, 140, 20))
axis(2, seq(20, 140, 40))
mom_hs_bar <- mean(kidiq$mom_hs)
subset <- sample(n_sims_4, 10)
for (i in subset){
  curve(cbind(1, mom_hs_bar, x, x*mom_hs_bar) %*% sims_4[i,1:4], lwd=.5,
        col="gray", add=TRUE)
}
curve(cbind(1, mom_hs_bar, x, x*mom_hs_bar) %*% coef(fit_4), col="black", add=TRUE)
jitt <- runif(nrow(kidiq), -.03, .03)
plot(kidiq$mom_hs + jitt, kidiq$kid_score, 
     xlab="Mother completed high school", ylab="Child test score", 
     bty="l", pch=20, xaxt="n", yaxt="n")
axis(1, c(0,1))
axis(2, seq(20, 140, 40))
mom_iq_bar <- mean(kidiq$mom_iq)
for (i in subset){
  curve(cbind(1, x, mom_iq_bar, x*mom_iq_bar) %*% sims_4[i,1:4], lwd=.5,
        col="gray", add=TRUE)
}
curve(cbind(1, x, mom_iq_bar, x*mom_iq_bar) %*% coef(fit_4), col="black", add=TRUE)
par(mfrow=c(1,1))

# Posterior predictive check
# https://mc-stan.org/bayesplot/articles/graphical-ppcs.html
pp_check(fit_4)
yrep <- posterior_predict(fit_4, draws = 500)
ppc_dens_overlay(kidiq$kid_score, yrep[1:50, ])


#### A Few Special Cases ####

## Estimating a Proportion ##
y <- rep(c(0, 1), c(40, 10))
simple <- data.frame(y)
fit_prop <- stan_glm(y ~ 1, family = binomial(link = "logit"), data = simple)
print(fit_prop, digits = 2)

# Predicted probability
round( plogis(fit_prop$coefficients) , 2)

# Plus/Minus 1 standard error bounds on predicted probability
round( plogis(fit_prop$coefficients + (c(-1,1)*fit_prop$ses)) , 3)

# Inference on the probability scale
new <- data.frame(x=0)
epred <- posterior_epred(fit_prop, newdata = new)
round(mean(epred), 2)
round(sd(epred), 2)


## Comparing Two Proportions ##
x <- rep(c(0, 1), c(50, 60))
y <- rep(c(0, 1, 0, 1), c(40, 10, 40, 20))
simple <- data.frame(x, y)
fit_props <- stan_glm(y ~ x, family = binomial(link = "logit"), data = simple)
print(fit_props, digits = 2)

# Difference in probabilities
new <- data.frame(x = c(0, 1))
epred <- posterior_epred(fit_props, newdata = new)
diff <- epred[,2] - epred[,1]
print( c( round(mean(diff), 2) , round(sd(diff), 2) ) )


## Estimating a Mean ##
n_0 <- 200
set.seed(2141)
y_0 <- rnorm(n = n_0, mean = 2.0, sd = 5.0)
fake_0 <- data.frame(y_0)

fit_m <- stan_glm(y_0 ~ 1, data = fake_0)
print(fit_m, digits = 2)


## Comparing Two Means ##
n_1 <- 300
set.seed(2141)
y_1 <- rnorm(n = n_1, mean = 8.0, sd = 5.0)

diff <- mean(y_1) - mean(y_0)
se_0 <- sd(y_0) / sqrt(n_0)
se_1 <- sd(y_1) / sqrt(n_1)
se <- sqrt(se_0^2 + se_1^2)

# difference in means
print(round(diff, 2))

# standard error of difference in means
print(round(se, 2))

# difference in means as a regression on an indicator variable
n <- n_0 + n_1
y <- c(y_0, y_1)
x <- c(rep(0, n_0), rep(1, n_1))
fake <- data.frame(y, x)
fit_dim <- stan_glm(y ~ x, data = fake)
print(fit_dim, digits = 2)




#### Jittering points: RwG pg. 141 ####
library(haven)
concord1 <- haven::read_dta('/Users/clinton/Documents/NYU Regression/rwg/concord1.dta')
head(concord1)

f0 <- lm(educat ~ retire, data = concord1)

## Original data
plot(concord1$retire, concord1$educat, 
     xlab="", ylab="Education in Years", main = "Original Data", xaxt="n")
axis(side = 1, at = c(0, 1), labels = c("not retired", "retired"))
abline(f0, lwd = 2)

## Jittered data
jitt_x <- runif(nrow(concord1), -.03, .03)
jitt_y <- runif(nrow(concord1), -2.0, 2.0)

plot(concord1$retire + jitt_x, concord1$educat + jitt_y, 
     xlab="", ylab="Education in Years", main = "Jittered Data", xaxt="n")
axis(side = 1, at = c(0, 1), labels = c("not retired", "retired"))
abline(f0, lwd = 2)
