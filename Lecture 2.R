library("car")
library("broom")
library("broom.mixed")
library("gvlma")
library("haven")
library("MASS")
library("olsrr")
library("tidyverse")
library("rstan")
library("rstanarm")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library("ggplot2")
library("ggpubr")
library("grid")
library("gridExtra")
library("humanize")
library("scales")
library("patchwork")
library("wooldridge")



# ROS Ch 11 pg. 153 Assumptions, Diagnostics, and Model Evaluation

# Regression Diagnostics
# https://www.statmethods.net/stats/rdiagnostics.html
fit <- lm( mpg ~ disp + hp + wt + drat , data = mtcars )
summary(fit)

# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics

# Diagnostic Plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)
par(mfrow=c(1,1))

# Evaluate Nonlinearity
# component + residual plot
car::crPlots(fit)


# Test for Autocorrelated Errors
car::durbinWatsonTest(fit)


# Evaluate homoscedasticity
# non-constant error variance test
car::ncvTest(fit)

# plot studentized residuals vs. fitted values
car::spreadLevelPlot(fit)


# Normality of Residuals
# qq plot for studentized resid
car::qqPlot(fit, main="QQ Plot")

# distribution of studentized residuals
sresid <- MASS::studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)


# Assessing Outliers
# https://www.statmethods.net/stats/rdiagnostics.html
car::outlierTest(fit) # Bonferonni p-value for most extreme obs

car::qqPlot(fit, main="QQ Plot") # qq plot for studentized resid

car::leveragePlots(fit) # leverage plots


# Influential Observations
# added variable plots
car::avPlots(fit)

# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)

# Influence Plot
car::influencePlot(fit, id=TRUE, main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


# Evaluate Collinearity
vif(fit) # variance inflation factors
sqrt(vif(fit)) > 2 # problem?


# Global test of model assumptions
# The gvlma() function in the gvlma package, 
# performs a global validation of linear model assumptions 
# as well separate evaluations of skewness, kurtosis, and heteroscedasticity.
gvmodel <- gvlma::gvlma(fit)
summary(gvmodel)




# Nonnormality: Regression with Graphics: Concord Water Study

# Figure 4.9 Four views showing nonnormality of residuals from regression of 1981 household water use on six predictors Pg. 124

concord1 <- haven::read_dta('concord1.dta')
head(concord1)

# Example of Chapter 3:
# Y is postshortage (1981) water use
# X1 is household income
# X2 is preshortage (1980) water use
# X3 is education of household head
# X4 is a dummy variable for retirement
# X5 is number of people in household at time of water shortage
# X6 is increase in number of people, summer 1981 minus summer 1980

fit_water <- lm(water81 ~ income + water80 + educat + retire + peop81 + cpeop, data = concord1)

arm::display(fit_water, digits = 1)

# Save residuals in concord1
concord1$residuals <- resid(fit_water)

# Histogram, Boxplot, Symmetry plot, and Quantile-Normal plot of residuals

# Histogram
# https://www.statology.org/overlay-normal-curve-histogram-in-r/
p1 <- ggplot(concord1, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill='lightgray', col='black') +
  stat_function(fun = dnorm, 
                args = list(mean = mean(concord1$residuals),
                            sd   =   sd(concord1$residuals))) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(title = "Histogram",
       y = "Density", 
       x = "Residual") + 
  theme_light()
p1

# Boxplot
p2 <- ggplot(concord1, aes(x=factor(0), y = residuals)) +
  stat_boxplot(geom = "errorbar",
               width = 0.10) + 
  geom_boxplot(outlier.shape = 1) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "black") +
  labs(title = "Boxplot",
       y = "CPI", 
       x = NULL) + 
  theme_light() +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())
p2

# Symmetry Plot
# LearnEDAfunctions::symplot(x)
# https://rdrr.io/github/bayesball/LearnEDAfunctions/src/R/symplot.R
symdat <- function(d){
  n <- length(d)
  no <- floor((n + 1) / 2)
  sd <- sort(d)
  i <- 1 : no
  u <- sd[n + 1 - i] - median(d)
  v <- median(d) - sd[i]
  return(list("v"=v, "u"=u))
}

dat <- as.data.frame(symdat(concord1$residuals))

p3 <- ggplot(dat, aes(v, u)) + 
  geom_point(shape=1) + 
  geom_abline() +
  labs(title = "Symmetry Plot",
       y="Distance above median",
       x="Distance below median") + 
  theme_light()
p3

# Quantile-Normal Plot
p4 <- ggplot(concord1, aes(sample = residuals)) + 
  stat_qq(shape=1) + 
  stat_qq_line() +
  labs(title = "Quantile-Normal Plot",
       y="Residual",
       x="Gaussian (normal) Distribution") + 
  theme_light()
p4

# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# https://cran.r-project.org/web/packages/egg/vignettes/Overview.html
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)




# Influence Analysis: Regression with Graphics: Concord Water Study

concord1 <- haven::read_dta('concord1.dta')
head(concord1)

# Example of Chapter 3:
# Y is postshortage (1981) water use
# X1 is household income
# X2 is preshortage (1980) water use
# X3 is education of household head
# X4 is a dummy variable for retirement
# X5 is number of people in household at time of water shortage
# X6 is increase in number of people, summer 1981 minus summer 1980

fit_water <- lm(water81 ~ income + water80 + educat + retire + peop81 + cpeop, data = concord1)

arm::display(fit_water, digits = 2)

# A high-income, high-water-use household: Household/Case #134
concord1["125", ]

# DFBETAS measures the influence of the ith case on the kth regression coefficient
# DFBETAS answers the question "By how many standard errors does betak change, if we drop case i?"
# If DFBETAS > 0, case i pulls betak up
# If DFBETAS < 0, case i pulls betak down

inflm.water <- influence.measures(fit_water)
inflm.water          # all


# Table 4.3 Page 127
# Summary statistics for DFBETAS of coefficients in household water use regression
as_tibble(inflm.water$infmat[, c("dfb.incm", "dfb.wt80", "dfb.edct", "dfb.retr", "dfb.pp81", "dfb.cpep")]) %>%
  broom::tidy() %>%
  dplyr::select(column, n, mean, sd, min, max)


which(apply(inflm.water$is.inf, 1, any))

# which observations 'are' influential
( summ.inflm <- summary(inflm.water)) # only these

plot(fit_water, which = 5) # an enhanced version of that via plot(<lm>)


# DFBETAS value for Household/Case 134
summ.inflm["125", "dfb.incm"]


# Calculate a DFBETAS by-hand: Influence of one high-income, high-water-use household: Household #134
fit_without_household_134 <- lm(water81 ~ income + water80 + educat + retire + peop81 + cpeop, data = subset(concord1, case != 134))

arm::display(fit_without_household_134, digits = 2)

# difference between two income coefficients
( numerator <- coef(fit_water)[2] - coef(fit_without_household_134)[2] )

# standard deviation of residuals, with household #134 deleted
sigma(fit_without_household_134)

# regress income on the other five X variables (not deleting case i)
fit_income <- lm(income ~ water80 + educat + retire + peop81 + cpeop, data = concord1)

arm::display(fit_income, digits = 2)

# residual sum of squares RSS = 60,129
# method 1
deviance(fit_income)
# method 2
sum(resid(fit_income)^2)
# method 3
anova(fit_income)[[2]][6]

# denominator
( denominator <- sigma(fit_without_household_134) / sqrt(deviance(fit_income)) )


# Calculate DFBETAS for Household #134:  1.34

# Interpretation: With household #134 included, the coefficient on income is 1.34 standard errors higher than it otherwise would be.  This is the most influential case; for no other household is |DFBETAS| so large.
( DFBETAS_134 <- numerator / denominator )




# Mean and standard deviation for prior
kidiq <- read.csv(url("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv"))
head(kidiq)

paste("mean(kid_score) =", round( mean(kidiq$kid_score) ,2) )
paste("sd(kid_score) =",   round(   sd(kidiq$kid_score) ,2) )

paste("mean(mom_iq) =", mean(kidiq$mom_iq))
paste("sd(mom_iq) =",   sd(kidiq$mom_iq))

mean_mom_iq <- mean(kidiq$mom_iq)
mom_iq_centered = kidiq$mom_iq - mean_mom_iq


# Fit a classical linear regression
options(show.signif.stars = FALSE, 
        show.coef.Pvalues = FALSE)
fit_ols <- lm(kid_score ~ mom_iq_centered, data=kidiq)
summary( fit_ols )
arm::display( fit_ols )

# Diagnostic Plots
par(mfrow=c(2,2))
plot(fit_ols)
par(mfrow=c(1,1))

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit_ols)
par(mfrow=c(1,1))


# Display confidence intervals for the estimates
round( confint( fit_ols , level = .95) , 2 )


# Residuals
kidiq$kid_score - fit_ols$fitted.values
summary( kidiq$kid_score - fit_ols$fitted.values )

residuals( fit_ols )
summary( residuals(fit_ols) )


# Coefficients: b0 and b1
# b1
( b1 <- sum( (mom_iq_centered - mean(mom_iq_centered)) * (kidiq$kid_score - mean(kidiq$kid_score)) ) / 
  sum( (mom_iq_centered - mean(mom_iq_centered))^2 ) )
# b0
mean(kidiq$kid_score) - (b1 * mean(mom_iq_centered))


# V1: standard errors of b0 and b1
# https://stats.stackexchange.com/questions/44838/how-are-the-standard-errors-of-coefficients-calculated-in-a-regression
y <- kidiq$kid_score
X <- cbind(1, mom_iq_centered)

betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
var_betaHat <- anova(fit_ols)[[3]][2] * solve(t(X) %*% X)
#------comparison------
#estimate
fit_ols$coef
c(betaHat[1], betaHat[2])
#standard error
summary(fit_ols)$coefficients[, 2]
sqrt(diag(var_betaHat))

# V2: standard errors of b0 and b1
# using direct calculations
vY <- kidiq$kid_score                        # dependent variable
mX <- cbind(1, mom_iq_centered)              # design matrix

vBeta <- solve( t(mX) %*% mX, t(mX) %*% vY)                         # coefficient estimates
dSigmaSq <- sum( (vY - mX %*% vBeta)^2 ) / ( nrow(mX) - ncol(mX) )  # estimate of sigma-squared
mVarCovar <- dSigmaSq * chol2inv( chol( t(mX) %*% mX ) )            # variance covariance matrix
vStdErr <- sqrt( diag(mVarCovar) )                                  # coeff. est. standard errors
print( cbind(vBeta, vStdErr) )                                      # output

# standard error of b0
sqrt( sum( (kidiq$kid_score - fit_ols$fitted.values)^2 ) / (434-2) ) *
  sqrt( (1/434) + ( mean(mom_iq_centered)^2 / sum( (mom_iq_centered - mean(mom_iq_centered))^2 ) ) )

# standard error of b1
sqrt( sum( (kidiq$kid_score - fit_ols$fitted.values)^2 ) / (434-2) ) / 
  sqrt( sum( (mom_iq_centered - mean(mom_iq_centered))^2 ) )


# "p-value" from t distribution for b1
x <- seq(from = -15, to = 15, length.out = 1000)       # the interval [-15 15]
f <- dt(x, df = 432)                                   # t distribution with 432 degrees of freedom
ggplot(data.frame(x=x, y=f), aes(x=x, y=y)) + geom_line()

pt(-10.42, df = 432) + pt(10.42, df = 432, lower.tail = FALSE)


# residual standard error
y <- kidiq$kid_score
sqrt( sum( (y - fit_ols$fitted.values)^2 ) / (434 - 2) )


# R-squared
y <- kidiq$kid_score
y_bar <- mean( kidiq$kid_score , na.rm = TRUE )
( r.squared <- sum( (fit_ols$fitted.values - y_bar)^2 ) / sum( (y - y_bar)^2 ) )

# Adjusted R-squared
( r.squared.adj <- r.squared - ( (2-1) / (434-2)*(1-r.squared) ) )


# F-statistic "by hand"
y <- kidiq$kid_score
y_bar <- mean( kidiq$kid_score , na.rm = TRUE )

( sum( (fit_ols$fitted.values - y_bar)^2 ) / (2-1) ) / 
( sum( (y - fit_ols$fitted.values)^2 ) / (434-2) )

# "Multiplication" version (flip degrees of freedoms)
( sum( (fit_ols$fitted.values - y_bar)^2 ) / 
    sum( (y - fit_ols$fitted.values)^2 ) ) * ( (434-2) / (2-1) )

# F-statistic with `anova()`
anova(fit_ols)

# Check the sums of squares
sum( (fit_ols$fitted.values - y_bar)^2 )
sum( (y - fit_ols$fitted.values)^2 )

# F-statistic = Model MS / Residuals MS
anova(fit_ols)[[3]][1] / anova(fit_ols)[[3]][2]


# Critical values for this F distribution
( crit10  <- qf( 0.90  , df1 = 1 , df2 = 432 ) )
( crit05  <- qf( 0.95  , df1 = 1 , df2 = 432 ) )
( crit025 <- qf( 0.975 , df1 = 1 , df2 = 432 ) )
( crit01  <- qf( 0.99  , df1 = 1 , df2 = 432 ) )
( crit001 <- qf( 0.999 , df1 = 1 , df2 = 432 ) )



# Plot points and regression line (x-axis centered)
kidiq %>%
  ggplot(aes(x = mom_iq_centered, y = kid_score)) +
  geom_abline(intercept = coef(fit_ols)[1], 
              slope     = coef(fit_ols)[2]) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  xlim(-40,40) +
  labs(x = "Mom IQ (centered)", y = "Kid  Score", title = "Classical Linear Regression of Kid Score on Mom IQ (centered)") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))

# Plot points and regression line (x-axis original units)
labels <-
  c(-40, -20, 0, 20, 40) + mean(kidiq$mom_iq) %>% 
  round(digits = 0)

kidiq %>%
  ggplot(aes(x = mom_iq_centered, y = kid_score)) +
  geom_abline(intercept = coef(fit_ols)[1], 
              slope     = coef(fit_ols)[2]) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  scale_x_continuous("Mom IQ",
                     breaks = c(-40, -20, 0, 20, 40),
                     labels = labels) +
  labs(y = "Kid  Score", title = "Classical Linear Regression of Kid Score on Mom IQ") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))


# F-Test (bivariate regression)
# Standardize outcome and predictor variables
# Regression with Graphics: Concord Water Study pg. 2
concord1 <- haven::read_dta("concord1.dta")
glimpse(concord1)

mean(concord1$water81)
sd(concord1$water81)

mean(concord1$income)
sd(concord1$income)

# Fit a classical linear regression
options(show.signif.stars = FALSE, 
        show.coef.Pvalues = FALSE)
fit_ols <- lm(water81 ~ income, data=concord1)
summary( fit_ols )
arm::display( fit_ols )
par(mfrow=c(2,2))
plot(fit_ols)
par(mfrow=c(1,1))

# Display confidence intervals for the estimates
round( confint( fit_ols , level = .95) , 2 )

# Plot points and regression line
# ggpubr: https://www.roelpeters.be/how-to-add-a-regression-equation-and-r-squared-in-ggplot2/
concord1 %>%
  ggplot(aes(x = income, y = water81)) +
  #geom_abline(intercept = coef(fit_ols)[1], 
  #            slope     = coef(fit_ols)[2]) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  geom_smooth(method = "lm", color="darkblue") +                                                 # , se=FALSE
  ggpubr::stat_regline_equation(label.y = 6500, label.x = 70, aes(label = ..eq.label..)) +
  ggpubr::stat_regline_equation(label.y = 6000, label.x = 70, aes(label = ..rr.label..)) +
  labs(x = "Income in Thousands", y = "Summer 1981 Water Use", title = "") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))

# F-statistic "by hand"
y <- concord1$water81
y_bar <- mean( concord1$water81 , na.rm = TRUE )

( F_statistic <- ( sum( (fit_ols$fitted.values - y_bar)^2 ) / (2-1) ) / 
  ( sum( (y - fit_ols$fitted.values)^2 ) / (496-2) ) )


( F_statistic <- ( sum( (fit_ols$fitted.values - y_bar)^2 ) / 
                     sum( (y - fit_ols$fitted.values)^2 ) ) * ( (496-2) / (2-1) ) )


# In bivariate regression, F = t^2
res <- summary( fit_ols )
income_tval <- res$coefficients[2, "t value"]
income_tval^2


# F distribution
# define x-axis
x <- seq(-0.1, 5.1, length=1000)
# calculate F distribution probabilities
y <- df(x, df1 = 1 ,  df2 = 494)
# plot F distribution
plot(x, y, type = 'l')


# Critical values for this F distribution
( crit10  <- qf( 0.90  , df1 = 1 , df2 = 494 ) )
( crit05  <- qf( 0.95  , df1 = 1 , df2 = 494 ) )
( crit025 <- qf( 0.975 , df1 = 1 , df2 = 494 ) )
( crit01  <- qf( 0.99  , df1 = 1 , df2 = 494 ) )
( crit001 <- qf( 0.999 , df1 = 1 , df2 = 494 ) )


# F-statistic with `anova()`
anova(fit_ols)

y <- concord1$water81
sum( (fit_ols$fitted.values - y_bar)^2 )
sum( (y - fit_ols$fitted.values)^2 )

anova(fit_ols)[[3]][1] / anova(fit_ols)[[3]][2]


# Bayesian regression
# Optional: standardize outcome and predictor variables
# Many options: https://stackoverflow.com/questions/15215457/standardize-data-columns-in-r
# e.g.  myScale <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}
water81_std <- scale(concord1$water81)
income_std <- scale(concord1$income)

# Regress postshortage water use (Y) on income (X)
fit_water81 <- stan_glm(water81 ~ income, data=concord1, refresh = 0)
print(fit_water81, digits=2)
summary(fit_water81, digits=2)
broom.mixed::tidy(fit_water81)
plot(fit_water81)

# Plot posterior draws of the line, y = a + bx
plot(concord1$income, concord1$water81, type="n", xlab="Income (thousands)", ylab="Postshortage Water Use (cubic feet)", mgp=c(2,.5,0), main="Data and range of possible linear fits", bty="l", cex.lab=1.3, cex.main=1.3)
sims <- as.matrix(fit_water81)
n_sims <- nrow(sims)
for (s in sample(n_sims, 50))
  abline(sims[s,1], sims[s,2], col="gray50", lwd=0.5)
with(concord1, points(jitter(income, 5), water81, pch=20, cex=1.25, col="royalblue"))

# F-statistic "by hand"
y <- concord1$water81
y_bar <- mean( concord1$water81 , na.rm = TRUE )
fitted_values <- predict(fit_water81)

( F_statistic <- ( sum( (fitted_values - y_bar)^2 ) / (2-1) ) / 
    ( sum( (y - fitted_values)^2 ) / (496-2) ) )


( F_statistic <- ( sum( (fitted_values - y_bar)^2 ) / 
                     sum( (y - fitted_values)^2 ) ) * ( (496-2) / (2-1) ) )



# calculate F distribution probabilities
# https://dtkaplan.github.io/SM2-bookdown/hypothesis-testing-on-whole-models.html#r-and-the-f-statistic
y <- df(x, df1 = 10 ,  df2 = 5)
# plot F distribution
plot(x, y, type = 'l')
plot(x, y, type = 'l', lwd = 3, col = 'steelblue', xlab = 'x', ylab = 'Probability', main = 'F(4,491) Distribution Plot')

df1s <- c(10,50,50,50)
df2s <- c(5,25,5,45)
dfs <- cbind(df1s,df2s)

par(mfrow=c(2,2))
for (i in 1:nrow(dfs)) {
  # print(dfs[[i,"df1s"]])
  y <- df(x, df1 = dfs[[i,"df1s"]] ,  df2 = dfs[[i,"df2s"]])
  plot(x, y, type = 'l', lwd = 3, col = 'steelblue', 
       xlab = 'x', ylab = 'Probability', 
       main = paste0('F(', dfs[[i,"df1s"]], ',', dfs[[i,"df2s"]] ,') Distribution Plot') )
}
par(mfrow=c(1,1))


# F distribution
# define x-axis
x <- seq(-0.1, 5.1, length=1000)
# calculate F distribution probabilities
y <- df(x, df1 = 3 ,  df2 = 50)
# plot F distribution
plot(x, y, type = 'l')
plot(x, y, type = 'l', lwd = 3, col = 'steelblue', xlab = 'x', ylab = 'Probability', main = 'F(3,50) Distribution Plot')

# Illustrate 95th percentile
nframe <- data.frame(x=x, y=y)

# Calculate the 95th percentile
line <- qf(0.95, df1 = 3 ,  df2 = 50)
xstr <- sprintf("qf(0.95) = %.2f", line)

# To the right of the 95th percentile
nframe95 <- subset(nframe, nframe$x > line)

# Plot it
ggplot(nframe, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data = nframe95, aes(x=x, y=y), fill = "steelblue") +
  geom_vline(aes(xintercept = line), linetype = 2) +
  annotate(geom = "text", x = 3.15, y = 0, label = xstr, vjust = 1.5) +
  labs(title = "F(3,50) Distribution (area to right of 95th percentile colored blue)") +  # subtitle = xstr
  #theme_classic() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))



# F-statistic: Multiple regression

glimpse(concord1)

# x1 - household income, in thousands of dollars
# x2 - preshortage water use, in cubic feet
# x3 - education of household head, in years
# x4 - retirement, coded 1 if household head is retired and 0 otherwise
# x5 - number of people living in household at time of water shortage (summer 1981)
# x6 - change in the number of people, summer 1981 minus summer 1980

# Unrestricted (larger) model
# Fit a classical linear regression
options(show.signif.stars = FALSE, 
        show.coef.Pvalues = FALSE)
fit_ols <- lm(water81 ~ income + water80 + educat + retire + peop81 + cpeop, 
              data=concord1)
summary( fit_ols )
arm::display( fit_ols )
par(mfrow=c(2,2))
plot(fit_ols)
par(mfrow=c(1,1))

#emmeans::ref_grid(fit_ols)
#water81.pred <- matrix(predict(ref_grid(fit_ols)), nrow = 6)
#water81.pred
# ROS pg. 160
plot(fit_ols$fitted.values, concord1$water81, 
     xlab = "Linear predictor, y_hat", ylab = "Outcome, y")
abline(0,1)


# Restricted (smaller) model

# Fit a classical linear regression
options(show.signif.stars = FALSE, 
        show.coef.Pvalues = FALSE)
fit_ols <- lm(water81 ~ water80 + retire + peop81 + cpeop, 
              data=concord1)
summary( fit_ols )
arm::display( fit_ols )
par(mfrow=c(2,2))
plot(fit_ols)
par(mfrow=c(1,1))

# ROS pg. 160
plot(fit_ols$fitted.values, concord1$water81, 
     xlab = "Linear predictor, y_hat", ylab = "Outcome, y")
abline(0,1)


# Calculate F-statistic for nested models "by hand"
fit_unrestricted <- lm(water81 ~ income + water80 + educat + retire + peop81 + cpeop, data=concord1)
fit_restricted <- lm(water81 ~ water80 + retire + peop81 + cpeop, data=concord1)

RSS_5 <- sum( (concord1$water81 - fit_restricted$fitted.values)^2 )
RSS_7 <- sum( (concord1$water81 - fit_unrestricted$fitted.values)^2 )
H <- 2
n_minus_K <- 489

( F_statistic <- ( (RSS_5 - RSS_7) / H ) / ( RSS_7 / n_minus_K ) )


# Critical values for this F distribution
( crit10  <- qf( 0.90  , df1 = 2 , df2 = 489 ) )
( crit05  <- qf( 0.95  , df1 = 2 , df2 = 489 ) )
( crit025 <- qf( 0.975 , df1 = 2 , df2 = 489 ) )
( crit01  <- qf( 0.99  , df1 = 2 , df2 = 489 ) )
( crit001 <- qf( 0.999 , df1 = 2 , df2 = 489 ) )


# Calculate F-statistic for nested models with `anova()`
anova(fit_restricted, fit_unrestricted)


# F distribution
# define x-axis
x <- seq(-0.1, 25.1, length=1000)
# calculate F distribution probabilities
y <- df(x, df1 = 2 ,  df2 = 489)
# plot F distribution
plot(x, y, type = 'l')
plot(x, y, type = 'l', lwd = 3, col = 'steelblue', xlab = 'x', ylab = 'Probability', main = 'F(2,489) Distribution Plot')

# Illustrate area to right of 19.24
nframe <- data.frame(x=x, y=y)

# Calculate line position
line <- qf(pf(19.24, df1 = 2,  df2 = 489), df1 = 2,  df2 = 489)
pval <- pf(19.24, df1 = 2,  df2 = 489, lower.tail = FALSE)
xstr <- sprintf("pf(>=19.24) = %.9f", pval)

# To the right of the F value
nframe1924 <- subset(nframe, nframe$x >= line)

# Plot it
ggplot(nframe, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data = nframe1924, aes(x=x, y=y), fill = "steelblue") +
  geom_vline(aes(xintercept = line), linetype = 2) +
  annotate(geom = "text", x = 22.15, y = 0, label = xstr, vjust = 1.5) +
  labs(title = "F(2,489) Distribution (area to right of 19.24 colored blue)") +  # subtitle = xstr
  #theme_classic() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))


# F-statistic for restricted model
fit_restricted <- lm(water81 ~ water80 + retire + peop81 + cpeop, data=concord1)
summary(fit_restricted)

ESS <- sum( (fit_restricted$fitted.values - y_bar)^2 )
RSS <- sum( (concord1$water81 - fit_restricted$fitted.values)^2 )
K_minus_1 <- (5-1)
n_minus_K <- (496-5)

( F_statistic <- ( ESS / K_minus_1 ) / ( RSS / n_minus_K ) )

( F_statistic <- ( ESS / RSS ) * ( n_minus_K / K_minus_1 ) )


# F-Test Example 2

# Unrestricted (larger) model

# Fit a classical linear regression
options(show.signif.stars = FALSE, 
        show.coef.Pvalues = FALSE)
mlb1 <- wooldridge::mlb1
fit_unrestricted <- lm(log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr, 
                       data=mlb1)
options("scipen"=100, "digits"=4)
summary( fit_unrestricted )
options("scipen"=0, "digits"=7)
arm::display( fit_unrestricted )
par(mfrow=c(2,2))
plot( fit_unrestricted )
par(mfrow=c(1,1))

plot(fit_unrestricted$fitted.values, log(mlb1$salary), 
     xlab = "Linear predictor, y_hat", ylab = "Outcome, y")
abline(0,1)


# Restricted (smaller) model

# Fit a classical linear regression
options(show.signif.stars = FALSE, 
        show.coef.Pvalues = FALSE)
mlb1 <- wooldridge::mlb1
fit_restricted <- lm(log(salary) ~ years + gamesyr, data=mlb1)
options("scipen"=100, "digits"=4)
summary( fit_restricted )
options("scipen"=0, "digits"=7)
arm::display( fit_restricted )
par(mfrow=c(2,2))
plot( fit_restricted )
par(mfrow=c(1,1))

plot(fit_restricted$fitted.values, log(mlb1$salary), 
     xlab = "Linear predictor, y_hat", ylab = "Outcome, y")
abline(0,1)


# Calculate F-statistic for nested models
fit_unrestricted <- lm(log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr, data=mlb1)
fit_restricted   <- lm(log(salary) ~ years + gamesyr, data=mlb1)

RSS_3 <- sum( (log(mlb1$salary) - fit_restricted$fitted.values)^2 )
RSS_6 <- sum( (log(mlb1$salary) - fit_unrestricted$fitted.values)^2 )
H <- 3
n_minus_K <- (353-6)

( F_statistic <- ( (RSS_3 - RSS_6) / H ) / ( RSS_6 / n_minus_K ) )

( F_statistic <- ( (RSS_3 - RSS_6) / RSS_6 ) * ( n_minus_K / H ) )

# Calculate F-statistic for nested models with `anova()`
anova(fit_restricted, fit_unrestricted)


# Critical values for this F distribution
( crit10  <- qf( 0.90  , df1 = 3 , df2 = 347 ) )
( crit05  <- qf( 0.95  , df1 = 3 , df2 = 347 ) )
( crit025 <- qf( 0.975 , df1 = 3 , df2 = 347 ) )
( crit01  <- qf( 0.99  , df1 = 3 , df2 = 347 ) )
( crit001 <- qf( 0.999 , df1 = 3 , df2 = 347 ) )


# F distribution
# define x-axis
x <- seq(-0.1, 12.1, length=1000)
# calculate F distribution probabilities
y <- df(x, df1 = 3 ,  df2 = 347)
# plot F distribution
plot(x, y, type = 'l')
plot(x, y, type = 'l', lwd = 3, col = 'steelblue', xlab = 'x', ylab = 'Probability', main = 'F(3,347) Distribution Plot')

# Illustrate area to right of 19.24
nframe <- data.frame(x=x, y=y)

# Calculate line position
line <- qf(pf(9.55, df1 = 3 ,  df2 = 347), df1 = 3 ,  df2 = 347)
pval <- pf(9.55, df1 = 3 ,  df2 = 347, lower.tail = FALSE)
xstr <- sprintf("pf(>9.55) = %.7f", pval)

# To the right of the 95th percentile
nframe955 <- subset(nframe, nframe$x >= line)

# Plot it
ggplot(nframe, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data = nframe955, aes(x=x, y=y), fill = "steelblue") +
  geom_vline(aes(xintercept = line), linetype = 2) +
  annotate(geom = "text", x = 10.75, y = 0, label = xstr, vjust = 1.5) +
  labs(title = "F(3,347) Distribution (area to right of 9.55 colored blue)") +  # subtitle = xstr
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))




# The R-Squared Form of the F Statistic
# Introductory Econometrics, 7E Page 145
mlb1 <- wooldridge::mlb1

# Calculate F-statistic for nested models

# R-squared for unrestricted model
fit_unrestricted <- lm(log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr, data=mlb1)
r2_unrestricted <- summary(fit_unrestricted)$r.squared

# R-squared for restricted model
fit_restricted   <- lm(log(salary) ~ years + gamesyr, data=mlb1)
r2_restricted <- summary(fit_restricted)$r.squared

# Number of fewer parameters in restricted model
H <- 3

# Number of observations minus number of estimated parameters
# in the unrestricted model
n_minus_K <- (353-6)

# R-squared form of the F-statistic
( F_statistic <- ((r2_unrestricted - r2_restricted) / H) / ((1 - r2_unrestricted) / n_minus_K) )

# "Multiplication" version (flip degrees of freedoms)
( F_statistic <- ((r2_unrestricted - r2_restricted) / (1 - r2_unrestricted)) * (n_minus_K / H) )




#### Regression with Graphics: Concord Water Study ####

# Power Transformations

concord1 <- haven::read_dta('concord1.dta')
head(concord1)

# Example of Chapter 3:
# Y is postshortage (1981) water use
# X1 is household income
# X2 is preshortage (1980) water use
# X3 is education of household head
# X4 is a dummy variable for retirement
# X5 is number of people in household at time of water shortage
# X6 is increase in number of people, summer 1981 minus summer 1980

# Power transformation in bivariate regression (Page 55)

# Raise both 1981 water use and household income to the 0.3 power
wtr81_3 <- concord1$water81^0.3

inc_3 <- concord1$income^0.3


# Figure 2.13: Four views of the distribution of household income (positively skewed)
library(grid)
library(gridExtra)

# 1. Histogram
# https://www.statology.org/overlay-normal-curve-histogram-in-r/
p1 <- ggplot(concord1, aes(x = income)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill='lightgray', col='black') +
  stat_function(fun = dnorm, 
                args = list(mean = mean(concord1$income),
                            sd   =   sd(concord1$income))) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(title = "Histogram",
       y = "Density", 
       x = "Income") + 
  theme_light()
p1

# 2. Boxplot
p2 <- ggplot(concord1, aes(x=factor(0), y = income)) +
  stat_boxplot(geom = "errorbar",
               width = 0.10) + 
  geom_boxplot(outlier.shape = 1) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "black") +
  labs(title = "Boxplot",
       y = "Income", 
       x = NULL) + 
  theme_light() +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())
p2

# 3. Symmetry Plot
# LearnEDAfunctions::symplot(x)
# https://rdrr.io/github/bayesball/LearnEDAfunctions/src/R/symplot.R
symdat <- function(d){
  n <- length(d)
  no <- floor((n + 1) / 2)
  sd <- sort(d)
  i <- 1 : no
  u <- sd[n + 1 - i] - median(d)
  v <- median(d) - sd[i]
  return(list("v"=v, "u"=u))
}

dat <- as.data.frame(symdat(concord1$income))

p3 <- ggplot(dat, aes(v, u)) + 
  geom_point(shape=1) + 
  geom_abline() +
  labs(title = "Symmetry Plot",
       y="Distance above median",
       x="Distance below median") + 
  theme_light()
p3

# 4. Quantile-Normal Plot
p4 <- ggplot(concord1, aes(sample = income)) + 
  stat_qq(shape=1) + 
  stat_qq_line() +
  labs(title = "Quantile-Normal Plot",
       y="Residual",
       x="Gaussian (normal) Distribution") + 
  theme_light()
p4

# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# https://cran.r-project.org/web/packages/egg/vignettes/Overview.html
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)



# Figure 2.14: Four views of the distribution of income to the .3 power (symmetrical)
# 1. Histogram
p1 <- ggplot(concord1, aes(x = income^.3)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill='lightgray', col='black') +
  stat_function(fun = dnorm, 
                args = list(mean = mean(concord1$income^.3),
                            sd   =   sd(concord1$income^.3))) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_x_continuous(limits = c(1,4)) +
  labs(title = "Histogram",
       y = "Density", 
       x = "Income^.3") + 
  theme_light()
p1

# 2. Boxplot
p2 <- ggplot(concord1, aes(x=factor(0), y = income^.3)) +
  stat_boxplot(geom = "errorbar",
               width = 0.10) + 
  geom_boxplot(outlier.shape = 1) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "black") +
  labs(title = "Boxplot",
       y = "Income^.3", 
       x = NULL) + 
  theme_light() +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())
p2

# 3. Symmetry Plot
# LearnEDAfunctions::symplot(x)
# https://rdrr.io/github/bayesball/LearnEDAfunctions/src/R/symplot.R
symdat <- function(d){
  n <- length(d)
  no <- floor((n + 1) / 2)
  sd <- sort(d)
  i <- 1 : no
  u <- sd[n + 1 - i] - median(d)
  v <- median(d) - sd[i]
  return(list("v"=v, "u"=u))
}

dat <- as.data.frame(symdat(concord1$income^.3))

p3 <- ggplot(dat, aes(v, u)) + 
  geom_point(shape=1) + 
  geom_abline() +
  labs(title = "Symmetry Plot",
       y="Distance above median",
       x="Distance below median") + 
  theme_light()
p3

# 4. Quantile-Normal Plot
p4 <- ggplot(concord1, aes(sample = income^.3)) + 
  stat_qq(shape=1) + 
  stat_qq_line() +
  labs(title = "Quantile-Normal Plot",
       y="Residual",
       x="Gaussian (normal) Distribution") + 
  theme_light()
p4

# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# https://cran.r-project.org/web/packages/egg/vignettes/Overview.html
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)




# Reduce skew in income by raising income to the .3 power

# 1. Histogram (income - natural units)
# https://www.statology.org/overlay-normal-curve-histogram-in-r/
p1 <- ggplot(concord1, aes(x = income)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill='lightgray', col='black') +
  stat_function(fun = dnorm, 
                args = list(mean = mean(concord1$income),
                            sd   =   sd(concord1$income))) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(title = "Histogram",
       y = "Density", 
       x = "Income") + 
  theme_light()
p1

# 1. Histogram (income^.3)
p2 <- ggplot(concord1, aes(x = income^.3)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill='lightgray', col='black') +
  stat_function(fun = dnorm, 
                args = list(mean = mean(concord1$income^.3),
                            sd   =   sd(concord1$income^.3))) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_x_continuous(limits = c(1,4)) +
  labs(title = "Histogram",
       y = "Density", 
       x = "Income^.3") + 
  theme_light()
p2

# 2. Boxplot (income - natural units)
p3 <- ggplot(concord1, aes(x=factor(0), y = income)) +
  stat_boxplot(geom = "errorbar",
               width = 0.10) + 
  geom_boxplot(outlier.shape = 1) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "black") +
  labs(title = "Boxplot",
       y = "Income", 
       x = NULL) + 
  theme_light() +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())
p3

# 2. Boxplot (income^.3)
p4 <- ggplot(concord1, aes(x=factor(0), y = income^.3)) +
  stat_boxplot(geom = "errorbar",
               width = 0.10) + 
  geom_boxplot(outlier.shape = 1) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "black") +
  labs(title = "Boxplot",
       y = "Income^.3", 
       x = NULL) + 
  theme_light() +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())
p4

# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# https://cran.r-project.org/web/packages/egg/vignettes/Overview.html
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)




# Table 2.3: Regression of .3 power of 1981 water use (wtr81_3)
# on .3 power of household income (inc_3)
fit_3 <- lm(wtr81_3 ~ inc_3, data = concord1)

arm::display(fit_3, digits = 2)

# Figure 2.16: residuals-versus-predicted plot showing residuals
# from transformed-variables regression
plot(fit_3$fitted.values, resid(fit_3), ylim = c(-6,6),
     xlab="transformed predicted values (Y_hat*)", 
     ylab="transformed residuals (e*)")
abline(0, 0)

# Figure 2.16: residuals-versus-predicted plot showing residuals
# from transformed-variables regression

# car qqPlot version 
car::qqPlot(resid(fit_3), main="QQ Plot")

# ggplot2 version
ggplot(concord1, aes(sample = resid(fit_3))) + 
  stat_qq(shape=1) + 
  stat_qq_line() +
  scale_x_continuous(limits = c(-4,4),
                     breaks = seq(-4,4,2)) +
  labs(title = "Quantile-Normal Plot",
       y="Residual",
       x="Gaussian (normal) Distribution") + 
  theme_light()


# Figure 2.15: The relationship between transformed water use (Y^.3) 
# and transformed income (X^.3) is linear
plot(concord1$income^.3, concord1$water81^.3, xlab="Income to .3 power", ylab="Water Use to .3 power")
abline(coef(fit_3))

# ggplot version
ggplot(concord1, aes(income^.3, water81^.3)) +
  geom_point(pch=1, size=2) +
  geom_abline(intercept = coef(fit_3)[1], slope = coef(fit_3)[2]) +
  labs(x = "Income to .3 power", y = "Water Use to .3 power")


# 1. Obtain the predicted values (Y_hat*) from the transformed-variables equation. Then do the following:
Y_hat_star <- fit_3$fitted.values

# 2. Convert transformed predicted values (Y_hat*) back into the natural units of Y (obtaining Y_hat)
# We perform step 2 only if Y was transformed. If Y was not transformed, we simply graph Y_hat against X

# Since we transformed water use (Y) by raising to the .3 power --  Y* = Y^.3  -- the appropriate inverse
# transformation, applied to the predicted .3 power of water use (Y_hat^.3), is:  Y_hat = (Y_hat*)
Y_hat <- Y_hat_star^(1 / 0.3)


# 3. Graph Y_hat against X
# Figure 2.17: Curvilinear relation of water use to income

# 3a. Create a scatterplot with income on x-axis and water use on y-axis
plot(concord1$income, concord1$water81, xlab="Income in Thousands", ylab="Summer 1981 Water Use")

# In the above scatterplot, x is income, so below, we raise x to the .3 power since
# income^.3 is the predictor in the fit_3 regression model
# Then, after calculating the fitted values with: coef(fit_3)[1] + coef(fit_3)[2]*(x^0.3)
# We raise these fitted values to the 1 / 0.3 power, the appropriate inverse transformation, to
# return them to their natural units -- cubic feet of water

# Having returned the fitted values to the natural units of Y, 
# graphing them against income (X), they trace out a gentle curve
curve(( coef(fit_3)[1] + coef(fit_3)[2]*(x^0.3) )^(1/0.3), add=TRUE)


# 3b. ggplot version
ggplot(concord1, aes(income, water81)) +
  geom_point(pch=1, size=2) +
  geom_line(aes(y = ( coef(fit_3)[1] + coef(fit_3)[2]*(income^0.3) )^(1/0.3) )) +
  labs(x = "Income in Thousands", y = "Summer 1981 Water Use")


#### END: Regression with Graphics: Concord Water Study ####




# we set the seed to make the results of `runif()` reproducible.
# https://bookdown.org/content/4857/geocentric-models.html#why-normal-distributions-are-normal
set.seed(4)

pos <- 
  # make data with 100 people, 16 steps each with a starting point of `step == 0` (i.e., 17 rows per person)
  crossing(person = 1:100,
           step   = 0:16) %>% 
  # for all steps above `step == 0` simulate a `deviation`
  mutate(deviation = map_dbl(step, ~if_else(. == 0, 0, runif(1, -1, 1)))) %>% 
  # after grouping by `person`, compute the cumulative sum of the deviations, then `ungroup()`
  group_by(person) %>%
  mutate(position = cumsum(deviation)) %>% 
  ungroup()

glimpse(pos)


# Figure 4.2 (top)
ggplot(data = pos, 
       aes(x = step, y = position, group = person)) +
  geom_vline(xintercept = c(4, 8, 16), linetype = 2) +
  geom_line(aes(color = person < 2, alpha  = person < 2)) +
  scale_color_manual(values = c("skyblue4", "black")) +
  scale_alpha_manual(values = c(1/5, 1)) +
  scale_x_continuous("step number", breaks = c(0, 4, 8, 12, 16)) +
  theme_classic() +
  theme(legend.position = "none")

# Figure 4.2. (bottom a.)
p1 <-
  pos %>%
  filter(step == 4) %>%
  ggplot(aes(x = position)) +
  geom_line(stat = "density", color = "dodgerblue1") +
  labs(title = "4 steps", y = "Density") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))

# Figure 4.2. (bottom b.)
p2 <-
  pos %>%
  filter(step == 8) %>%
  ggplot(aes(x = position)) +
  geom_density(color = "dodgerblue2", outline.type = "full") +
  labs(title = "8 steps", y = "Density") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))

# this is an intermediary step to get an SD value
sd <-
  pos %>%
  filter(step == 16) %>%
  summarise(sd = sd(position)) %>% 
  pull(sd)

# Figure 4.2. (bottom c.)
p3 <-
  pos %>%
  filter(step == 16) %>%
  ggplot(aes(x = position)) +
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = sd),
                linetype = 2) +
  geom_density(color = "transparent", fill = "dodgerblue3", alpha = 1/2) +
  labs(title = "16 steps", y = "Density") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))

# library(patchwork)
# combine the ggplots
(p1 | p2 | p3) & coord_cartesian(xlim = c(-6, 6))



# Normal by addition
pos <- replicate( 1000 , sum( runif(16 , -1 , 1) ) )
hist(pos)
plot(density(pos), xlab = "", main = "Normal by addition")

# Normal by multiplication
growth <- replicate( 10000 , prod( 1 + runif(12 , 0 , 0.1) ) )
hist(growth)
plot(density(growth), xlab = "", main = "Normal by multiplication")

# Normal by log-multiplication
log.big <- replicate( 10000 , log( prod( 1 + runif(12 , 0 , 0.5) ) ) )
hist(log.big)
plot(density(log.big), xlab = "", main = "Normal by log-multiplication")
