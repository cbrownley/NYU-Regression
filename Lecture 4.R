library("afex")
library("car")
library("emmeans")
library("multcomp")
library("broom")
library("broom.mixed")
library("haven")
library("HSAUR")
library("tidyverse")
library("cmdstanr")
library("rethinking")
library("rstanarm")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library("ggplot2")
library("gridExtra")
library("bayesplot")
library("tidybayes")
library("humanize")
library("scales")
library("patchwork")
library("wooldridge")




# Oneway Analysis of Variance (ANOVA)
# Regression with Graphics: Cancer, bedrock, and radon pg. 92
radon <- read_dta("/Users/clinton/Documents/NYU\ Regression/rwg/radon.dta")
glimpse(radon)


# Mean cancer rate, standard deviation of cancer rate, 
# and number of observations by locale
radon %>% 
  group_by(locale) %>%
  mutate(locale = ifelse(locale == 1, "Reading Prong", ifelse(locale == 2, "Fringe", "Control"))) %>%
  summarise(mean_cancer = mean(cancer, na.rm = TRUE),
            sd_cancer   = sd(  cancer, na.rm = TRUE),
            n = n()) %>%
  arrange(desc(mean_cancer))


# Boxplots and jittered data points of cancer rates by locale
radon %>% 
  mutate(locale = ifelse(locale == 1, "Reading Prong", 
                         ifelse(locale == 2, "Fringe", "Control"))) %>%
  ggplot(data = ., aes(x = reorder(factor(locale), -cancer), y = cancer)) +  # order categories by cancer rate, descending
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  stat_summary(geom="text", fun=mean,
               aes(label=sprintf("Mean rate: %1.1f", ..y..)),
               position=position_nudge(x=.14, y=.06), size=3) +
  stat_summary(geom="point", fun=mean, shape="*", size=10, color="black", fill="black") +
  labs(x = "Bedrock Area", y = "Lung Cancer Rate", title = "") +  # : 1-Reading Prong, 2-fringe (on Reading Prong borders), 3-control (nearby but not on Reading Prong)
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))


# Jittered points by bedrock area
jitt <- runif(nrow(radon), -.03, .03)
plot(radon$locale + jitt, radon$cancer, 
     xlab="Bedrock Area", ylab="Lung Cancer Rate", 
     bty="l", pch=1, xaxt="n", yaxt="n")
axis(1, c(1,2,3))
axis(2, seq(0, 12, 2))


# Plot three normal distributions
x <- seq(from=-5, to=10, length.out=1000)
g1 <- dnorm(x, mean=0, sd=1)
g2 <- dnorm(x, mean=2, sd=1)
g3 <- dnorm(x, mean=5, sd=1)
norm_frame <- data.frame(x=x, g1=g1, g2=g2, g3=g3)

ggplot(norm_frame) +
  geom_line(aes(x=x, y=g1), color="firebrick") +
  geom_vline(aes(xintercept = 0), linetype = 2, color="darkgreen") +
  annotate(geom = "text", x = 0, y = 0, label = as.character(expression(mu[1])), vjust=1.7) +
  geom_line(aes(x=x, y=g2), color="firebrick") +
  geom_vline(aes(xintercept = 2), linetype = 2, color="darkgreen") +
  annotate(geom = "text", x = 2, y = 0, label = as.character(expression(mu[2])), vjust=1.7) +
  geom_line(aes(x=x, y=g3), color="firebrick") +
  geom_vline(aes(xintercept = 5), linetype = 2, color="darkgreen") +
  annotate(geom = "text", x = 5, y = 0, label = as.character(expression(mu[3])), vjust=1.7) +
  labs(x="", y="", title = "") +
  #theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(hjust=0.5), 
        plot.subtitle=element_text(hjust=0.5))


# Normal quantile plots
reading_prong <- radon %>% filter(locale==1) %>% dplyr::select(cancer)
qqnorm(reading_prong$cancer, pch = 16, col = rgb(0, 0, 0, alpha = 1),
       main = "Reading Prong: Normal Q-Q Plot")
qqline(reading_prong$cancer, col = rgb(1, 0, 0, 1.0))

fringe <- radon %>% filter(locale==2) %>% dplyr::select(cancer)
qqnorm(fringe$cancer, pch = 16, col = rgb(0, 0, 0, alpha = 1),
       main = "Fringe: Normal Q-Q Plot")
qqline(fringe$cancer, col = rgb(1, 0, 0, 1.0))

control <- radon %>% filter(locale==3) %>% dplyr::select(cancer)
qqnorm(control$cancer, pch = 16, col = rgb(0, 0, 0, alpha = 1),
       main = "Control: Normal Q-Q Plot")
qqline(control$cancer, col = rgb(1, 0, 0, 1.0))


# Mean and standard deviation of cancer rate by locale
tapply(radon$cancer, factor(radon$locale), mean)
tapply(radon$cancer, factor(radon$locale), sd)


# Plot univariate effects of one or more factors, typically for a designed experiment as analyzed by aov()
plot.design( radon$cancer ~ factor(radon$locale) )


# EXAMPLE: F distribution (color p-value in right-hand tail)
# define x-axis
x <- seq(-0.1, 5.1, length=1000)
# calculate F distribution probabilities
y <- df( x , df1 = 3 ,  df2 = 50 )
# create data.frame
nframe <- data.frame(x=x, y=y)

# Calculate the line position
line <- qf(pf(3.0, df1 = 3,  df2 = 50), df1 = 3,  df2 = 50)
pval <- pf(3.0, df1 = 3,  df2 = 50, lower.tail = FALSE)
xstr <- sprintf("pf(>=3.0) = %.3f", pval)

# To the right of the F value
nframe3 <- subset(nframe, nframe$x >= line)

# Plot it
ggplot(nframe, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data = nframe3, aes(x=x, y=y), fill = "steelblue") +
  geom_vline(aes(xintercept = line), linetype = 2) +
  annotate(geom = "text", x = 3.5, y = 0, label = xstr, vjust = 1.5) +
  labs(title = "F(3,50) Distribution (area to right of 3.0 colored blue)") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))


# ANOVA
# Step 1: Create an aov object
res_aov <- aov(cancer ~ 1 + factor(locale), data = radon)

# Step 2: Review the summary of the aov object
coef(res_aov)

summary(res_aov)

print(res_aov)
res <- print(res_aov)
names(res)


# Calcs "by-hand"
cancer_mean <- mean(radon$cancer, na.rm = TRUE)

cancer_reading_prong <- radon %>% filter(locale == 1) %>% pull(cancer)
cancer_fringe        <- radon %>% filter(locale == 2) %>% pull(cancer)
cancer_control       <- radon %>% filter(locale == 3) %>% pull(cancer)

cancer_reading_prong_mean <- radon %>% filter(locale == 1) %>% pull(cancer) %>% mean(.)
cancer_fringe_mean        <- radon %>% filter(locale == 2) %>% pull(cancer) %>% mean(.)
cancer_control_mean       <- radon %>% filter(locale == 3) %>% pull(cancer) %>% mean(.)

# SSG measures variation of the group means around the overall mean, X_bari - X_bar
SSG <- sum( (res$fitted.values - cancer_mean)^2 )
SSG

# SSR measures variation of each observation around its group mean, Xij - X_bari
SSR <- sum( (cancer_reading_prong - cancer_reading_prong_mean)^2 , 
            (cancer_fringe - cancer_fringe_mean)^2 , 
            (cancer_control - cancer_control_mean)^2 )
SSR

# SST measures variation of the data around the overall mean
SST <- sum( (radon$cancer - cancer_mean)^2 )
SST

# Check that SST = SSG + SSR
all.equal(SST, SSG+SSR)

# Residual standard error
N <- dim(radon)[1] # number of observations (30)
k <- 3             # number of predictors (3)
sqrt( sum( (radon$cancer - res$fitted.values)^2 ) / (N - k) )

# MSG = SSG / SSG_degrees_of_freedom
MSG <- SSG / (k - 1)
MSG

# MSE = SSR / SSR_degrees_of_freedom
MSR <- SSR / (N - k)
MSR

# F value = MSG / MSR
F_statistic <- MSG / MSR
F_statistic


# Critical values for this F distribution
( crit10  <- qf( 0.90  , df1 = (k-1) , df2 = (N-k) ) )
( crit05  <- qf( 0.95  , df1 = (k-1) , df2 = (N-k) ) )
( crit025 <- qf( 0.975 , df1 = (k-1) , df2 = (N-k) ) )
( crit01  <- qf( 0.99  , df1 = (k-1) , df2 = (N-k) ) )
( crit001 <- qf( 0.999 , df1 = (k-1) , df2 = (N-k) ) )


# F distribution
# define x-axis
x <- seq(-0.1, 8.1, length=1000)
# calculate F distribution probabilities
y <- df( x , df1 = (k-1) ,  df2 = (N-k) )
# plot F distribution
plot(x, y, type = 'l')
plot(x, y, type = 'l', lwd = 3, col = 'steelblue', xlab = 'x', ylab = 'Probability', main = 'F(2,27) Distribution Plot')

# Illustrate 95th percentile
nframe <- data.frame(x=x, y=y)

# Calculate the line position
line <- qf(pf(6.13, df1 = 2,  df2 = 27), df1 = 2,  df2 = 27)
pval <- pf(6.13, df1 = 2,  df2 = 27, lower.tail = FALSE)
xstr <- sprintf("pf(>=6.13) = %.4f", pval)

# To the right of the F value
nframe613 <- subset(nframe, nframe$x >= line)

# Plot it
ggplot(nframe, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data = nframe613, aes(x=x, y=y), fill = "steelblue") +
  geom_vline(aes(xintercept = line), linetype = 2) +
  annotate(geom = "text", x = 7.1, y = 0, label = xstr, vjust = 1.5) +
  labs(title = "F(2,27) Distribution (area to right of 6.13 colored blue)") +  # subtitle = xstr
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))


# Compute Tukey Honest Significant Differences (TukeyHSD)
# https://www.itl.nist.gov/div898/handbook/prc/section4/prc471.htm
# https://rpubs.com/aaronsc32/post-hoc-analysis-tukey
TukeyHSD(res_aov)

# Optional arguments
TukeyHSD(res_aov, ordered = TRUE, conf.level = 0.89)

# Plot the results
plot( TukeyHSD(res_aov) )


# Formulate as a linear regression
cancer_ols <- lm(cancer ~ 1 + reading + fringe, data = radon)
options(digits = 2,
        show.signif.stars = FALSE, 
        show.coef.Pvalues = FALSE)
summary(cancer_ols)  


# Formulate as a linear regression
# Add locale as a factor with reordered levels so Control is comparison category
radon$locale <- factor(radon$locale, levels = c(3, 1, 2))
cancer_ols <- lm(cancer ~ 1 + locale, data = radon)
summary(cancer_ols) 

aov(cancer_ols)

TukeyHSD( aov(cancer_ols) )


# Control (locale=3) is the base, comparison category
# Based on t values for locales 1 and 2:
# Reading Prong (locale=1)  is sig diff from locale 3
# Fringe (locale=2) is NOT sig diff from locale 3
# What about the difference between locales 1 and 2, is it sig diff?
# Based on the coefficient estimates, the diff between
# locales 1 and 2 (i.e. locale 2 minus locale 1) is:
# 0.800 - 1.975 = -1.175
summary(cancer_ols)$coefficients["locale2","Estimate"] - summary(cancer_ols)$coefficients["locale1","Estimate"]
# Is this diff significant?  We don't know because we don't have the
# standard error of the estimate.  How can we get it?
# One way is to make Fringe (locale=2) the base, comparison category
radon$locale <- factor(radon$locale, levels = c(2, 1, 3))
fringe_base <- lm(cancer ~ 1 + locale, data = radon)
summary(fringe_base) 
# The output shows the t value for the difference between
# locale 2 and locale 3 is: -1.55, so the diff is NOT significant
# This method of changing the base, comparison category 
# to examine specific pairwise comparisons 
# complements (and is an alternative to) the TukeyHSD test


# Add locale as a factor with reordered levels so Control is comparison category
radon$locale <- factor(radon$locale, levels = c(3, 1, 2))


# Contrasts with emmeans
# https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html
# https://cran.r-project.org/web/packages/afex/vignettes/afex_anova_example.html#post-hoc-contrasts-and-plotting
emm1 <- emmeans(cancer_ols, ~ locale)
emm1

pairs(emm1)

summary(as.glht(pairs(emm1)), test = adjusted("free"))


cancer_emm <- emmeans(cancer_ols, pairwise ~ locale)

coef(cancer_emm)

contrast(cancer_emm)

pairs(cancer_emm)


# Plot the results
plot( emmeans(cancer_ols, pairwise ~ locale) )

emmip(cancer_ols, 1 ~ locale)

afex_plot(cancer_ols, x = "locale") + theme_classic()




# Formulate as a linear regression
cancer_bayes <- stan_glm(cancer ~ 1 + reading + fringe, data = radon, 
                         prior_intercept=NULL, prior=NULL, prior_aux=NULL)
summary(cancer_bayes)

# Plot posterior distributions of parameter estimates
posterior <- as.matrix(cancer_bayes)

mcmc_areas(
  posterior,
  pars = c("reading", "fringe", "(Intercept)"),
  prob = 0.89, # 89% intervals
  #prob_outer = 0.99, # 99%
  point_est = "mean"
)

# Plot posterior distributions of cancer rates by locale
as.data.frame(cancer_bayes) %>% 
  mutate(Control = `(Intercept)`, 
         Fringe  = `(Intercept)` + fringe, 
         `Reading Prong` = `(Intercept)` + reading) %>% 
  mcmc_areas(pars = c("Reading Prong", "Fringe", "Control")) + 
  labs(x="Lung Cancer Rate")


# Plot the pairwise differences in mean cancer rate between locales
posterior <- as.matrix(cancer_bayes)
posterior[1:5, ]

diff_RC <- posterior[ , "reading"]
diff_RF <- posterior[ , "reading"] - posterior[ , "fringe"]
diff_FC <- posterior[ , "fringe"]
posterior <- cbind(posterior, diff_RC)
posterior <- cbind(posterior, diff_RF)
posterior <- cbind(posterior, diff_FC)
posterior[1:5, ]

as.data.frame(posterior) %>% 
  dplyr::select(diff_RC, diff_RF, diff_FC) %>% 
  mcmc_areas(pars = c("diff_RC", "diff_RF", "diff_FC")) + 
  labs(x="Difference in Mean Lung Cancer Rate between Locales")


# 95% CIs for differences in means between locales
n <- length(diff_RC)
# CI for diff_RC
mean(diff_RC) + qt(c(0.025,0.975), n-1) * sd(diff_RC)
# CI for diff_RF
mean(diff_RF) + qt(c(0.025,0.975), n-1) * sd(diff_RF)
# CI for diff_FC
mean(diff_FC) + qt(c(0.025,0.975), n-1) * sd(diff_FC)


# p-values for mean differences
length(diff_RC[diff_RC <= 0]) / length(diff_RC)
length(diff_RF[diff_RF <= 0]) / length(diff_RF)
length(diff_FC[diff_FC <= 0]) / length(diff_FC)


# All three diffs in a row
tibble(posterior) %>%
  #dplyr::select(diff_RC, diff_RF, diff_FC) %>%
  mutate(mean_RC = mean(diff_RC),
         sd_RC = sd(diff_RC),
         p2.5_RC = quantile(diff_RC, 0.025),
         p97.5_RC = quantile(diff_RC, 0.975),
         
         mean_RF = mean(diff_RF),
         sd_RF = sd(diff_RF),
         p2.5_RF = quantile(diff_RF, 0.025),
         p97.5_RF = quantile(diff_RF, 0.975),
         
         mean_FC = mean(diff_FC),
         sd_FC = sd(diff_FC),
         p2.5_FC = quantile(diff_FC, 0.025),
         p97.5_FC = quantile(diff_FC, 0.975) ) %>%
  filter(row_number() == 1) %>%
  dplyr::select(mean_RC, sd_RC, p2.5_RC, p97.5_RC,
                mean_RF, sd_RF, p2.5_RF, p97.5_RF,
                mean_FC, sd_FC, p2.5_FC, p97.5_FC)


# Stack individual diffs on top of one another as rows in a table
diff_RC_stats <- tibble(posterior) %>%
  mutate(`contrast` = "RC",
         mean = mean(diff_RC),
         sd = sd(diff_RC),
         `2.5%` = quantile(diff_RC, 0.025),
         `97.5%` = quantile(diff_RC, 0.975) ) %>%
  filter(row_number() == 1) %>%
  dplyr::select(`contrast`, mean, sd, `2.5%`, `97.5%`)

diff_RF_stats <- tibble(posterior) %>%
  mutate(`contrast` = "RF",
         mean = mean(diff_RF),
         sd = sd(diff_RF),
         `2.5%` = quantile(diff_RF, 0.025),
         `97.5%` = quantile(diff_RF, 0.975) ) %>%
  filter(row_number() == 1) %>%
  dplyr::select(`contrast`, mean, sd, `2.5%`, `97.5%`)

diff_FC_stats <- tibble(posterior) %>% 
  mutate(`contrast` = "FC", 
         mean = mean(diff_FC),
         sd = sd(diff_FC),
         `2.5%` = quantile(diff_FC, 0.025),
         `97.5%` = quantile(diff_FC, 0.975) ) %>%
  filter(row_number() == 1) %>%
  dplyr::select(`contrast`, mean, sd, `2.5%`, `97.5%`)


rbind(diff_RC_stats, diff_RF_stats, diff_FC_stats)


# Formulate as a linear regression
# Add locale as a factor with reordered levels so Control is comparison category
radon$locale <- factor(radon$locale, levels = c(3, 1, 2))
cancer_bayes <- stan_glm(cancer ~ 1 + locale, data = radon, 
                         prior_intercept=NULL, prior=NULL, prior_aux=NULL)
summary(cancer_bayes)

print(cancer_bayes, digits=2)
tidy(cancer_bayes, digits=2)

# Contrasts with emmeans
emm_b <- emmeans(cancer_bayes, ~ locale)
emm_b
pairs(emm_b)




# SR pg. 157 Formulate as a linear regression with index variables
d <- radon %>% mutate(locale = ifelse(locale == 1, "Reading Prong", 
                                      ifelse(locale == 2, "Fringe", "Control")))

d$locale <- factor(d$locale, levels = c("Reading Prong", "Fringe", "Control"))
d$locale_id <- as.integer( d$locale )

m5.9 <- quap(
  alist(
    cancer ~ dnorm( mu , sigma ) ,
    mu <- a[locale_id] ,
    a[locale_id] ~ dnorm( 0 , 3 ) ,
    sigma ~ dexp( 1 )
  ), data = d)
precis( m5.9 , depth = 2 )

labels <- paste( "a[" , 1:3 , "]:" , levels(d$locale) , sep = "" )
plot( precis( m5.9 , depth = 2 , pars = "a" ) , labels = labels)      #  , xlab = "expected cancer rate"

# Plot contrasts (SR pg. 157)
post <- extract.samples(m5.9)
post$diff_12 <- post$a[,1] - post$a[,2]
post$diff_13 <- post$a[,1] - post$a[,3]
post$diff_23 <- post$a[,2] - post$a[,3]
precis( post[c("diff_13", "diff_12", "diff_23")] , depth = 2 , prob = 0.95 )

labels <- c("Reading Prong - Control" , "Reading Prong - Fringe" , "Fringe - Control")
plot( precis( data.frame(post$diff_13, post$diff_12, post$diff_23) ) , labels = labels)      #  , xlab = "Difference in expected cancer rates"




# HSAUR ANOVA pg. 59
data("weightgain", package = "HSAUR")

tapply(weightgain$weightgain, list(weightgain$source, weightgain$type), mean)

tapply(weightgain$weightgain, list(weightgain$source, weightgain$type), sd)

weightgain %>%
  group_by(source, type) %>%
  summarise(mean = mean(weightgain, na.rm = TRUE),
            sd   = sd(  weightgain, na.rm = TRUE),
            n = n())

plot.design(weightgain)

wg_aov <- aov(weightgain ~ source + type, 
              data = weightgain)

summary(wg_aov)

coef(wg_aov)

options("contrasts")

interaction.plot(weightgain$type, weightgain$source, weightgain$weightgain)

coef( aov( weightgain ~ source + type + source:type, 
           data = weightgain, 
           contrasts = list(source = contr.sum) ) )


# Formulate as a linear regression
weightgain <- weightgain %>% 
  mutate(source = factor(source), 
         type   = factor(type))

wg_fit <- lm(weightgain ~ 1 + source + type + source:type, 
             data = weightgain)

summary(wg_fit)

aov(wg_fit)

ref_grid(wg_fit)

# Plot contrasts between types, separately by source
emmip(wg_fit, ~ type | source)

# Plot contrasts between types with different lines for sources
interaction.plot(weightgain$type, weightgain$source, weightgain$weightgain)

# Print contrast estimates and p-values
emmeans(wg_fit, pairwise ~ type | source)




# # HSAUR ANOVA pg. 59
# Bayesian regression versions
# https://mc-stan.org/rstanarm/articles/aov.html
# Option 1
post1 <- stan_aov(weightgain ~ source * type, 
                  data = weightgain,
                  prior = R2(location = 0.5), adapt_delta = 0.999,
                  seed = 12345)
post1


# Option 2
post2 <- stan_lmer(weightgain ~ 1 + (1|source) + (1|type) + (1|source:type),
                   data = weightgain, 
                   prior_intercept = cauchy(),
                   prior_covariance = decov(shape = 2, scale = 2),
                   adapt_delta = 0.999, iter = 3000, seed = 12345)
post2




# DBDA 2E ANOVA pg. 562
# Fruit fly
fruit_fly <- read_csv("/Users/clinton/Downloads/DBDA2Eprograms/FruitflyDataReduced.csv")

fruit_fly$CompanionNumber <- factor(fruit_fly$CompanionNumber, 
                                    levels = unique(fruit_fly$CompanionNumber))

fruit_fly %>%
  group_by(CompanionNumber) %>%
  summarise(mean = mean(Longevity, na.rm = TRUE),
            sd   = sd(  Longevity, na.rm = TRUE),
            n = n()) %>%
  arrange(desc(mean))

boxplot(fruit_fly$Longevity ~ fruit_fly$CompanionNumber, 
        data = fruit_fly, 
        ylab = "Response", xlab = "Treatment")

# Formulate as a linear regression
fruit_fly_fit <- lm(Longevity ~ 1 + CompanionNumber, data = fruit_fly)

summary(fruit_fly_fit)

aov(fruit_fly_fit)

# Plot means of treatments
emmip(fruit_fly_fit, ~ CompanionNumber)

# Print contrast estimates and p-values
fruit_fly_emm <- emmeans(fruit_fly_fit, pairwise ~ CompanionNumber)

coef(fruit_fly_emm)

contrast(fruit_fly_emm)

pairs(fruit_fly_emm)

afex_plot(fruit_fly_fit, x = "CompanionNumber") + theme_classic()


# An R Companion for the Handbook of Biological Statistics
# https://rcompanion.org/rcompanion/h_01.html