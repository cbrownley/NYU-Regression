library(broom)
library(broom.mixed)
library(gmodels)
library(tidyverse)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(MASS)
library(rstan)
library(rstanarm)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(ggplot2)
library(bayesplot)
library(patchwork)




# Base R
# https://www.statmethods.net/stats/frequencies.html
# https://www.sheffield.ac.uk/media/30582/download?attachment (formerly: https://www.sheffield.ac.uk/polopoly_fs/1.885176!/file/92_Categorical.pdf)

# Example: IPS pg. 625
# Observed counts for sports goals
goal <- c( rep("HSC-HM", 14), rep("HSC-LM",  7), rep("LSC-HM", 21), rep("LSC-LM", 25),
           rep("HSC-HM", 31), rep("HSC-LM", 18), rep("LSC-HM",  5), rep("LSC-LM", 13) )

sex  <- c( rep("Female", 67), rep("Male", 67) )

# Create table based on categorical variables
table(goal,sex)

# Add row and column totals
addmargins( table(goal,sex) )

round( addmargins(table(goal,sex)) / sum(table(goal,sex)) * 100 , 2 )

# Display column (margin=2) proportions rounded to two decimals
round( prop.table( table(goal,sex) , margin = 2) , digits=2 )

# Display column (margin=2) percents rounded to integer
round( prop.table( table(goal,sex) , margin = 2) * 100 , digits=0 )


# Expected cell counts
margin.table( table(goal,sex) , margin = 1 )

margin.table( table(goal,sex) , margin = 2 )

# Marginal distribution of sports goals across females and males (as a percent)
round( margin.table( table(goal,sex) , margin = 1 ) / sum( table(goal,sex) ) * 100 , digits=1 )

# Expected cell counts in Females column "by hand": row total * column total / total number of observations
round( margin.table( table(goal,sex) , margin = 1 ) * margin.table(table(goal,sex), margin=2 )[1] / sum( table(goal,sex) ) , digits=1 )

# Expected cell counts in Males column "by hand": row total * column total / total number of observations
round( margin.table( table(goal,sex) , margin = 1 ) * margin.table(table(goal,sex), margin=2 )[2] / sum( table(goal,sex) ) , digits=1 )

# Expected cell counts for all cells with gmodels::CrossTable
# https://stackoverflow.com/questions/34214787/is-there-an-r-function-to-get-a-table-of-expected-counts/34214881
CrossTable( table(goal,sex) , digits = 1, expected = TRUE , prop.r = FALSE , prop.c = FALSE , prop.t = FALSE , prop.chisq = FALSE )

CrossTable( table(goal,sex) , digits = 1, expected = TRUE , prop.r = TRUE , prop.c = TRUE , prop.t = TRUE , prop.chisq = FALSE )


# Comparison of sports goals distributions
# for females and males
par(mfrow=c(1,2))
barplot(prop.table(table(goal,sex),2)[,1]*100,
        names.arg = rownames(table(goal,sex)),
        ylim = c(0,50),
        xlab = "Sports goals", 
        ylab = "Percent of subjects",
        main = "Females",
        col = "orange")
barplot(prop.table(table(goal,sex),2)[,2]*100,
        names.arg = rownames(table(goal,sex)),
        ylim = c(0,50),
        xlab = "Sports goals", 
        ylab = "Percent of subjects",
        main = "Males",
        col = "steelblue")
par(mfrow=c(1,1))


# Chi-Square(df=8) Distribution
x <- seq(-0.1, 30.1, length=1000)
y <- dchisq(x, df = 8)
plot(x, y, type = 'l', lwd = 3, col = 'steelblue', 
     xlab = 'x', ylab = 'Probability', 
     main = paste0('Chi-Square(', 8,') Distribution Plot') )


# Chi-Square Distributions
# degrees of freedom vary from 1 to 8
x <- seq(-0.1, 30.1, length=1000)
df1s <- c(1,3,5,7)
df2s <- c(2,4,6,8)
dfs <- cbind(df1s,df2s)

par(mfrow=c(4,2))
for (i in 1:nrow(dfs)) {
  # print(dfs[[i,"df1s"]])
  y <- dchisq(x, df = dfs[[i,"df1s"]])
  plot(x, y, type = 'l', lwd = 3, col = 'steelblue', 
       xlab = 'x', ylab = 'Probability', 
       main = paste0('Chi-Square(', dfs[[i,"df1s"]],') Distribution Plot') )
  y <- dchisq(x, df = dfs[[i,"df2s"]])
  plot(x, y, type = 'l', lwd = 3, col = 'steelblue', 
       xlab = 'x', ylab = 'Probability', 
       main = paste0('Chi-Square(', dfs[[i,"df2s"]],') Distribution Plot') )
}
par(mfrow=c(1,1))



# Chi-Square Test
( Xsq <- chisq.test( table(goal,sex) ) )

tidy(Xsq)

names(Xsq)

Xsq$observed

Xsq$expected


# Chi-Square Statistic "by hand"
sum( (Xsq$observed - Xsq$expected)^2 / Xsq$expected )


# Degrees of freedom
( degrees_of_freedom <- (nrow(table(goal,sex)) - 1) * (ncol(table(goal,sex)) - 1) )


# Critical values for this chi-square distribution
( crit10  <- qchisq( 0.90  , df = 3 ) )
( crit05  <- qchisq( 0.95  , df = 3 ) )
( crit025 <- qchisq( 0.975 , df = 3 ) )
( crit01  <- qchisq( 0.99  , df = 3 ) )
( crit001 <- qchisq( 0.999 , df = 3 ) )


# Chi-Square(df=3) distribution
# define x-axis
x <- seq(-0.1, 30.1, length=1000)
# calculate Chi-Square(df=3) distribution probabilities
y <- dchisq(x, df = 8)
# plot Chi-Square(df=3) distribution
plot(x, y, type = 'l')
plot(x, y, type = 'l', lwd = 3, col = 'steelblue', xlab = 'x', ylab = 'Probability', main = 'Chi-Square(3) Distribution Plot')

# Illustrate area to right of 24.89
nframe <- data.frame(x=x, y=y)

# Calculate line position
line <- qchisq(pchisq(24.89785, df = 3), df = 3)
pval <- pchisq(24.89785, df = 3, lower.tail = FALSE)
xstr <- sprintf("pchisq(>=24.89) = %.7f", pval)

# To the right of the F value
nframe2489 <- subset(nframe, nframe$x >= line)

# Plot it
ggplot(nframe, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data = nframe2489, aes(x=x, y=y), fill = "steelblue") +
  geom_vline(aes(xintercept = line), linetype = 2) +
  annotate(geom = "text", x = 28, y = 0, label = xstr, vjust = 1.5) +
  labs(title = "Chi-Square(3) Distribution (area to right of 24.89 colored blue)") +  # subtitle = xstr
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))


# tibble
# Example: IPS pg. 625
# Observed counts for sports goals
sports <- tibble(
  count = c(14, 31, 7, 18, 21, 5, 25, 13),
  goal = factor(c("HSC-HM", "HSC-HM", "HSC-LM", "HSC-LM", "LSC-HM", "LSC-HM", "LSC-LM", "LSC-LM")), 
  gender = factor(c("Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male")),
)
sports
glimpse(sports)




# Exponential transform produces non-negative values while preserving order
x <- seq(from = -100.0 , to = 10.0 , by = 0.2)
f_x <- exp(x)
plot(x, f_x, type="l")
tibble(x, f_x)




# Poisson distribution takes a non-negative lambda and 
# gives a probability for each integer from 0 to infinity
x <- seq.int(from = 0, to = 51, by = 1)
barplot(height = dpois(x, lambda = 1.8), 
        names.arg = 0:51, 
        main = expression(paste("dpois(y|", lambda, " = 1.8)")), 
        xlab = 'y', 
        ylab = 'p(y)')




# Example: IPS pg. 625
# Models: GLMs pg. 212

# 1. Minimal model (expected frequency)
# https://cran.r-project.org/web/packages/broom/vignettes/broom.html
m0.1 <- glm(count ~ 1, 
    family = poisson(link = "log"), 
    data = sports)
tidy(m0.1)
augment(m0.1)
glance(m0.1)
exp( tidy(m0.1) %>% pull(estimate) )


# 2. Additive model (expected frequencies)
# https://cran.r-project.org/web/packages/broom/vignettes/broom.html
m0.2 <- glm(count ~ 1 + goal + gender, 
    family = poisson(link = "log"), 
    data = sports)
tidy(m0.2)
augment(m0.2)
glance(m0.2)
# Reference category: HSC-HM and Female
exp( tidy(m0.2) %>% filter(term == "(Intercept)") %>% pull(estimate) )


# Exponentiated fitted values from the additive model equal the expected counts
matrix( sapply(augment(m0.2)$.fitted, FUN = exp) , 
        nrow = 4 , ncol = 2 , byrow = TRUE )

# Expected counts
Xsq$expected


# 3. Saturated model (equal to observed frequencies)
# https://cran.r-project.org/web/packages/broom/vignettes/broom.html
m0.3 <- glm(count ~ 1 + goal + gender + goal:gender, 
    family = poisson(link = "log"), 
    data = sports)
tidy(m0.3)
augment(m0.3)
glance(m0.3)
# Reference category: HSC-HM and Female
# Equal to observed frequency, 14
exp( tidy(m0.3) %>% filter(term == "(Intercept)") %>% pull(estimate) )


# Fitted values from the saturated model
matrix( augment(m0.3)$.fitted , 
        nrow = 4 , ncol = 2 , byrow = TRUE )

# Exponentiated fitted values from the saturated model equal the observed counts
matrix( sapply(augment(m0.3)$.fitted, FUN = exp) , 
        nrow = 4 , ncol = 2 , byrow = TRUE )

# Observed counts
Xsq$observed


# Compare model coefficients
# https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html

# Untransformed coefficients (on logarithmic scale)
tab_model(m0.1, m0.2, m0.3, transform = NULL, digits = 2,
          show.ci = FALSE, show.se = TRUE, collapse.se = TRUE,  
          dv.labels = c("Minimal model", "Additive model", "Saturated model"))

# Transformed (exponentiated) coefficients (i.e. multiplicative effects) 
tab_model(m0.1, m0.2, m0.3, digits = 2,
          show.ci = FALSE, show.se = TRUE, collapse.se = TRUE,  
          dv.labels = c("Minimal model", "Additive model", "Saturated model"))




## FOR SCREENSHOT ##

# Minimal model (expected frequency)
m0.1 <- glm(count ~ 1, family = poisson(link = "log"), 
            data = sports)
tidy(m0.1)
exp( tidy(m0.1) %>% pull(estimate) )

# 2. Additive model (expected frequencies)
m0.2 <- glm(count ~ 1 + goal + gender, family = poisson(link = "log"), 
            data = sports)
tidy(m0.2)
# reference category: HSC-HM and Female
exp( tidy(m0.2) %>% filter(term == "(Intercept)") %>% pull(estimate) )

# 3. Saturated model (equal to observed frequencies)
m0.3 <- glm(count ~ 1 + goal + gender + goal:gender, 
            family = poisson(link = "log"), 
            data = sports)

tidy(m0.3)
# reference category: HSC-HM and Female
# equal to observed frequency, 14
exp( tidy(m0.3) %>% filter(term == "(Intercept)") %>% pull(estimate) )

## END FOR SCREENSHOT ##


# Estimate Bayesian version with stan_glm
# 3. Saturated model (equal to observed frequencies)
b0.3 <- stan_glm(count ~ 1 + goal + gender + goal:gender, 
                 family = poisson(link = "log"), data = sports)
tidy(b0.3)
# reference category: HSC-HM and Female
# equal to observed frequency, 14
exp( tidy(b0.3) %>% filter(term == "(Intercept)") %>% pull(estimate) )

# Coefficients from the saturated model (on logarithmic scale)
matrix( round(b0.3$coefficients,2) , nrow = 4 , ncol = 2 , byrow = TRUE )

# Exponentiated coefficients from the saturated model (multiplicative effects)
matrix( round(exp(b0.3$coefficients),2) , nrow = 4 , ncol = 2 , byrow = TRUE )

# Exponentiated fitted values from the saturated model equal the observed counts
matrix( round(b0.3$fitted) , nrow = 4 , ncol = 2 , byrow = TRUE )

# Observed counts
Xsq$observed


# Draws from the posterior distribution
b0.3_post <- as.matrix(b0.3)

mcmc_areas(b0.3_post, prob = 0.89)

# Means of posterior distributions of cell tendencies equal the observed counts
as.data.frame(b0.3) %>% 
  mutate(`Female_HSC-HM` = exp(`(Intercept)`), 
         `Female_HSC-LM` = exp(`(Intercept)` + `goalHSC-LM`), 
         `Female_LSC-HM` = exp(`(Intercept)` + `goalLSC-HM`), 
         `Female_LSC-LM` = exp(`(Intercept)` + `goalLSC-LM`), 
         `Male_HSC-HM` = exp(`(Intercept)` + `genderMale`), 
         `Male_HSC-LM` = exp(`(Intercept)` + `goalHSC-LM` + `genderMale` + `goalHSC-LM:genderMale`), 
         `Male_LSC-HM` = exp(`(Intercept)` + `goalLSC-HM` + `genderMale` + `goalLSC-HM:genderMale`), 
         `Male_LSC-LM` = exp(`(Intercept)` + `goalLSC-LM` + `genderMale` + `goalLSC-LM:genderMale`) ) %>%
  dplyr::select("Female_HSC-HM", "Female_HSC-LM", "Female_LSC-HM", "Female_LSC-LM", 
                "Male_HSC-HM", "Male_HSC-LM", "Male_LSC-HM", "Male_LSC-LM") %>%
  colMeans() %>% round() %>% matrix(nrow=4, ncol=2)


# Posterior distributions of cell tendencies convey uncertainty
plot_title <- ggtitle("Posterior distributions of cell tendencies by sex and sports goal")

as.data.frame(b0.3) %>% 
  mutate(`Female_HSC-HM` = exp(`(Intercept)`), 
         `Female_HSC-LM` = exp(`(Intercept)` + `goalHSC-LM`), 
         `Female_LSC-HM` = exp(`(Intercept)` + `goalLSC-HM`), 
         `Female_LSC-LM` = exp(`(Intercept)` + `goalLSC-LM`), 
         `Male_HSC-HM` = exp(`(Intercept)` + `genderMale`), 
         `Male_HSC-LM` = exp(`(Intercept)` + `goalHSC-LM` + `genderMale` + `goalHSC-LM:genderMale`), 
         `Male_LSC-HM` = exp(`(Intercept)` + `goalLSC-HM` + `genderMale` + `goalLSC-HM:genderMale`), 
         `Male_LSC-LM` = exp(`(Intercept)` + `goalLSC-LM` + `genderMale` + `goalLSC-LM:genderMale`) ) %>%
  mcmc_hist(pars = c("Female_HSC-HM", "Male_HSC-HM", "Female_HSC-LM", "Male_HSC-LM", 
                     "Female_LSC-HM", "Male_LSC-HM", "Female_LSC-LM", "Male_LSC-LM"),
            facet_args = list(nrow=4, ncol=2)) +
  xlim(0,50) +
  plot_title +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))


# Compare model coefficients
# https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html

# Untransformed coefficients (on logarithmic scale)
tab_model(m0.3, b0.3, transform = NULL, digits = 2,
          show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, 
          show.p = FALSE, show.r2 = FALSE, show.obs = FALSE,
          dv.labels = c("Classical model", "Bayesian model") )

# Transformed (exponentiated) coefficients (i.e. multiplicative effects) 
tab_model(m0.3, b0.3, digits = 2,
          show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, 
          show.p = FALSE, show.r2 = FALSE, show.obs = FALSE,
          dv.labels = c("Classical model", "Bayesian model") )



# Example: IPS pg. 635
# Observed counts for smoking habits and socioeconomic status (ses)
smoking <- c( rep("Current", 51), rep("Former",  92), rep("Never", 68),
              rep("Current", 22), rep("Former",  21), rep("Never", 9),
              rep("Current", 43), rep("Former",  28), rep("Never", 22) )

ses  <- factor( c( rep("High", 211), rep("Middle",  52), rep("Low", 93) ) ,
                levels = c("High", "Middle", "Low") )

# Create table based on categorical variables
table(smoking,ses)

# Add row and column totals
addmargins( table(smoking,ses) )

round( addmargins(table(smoking,ses)) / sum(table(smoking,ses)) * 100 , 2 )

# Display column (margin=2) proportions rounded to two decimals
round( prop.table( table(smoking,ses) , margin = 2) , digits=2 )

# Display column (margin=2) percents rounded to integer
round( prop.table( table(smoking,ses) , margin = 2) * 100 , digits=0 )


# Expected cell counts
margin.table( table(smoking,ses) , margin = 1 )

margin.table( table(smoking,ses) , margin = 2 )

# Marginal distribution of smoking across SES (as a percent)
round( margin.table( table(smoking,ses) , margin = 1 ) / sum( table(smoking,ses) ) * 100 , digits=1 )

# Expected cell counts in High SES column "by hand": row total * column total / total number of observations
round( margin.table( table(smoking,ses) , margin = 1 ) * margin.table(table(smoking,ses), margin=2 )[1] / sum( table(smoking,ses) ) , digits=1 )

# Expected cell counts in Middle SES column "by hand": row total * column total / total number of observations
round( margin.table( table(smoking,ses) , margin = 1 ) * margin.table(table(smoking,ses), margin=2 )[2] / sum( table(smoking,ses) ) , digits=1 )

# Expected cell counts in Low SES column "by hand": row total * column total / total number of observations
round( margin.table( table(smoking,ses) , margin = 1 ) * margin.table(table(smoking,ses), margin=2 )[3] / sum( table(smoking,ses) ) , digits=1 )

# Expected cell counts for all cells with gmodels::CrossTable
# https://stackoverflow.com/questions/34214787/is-there-an-r-function-to-get-a-table-of-expected-counts/34214881
CrossTable( table(smoking,ses) , digits = 1, expected = TRUE , prop.r = FALSE , prop.c = FALSE , prop.t = FALSE , prop.chisq = FALSE )

CrossTable( table(smoking,ses) , digits = 1, expected = TRUE , prop.r = TRUE , prop.c = TRUE , prop.t = TRUE , prop.chisq = FALSE )


# Comparison of smoking habits distributions by SES
# for females and males
par(mfrow=c(1,3))
barplot(prop.table(table(smoking,ses),2)[,1]*100,
        names.arg = rownames(table(smoking,ses)),
        ylim = c(0,50),
        xlab = "Smoking habits", 
        ylab = "Percent of subjects",
        main = "High SES",
        col = "steelblue")
barplot(prop.table(table(smoking,ses),2)[,2]*100,
        names.arg = rownames(table(smoking,ses)),
        ylim = c(0,50),
        xlab = "Smoking habits", 
        ylab = "Percent of subjects",
        main = "Middle SES",
        col = "orange")
barplot(prop.table(table(smoking,ses),2)[,3]*100,
        names.arg = rownames(table(smoking,ses)),
        ylim = c(0,50),
        xlab = "Smoking habits", 
        ylab = "Percent of subjects",
        main = "Low SES",
        col = "darkgreen")
par(mfrow=c(1,1))


# Chi-Square(df=4) Distribution
x <- seq(-0.1, 30.1, length=1000)
y <- dchisq(x, df = 4)
plot(x, y, type = 'l', lwd = 3, col = 'steelblue', 
     xlab = 'x', ylab = 'Probability', 
     main = paste0('Chi-Square(', 4,') Distribution Plot') )


# Chi-Square Distributions
# degrees of freedom vary from 1 to 8
x <- seq(-0.1, 30.1, length=1000)
df1s <- c(1,3,5,7)
df2s <- c(2,4,6,8)
dfs <- cbind(df1s,df2s)

par(mfrow=c(4,2))
for (i in 1:nrow(dfs)) {
  # print(dfs[[i,"df1s"]])
  y <- dchisq(x, df = dfs[[i,"df1s"]])
  plot(x, y, type = 'l', lwd = 3, col = 'steelblue', 
       xlab = 'x', ylab = 'Probability', 
       main = paste0('Chi-Square(', dfs[[i,"df1s"]],') Distribution Plot') )
  y <- dchisq(x, df = dfs[[i,"df2s"]])
  plot(x, y, type = 'l', lwd = 3, col = 'steelblue', 
       xlab = 'x', ylab = 'Probability', 
       main = paste0('Chi-Square(', dfs[[i,"df2s"]],') Distribution Plot') )
}
par(mfrow=c(1,1))



# Chi-Square Test
( Xsq <- chisq.test( table(smoking,ses) ) )

tidy(Xsq)

names(Xsq)

Xsq$observed

Xsq$expected


# Chi-Square Statistic "by hand"
sum( (Xsq$observed - Xsq$expected)^2 / Xsq$expected )


# Degrees of freedom
( degrees_of_freedom <- (nrow(table(smoking,ses)) - 1) * (ncol(table(smoking,ses)) - 1) )


# Critical values for this chi-square distribution
( crit10  <- qchisq( 0.90  , df = 4 ) )
( crit05  <- qchisq( 0.95  , df = 4 ) )
( crit025 <- qchisq( 0.975 , df = 4 ) )
( crit01  <- qchisq( 0.99  , df = 4 ) )
( crit001 <- qchisq( 0.999 , df = 4 ) )


# Chi-Square(df=4) distribution
# define x-axis
x <- seq(-0.1, 30.1, length=1000)
# calculate Chi-Square(df=4) distribution probabilities
y <- dchisq(x, df = 4)
# plot Chi-Square(df=4) distribution
plot(x, y, type = 'l')
plot(x, y, type = 'l', lwd = 3, col = 'steelblue', xlab = 'x', ylab = 'Probability', main = 'Chi-Square(4) Distribution Plot')

# Illustrate area to right of 18.51
nframe <- data.frame(x=x, y=y)

# Calculate line position
line <- qchisq(pchisq(18.50974, df = 4), df = 4)
pval <- pchisq(18.50974, df = 4, lower.tail = FALSE)
xstr <- sprintf("pchisq(>=18.51) = %.7f", pval)

# To the right of the F value
nframe1851 <- subset(nframe, nframe$x >= line)

# Plot it
ggplot(nframe, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data = nframe1851, aes(x=x, y=y), fill = "steelblue") +
  geom_vline(aes(xintercept = line), linetype = 2) +
  annotate(geom = "text", x = 22, y = 0, label = xstr, vjust = 1.5) +
  labs(title = "Chi-Square(4) Distribution (area to right of 18.51 colored blue)") +  # subtitle = xstr
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))


# Example: IPS pg. 635
# Models: GLMs pg. 212

# Observed counts for smoking habits and socioeconomic status (ses)
smoking_ses <- tibble(
  count = c(51, 22, 43, 92, 21, 28, 68, 9, 22),
  smoking = factor(c("Current", "Current", "Current",  "Former", "Former", "Former", "Never", "Never", "Never")), 
  ses = factor(c("High", "Middle", "Low", "High", "Middle", "Low", "High", "Middle", "Low")),
)
smoking_ses
glimpse(smoking_ses)

# 1. Minimal model (expected frequency)
# https://cran.r-project.org/web/packages/broom/vignettes/broom.html
m0.1 <- glm(count ~ 1, 
            family = poisson(link = "log"), 
            data = smoking_ses)
tidy(m0.1)
augment(m0.1)
glance(m0.1)
exp( tidy(m0.1) %>% pull(estimate) )


# 2. Additive model (expected frequencies)
# https://cran.r-project.org/web/packages/broom/vignettes/broom.html
m0.2 <- glm(count ~ 1 + smoking + ses, 
            family = poisson(link = "log"), 
            data = smoking_ses)
tidy(m0.2)
augment(m0.2)
glance(m0.2)
# Reference category: smokingCurrent and sesHigh
exp( tidy(m0.2) %>% filter(term == "(Intercept)") %>% pull(estimate) )


# Exponentiated fitted values from the additive model equal the expected counts
matrix( sapply(augment(m0.2)$.fitted, FUN = exp) , 
        nrow = 3 , ncol = 3 , byrow = TRUE )

# Expected counts
Xsq$expected


# 3. Saturated model (equal to observed frequencies)
# https://cran.r-project.org/web/packages/broom/vignettes/broom.html
m0.3 <- glm(count ~ 1 + smoking + ses + smoking:ses, 
            family = poisson(link = "log"), 
            data = smoking_ses)
tidy(m0.3)
augment(m0.3)
glance(m0.3)
# Reference category: smokingCurrent and sesHigh
# Equal to observed frequency, 51
exp( tidy(m0.3) %>% filter(term == "(Intercept)") %>% pull(estimate) )


# Fitted values from the saturated model
matrix( augment(m0.3)$.fitted , 
        nrow = 3 , ncol = 3 , byrow = TRUE )

# Exponentiated fitted values from the saturated model equal the observed counts
matrix( sapply(augment(m0.3)$.fitted, FUN = exp) , 
        nrow = 3 , ncol = 3 , byrow = TRUE )

# Observed counts
Xsq$observed


# Compare model coefficients
# https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html

# Untransformed coefficients (on logarithmic scale)
tab_model(m0.1, m0.2, m0.3, transform = NULL, digits = 2,
          show.ci = FALSE, show.se = TRUE, collapse.se = TRUE,  
          dv.labels = c("Minimal model", "Additive model", "Saturated model"))

# Transformed (exponentiated) coefficients (i.e. multiplicative effects) 
tab_model(m0.1, m0.2, m0.3, digits = 2,
          show.ci = FALSE, show.se = TRUE, collapse.se = TRUE,  
          dv.labels = c("Minimal model", "Additive model", "Saturated model"))


## FOR SCREENSHOT ##

# Minimal model (expected frequency)
m0.1 <- glm(count ~ 1, family = poisson(link = "log"), 
            data = smoking_ses)
tidy(m0.1)
exp( tidy(m0.1) %>% pull(estimate) )

# 2. Additive model (expected frequencies)
m0.2 <- glm(count ~ 1 + smoking + ses, family = poisson(link = "log"), 
            data = smoking_ses)

summary(m0.2)
tidy(m0.2)
# reference category: Current and High
exp( tidy(m0.2) %>% filter(term == "(Intercept)") %>% pull(estimate) )

# 3. Saturated model (equal to observed frequencies)
m0.3 <- glm(count ~ 1 + smoking + ses + smoking:ses, 
            family = poisson(link = "log"), 
            data = smoking_ses)

summary(m0.3)
tidy(m0.3)
# reference category: Current and High
# equal to observed frequency, 51
exp( tidy(m0.3) %>% filter(term == "(Intercept)") %>% pull(estimate) )

## END FOR SCREENSHOT ##


# Estimate Bayesian version with stan_glm
# 3. Saturated model (equal to observed frequencies)
b0.3 <- stan_glm(count ~ 1 + smoking + ses + smoking:ses, 
                 family = poisson(link = "log"), data = smoking_ses)
tidy(b0.3)
# reference category: CurrentSmoker and HighSES
# equal to observed frequency, 51
exp( tidy(b0.3) %>% filter(term == "(Intercept)") %>% pull(estimate) )

# Coefficients from the saturated model (on logarithmic scale)
matrix( round(b0.3$coefficients,2) , nrow = 3 , ncol = 3 , byrow = TRUE )

# Exponentiated coefficients from the saturated model (multiplicative effects)
matrix( round(exp(b0.3$coefficients),2) , nrow = 3 , ncol = 3 , byrow = TRUE )

# Exponentiated fitted values from the saturated model equal the observed counts
matrix( round(b0.3$fitted) , nrow = 3 , ncol = 3 , byrow = TRUE )

# Observed counts
Xsq$observed


# Draws from the posterior distribution
b0.3_post <- as.matrix(b0.3)

mcmc_areas(b0.3_post, prob = 0.89)

# Means of posterior distributions of cell tendencies equal the observed counts
as.data.frame(b0.3) %>% 
  mutate(`Current_High` = exp(`(Intercept)`), 
         `Current_Middle` = exp(`(Intercept)` + `sesMiddle`), 
         `Current_Low` = exp(`(Intercept)` + `sesLow`), 
         `Former_High` = exp(`(Intercept)` + `smokingFormer`), 
         `Former_Middle` = exp(`(Intercept)` + `smokingFormer` + `sesMiddle` + `smokingFormer:sesMiddle`), 
         `Former_Low` = exp(`(Intercept)` + `smokingFormer` + `sesLow` + `smokingFormer:sesLow`), 
         `Never_High` = exp(`(Intercept)` + `smokingNever`),
         `Never_Middle` = exp(`(Intercept)` + `smokingNever` + `sesMiddle` + `smokingNever:sesMiddle`),
         `Never_Low` = exp(`(Intercept)` + `smokingNever` + `sesLow` + `smokingNever:sesLow`) ) %>%
  dplyr::select("Current_High", "Current_Middle", "Current_Low", 
                "Former_High", "Former_Middle", "Former_Low",
                "Never_High", "Never_Middle", "Never_Low") %>%
  colMeans() %>% round() %>% matrix(nrow=3, ncol=3) %>% t()


# Posterior distributions of cell tendencies convey uncertainty
plot_title <- ggtitle("Posterior distributions of cell tendencies by smoking habit and SES")

as.data.frame(b0.3) %>% 
  mutate(`Current_High` = exp(`(Intercept)`), 
         `Current_Middle` = exp(`(Intercept)` + `sesMiddle`), 
         `Current_Low` = exp(`(Intercept)` + `sesLow`), 
         `Former_High` = exp(`(Intercept)` + `smokingFormer`), 
         `Former_Middle` = exp(`(Intercept)` + `smokingFormer` + `sesMiddle` + `smokingFormer:sesMiddle`), 
         `Former_Low` = exp(`(Intercept)` + `smokingFormer` + `sesLow` + `smokingFormer:sesLow`), 
         `Never_High` = exp(`(Intercept)` + `smokingNever`),
         `Never_Middle` = exp(`(Intercept)` + `smokingNever` + `sesMiddle` + `smokingNever:sesMiddle`),
         `Never_Low` = exp(`(Intercept)` + `smokingNever` + `sesLow` + `smokingNever:sesLow`) ) %>% 
  mcmc_hist(pars = c("Current_High", "Current_Middle", "Current_Low", 
                     "Former_High", "Former_Middle", "Former_Low",
                     "Never_High", "Never_Middle", "Never_Low"),
            facet_args = list(nrow=3, ncol=3)) +
  #xlim(0,150) +
  plot_title +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))


# Compare model coefficients
# https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html

# Untransformed coefficients (on logarithmic scale)
tab_model(m0.3, b0.3, transform = NULL, digits = 2,
          show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, 
          show.p = FALSE, show.r2 = FALSE, show.obs = FALSE,
          dv.labels = c("Classical model", "Bayesian model") )

# Transformed (exponentiated) coefficients (i.e. multiplicative effects) 
tab_model(m0.3, b0.3, digits = 2,
          show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, 
          show.p = FALSE, show.r2 = FALSE, show.obs = FALSE,
          dv.labels = c("Classical model", "Bayesian model") )




# Poisson Example
# ROS pg. 264

# Set random seed for reproducability
SEED <- 3579

# Simulate fake data
n <- 100
x <- runif(n, -2, 2)
a <- 1
b <- 2
linpred <- a + b*x
y <- rpois(n, exp(linpred))
fake <- data.frame(x=x, y=y)
head(fake)

# Fit Poisson regression model
fit_fake <- stan_glm(y ~ x, family=poisson(link="log"), data=fake)
print(fit_fake)

# Plot simulated data and regression line
plot(x, y, 
     ylim=c(-1, 200), yaxs="i", bty="l", 
     main="Simulated data from Poisson regression", 
     cex.main=0.9, type="n")
## curve(exp(a + b*x), from=-2.5, to=2.5, add=TRUE)
## Don't bother plotting true curve because it is so close to the fitted curve
curve(exp(coef(fit_fake)[1] + coef(fit_fake)[2]*x), from=-2.5, to=2.5, add=TRUE)
points(x, y, pch=20, cex=.6)




# Negative binomial model for overdispersion
# ROS pg. 266
phi_grid <- c(0.1, 1, 10)

K <- length(phi_grid)

y_nb <- as.list(rep(NA, K))

fake_nb <- as.list(rep(NA, K))

fit_nb <- as.list(rep(NA, K))

for (k in 1:K){
  y_nb[[k]] <- MASS::rnegbin(n, exp(linpred), phi_grid[k])
  
  fake_nb[[k]] <- data.frame(x=x, y=y_nb[[k]])
  
  fit_nb[[k]] <- stan_glm(y ~ x, family=neg_binomial_2(link="log"), 
                          data=fake_nb[[k]], refresh=0)
  
  print(fit_nb[[k]])
}


# Plot simulated data and regression lines 
# from overdispersed Poisson (negative binomial) regressions
par(mar=c(3,3,1,1), mgp=c(1.7,.5,0), tck=-.01)
par(mfrow=c(1,3), oma=c(0,0,2,0))
for (k in 1:K) {
  phi <- phi_grid[k]
  if (phi==0.1)
    plot(x, y_nb[[k]], ylim=c(-1, 200), yaxs="i", bty="l", ylab="y", main=expression(paste(phi, " = ", 0.1)), type="n")
  else if (phi==1)
    plot(x, y_nb[[k]], ylim=c(-1, 200), yaxs="i", bty="l", ylab="y", main=expression(paste(phi, " = ", 1)), type="n")
  else if (phi==10)
    plot(x, y_nb[[k]], ylim=c(-1, 200), yaxs="i", bty="l", ylab="y", main=expression(paste(phi, " = ", 10)), type="n")
  ## curve(exp(a + b*x), from=-2.5, to=2.5, add=TRUE)
  ## Don't bother plotting true curve because it is so close to the fitted curve
  curve(exp(coef(fit_nb[[k]])[1] + coef(fit_nb[[k]])[2]*x), from=-2.5, to=2.5, add=TRUE)
  points(x, y_nb[[k]], pch=20, cex=.7)
}
mtext("Simulated data from overdispersed Poisson (negative binomial) regression", outer=TRUE, side=3, line=1, cex=0.8)
par(mfrow=c(1,1))




# Roaches: Pest Management
# ROS pg. 268
# https://mc-stan.org/rstanarm/articles/count.html
data(roaches)
(n <- nrow(roaches))

# Set random seed for reproducability
SEED <- 3579

# Scale the number of roaches by 100
roaches$roach100 <- roaches$roach1 / 100
head(roaches)

# Negative-binomial model is over-dispersed compared to Poisson
fit_1 <- stan_glm(y ~ roach100 + treatment + senior, 
                  family=neg_binomial_2, offset=log(exposure2), 
                  data=roaches, seed=SEED)
prior_summary(fit_1)
print(fit_1, digits=2)

loo_1 <- loo(fit_1)


# exposure as add'l predictor instead of offset
fit_nb <- stan_glm(y ~ roach100 + exposure2 + treatment + senior, 
                   family=neg_binomial_2, 
                   data=roaches, seed=SEED, refresh=0)
print(fit_nb, digits=2)


# Graphical posterior predictive checking
yrep_1 <- posterior_predict(fit_1)

n_sims <- nrow(yrep_1)

sims_display <- sample(n_sims, 100)

ppc_1 <- ppc_dens_overlay(log10(roaches$y+1), log10(yrep_1[sims_display,]+1))+
  xlab('log10(y+1)') +
  labs(title="negative-binomial") +
  theme(axis.line.y = element_blank())

ppc_1

# ppc with proportion of zero counts test statistic
ppc_stat(y=roaches$y, yrep=yrep_1, stat=function(y) mean(y==0))

print(mean(roaches$y==0), digits=2)

print(mean(yrep_1==0), digits=2)

# ppc with proportion of counts of 1 test statistic
ppc_stat(y=roaches$y, yrep=yrep_1, stat=function(y) mean(y==1))

print(mean(roaches$y==1), digits=2)

print(mean(yrep_1==1), digits=2)

# ppc with 95% quantile test statistic
ppc_stat(y=roaches$y, yrep=yrep_1, stat=function(y) quantile(y, probs=0.95))

# ppc with 99% quantile test statistic
ppc_stat(y=roaches$y, yrep=yrep_1, stat=function(y) quantile(y, probs=0.99))

# ppc with max count test statistic
ppc_stat(y=roaches$y, yrep=yrep_1, stat=max)

print(max(roaches$y), digits=2)

print(max(yrep_1), digits=2)


# Poisson is a special case of negative-binomial
fit_2 <- stan_glm(y ~ roach100 + treatment + senior, 
                  family=poisson, offset=log(exposure2), 
                  data=roaches, seed=SEED)
prior_summary(fit_2)
print(fit_2, digits=2)

loo_2 <- loo(fit_2)


# exposure as add'l predictor instead of offset
fit_p <- stan_glm(y ~ roach100 + exposure2 + treatment + senior, 
                  family=poisson, 
                  data=roaches, seed=SEED, refresh=0)
print(fit_p, digits=2)


# Graphical posterior predictive checking
# instead of y, we plot log10(y+1) to better show the 
# differences in the shape of the predictive distribution
yrep_2 <- posterior_predict(fit_2)

n_sims <- nrow(yrep_2)

sims_display <- sample(n_sims, 100)

ppc_2 <- ppc_dens_overlay(log10(roaches$y+1), log10(yrep_2[sims_display,]+1)) +
  xlim(0,3) +
  xlab('log10(y+1)') +
  labs(title="Poisson") +
  theme(axis.line.y = element_blank())

ppc_2

# ppc with proportion of zero counts test statistic
ppc_stat(y=roaches$y, yrep=yrep_2, stat=function(y) mean(y==0))

print(mean(roaches$y==0), digits=2)

print(mean(yrep_2==0), digits=2)

# ppc with proportion of counts of 1 test statistic
ppc_stat(y=roaches$y, yrep=yrep_2, stat=function(y) mean(y==1))

print(mean(roaches$y==1), digits=2)

print(mean(yrep_2==1), digits=2)

# ppc with 95% quantile test statistic
ppc_stat(y=roaches$y, yrep=yrep_2, stat=function(y) quantile(y, probs=0.95))

# ppc with 99% quantile test statistic
ppc_stat(y=roaches$y, yrep=yrep_2, stat=function(y) quantile(y, probs=0.99))

# ppc with max count test statistic
ppc_stat(y=roaches$y, yrep=yrep_2, stat=max)

print(max(roaches$y), digits=2)

print(max(yrep_2), digits=2)


ppc_2 | ppc_1

# Compare the two models
loo_compare(loo_1, loo_2)





### In-class exercise ###

DAT <- as.table( rbind( c(0, 11, 2, 0), 
                        c(2, 0, 0, 2),
                        c(1, 3, 1, 1),
                        c(1, 0, 0, 1) ) )
dimnames(DAT) <- list( hair_color = c("Black", "Blond", "Brown", "Red"),
                       eye_color  = c("Blue", "Brown", "Green", "Hazel") )
DAT

addmargins(DAT)

round( addmargins(DAT) / sum(DAT) * 100 , 2 )

# Display row (margin=1) proportions rounded to two decimals
round( prop.table( DAT , margin = 1) , digits=2 )

# Display column (margin=2) proportions rounded to two decimals
round( prop.table( DAT , margin = 2) , digits=2 )

# Marginal distribution of eye color
margin.table( DAT , margin = 1 )

# Marginal distribution of hair color
margin.table( DAT , margin = 2 )

# Marginal distribution of eye color across hair colors (as a percent)
round( margin.table( DAT, margin = 1 ) / sum( DAT ) * 100 , digits=1 )

# Marginal distribution of hair color across eye colors (as a percent)
round( margin.table( DAT, margin = 2 ) / sum( DAT ) * 100 , digits=1 )

# Expected cell counts in Black Hair column "by hand": row total * column total / total number of observations
round( margin.table( DAT , margin = 1 ) * margin.table(DAT, margin=2 )[1] / sum( DAT ) , digits=1 )

# Expected cell counts in Blond Hair column "by hand": row total * column total / total number of observations
round( margin.table( DAT , margin = 1 ) * margin.table(DAT, margin=2 )[2] / sum( DAT ) , digits=1 )

# Expected cell counts in Green Eyes column "by hand": row total * column total / total number of observations
round( margin.table( DAT , margin = 2 ) * margin.table(DAT, margin=1 )[3] / sum( DAT ) , digits=1 )

# Expected cell counts for all cells with gmodels::CrossTable
gmodels::CrossTable( DAT , digits = 1, expected = TRUE , prop.r = FALSE , prop.c = FALSE , prop.t = FALSE , prop.chisq = FALSE )




# Chi-Square Test
( Xsq <- chisq.test( DAT ) ) # table(eye_color,hair_color)

tidy(Xsq)

names(Xsq)

Xsq$observed

Xsq$expected




DAT_tibble <- dplyr::as_tibble(DAT) %>% dplyr::mutate(eye_color = factor(eye_color), hair_color = factor(hair_color))
DAT_tibble

# 2. Additive model (expected frequencies)
he2 <- glm(n ~ 1 + eye_color + hair_color, family = poisson(link = "log"), data = DAT_tibble)
tidy(he2)

# Exponentiated fitted values from the additive model equal the expected counts
matrix( sapply(augment(he2)$.fitted, FUN = exp) , 
        nrow = 4 , ncol = 4 , byrow = FALSE )

# Expected counts
Xsq$expected


# 3. Saturated model (equal to observed frequencies)
he3 <- glm(n ~ 1 + eye_color + hair_color + eye_color:hair_color, family = poisson(link = "log"), data = DAT_tibble)
tidy(he3)

# Exponentiated fitted values from the saturated model equal the observed counts
matrix( sapply(augment(he3)$.fitted, FUN = exp) , 
        nrow = 4 , ncol = 4 , byrow = FALSE )

# Observed counts
Xsq$observed








## EXTRA ##

# DBDA2E Ch 24
# Count Predicted Variable
# HairEyeColor.csv
hair_eye_color <- read_csv("/Users/clinton/Downloads/DBDA2Eprograms/HairEyeColor.csv")
glimpse(hair_eye_color)

hair_eye_color <-  hair_eye_color %>% 
  mutate(Hair = factor(Hair, levels = c("Black", "Blond", "Brown", "Red")),
         Eye = factor(Eye, levels = c("Blue", "Brown", "Green", "Hazel")) )

glimpse(hair_eye_color)
levels(hair_eye_color$Hair)
levels(hair_eye_color$Eye)
hair_eye_color

hair_eye_tab <- tidyr::pivot_wider(hair_eye_color, names_from = Hair, values_from = Count)
hair_eye_tab

eye_color <- c( rep("Blue", 20), rep("Brown",  68), rep("Green", 5), rep("Hazel", 15), 
                rep("Blue", 94), rep("Brown",  7), rep("Green", 16), rep("Hazel", 10), 
                rep("Blue", 84), rep("Brown",  119), rep("Green", 29), rep("Hazel", 54),
                rep("Blue", 17), rep("Brown",  26), rep("Green", 14), rep("Hazel", 14) )

hair_color  <- factor( c( rep("Black", 108), rep("Blond",  127), rep("Brown", 286) , rep("Red", 71) ), 
                       levels = c("Black", "Blond", "Brown", "Red") )


# Create table based on categorical variables
table(eye_color,hair_color)

# Add row and column totals
addmargins( table(eye_color,hair_color) )

round( addmargins(table(eye_color,hair_color)) / sum(table(eye_color,hair_color)) * 100 , 2 )




# Another hair and eye color dataset
head(HairEyeColor)

# Sum over females and males
# https://stackoverflow.com/questions/36980483/how-to-sum-array-along-3rd-dimension
rowSums(HairEyeColor, dims = 2)

addmargins( rowSums(HairEyeColor, dims = 2) )


# https://stat.ethz.ch/pipermail/r-help/2008-October/178070.html
x <- rchisq(100, 5)
hist(x, prob=TRUE)
curve( dchisq(x, df=5), col='green', add=TRUE)
curve( dchisq(x, df=10), col='red', add=TRUE )
lines( density(x), col='orange')


# Two-Way Tables

# Binge drinking by sex

# tribble
drinkers <- tribble(
  ~`Frequent binge drinker`, ~Men, ~Women, ~Total,
  #--|--|--|----
  "Yes", 1630, 1684, 3314,
  "No", 5550, 8232, 13782,
  "Total", 7180, 9916, 17096
)
drinkers
glimpse(drinkers)

# tibble
drinkers <- tibble(
  count = c(1630, 1684, 5550, 8232),
  is_binge_drinker = c(1, 1, 0, 0), 
  is_male = c(1, 0, 1, 0)
)
drinkers
glimpse(drinkers)

with(drinkers, xtabs(count ~ is_binge_drinker + is_male))

glm(count ~ 1, 
    family = poisson(link = "log"), 
    data = drinkers)

glm(count ~ 1 + factor(is_binge_drinker) + factor(is_male), 
    family = poisson(link = "log"), 
    data = drinkers)

glm(count ~ 1 + factor(is_binge_drinker) + factor(is_male) + factor(is_binge_drinker):factor(is_male), 
    family = poisson(link = "log"), 
    data = drinkers)


# Sports goals by sex

# tribble
sports <- tribble(
  ~Goal, ~Women, ~Men, ~Total,
  #--|--|--|----
  "HSC-HM", 14, 31, 45,
  "HSC-LM", 7, 18, 25,
  "LSC-HM", 21, 5, 26,
  "LSC-LM", 25, 13, 38,
  "Total", 67, 67, 134
)
sports
glimpse(sports)

# tibble
sports <- tibble(
  count = c(14, 31, 7, 18, 21, 5, 25, 13),
  goal = c("HSC-HM", "HSC-LM", "LSC-HM", "LSC-LM", "HSC-HM", "HSC-LM", "LSC-HM", "LSC-LM"), 
  gender = c("Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male"),
  is_female = c(1, 0, 1, 0, 1, 0, 1, 0)
)
sports
glimpse(sports)


glm(count ~ 1, 
    family = poisson(link = "log"), 
    data = sports)

glm(count ~ 1 + factor(goal) + factor(gender), 
    family = poisson(link = "log"), 
    data = sports)

glm(count ~ 1 + factor(goal) + factor(gender) + factor(goal):factor(gender), 
    family = poisson(link = "log"), 
    data = sports)

