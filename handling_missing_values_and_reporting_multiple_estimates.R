# Load libraries
library(arm)
library(broom)
library(broom.mixed)
library(fastDummies)

# tidyverse: https://www.tidyverse.org/
library(tidyverse)

# lavaan: https://lavaan.ugent.be/
library(lavaan)

# mice: https://stefvanbuuren.name/fimd/
library(mice)

# DMwR2: https://ltorgo.github.io/DMwR2/
library(DMwR2)

# brms: https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html
library(brms)

# ggmice: https://cran.r-project.org/web/packages/ggmice/vignettes/ggmice.html
library(ggplot2)
library(ggmice)

# sjPlot: https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html
library(sjPlot)
library(sjmisc)
library(sjlabelled)




# Load algae dataset
data("algae")
algae <- algae[, 1:12]

algae$season <- factor(algae$season, levels = c("spring", "summer", "autumn", "winter"))
algae$size <- factor(algae$size, levels = c("small", "medium", "large"))
algae$speed <- factor(algae$speed, levels = c("low", "medium", "high"))

algae <- bind_cols(algae[, 12], scale(algae[, 4:11]), algae[, 1:3])

dim(algae)
head(algae)
summary(algae)

colnames(algae)
# a1: frequency of harmful algae 1
# mxPH: maximum pH value
# mnO2: minimum value of oxygen
# Cl: mean value of chloride
# NO3: mean value of nitrates
# NH4: mean value of ammonium
# oPO4: mean value of arthophsphate
# PO4: mean of total phosphate
# Chla: mean of chlorophyll
# season: season of year when water collected
# size: size of river
# speed: speed of river

# count number of rows with missing values
nrow( algae[!complete.cases(algae), ] )

# count number of missing values per column
colSums( is.na(algae) )

# missing data pattern: blue is observed; red is missing
mice::md.pattern(algae)

ggmice::plot_pattern(algae, square = TRUE, rotate = TRUE)

ggmice::plot_corr(algae, rotate = TRUE)




### Missing Data: Ad-hoc solutions ###

## Listwise Deletion: Remove the observations with unknown values ##

algae_complete <- na.omit(algae)
dim(algae_complete)
head(algae_complete)

fit1 <- lm(a1 ~ mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla + season + size + speed, data = algae_complete)

arm::display(fit1)


## Central Tendency Imputation: Fill in unknowns with the most frequent column values ##

?centralImputation
?centralValue
?mice

# DMwR2::centralImputation
algae_imp_most_freq <- DMwR2::centralImputation(algae)

fit2a <- lm(a1 ~ mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla + season + size + speed, data = algae_imp_most_freq)

arm::display(fit2a)

# mice mean imputation
algae_imp_mean <- mice(algae, method = "mean", m = 1, maxit = 1)

fit2b <- with(algae_imp_mean, lm(a1 ~ mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla + season + size + speed))

arm::display(pool(fit2b))


## Regression Imputation: Fill in unknowns with weighted average of values of nearest neighbors or predictions from fitted model ##

# DMwR2::knnImputation
algae_imp_knn <- DMwR2::knnImputation(algae, k = 10, meth = "weighAvg")

fit3a <- lm(a1 ~ mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla + season + size + speed, data = algae_imp_knn)

arm::display(fit3a)

# mice regression imputation
algae_imp_reg <- mice(algae, method = "norm.predict", seed = 1, m = 1, print = FALSE)

fit3b <- with(algae_imp_reg, lm(a1 ~ mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla + season + size + speed))

summary(pool(fit3b))

# mice stochastic regression imputation
algae_imp_reg_noise <- mice(algae, method = "norm.nob", m = 1, maxit = 1, seed = 1, print = FALSE)

fit3c <- with(algae_imp_reg_noise, lm(a1 ~ mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla + season + size + speed))

summary(pool(fit3c))




### Multiple Imputation ###

## mice: multiple imputation
algae_imp <- mice(algae, seed = 1, m = 20, print = FALSE)

fit4 <- with(algae_imp, lm(a1 ~ mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla + season + size + speed))

est4 <- pool(fit4)

summary(est4)


## brms: impute missing values with mice before model fitting

fit5 <- brm_multiple(a1 ~ mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + Chla + season + size + speed, data = algae_imp)

summary(fit5)




### Model Comparison ###

sjPlot::tab_model(fit1, 
                  fit2a, pool(fit2b), 
                  fit3a, pool(fit3b), pool(fit3c),
                  pool(fit4),
                  show.ci = FALSE, 
                  show.se = TRUE,
                  collapse.se = TRUE,
                  show.p = FALSE,
                  show.r2 = FALSE,
                  dv.labels = c("M1", "M2", "M3", "M4", "M5", "M6", "M7"),
                  string.est = "Est.",
                  digits = 1)


sjPlot::tab_model(fit5,
                  show.ci = FALSE, 
                  show.se = TRUE,
                  collapse.se = TRUE,
                  show.p = FALSE,
                  show.r2 = FALSE,
                  dv.labels = c("M8"),
                  string.est = "Est.",
                  digits = 1)

