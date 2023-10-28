#### Regression with Graphics by Lawrence Hamilton ####

#### Chapter 4: Regression Criticism ####

library(arm)
library(corrplot)
library(haven)
library(Hmisc)
library(psych)
library(rstatix)

setwd("/Users/clinton/Documents/NYU Regression/rwg")

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

fit_1 <- lm(water81 ~ income + water80 + educat + retire + peop81 + cpeop, data = concord1)

arm::display(fit_1, digits = 1)


## Correlation and Scatterplot Matrices ##

## Table 4.2: Correlation matrix for 1981 household water use and predictors
model.variables <- c("income", "water80", "educat", "retire", "peop81", "cpeop", "water81")
model.subset <- concord1 %>% dplyr::select(all_of(model.variables))

cor.mat1 <- cor(model.subset)
round(cor.mat1, 2)

cor.mat2 <- rstatix::cor_mat(model.subset)
cor.mat2 %>% rstatix::pull_lower_triangle()

corrplot(cor.mat1, method = "color", type = "lower", diag = FALSE, 
         outline = TRUE, addCoef.col = 'black', number.cex = 0.8,
         tl.col = "black", tl.srt = 60, mar = c(0, 0, 2, 0),
         title = "Table 4.2 Correlation matrix")


## Figure 4.1: Scatterplot matrix corresponding to Table 4.2 (household water use and predictors)

# without jitter
pairs(model.subset, upper.panel = NULL, pch = 19, cex = 0.5)

# with jitter
pairs(lapply(model.subset, jitter, 2), upper.panel = NULL, pch = 19, cex = 0.5)

# with psych's pairs.panels
psych::pairs.panels(model.subset, 
                    method = "pearson",
                    hist.col = "steelblue", # "lightgray"
                    density = TRUE,
                    ellipses = FALSE)


## Residual versus Predicted Y Plots ##

## Figure 4.3: Residuals versus predicted values from regression of 1981 household water use on seven predictors

plot(fitted(fit_1), resid(fit_1), 
     ylim = c(-6000, 6000),
     xlab = "Predicted values (Yhat)",
     ylab = "Residuals (e)", 
     main = "Residuals (e) vs Predicted values (Yhat)")
abline(0, 0, lty = 2, lwd = 2)

## Figure 4.4: Absolute residuals versus predicted values
abs.resid <- abs(resid(fit_1))
pred.y <- fitted(fit_1)

plot(pred.y, abs.resid, cex = 0.5,
     xlab = "Predicted values (Yhat)",
     ylab = "Residuals |e|",
     main = "Absolute Residuals |e| vs Predicted values (Yhat)")

lines(lowess(pred.y, abs.resid, f = 1/10), col='black', lty = 2, lwd = 1)


