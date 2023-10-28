
#### Regression and Other Stories ####

#### START: Mother Attributes and Kid Test Score - One intercept / One slope Model ####

## SOURCE: https://avehtari.github.io/ROS-Examples/KidIQ/kidiq.html
library(rstanarm)
data(kidiq)
str(kidiq)

fit_2 <- stan_glm(kid_score ~ mom_iq, data=kidiq, refresh = 0)
print(fit_2)
summary(fit_2)

colors <- ifelse(kidiq$mom_hs==1, "black", "gray")

plot(kidiq$mom_iq, kidiq$kid_score,
     xlab="Mother IQ score", ylab="Child test score", col=colors, pch=20)
abline(coef(fit_2), lwd=2)

#### END: Mother Attributes and Kid Test Score - One intercept / One slope Model ####




#### START: Mother Attributes and Kid Test Score - Varying-intercepts, Same slope Model ####

## SOURCE: https://avehtari.github.io/ROS-Examples/KidIQ/kidiq.html
library(rstanarm)
data(kidiq)
str(kidiq)

fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq, refresh = 0)
print(fit_3)
summary(fit_3)

colors <- ifelse(kidiq$mom_hs==1, "black", "gray")

plot(kidiq$mom_iq, kidiq$kid_score,
     xlab="Mother IQ score", ylab="Child test score", col=colors, pch=20)

b_hat <- coef(fit_3)
abline(b_hat[1] + b_hat[2], b_hat[3], col="black", lwd=2)
abline(b_hat[1], b_hat[3], col="gray", lwd=2)

#### END: Mother Attributes and Kid Test Score - Varying-intercepts, Same slope Model ####




#### START: Mother Attributes and Kid Test Score - Varying-intercepts, Varying-slopes Model ####

## SOURCE: https://avehtari.github.io/ROS-Examples/KidIQ/kidiq.html
library(rstanarm)
data(kidiq)
str(kidiq)

fit_4 <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=kidiq,
                  refresh = 0)
print(fit_4)
summary(fit_4)

colors <- ifelse(kidiq$mom_hs==1, "black", "gray")

plot(kidiq$mom_iq, kidiq$kid_score,
     xlab="Mother IQ score", ylab="Child test score", col=colors, pch=20)

b_hat <- coef(fit_4)
abline(b_hat[1] + b_hat[2], b_hat[3] + b_hat[4], col="black", lwd=2)
abline(b_hat[1], b_hat[3], col="gray", lwd=2)

#### END: Mother Attributes and Kid Test Score - Varying-intercepts, Varying-slopes Model ####




#### Statistical Rethinking, 2nd Edition ####

#### START: Terrain Ruggedness and GDP - One intercept / One slope Model ####

## SOURCE: https://github.com/rmcelreath/rethinking/blob/master/book_code_boxes.txt
library(rethinking)
data(rugged)
d <- rugged
str(d)

# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )
str(d)

# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]

# rescale variables
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
str(dd)

# make variable to index Africa (1) or not (2)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )
str(dd)


m8.1 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a + b*( rugged_std - 0.215 ) ,
    a ~ dnorm( 1 , 0.1 ) ,
    b ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp(1)
  ) , data=dd )

precis( m8.1 )


rugged_seq <- seq( from=-0.1 , to=1.1 , length.out=30 )

# compute mu over samples, fixing cid=2 and then cid=1
mu.m8.1 <- link( m8.1 , data=data.frame( rugged_std=rugged_seq ) )

# summarize to means and intervals
mu.m8.1_mu <- apply( mu.m8.1 , 2 , mean )
mu.m8.1_ci <- apply( mu.m8.1 , 2 , PI , prob=0.97 )

plot( dd$rugged_std , dd$log_gdp_std , pch=ifelse(dd$cid==1, 16, 1) , col=ifelse(dd$cid==1, rangi2, "black") ,
      xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
      xlim=c(0,1) )
lines( rugged_seq , mu.m8.1_mu , lwd=2 )
shade( mu.m8.1_ci , rugged_seq )
mtext("Model that doesn't distinguish between African and Non-African nations. African nations shown in blue.")

#### END: Terrain Ruggedness and GDP - One intercept / One slope Model ####




#### START: Terrain Ruggedness and GDP - Varying-intercepts, Same slope Model ####

## SOURCE: https://github.com/rmcelreath/rethinking/blob/master/book_code_boxes.txt
library(rethinking)
data(rugged)
d <- rugged
str(d)

# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )
str(d)

# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]

# rescale variables
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)

# make variable to index Africa (1) or not (2)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )
str(dd)


m8.2 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dd )

precis( m8.2 , depth=2 )


rugged_seq <- seq( from=-0.1 , to=1.1 , length.out=30 )

# compute mu over samples, fixing cid=2 and then cid=1
mu.NotAfrica <- link( m8.2 ,
                      data=data.frame( cid=2 , rugged_std=rugged_seq ) )
mu.Africa <- link( m8.2 ,
                   data=data.frame( cid=1 , rugged_std=rugged_seq ) )

# summarize to means and intervals
mu.NotAfrica_mu <- apply( mu.NotAfrica , 2 , mean )
mu.NotAfrica_ci <- apply( mu.NotAfrica , 2 , PI , prob=0.97 )
mu.Africa_mu <- apply( mu.Africa , 2 , mean )
mu.Africa_ci <- apply( mu.Africa , 2 , PI , prob=0.97 )

plot( dd$rugged_std , dd$log_gdp_std , pch=ifelse(dd$cid==1, 16, 1) , col=ifelse(dd$cid==1, rangi2, "black") ,
      xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
      xlim=c(0,1) )
lines( rugged_seq , mu.Africa_mu , lwd=2 )
shade( mu.Africa_ci , rugged_seq , col=col.alpha(rangi2,0.3) )
lines( rugged_seq , mu.NotAfrica_mu , lwd=2 )
shade( mu.NotAfrica_ci , rugged_seq )
mtext("Indicator for African nations has no effect on the slope. African nations shown in blue.")

#### END: Terrain Ruggedness and GDP - Varying-intercepts / Same slope Model ####




#### START: Terrain Ruggedness and GDP - Varying-intercepts / Varying-slopes Model ####

## SOURCE: https://github.com/rmcelreath/rethinking/blob/master/book_code_boxes.txt
library(rethinking)
data(rugged)
d <- rugged
str(d)

# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )
str(d)

# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]

# rescale variables
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)

# make variable to index Africa (1) or not (2)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )
str(dd)


m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dd )

precis( m8.3 , depth=2 )


rugged_seq <- seq( from=-0.1 , to=1.1 , length.out=30 )

# compute mu over samples, fixing cid=2 and then cid=1
mu.NotAfrica <- link( m8.3 ,
                      data=data.frame( cid=2 , rugged_std=rugged_seq ) )
mu.Africa <- link( m8.3 ,
                   data=data.frame( cid=1 , rugged_std=rugged_seq ) )

# summarize to means and intervals
mu.NotAfrica_mu <- apply( mu.NotAfrica , 2 , mean )
mu.NotAfrica_ci <- apply( mu.NotAfrica , 2 , PI , prob=0.97 )
mu.Africa_mu <- apply( mu.Africa , 2 , mean )
mu.Africa_ci <- apply( mu.Africa , 2 , PI , prob=0.97 )

plot( dd$rugged_std , dd$log_gdp_std , pch=ifelse(dd$cid==1, 16, 1) , col=ifelse(dd$cid==1, rangi2, "black") ,
      xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
      xlim=c(0,1) )
lines( rugged_seq , mu.Africa_mu , lwd=2 )
shade( mu.Africa_ci , rugged_seq , col=col.alpha(rangi2,0.3) )
lines( rugged_seq , mu.NotAfrica_mu , lwd=2 )
shade( mu.NotAfrica_ci , rugged_seq )
mtext("Adding an interaction between slope and continent recovers the change in slope. African nations shown in blue.")


## Compare models
compare( m8.1 , m8.2 , m8.3 , func=PSIS )

#### END: Terrain Ruggedness and GDP - Varying-intercepts / Varying-slopes Model ####




#### Statistical Rethinking, 2nd Edition ####

#### START: Water, Shade, & Tulip Growth - Model WITHOUT Interaction ####

## SOURCE: https://github.com/rmcelreath/rethinking/blob/master/book_code_boxes.txt
library(rethinking)
data(tulips)
d <- tulips
str(d)

## R code 8.20
d$blooms_std <- d$blooms / max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)
str(d)

## R code 8.23
m8.4 <- quap(
  alist(
    blooms_std ~ dnorm( mu , sigma ) ,
    mu <- a + bw*water_cent + bs*shade_cent ,
    a ~ dnorm( 0.5 , 0.25 ) ,
    bw ~ dnorm( 0 , 0.25 ) ,
    bs ~ dnorm( 0 , 0.25 ) ,
    sigma ~ dexp( 1 )
  ) , data=d )

precis( m8.4 , depth=2 )


par(mfrow=c(1,3)) # 3 plots in 1 row
for ( s in -1:1 ) {
  idx <- which( d$shade_cent==s )
  plot( d$water_cent[idx] , d$blooms_std[idx] , xlim=c(-1,1) , ylim=c(0,1) ,
        xlab="water" , ylab="blooms" , pch=16 , col=rangi2 )
  mu <- link( m8.4 , data=data.frame( shade_cent=s , water_cent=-1:1 ) )
  for ( i in 1:20 ) lines( -1:1 , mu[i,] , col=col.alpha("black",0.3) )
  mtext(paste0("shade = ", s))
}

mtext("Non-interaction model: Water helps (positive slopes). Shade hurts (lines sink left to right). But slope doesn't vary across shade levels.", side = 3, line = -2, outer = TRUE)
par(mfrow=c(1,1))

#### END: Water, Shade, & Tulip Growth - Model WITHOUT Interaction ####




#### START: Water, Shade, & Tulip Growth - Model WITH Interaction ####

## SOURCE: https://github.com/rmcelreath/rethinking/blob/master/book_code_boxes.txt
m8.5 <- quap(
  alist(
    blooms_std ~ dnorm( mu , sigma ) ,
    mu <- a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent ,
    a ~ dnorm( 0.5 , 0.25 ) ,
    bw ~ dnorm( 0 , 0.25 ) ,
    bs ~ dnorm( 0 , 0.25 ) ,
    bws ~ dnorm( 0 , 0.25 ) ,
    sigma ~ dexp( 1 )
  ) , data=d )

precis( m8.5 , depth=2 )

par(mfrow=c(1,3)) # 3 plots in 1 row
for ( s in -1:1 ) {
  idx <- which( d$shade_cent==s )
  plot( d$water_cent[idx] , d$blooms_std[idx] , xlim=c(-1,1) , ylim=c(0,1) ,
        xlab="water" , ylab="blooms" , pch=16 , col=rangi2 )
  mu <- link( m8.5 , data=data.frame( shade_cent=s , water_cent=-1:1 ) )
  for ( i in 1:20 ) lines( -1:1 , mu[i,] , col=col.alpha("black",0.3) )
  mtext(paste0("shade = ", s))
}

mtext("Interaction model: Water helps. Shade hurts. Now slopes vary across shade levels. Effect of water decreases as shade increases.", side = 3, line = -2, outer = TRUE)
par(mfrow=c(1,1))

#### END: Water, Shade, & Tulip Growth - Model WITH Interaction ####



#### Mastering Metrics ####

#### START: Banks: Differences-in-Differences (DD) ####
library(dplyr)
library(ggplot2)
library(haven)
library(readr)
library(reshape2)
library(lubridate)

banks <- read_csv('/Users/clinton/Documents/Mastering Metrics/Chapter 5/banks.csv')

## Visualize annual trends
banks %>%
  mutate(date = make_date(year, month, day)) %>%
  group_by(year = floor_date(date, 'year')) %>%
  summarise(`Sixth District` = mean(bib6, na.rm = TRUE),
            `Eighth District` = mean(bib8, na.rm = TRUE)) %>%
  arrange(year) %>%
  melt(., id.vars = 'year', variable.name = 'series') %>%
  
  ggplot(., aes(x=year, y=value, colour=series)) +
    geom_line() + 
    geom_point() +
    scale_colour_manual(values = c("Sixth District" = "steelblue","Eighth District" = "orange")) + 
    labs(title = 'Figure 5.2: Trends in bank failures in the Sixth and Eighth Federal Reserve Districts',
         x = 'Year', 
         y = 'Number of banks in business',
         color = 'District') + 
    theme_classic()


## DD with four numbers
# treat(post-pre) - control(post-pre)
banks %>%
  mutate(date = make_date(year, month, day)) %>%
  group_by(year = floor_date(date, 'year')) %>%
  filter(year %in% c(ymd('1930-01-01'), ymd('1931-01-01'))) %>%
  summarise(`Sixth District` = mean(bib6, na.rm = TRUE),
            `Eighth District` = mean(bib8, na.rm = TRUE)) %>%
  arrange(year) %>%
  summarise(treat.diff = (`Sixth District`[2] - `Sixth District`[1]),
            ctrl.diff = (`Eighth District`[2] - `Eighth District`[1]),
            dd.estimate = 
              (`Sixth District`[2] - `Sixth District`[1]) - 
              (`Eighth District`[2] - `Eighth District`[1]))
  
# post(treat-control) - pre(treat-control)
banks %>%
  mutate(date = make_date(year, month, day)) %>%
  group_by(year = floor_date(date, 'year')) %>%
  filter(year %in% c(ymd('1930-01-01'), ymd('1931-01-01'))) %>%
  summarise(`Sixth District` = mean(bib6, na.rm = TRUE),
            `Eighth District` = mean(bib8, na.rm = TRUE)) %>%
  arrange(year) %>%
  summarise(post.diff = (`Sixth District`[2] - `Eighth District`[2]),
            pre.diff = (`Sixth District`[1] - `Eighth District`[1]),
            dd.estimate = 
              (`Sixth District`[2] - `Eighth District`[2]) - 
              (`Sixth District`[1] - `Eighth District`[1]))


## Regression DD
banks_dd <- banks %>%
  mutate(date = make_date(year, month, day)) %>%
  group_by(year = floor_date(date, 'year')) %>%
  summarise(`Sixth District` = mean(bib6, na.rm = TRUE),
            `Eighth District` = mean(bib8, na.rm = TRUE)) %>%
  melt(., id.vars = 'year', variable.name = 'series') %>%
  arrange(year) %>%
  rename(date=year, district=series, bib=value) %>%
  mutate(treat = ifelse(district == 'Sixth District', 1., 0.),
         post = ifelse(year(date) >= '1931', 1., 0.))

banks_dd


banks_fit <- stan_glm(bib ~ treat + post + treat:post, data=banks_dd, 
                      refresh = 0)
print(banks_fit)
summary(banks_fit)

#### END: Banks ####




#### Mastering Metrics ####

#### START: Minimum Legal Drinking Age (MLDA) Regression Discontinuity (RD) ####
library(dplyr)
library(ggplot2)
library(haven)
library(readr)
library(reshape2)
library(lubridate)

MLDA <- haven::read_dta('/Users/clinton/Documents/Mastering Metrics/Chapter 4/AEJfigs.dta')

glimpse(MLDA)
head(MLDA)

MLDA <- MLDA[complete.cases(MLDA$all), ]

# Figure 4.2 A sharp RD estimate of MLDA mortality effects (Page 150)
ggplot(MLDA, aes(x = agecell, y = all)) +
  geom_point() +
  geom_vline(xintercept = 21, lty = 2) + 
  labs(title = 'Figure 4.2 A sharp RD estimate of MLDA mortality effects',
       x = 'Age', 
       y = 'Death rate from all causes (per 100,000)') + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))

# treatment indicator
# D=1 indicates legal drinking age
# D=0 indicates otherwise
MLDA$D <- ifelse(MLDA$agecell >= 21, 1, 0)

# linear regression (no interaction term)
mlda_fit1 <- stan_glm(all ~ D + agecell, data = MLDA, refresh = 0)

summary(mlda_fit1)


MLDA$pred1 = predict(mlda_fit1)


ggplot(MLDA, aes(agecell, all)) +
  geom_point(aes(color = factor(D)), show.legend = FALSE) +
  geom_line(data = MLDA %>% filter(D == 0), aes(y = pred1), color="darkgray") +
  geom_line(data = MLDA %>% filter(D == 1), aes(y = pred1), color="black") +
  geom_vline(xintercept = 21, lty = 2) + 
  scale_color_manual(values = c("darkgray", "black")) +
  scale_y_continuous(breaks=seq(80, 115, 5), limits = c(79, 116)) + 
  labs(title = 'Figure 4.2 A sharp RD estimate of MLDA mortality effects',
       x = 'Age', 
       y = 'Death rate from all causes (per 100,000)') + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))



# center the running variable (age) around the cutoff (21)
MLDA$agecell_cent <- MLDA$agecell - 21

# linear regression (no interaction term)
mlda_fit2 <- stan_glm(all ~ D + agecell_cent + D:agecell_cent + 
                            I(agecell_cent^2) + D:I(agecell_cent^2), 
                      data = MLDA, refresh = 0)

summary(mlda_fit2)


MLDA$pred2 = predict(mlda_fit2)


ggplot(MLDA, aes(agecell, all)) +
  geom_point(aes(color = factor(D)), show.legend = FALSE) +
  
  geom_line(data = MLDA %>% filter(D == 0), aes(y = pred1), lty = 2, color="darkgray") +
  geom_line(data = MLDA %>% filter(D == 1), aes(y = pred1), lty = 2, color="black") +
  
  geom_line(data = MLDA %>% filter(D == 0), aes(y = pred2), color="darkgray") +
  geom_line(data = MLDA %>% filter(D == 1), aes(y = pred2), color="black") +
  
  geom_vline(xintercept = 21, lty = 2) + 
  scale_color_manual(values = c("darkgray", "black")) +
  scale_y_continuous(breaks=seq(80, 115, 5), limits = c(79, 116)) + 
  labs(title = 'Figure 4.4 Quadratic control in a RD design',
       x = 'Age', 
       y = 'Death rate from all causes (per 100,000)') + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))




#### Introductory Econometrics: A Modern Approach, 7E ####

#### START: Models with Interaction Terms Example 6.3 Page 193 ####
library(wooldridge)
data("attend")

head(attend)

# center attendance rate, prior GPA, and ACT score 
attend$atndrte_cent <- attend$atndrte - mean(attend$atndrte)
attend$priGPA_cent <- attend$priGPA - mean(attend$priGPA)
attend$ACT_cent <- attend$ACT - mean(attend$ACT)
str(attend)

# lm() uncentered (no interaction term, no quadratics)
attendance_fit <- lm(stndfnl ~ atndrte + priGPA, data=attend)

summary(attendance_fit)


# lm() uncentered (interaction term, no quadratics)
attendance_fit <- lm(stndfnl ~ atndrte + priGPA + priGPA:atndrte, data=attend)

summary(attendance_fit)


# lm() centered (interaction term, quadratics)
attendance_fit <- lm(stndfnl ~ atndrte_cent + priGPA_cent + I(priGPA_cent^2) + ACT_cent + I(ACT_cent^2) + priGPA_cent:atndrte_cent, data=attend)

summary(attendance_fit)

arm::display(attendance_fit, digits = 4)


# stan_glm() centered (interaction term, quadratics)
attendance_fit <- stan_glm(stndfnl ~ atndrte_cent + priGPA_cent + I(priGPA_cent^2) + ACT_cent + I(ACT_cent^2) + priGPA_cent:atndrte_cent, data=attend, refresh = 0)

print(attendance_fit, digits = 4)

rstan::summary(attendance_fit, digits = 4)




# 3D Plot
# source: https://rpubs.com/pjozefek/576206
library(wooldridge)
data("attend")
library(plot3D)

# set the x, y, and z variables
x <- attend$atndrte
y <- attend$priGPA
z <- attend$stndfnl

# NO INTERACTION (Flat Surface)

# Compute the linear regression (no interaction term)
fit <- lm(z ~ x + y)

# create a grid from the x and y values (min to max) and predict values for every point
# this will become the regression plane
grid.lines = 40
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred )
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

# create the fitted points for droplines to the surface
fitpoints <- predict(fit)

# scatter plot with regression plane
# theta = 40, phi = 10
scatter3D(x, y, z, pch = 1, cex = .2, colvar = NULL, col = "#cccccc", 
          theta = 40, phi = 10, bty="b",
          xlab = "Attendance Rate", ylab = "Prior GPA", zlab = "Final Exam Performance",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = TRUE, fit = fitpoints, 
                      col=ramp.col(col = c("dodgerblue3", "seagreen2"), 
                                   n = 300, 
                                   alpha=0.9), 
                      border="black"), 
          main = "Effects of Attendance on Final Exam Performance")


# INTERACTION TERM (Curved Surface)

# Compute the linear regression (with interaction term)
fit <- lm(z ~ x + y + x:y)

# create a grid from the x and y values (min to max) and predict values for every point
# this will become the regression plane
grid.lines = 40
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred )
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

# create the fitted points for droplines to the surface
fitpoints <- predict(fit)

# scatter plot with regression plane
# theta = 40, phi = 10
scatter3D(x, y, z, pch = 1, cex = .2, colvar = NULL, col = "#cccccc", 
          theta = 40, phi = 10, bty="b",
          xlab = "Attendance Rate", ylab = "Prior GPA", zlab = "Final Exam Performance",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = TRUE, fit = fitpoints, 
                      col=ramp.col(col = c("dodgerblue3", "seagreen2"), 
                                   n = 300, 
                                   alpha=0.9), 
                      border="black"), 
          main = "Effects of Attendance on Final Exam Performance")




# CHANGE PERSPECTIVE: ROTATE

# theta = 20, phi = 10
scatter3D(x, y, z, pch = 1, cex = .5, colvar = NULL, col = "red", 
          theta = 20, phi = 10, bty="b",
          xlab = "Attendance Rate", ylab = "Prior GPA", zlab = "Final Exam Performance",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = TRUE, fit = fitpoints, 
                      col=ramp.col(col = c("dodgerblue3", "seagreen2"), 
                                   n = 300, 
                                   alpha=0.9), 
                      border="black"), 
          main = "Effects of Attendance on Final Exam Performance")

# theta = 0, phi = 10
scatter3D(x, y, z, pch = 1, cex = .5, colvar = NULL, col = "red", 
          theta = 0, phi = 10, bty="b",
          xlab = "Attendance Rate", ylab = "Prior GPA", zlab = "Final Exam Performance",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = TRUE, fit = fitpoints, 
                      col=ramp.col(col = c("dodgerblue3", "seagreen2"), 
                                   n = 300, 
                                   alpha=0.9), 
                      border="black"), 
          main = "Effects of Attendance on Final Exam Performance")

# theta = -20, phi = 10
scatter3D(x, y, z, pch = 1, cex = .5, colvar = NULL, col = "red", 
          theta = -20, phi = 10, bty="b",
          xlab = "Attendance Rate", ylab = "Prior GPA", zlab = "Final Exam Performance",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = TRUE, fit = fitpoints, 
                      col=ramp.col(col = c("dodgerblue3", "seagreen2"), 
                                   n = 300, 
                                   alpha=0.9), 
                      border="black"), 
          main = "Effects of Attendance on Final Exam Performance")

# theta = -40, phi = 10
scatter3D(x, y, z, pch = 1, cex = .5, colvar = NULL, col = "red", 
          theta = -40, phi = 10, bty="b",
          xlab = "Attendance Rate", ylab = "Prior GPA", zlab = "Final Exam Performance",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = TRUE, fit = fitpoints, 
                      col=ramp.col(col = c("dodgerblue3", "seagreen2"), 
                                   n = 300, 
                                   alpha=0.9), 
                      border="black"), 
          main = "Effects of Attendance on Final Exam Performance")

# theta = -60, phi = 10
scatter3D(x, y, z, pch = 1, cex = .5, colvar = NULL, col = "red", 
          theta = -60, phi = 10, bty="b",
          xlab = "Attendance Rate", ylab = "Prior GPA", zlab = "Final Exam Performance",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = TRUE, fit = fitpoints, 
                      col=ramp.col(col = c("dodgerblue3", "seagreen2"), 
                                   n = 300, 
                                   alpha=0.9), 
                      border="black"), 
          main = "Effects of Attendance on Final Exam Performance")

#### END: Models with Interaction Terms Example 6.3 Page 193 ####

