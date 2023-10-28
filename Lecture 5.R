library("car")
library("broom")
library("broom.mixed")
library("haven")
library("nlme")
library("MASS")
library("matrixStats")
library("tidyverse")
library("loo")
library("brms")
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
library("rcartocolor")
library("patchwork")
library("wooldridge")


# Plots for intro to Information Criteria
# Source: https://mc-stan.org/rstanarm/articles/continuous.html
library(rstanarm)
data(kidiq)
post1 <- stan_glm(kid_score ~ mom_hs, data = kidiq,
                  family = gaussian(link = "identity"),
                  seed = 12345)
post2 <- update(post1, formula = . ~ mom_iq)
post3 <- update(post1, formula = . ~ mom_hs + mom_iq)
(post4 <- update(post1, formula = . ~ mom_hs * mom_iq))

base <- ggplot(kidiq, aes(x = mom_hs, y = kid_score)) +
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) +
  scale_x_continuous(breaks = c(0,1), labels = c("No HS", "HS")) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))

base + geom_abline(intercept = coef(post1)[1], slope = coef(post1)[2],
                   color = "skyblue4", size = 1)

draws <- as.data.frame(post1)
colnames(draws)[1:2] <- c("a", "b")

base +
  geom_abline(data = draws, aes(intercept = a, slope = b),
              color = "skyblue", size = 0.2, alpha = 0.25) +
  geom_abline(intercept = coef(post1)[1], slope = coef(post1)[2],
              color = "skyblue4", size = 1)

draws <- as.data.frame(as.matrix(post2))
colnames(draws)[1:2] <- c("a", "b")
ggplot(kidiq, aes(x = mom_iq, y = kid_score)) +
  geom_point(size = 1) +
  geom_abline(data = draws, aes(intercept = a, slope = b),
              color = "skyblue", size = 0.2, alpha = 0.25) +
  geom_abline(intercept = coef(post2)[1], slope = coef(post2)[2],
              color = "skyblue4", size = 1) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))

reg0 <- function(x, ests) cbind(1, 0, x) %*% ests
reg1 <- function(x, ests) cbind(1, 1, x) %*% ests

args <- list(ests = coef(post3))
kidiq$clr <- factor(kidiq$mom_hs, labels = c("No HS", "HS"))
lgnd <- guide_legend(title = NULL)
base2 <- ggplot(kidiq, aes(x = mom_iq, fill = relevel(clr, ref = "HS"))) +
  geom_point(aes(y = kid_score), shape = 21, stroke = .2, size = 1) +
  guides(color = lgnd, fill = lgnd) +
  theme(legend.position = "right") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))

base2 +
  stat_function(fun = reg0, args = args, aes(color = "No HS"), size = 1.5) +
  stat_function(fun = reg1, args = args, aes(color = "HS"), size = 1.5)

reg0 <- function(x, ests) cbind(1, 0, x, 0 * x) %*% ests
reg1 <- function(x, ests) cbind(1, 1, x, 1 * x) %*% ests
args <- list(ests = coef(post4))
base2 +
  stat_function(fun = reg0, args = args, aes(color = "No HS"), size = 1.5) +
  stat_function(fun = reg1, args = args, aes(color = "HS"), size = 1.5)




# Information entropy
# SR pg. 206
# assume true probs of rain and shine
p <- c( 0.3 , 0.7 )
round( -sum( p*log(p) ) , 2 ) # information entropy

# assume Abu Dhabi probs of rain and shine
p <- c( 0.01 , 0.99 )
round( -sum( p*log(p) ) , 2 ) # information entropy

# assume probs of sun, rain, and snow
p <- c( 0.7 , 0.15 , 0.15 )
round( -sum( p*log(p) ) , 2 ) # information entropy




# Divergence (KL divergence)
# SR pg. 207
0.3*log(0.3/0.01) + 0.7*log(0.7/0.99)

0.3*log(0.3/0.3) + 0.7*log(0.7/0.7)

0.3*log(0.3/0.75) + 0.7*log(0.7/0.25)

# calculate divergence of q from p
q <- seq(from=0.01, to=0.99, by=0.01)
p <- 0.3

n.pairs <- length(q)
d.weather <- rep(NA, n.pairs)
for (i in 1:length(q)) d.weather[i] <- p * log( p / q[i] ) + (1-p) * log( (1-p) / (1-q[i]) )
d.weather

# plot divergence
plot(d.weather ~ q , type="l" , lwd=4 , xlab="q" , ylab="Divergence of q from p" , col="skyblue")
abline(v = 0.3 , col="darkgrey" , lwd=2 , lty=2)
text(0.33 , 1.5 , "q = p")




# Log probability
# probabilities range from [0, 1] so
# log-probabilities range [-infinity, 0]
ps <- seq(from=0.001, to=0.999, by=0.001)
log.ps <- log(ps)
plot(log.ps ~ ps , type="l" , lwd=4 , xlab="probability (p)" , ylab="log-probability" , col="skyblue")




# Deviance from a classical GLM model
# Fit classical logistic regression
wells_fit <- glm(switch ~ dist, family = binomial(link = "logit"), data = wells)
wells_fit

# Log likelihood
logLik(wells_fit)

# calculate the log-probability of each observation "by hand"
y <- wells$switch
yhat <- wells_fit$fitted.values
# calculate log likelihood of each observation
log_probs <- ( log(yhat) * y ) + ( log(1 - yhat) * (1 - y) )
# calculate log likelihood of model
sum( log_probs )


# Deviance (-2 * Log likelihood)
-2 * logLik(wells_fit)

wells_fit$deviance


# AIC (deviance + 2 * number of coefficients)
( -2 * logLik(wells_fit) ) + ( 2 * length(coef(wells_fit)) )

wells_fit$aic




# Deviance from a classical linear model
# Fit classical linear regression
cars <- datasets::cars
cars_fit <- lm(dist ~ speed, data = cars)

( res <- summary(cars_fit) )

res$sigma

broom::tidy(cars_fit)
broom::glance(cars_fit)
broom::augment(cars_fit)

# Log likelihood
logLik(cars_fit)

# calculate the log-probability of each observation "by hand"
a <- cars_fit$coefficients[1]
b <- cars_fit$coefficients[2]
logprobs <- vector(mode = "numeric", length = length(cars$dist))
for (n in 1:length(logprobs)) {
  mu <- a + b*cars$speed[n]
  logprobs[n] <- dnorm( cars$dist[n] , mu , res$sigma , log=TRUE )
}
# log likelihood of each observation
logprobs
# calculate log likelihood of model
sum(logprobs)


# Deviance (-2 * Log likelihood)
(-2) * logLik(cars_fit)


# AIC (deviance + 2 * number of coefficients)
( -2 * logLik(cars_fit) ) + ( 2 * length(coef(cars_fit)) )




# rstanarm version
cars_fitb <- stan_glm(dist ~ speed, data = cars, refresh=0)
summary(cars_fitb)

# compute in-sample log likelihood / lppd
# https://mc-stan.org/rstanarm/reference/log_lik.stanreg.html
# return an 𝑆 by 𝑁 matrix, where 
# 𝑆 is the size of the posterior sample 
# and 𝑁 is the number of data points
ll <- log_lik(cars_fitb)
dim(ll)

# compute in-sample log score (sum over all 50 observations)
sum(colMeans(ll))

# Deviance (-2 * Log likelihood)
(-2) * sum(colMeans(ll))




# Log-pointwise predictive density (lppd)
## SR code 7.1
sppnames <- c( "afarensis","africanus","habilis","boisei",
               "rudolfensis","ergaster","sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

## R code 7.2
d$mass_std <- (d$mass - mean(d$mass))/sd(d$mass)
d$brain_std <- d$brain / max(d$brain)

## R code 7.3
m7.1 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b*mass_std,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d )
precis( m7.1 )
exp(-1.71)

# calculate the lppd
set.seed(1)

lppd( m7.1 , n=1e4 )

sum( lppd( m7.1 , n=1e4 ) )




# calculate lppd "by hand"
# SR pg. 211
## R code 7.14
set.seed(1)

# calculate the log-probability of each observation
# return a matrix with a row for each sample
# and a column for each observation
logprob <- rethinking::sim( m7.1 , ll=TRUE , n=1e4 )
head( logprob )

# log_sum_exp computes the log of the sum of exponentiated values
# it takes all the log-probabilities for a given observation,
# exponentiates each, sums them, then takes the log
# then, subtract the log of the number of samples, which is
# the same as dividing the sum by the number of samples
n <- ncol(logprob)
ns <- nrow(logprob)
f <- function( i ) log_sum_exp( logprob[,i] ) - log(ns)
( lppds <- sapply( 1:n , f ) )

lppd <- sum( lppds )
round(lppd, 1)




### LPPD BY HAND ###
# https://cran.r-project.org/web/packages/loo/vignettes/loo2-non-factorized.html
library("loo")
posterior <- extract.samples( m7.1 )
y <- d$brain_std
N <- length(y)
S <- nrow(posterior)
loglik <- yloo <- sdloo <- matrix(nrow = S, ncol = N)

# roll your own sim
# SR pg. 110
# calculate the log-probability of each observation
# return a matrix with a row for each sample
# and a column for each observation
set.seed(1)
for (s in 1:S) {
  p <- posterior[s, ]
  yloo[s, ] <- p$a + p$b * d$mass_std
  sdloo[s, ] <- exp(p$log_sigma)
  loglik[s, ] <- dnorm(y, yloo[s, ], sdloo[s, ], log = TRUE)
}

head(loglik)
colMeans(loglik)

# use loo for psis smoothing
log_ratios <- -loglik
psis_result <- psis(log_ratios)
plot(psis_result, label_points = TRUE)


# log_sum_exp computes the log of the sum of exponentiated values
# it takes all the log-probabilities for a given observation,
# exponentiates each, sums them, then takes the log
# then, subtract the log of the number of samples, which is
# the same as dividing the sum by the number of samples
n.by_hand <- ncol(loglik)
ns.by_hand <- nrow(loglik)
f.by_hand <- function( i ) log_sum_exp( loglik[,i] ) - log(ns.by_hand)
( lppd.by_hand <- sapply( 1:n.by_hand , f.by_hand ) )


# log_mean_exp
# https://cran.r-project.org/web/packages/loo/vignettes/loo2-non-factorized.html
log_mean_exp <- function(x) {
  # more stable than log(mean(exp(x)))
  max_x <- max(x)
  max_x + log(sum(exp(x - max_x))) - log(length(x))
}

( exact_elpds <- apply(loglik , 2 , log_mean_exp) )

exact_elpd <- sum( exact_elpds )
round(exact_elpd, 1)




# rstanarm version
prior_summary( stan_glm(brain_std ~ mass_std, data = d , chains=2 , iter = 1e4 ) )
m7.1_rstanarm <- stan_glm(brain_std ~ mass_std, data = d , chains=2 , iter = 1e4 )  # adapt_delta = 0.999
summary( m7.1_rstanarm, digits=2 )

posterior <- as.data.frame( m7.1_rstanarm )
y <- d$brain_std
N <- length(y)
S <- nrow(posterior)
loglik <- yloo <- sdloo <- matrix(nrow = S, ncol = N)

# roll your own sim
# SR pg. 110 and pg. 222
# calculate the log-probability of each observation
# return a matrix with a row for each sample
# and a column for each observation
set.seed(1)
for (s in 1:S) {
  p <- posterior[s, ]
  yloo[s, ] <- p[,"(Intercept)"] + p[,"mass_std"] * d$mass_std
  sdloo[s, ] <- p[,"sigma"]
  loglik[s, ] <- dnorm(y, yloo[s, ], sdloo[s, ], log = TRUE)
}

head(loglik)
colMeans(loglik)

# log_sum_exp computes the log of the sum of exponentiated values
# it takes all the log-probabilities for a given observation,
# exponentiates each, sums them, then takes the log
# then, subtract the log of the number of samples, which is
# the same as dividing the sum by the number of samples
n.rstanarm <- ncol(loglik)
ns.rstanarm <- nrow(loglik)
f.rstanarm <- function( i ) log_sum_exp( loglik[,i] ) - log(ns.rstanarm)
( lppd.rstanarm <- sapply( 1:n.rstanarm , f.rstanarm ) )

# log_mean_exp
# https://cran.r-project.org/web/packages/loo/vignettes/loo2-non-factorized.html
log_mean_exp <- function(x) {
  # more stable than log(mean(exp(x)))
  max_x <- max(x)
  max_x + log(sum(exp(x - max_x))) - log(length(x))
}

( exact_elpds <- apply(loglik , 2 , log_mean_exp) )

exact_elpd <- sum( exact_elpds )
round(exact_elpd, 1)




### FOR SCREENSHOT ###
# extract posterior draws of parameters
# and set up constant variables
posterior <- extract.samples( m7.1 )
y <- d$brain_std
N <- length(y)
S <- nrow(posterior)
loglik <- yloo <- sdloo <- matrix(nrow = S, ncol = N)

# calculate the log-probability of each observation
# return a matrix with a row for each sample
# and a column for each observation
set.seed(1)
for (s in 1:S) {
  p <- posterior[s, ]
  yloo[s, ] <- p$a + p$b * d$mass_std
  sdloo[s, ] <- exp(p$log_sigma)
  loglik[s, ] <- dnorm(y, yloo[s, ], sdloo[s, ], log = TRUE)
}

# calculate the total log-probability score for the model and data
# larger values are better, because that indicates larger average accuracy
log_mean_exp <- function(x) {
  # more stable than log(mean(exp(x)))
  max_x <- max(x)
  max_x + log(sum(exp(x - max_x))) - log(length(x))
}

( lppds <- apply(loglik , 2 , log_mean_exp) )

sum( lppds )
### END FOR SCREENSHOT ###




# Deviance in and out of sample
# SR pg. 212

# SR code 7.1
sppnames <- c( "afarensis","africanus","habilis","boisei",
               "rudolfensis","ergaster","sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

# SR code 7.2
d$mass_std <- (d$mass - mean(d$mass))/sd(d$mass)
d$brain_std <- d$brain / max(d$brain)

# SR code 7.3
m7.1 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b*mass_std,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d )

# SR code 7.7
m7.2 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d , start=list(b=rep(0,2)) )

# SR code 7.8
m7.3 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 +
      b[3]*mass_std^3,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d , start=list(b=rep(0,3)) )

m7.4 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 +
      b[3]*mass_std^3 + b[4]*mass_std^4,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d , start=list(b=rep(0,4)) )

m7.5 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 +
      b[3]*mass_std^3 + b[4]*mass_std^4 +
      b[5]*mass_std^5,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d , start=list(b=rep(0,5)) )

# SR code 7.9
m7.6 <- quap(
  alist(
    brain_std ~ dnorm( mu , 0.001 ),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 +
      b[3]*mass_std^3 + b[4]*mass_std^4 +
      b[5]*mass_std^5 + b[6]*mass_std^6,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 )
  ), data=d , start=list(b=rep(0,6)) )


## R code 7.10
layout(matrix(c(1,2,3,4,5,6),3,2, byrow = TRUE))

post <- extract.samples(m7.1)
mass_seq <- seq( from=min(d$mass_std) , to=max(d$mass_std) , length.out=100 )
l <- link( m7.1 , data=list( mass_std=mass_seq ) )
mu <- apply( l , 2 , mean )
ci <- apply( l , 2 , PI )
plot( brain_std ~ mass_std , data=d )
lines( mass_seq , mu )
shade( ci , mass_seq )

post <- extract.samples(m7.2)
mass_seq <- seq( from=min(d$mass_std) , to=max(d$mass_std) , length.out=100 )
l <- link( m7.2 , data=list( mass_std=mass_seq ) )
mu <- apply( l , 2 , mean )
ci <- apply( l , 2 , PI )
plot( brain_std ~ mass_std , data=d )
lines( mass_seq , mu )
shade( ci , mass_seq )

post <- extract.samples(m7.3)
mass_seq <- seq( from=min(d$mass_std) , to=max(d$mass_std) , length.out=100 )
l <- link( m7.3 , data=list( mass_std=mass_seq ) )
mu <- apply( l , 2 , mean )
ci <- apply( l , 2 , PI )
plot( brain_std ~ mass_std , data=d )
lines( mass_seq , mu )
shade( ci , mass_seq )

post <- extract.samples(m7.4)
mass_seq <- seq( from=min(d$mass_std) , to=max(d$mass_std) , length.out=100 )
l <- link( m7.4 , data=list( mass_std=mass_seq ) )
mu <- apply( l , 2 , mean )
ci <- apply( l , 2 , PI )
plot( brain_std ~ mass_std , data=d )
lines( mass_seq , mu )
shade( ci , mass_seq )

post <- extract.samples(m7.5)
mass_seq <- seq( from=min(d$mass_std) , to=max(d$mass_std) , length.out=100 )
l <- link( m7.5 , data=list( mass_std=mass_seq ) )
mu <- apply( l , 2 , mean )
ci <- apply( l , 2 , PI )
plot( brain_std ~ mass_std , data=d )
lines( mass_seq , mu )
shade( ci , mass_seq )

post <- extract.samples(m7.6)
mass_seq <- seq( from=min(d$mass_std) , to=max(d$mass_std) , length.out=100 )
l <- link( m7.6 , data=list( mass_std=mass_seq ) )
mu <- apply( l , 2 , mean )
ci <- apply( l , 2 , PI )
plot( brain_std ~ mass_std , data=d )
lines( mass_seq , mu )
shade( ci , mass_seq )


# SR code 7.15
set.seed(1)
sapply( list(m7.1,m7.2,m7.3,m7.4,m7.5,m7.6) , function(m) sum(lppd(m)) )


# Score the right data (out-of-sample)

### WARNING: TAKES A FEW HOURS TO RUN ###
# SR code 7.16
N <- 20
kseq <- 1:5
dev <- sapply( kseq , function(k) {
  print(k);
  #r <- replicate( 1e4 , sim_train_test( N=N, k=k ) );
  r <- mcreplicate( 1e4 , sim_train_test( N=N, k=k ) , mc.cores=4 )
  c( mean(r[1,]) , mean(r[2,]) , sd(r[1,]) , sd(r[2,]) )
} )
### END: TAKES A FEW HOURS TO RUN ###

# SR code 7.17
# r <- mcreplicate( 1e4 , sim_train_test( N=N, k=k ) , mc.cores=4 )

# SR code 7.18
plot( 1:5 , dev[1,] , ylim=c( min(dev[1:2,])-5 , max(dev[1:2,])+10 ) ,
      xlim=c(1,5.1) , xlab="number of parameters" , ylab="deviance" ,
      pch=16 , col=rangi2 )
mtext( concat( "N = ",N ) )
points( (1:5)+0.1 , dev[2,] )
for ( i in kseq ) {
  pts_in <- dev[1,i] + c(-1,+1)*dev[3,i]
  pts_out <- dev[2,i] + c(-1,+1)*dev[4,i]
  lines( c(i,i) , pts_in , col=rangi2 )
  lines( c(i,i)+0.1 , pts_out )
}


# Regularizing priors (Normal distributions)
# Plot SR Fig 7.7 pg. 215
par(mfrow=c(1,1))
x <- seq(from=-3.1, to=3.1, length.out=1000)
n1 <- dnorm(x, mean=0, sd=1)
n0.5 <- dnorm(x, mean=0, sd=0.5)
n0.2 <- dnorm(x, mean=0, sd=0.2)
plot(x, n0.2, type = 'l', lty=1, lwd=2, xlab="parameter value", ylab="Density")
lines(x, n0.5, lty=1)
lines(x, n1, lty=2)


# Regularizing priors and out-of-sample deviance
### WARNING: EACH DEV TAKES A FEW MINUTES TO RUN ###
# SR pg. 216
N <- 20
kseq <- 1:5
dev1 <- sapply( kseq , function(k) {
  print(k);
  #r <- replicate( 1e4 , sim_train_test( N=N, k=k ) );
  r <- mcreplicate( 1e3 , sim_train_test( N=N, k=k, b_sigma=1 ) , mc.cores=4 )
  c( mean(r[1,]) , mean(r[2,]) , sd(r[1,]) , sd(r[2,]) )
} )
dev0.5 <- sapply( kseq , function(k) {
  print(k);
  #r <- replicate( 1e4 , sim_train_test( N=N, k=k ) );
  r <- mcreplicate( 1e3 , sim_train_test( N=N, k=k, b_sigma=0.5 ) , mc.cores=4 )
  c( mean(r[1,]) , mean(r[2,]) , sd(r[1,]) , sd(r[2,]) )
} )
dev0.2 <- sapply( kseq , function(k) {
  print(k);
  #r <- replicate( 1e4 , sim_train_test( N=N, k=k ) );
  r <- mcreplicate( 1e3 , sim_train_test( N=N, k=k, b_sigma=0.2 ) , mc.cores=4 )
  c( mean(r[1,]) , mean(r[2,]) , sd(r[1,]) , sd(r[2,]) )
} )
### END: EACH DEV TAKES A FEW MINUTES TO RUN ###

( devs <- rbind( dev1[1:2,], dev0.5[1:2,], dev0.2[1:2,] ) )

prior.seq <- seq(1, 5, by = 2)

plot( 1:5 , devs[1,] , ylim=c(48,60) , # c( min(devs[,])-5 , max(devs[,])+10 )
      xlim=c(1,5.1) , xlab="number of parameters" , ylab="deviance" ,
      pch=16 , col=rangi2 )
mtext( concat( "N = ",N ) )
points( (1:5) , devs[2,] )
for ( j in prior.seq ) {
    pts_in <- devs[j,]
    pts_out <- devs[j+1,]
    if (j == 1) { 
      lines( 1:5 , pts_in , type="l" , lty=2 , col=rangi2 )
      lines( 1:5 , pts_out , type="l" , lty=2 )
    } else if (j == 3) {
      lines( 1:5 , pts_in , type="l" , lty=1 , col=rangi2 )
      lines( 1:5 , pts_out , type="l" , lty=1 )
    } else {
      lines( 1:5 , pts_in , type="l" , lty=1 , lwd=2 , col=rangi2 )
      lines( 1:5 , pts_out , type="l" , lty=1 , lwd=2 )
    }
}



# LOO cross validation
# ROS pg. 172
SEED <- 1507

# Simulate fake data
x <- 1:20
n <- length(x)
a <- 0.2
b <- 0.3
sigma <- 1

# set the random seed to get reproducible results
# change the seed to experiment with variation due to random noise
set.seed(2141) 
y <- a + b*x + sigma*rnorm(n)
fake <- data.frame(x, y)


# Fit linear model
fit_all <- stan_glm(y ~ x,      data = fake,       seed=2141, refresh=0)


# Fit linear model without 18th observation
fit_minus_18 <- stan_glm(y ~ x, data = fake[-18,], seed=2141, refresh=0)


# Extract posterior draws
sims <- as.matrix(fit_all)

sims_minus_18 <- as.matrix(fit_minus_18)


# Compute posterior predictive distribution given x=18
condpred <- data.frame( y = seq(0, 9, length.out=100) )

condpred$x <- sapply( condpred$y, FUN = function(y) mean( dnorm(y, sims[,1] + sims[,2]*18, sims[,3])*6 + 18 ) )


# Compute LOO posterior predictive distribution given x=18
condpredloo <- data.frame( y = seq(0, 9, length.out=100) )

condpredloo$x <- sapply( condpredloo$y, FUN = function(y) mean( dnorm(y, sims_minus_18[,1] + sims_minus_18[,2]*18, sims_minus_18[,3])*6 + 18 ) )


# Create a plot with posterior mean and posterior predictive distribution
p1 <- ggplot(fake, aes(x = x, y = y)) +
  geom_point(color = "white", size = 3) +
  geom_point(color = "black", size = 2)

p2 <- p1 +
  geom_abline(
    intercept = mean(sims[, 1]),
    slope = mean(sims[, 2]),
    size = 1,
    color = "black"
  )

p3 <- p2 + 
  geom_path(data=condpred,aes(x=x,y=y), color="black") +
  geom_vline(xintercept=18, linetype=3, color="grey")

# Add LOO mean and LOO predictive distribution when x=18 is left out
p4 <- p3 +
  geom_point(data=fake[18,], color = "grey50", size = 5, shape=1) +
  geom_abline(
    intercept = mean(sims_minus_18[, 1]),
    slope = mean(sims_minus_18[, 2]),
    size = 1,
    color = "grey50",
    linetype=2
  ) +
  geom_path(data=condpredloo,aes(x=x,y=y), color="grey50", linetype=2)

p4


# Compute posterior and LOO residuals
# loo_predict() computes mean of LOO predictive distribution.
fake$residual <- fake$y - fit_all$fitted

fake$looresidual <- fake$y - loo_predict(fit_all)$value


# Plot posterior and LOO residuals
p1 <- ggplot(fake, aes(x = x, y = residual)) +
  geom_point(color = "black", size = 2, shape=16) +
  geom_point(aes(y=looresidual), color = "grey50", size = 2, shape=1) +
  geom_segment(aes(xend=x, y=residual, yend=looresidual)) +
  geom_hline(yintercept=0, linetype=2)

p1


# Standard deviations of posterior and LOO residuals
round( sd(fake$residual) , 2 )

round( sd(fake$looresidual) , 2 )


# Variance of residuals is connected to R^2, which can be defined as 1-var(res)/var(y)
round( 1 - var(fake$residual)/var(y) , 2 )

round( 1 - var(fake$looresidual)/var(y) , 2 )


# Compute log posterior predictive densities
# log_lik returns log(p(yi|θ(s)))
ll_1 <- log_lik(fit_all)


# Compute log(1S∑Ss=1p(yi|θ(s)) in computationally stable way
fake$lpd_post <- matrixStats::colLogSumExps(ll_1) - log(nrow(ll_1))


# Compute log LOO predictive densities
# loo uses fast approximate leave-one-out cross-validation
loo_1 <- loo(fit_all)
fake$lpd_loo <-loo_1$pointwise[,"elpd_loo"]


# Plot posterior and LOO log predictive densities
p1 <- ggplot(fake, aes(x = x, y = lpd_post)) +
  geom_point(color = "black", size = 2, shape=16) +
  geom_point(aes(y=lpd_loo), color = "grey50", size = 2, shape=1) +
  geom_segment(aes(xend=x, y=lpd_post, yend=lpd_loo)) +
  ylab("log predictive density")

p1



# Interpret loo cross validation log score
# ROS pg. 177
fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data = kidiq)

loo_3 <- loo::loo(fit_3)

print(loo_3)



# K-fold cross validation
# ROS pg. 179
# Set random seed for reproducability
SEED <- 1754

# Generate fake data
# 60×30 matrix representing 30 predictors that are 
# random but not independent; rather, we draw them from 
# a multivariate normal distribution with correlations 0.8
set.seed(SEED)

n <- 60

k <- 30

rho <- 0.8

Sigma <- rho*array(1, c(k,k)) + (1-rho)*diag(k)

X <- mvrnorm(n, rep(0,k), Sigma)

b <- c(c(-1, 1, 2), rep(0,k-3))

y <- X %*% b + rnorm(n)*2

fake <- data.frame(X, y)


# Weakly informative prior
fit_1 <- stan_glm(y ~ ., prior=normal(0, 10, autoscale=FALSE),
                  data=fake, seed=SEED, refresh=0)

( loo_1 <- loo(fit_1) )

plot(loo_1)

plot(loo_1, label_points = TRUE)


# In this case, Pareto smoothed importance sampling LOO fails, 
# but the diagnostic recognizes this with many high Pareto k values. 
# We can run slower, but more robust K-fold-CV
( kfold_1 <- kfold(fit_1, K=10) )


# An alternative weakly informative prior
# The regularized horseshoe prior hs() is weakly informative, 
# stating that it is likely that only small number of 
# predictors are relevant, but we don’t know which ones.

k0 <- 2 # prior guess for the number of relevant variables

tau0 <- k0/(k-k0) * 1/sqrt(n)

hs_prior <- hs(df=1, global_df=1, global_scale=tau0, slab_scale=3, slab_df=7)

fit_2 <- stan_glm(y ~ ., prior=hs_prior, data=fake, seed=SEED,
                  control=list(adapt_delta=0.995), refresh=200)

( loo_2 <- loo(fit_2) )

( loo_2 <- loo(fit_2, k_threshold = 0.7) )


# PSIS-LOO performs better now, but there is still one bad diagnostic value, 
# and thus we run again slower, but more robust K-fold-CV
( kfold_2 <- kfold(fit_2, K=10) )


# Comparison of models
# As PSIS-LOO fails, PSIS-LOO comparison underestimates the difference between the models. 
# The Pareto k diagnostic correctly identified the problem, and more robust K-fold-CV 
# shows that by using a better prior we can get better predictions
loo_compare(loo_1, loo_2)

loo_compare(kfold_1, kfold_2)

loo_compare(waic(fit_1), waic(fit_2))




# Cross validation with the `caret` package
# http://topepo.github.io/caret/index.html
# http://topepo.github.io/caret/model-training-and-tuning.html#between-models
library(caret)

# K-fold cross validation
fitControl <- trainControl(method = "cv", number = 10)

# Leave-one-out (LOOCV) cross validation
# fitControl <- trainControl(method = "LOOCV")


# 10-fold cross validation for model 1
set.seed(123)
m1_cv <- train(kid_score ~ 1 + mom_hs, 
               data = kidiq, 
               method = "lm", 
               trControl = fitControl)
m1_cv
summary(m1_cv)

# 10-fold cross validation for model 2
set.seed(123)
m2_cv <- train(kid_score ~ 1 + mom_hs + mom_iq, 
               data = kidiq, 
               method = "lm", 
               trControl = fitControl)
m2_cv
summary(m2_cv)

# 10-fold cross validation for model 3
set.seed(123)
m3_cv <- train(kid_score ~ 1 + mom_hs + mom_iq + mom_hs:mom_iq, 
               data = kidiq, 
               method = "lm", 
               trControl = fitControl)
m3_cv
summary(m3_cv)

# collect and view the resampling results
resamps <- resamples(list(m1 = m1_cv,
                          m2 = m2_cv,
                          m3 = m3_cv))
resamps

summary(resamps)

# compute the differences between the model performances 
# then use a simple t-test to evaluate the null hypothesis 
# that there is no difference between the models
difValues <- diff(resamps)
difValues

summary(difValues)

# plot it
bwplot(difValues, layout = c(3, 1))




# Overthinking box: WAIC calculations pg. 222

# classical linear regression to compare to example below
summary( lm(dist ~ speed, data = datasets::cars) )
# calculate log likelihood of model to compare to `sum(lppd)` below
logLik( lm(dist ~ speed, data = datasets::cars) )


# To see how the WAIC calculations work, consider the following regression:
# data(cars)
cars <- datasets::cars
m <- quap(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dexp(1)
  ) , data=cars )
set.seed(94)
post <- extract.samples(m, n=1000)

# We'll need the log-likelihood of each observation i 
# at each sample s from the posterior:
n_samples <- 1000
logprob <- sapply( 1:n_samples ,
                   function(s) {
                     mu <- post$a[s] + post$b[s]*cars$speed
                     dnorm( cars$dist , mu , post$sigma[s] , log=TRUE )
                   } )

# You end up with a 50-by-1000 matrix of log-likelihoods, with observations
# in rows and samples in columns

# Now to compute lppd, the Bayesian deviance, we average the samples 
# in each row, take the log, and add all of the logs together
# To do this with precision, you need to do the averaging on the log scale
n_cases <- nrow(cars)
lppd <- sapply( 1:n_cases , function(i) log_sum_exp(logprob[i,]) - log(n_samples) )
# Typing `sum(lppd)` will give you lppd.
sum(lppd)

# Now for the penalty term, pWAIC, we compute the variance across samples 
# for each observation, then add these together:
pWAIC <- sapply( 1:n_cases , function(i) var(logprob[i,]) )
# Typing `sum(pWAIC)` will give you the penalty term
sum(pWAIC)

# Finally, the following computes WAIC:
-2*( sum(lppd) - sum(pWAIC) )

# Compute the standard error of WAIC by computing the square root
# of the number of cases multiplied by the variance over the 
# individual observation terms in WAIC:
waic_vec <- -2*( lppd - pWAIC )
sqrt( n_cases*var(waic_vec) )



# kidiq example

# classical linear regression to compare to example below
summary( lm(kid_score ~ mom_hs + mom_iq, data=kidiq) )
# calculate log likelihood of model to compare to `sum(lppd)` below
logLik( lm(kid_score ~ mom_hs + mom_iq, data=kidiq) )


# calculate the log-probability of each observation
# compute sum of log posterior predictive densities
fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq)

posterior <- as.data.frame( fit_3 )
y <- kidiq$kid_score
N <- length(y)
S <- nrow(posterior)
loglik <- yloo <- sdloo <- matrix(nrow = S, ncol = N)

# roll your own sim
# SR pg. 110 and pg. 222
# calculate the log-probability of each observation
# return a matrix with a row for each sample
# and a column for each observation
set.seed(1)
for (s in 1:S) {
  p <- posterior[s, ]
  yloo[s, ] <- p[,"(Intercept)"] + p[,"mom_hs"] * kidiq$mom_hs + p[,"mom_iq"] * kidiq$mom_iq
  sdloo[s, ] <- p[,"sigma"]
  loglik[s, ] <- dnorm(y, yloo[s, ], sdloo[s, ], log = TRUE)
}

head(loglik)
colMeans(loglik)

# log_sum_exp computes the log of the sum of exponentiated values
# it takes all the log-probabilities for a given observation,
# exponentiates each, sums them, then takes the log
# then, subtract the log of the number of samples, which is
# the same as dividing the sum by the number of samples
n.rstanarm <- ncol(loglik)
ns.rstanarm <- nrow(loglik)
f.rstanarm <- function( i ) log_sum_exp( loglik[,i] ) - log(ns.rstanarm)
( lppd.rstanarm <- sapply( 1:n.rstanarm , f.rstanarm ) )

# log_mean_exp
# https://cran.r-project.org/web/packages/loo/vignettes/loo2-non-factorized.html
log_mean_exp <- function(x) {
  # more stable than log(mean(exp(x)))
  max_x <- max(x)
  max_x + log(sum(exp(x - max_x))) - log(length(x))
}

( exact_elpds <- apply(loglik , 2 , log_mean_exp) )

( exact_elpd <- sum( exact_elpds ) )


# Compute sum of log posterior predictive densities
# log_lik returns log(p(yi|θ(s)))
( ll_3 <- log_lik(fit_3) )


# compute log(1S∑Ss=1p(yi|θ(s)) in computationally stable way
( sum( matrixStats::colLogSumExps(ll_3) - log( nrow(ll_3) ) ) )


# Compute log LOO predictive densities
# loo uses fast approximate leave-one-out cross-validation
loo_3 <- loo::loo(fit_3)
loo_3$estimate["elpd_loo",1]

# More information on LOO elpd estimate including standard errors
loo_3



# Example 1: Demonstration adding pure noise predictors to a model
# ROS pg. 176
fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq, 
                  seed=SEED, refresh = 0)
print(fit_3)
summary(fit_3)


# Compute R^2 and LOO-R^2 manually
respost <- kidiq$kid_score - fit_3$fitted

resloo <- kidiq$kid_score - loo_predict(fit_3)$value

round( R2 <- 1 - var(respost)/var(kidiq$kid_score) , 2 )

round( R2loo <- 1 - var(resloo)/var(kidiq$kid_score) , 2 )


# Add five pure noise predictors to the data
set.seed(SEED)
n <- nrow(kidiq)
kidiqr <- kidiq
kidiqr$noise <- array(rnorm(5*n), c(n,5))


# Linear regression with additional noise predictors
fit_3n <- stan_glm(kid_score ~ mom_hs + mom_iq + noise, data=kidiqr,
                   seed=SEED, refresh = 0)
print(fit_3n)
summary(fit_3n)


# Compute R^2 and LOO-R^2 manually
respostn <- kidiq$kid_score - fit_3n$fitted

resloon <- kidiq$kid_score - loo_predict(fit_3n)$value

round( R2n <- 1 - var(respostn)/var(kidiq$kid_score) , 2 )

round( R2loon <- 1 - var(resloon)/var(kidiq$kid_score) , 2 )


# Alternative more informative regularized horseshoe prior
fit_3rhs <- stan_glm(kid_score ~ mom_hs + mom_iq, prior=hs(), data=kidiq,
                     seed=SEED, refresh = 0)

fit_3rhsn <- stan_glm(kid_score ~ mom_hs + mom_iq + noise, prior=hs(),
                      data=kidiqr, seed=SEED, refresh = 0)

round( median( bayes_R2(fit_3rhs) ) , 2 )

round( median( loo_R2(fit_3rhs) ) , 2 )

round( median( bayes_R2(fit_3rhsn) ) , 2 )

round( median( loo_R2(fit_3rhsn) ) , 2 )


# Compute sum of log posterior predictive densities
# log_lik returns log(p(yi|θ(s)))
ll_3 <- log_lik(fit_3)

ll_3n <- log_lik(fit_3n)


# compute log(1S∑Ss=1p(yi|θ(s)) in computationally stable way
round( sum( matrixStats::colLogSumExps(ll_3) - log(nrow(ll_3)) ) , 1 )

round( sum( matrixStats::colLogSumExps(ll_3n) - log(nrow(ll_3n)) ) , 1 )


# Compute log LOO predictive densities
# loo uses fast approximate leave-one-out cross-validation
loo_3 <- loo(fit_3)

loo_3n <- loo(fit_3n)

round( loo_3$estimate["elpd_loo",1] , 1 )

round( loo_3n$estimate["elpd_loo",1] , 1 )


# More information on LOO elpd estimate including standard errors
loo_3

loo_3n


# Example 2: compare various kidiq models
# ROS pg. 177

# Model using only the maternal high school indicator
fit_1 <- stan_glm(kid_score ~ mom_hs, data=kidiq, refresh = 0)

( loo_1 <- loo(fit_1) )


# Compare models using LOO log score (elpd)
loo_compare(loo_3, loo_1) %>% 
  print(simplify = F)


# Compare models using LOO-R^2
# we need to fix the seed to get comparison to work correctly in this case
set.seed(1414)
looR2_1 <- loo_R2(fit_1)

set.seed(1414)
looR2_3 <- loo_R2(fit_3)


round(mean(looR2_1), 2)

round(mean(looR2_3), 2)

round(mean(looR2_3 - looR2_1), 2)

round(  sd(looR2_3 - looR2_1), 2)


# Model with an interaction
fit_4 <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq,
                  data=kidiq, refresh=0)


# Compare models using LOO log score (elpd)
loo_4 <- loo(fit_4)

loo_compare(loo_3, loo_4) %>% 
  print(simplify = F)


# Compare models using LOO-R^2
set.seed(1414)
looR2_4 <- loo_R2(fit_4)

round( mean(looR2_4) , 2 )

round( mean(looR2_4 - looR2_3) , 2 )

round(   sd(looR2_4 - looR2_3) , 2 )



# Example: Poisson vs negative binomial for the roaches dataset
# https://mc-stan.org/loo/articles/loo2-example.html
# the 'roaches' data frame is included with the rstanarm package
data(roaches)
str(roaches)

# rescale to units of hundreds of roaches
roaches$roach1 <- roaches$roach1 / 100


# Fit Poisson model
fit1 <-
  stan_glm(
    formula = y ~ roach1 + treatment + senior,
    offset = log(exposure2),
    data = roaches,
    family = poisson(link = "log"),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_intercept = normal(0, 5, autoscale = TRUE),
    seed = 12345
  )

print(fit1)
summary(fit1)
pp_check(fit1)


# Computing PSIS-LOO and checking diagnostics
# In this case the elpd_loo estimate should not be considered reliable. 
# If we had a well-specified model we would expect the estimated effective 
# number of parameters (p_loo) to be smaller than or similar to the 
# total number of parameters in the model. Here p_loo is almost 300, 
# which is about 70 times the total number of parameters in the model, 
# indicating severe model misspecification
loo1 <- loo(fit1, save_psis = TRUE)
print(loo1)


# Plotting Pareto k diagnostics
plot(loo1)


# Marginal posterior predictive checks
# LOO-PIT values are cumulative probabilities for 𝑦𝑖 
# computed using the LOO marginal predictive distributions 𝑝(𝑦𝑖|𝑦−𝑖). 
# For a good model, the distribution of LOO-PIT values should be uniform

# The excessive number of values close to 0 indicates that 
# the model is under-dispersed compared to the data, and 
# we should consider a model that allows for greater dispersion
yrep <- posterior_predict(fit1)

ppc_loo_pit_overlay(
  y = roaches$y,
  yrep = yrep,
  lw = weights(loo1$psis_object)
)


# Try alternative model with more flexibility
# Here we will try negative binomial regression, 
# which is commonly used for overdispersed count data.
# Unlike the Poisson distribution, the negative binomial distribution 
# allows the conditional mean and variance of 𝑦 to differ
fit2 <- update(fit1, family = neg_binomial_2)

loo2 <- loo(fit2, save_psis = TRUE)
print(loo2)

plot(loo2, label_points = TRUE)


# If there are a small number of problematic 𝑘 values then 
# we can use a feature in rstanarm that lets us refit the model 
# once for each of these problematic observations. Each time the model is refit, 
# one of the observations with a high 𝑘 value is omitted and 
# the LOO calculations are performed exactly for that observation. 
# The results are then recombined with the approximate LOO calculations 
# already carried out for the observations without problematic 𝑘 values
if (any(pareto_k_values(loo2) > 0.7)) {
  loo2 <- loo(fit2, save_psis = TRUE, k_threshold = 0.7)
}

print(loo2)


# For further model checking we again examine the LOO-PIT values
yrep <- posterior_predict(fit2)

ppc_loo_pit_overlay(roaches$y, yrep, lw = weights(loo2$psis_object))


# Comparing the models on expected log predictive density
# We can use the loo_compare function to compare our two models 
# on expected log predictive density (ELPD) for new data:
loo_compare(loo1, loo2) %>% 
  print(simplify = F)
# The difference in ELPD is much larger than several times 
# the estimated standard error of the difference again indicating 
# that the negative-binomial model is expected to have better 
# predictive performance than the Poisson model. However, 
# according to the LOO-PIT checks there is still some misspecification, 
# and a reasonable guess is that a hurdle or zero-inflated model 
# would be an improvement (we leave that for another case study)



# Example 4: High pareto k ... switch to "robust" regression
# SR pg. 230  code 7.32
library(rethinking)

data(WaffleDivorce)

d <- WaffleDivorce
d$A <- rethinking::standardize( d$MedianAgeMarriage )
d$D <- rethinking::standardize( d$Divorce )
d$M <- rethinking::standardize( d$Marriage )

m5.1 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

m5.2 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

m5.3 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

# SR code 7.33
set.seed(24071847)
rethinking::compare( m5.1 , m5.2 , m5.3 , func = PSIS )


# SR code 7.34
set.seed(24071847)
PSIS_m5.3 <- PSIS(m5.3, pointwise=TRUE)

set.seed(24071847)
WAIC_m5.3 <- WAIC(m5.3, pointwise=TRUE)

plot( PSIS_m5.3$k , WAIC_m5.3$penalty , xlab="PSIS Pareto k" ,
      ylab="WAIC penalty" , col=rangi2 , lwd=2 )



# Normal vs Student-t distributions)
# Plot SR Fig 7.11 pg. 233

par(mfrow=c(1,2))

# probability scale
x <- seq(from=-6.1, to=6.1, length.out=1e4)
g <- dnorm(x, mean=0, sd=1)
t <- dt(x, df=2)
plot(x, g, type = 'l', lty=1, lwd=2, xlab="value", ylab="Density")
text(-2, .25, "Gaussian", cex = 1.2, font = 2)
lines(x, t, lty=1, lwd=2, col=rangi2)
text(2, .2, "Student-t", cex = 1.2, font = 2, col = rangi2)

# log-probability scale
x <- seq(from=-6.1, to=6.1, length.out=1e4)
gl <- dnorm(x, mean=0, sd=1, log=TRUE)
tl <- dt(x, df=2, log=TRUE)
plot(x, -gl, type = 'l', lty=1, lwd=2, xlab="value", ylab="minus log Density")
text(-3, 10, "Gaussian", cex = 1.2, font = 2)
lines(x, -tl, lty=1, lwd=2, col=rangi2)
text(4, 3, "Student-t", cex = 1.2, font = 2, col = rangi2)

par(mfrow=c(1,1))



# SR code 7.35
m5.3t <- quap(
  alist(
    D ~ dstudent( 2 , mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )


PSIS(m5.3t)


precis(m5.3)
precis(m5.3t)


set.seed(24071847)
rethinking::compare( m5.3 , m5.3t , func=PSIS )



# rstanarm / brms version
# https://bookdown.org/content/4857/ulysses-compass.html#outliers-and-other-illusions.
b5.1 <- stan_glm(D ~ 1 + A, data=d, refresh=0)
b5.2 <- stan_glm(D ~ 1 + M, data=d, refresh=0)
b5.3 <- stan_glm(D ~ 1 + M + A, data=d, refresh=0)

( loo_b5.1 <- loo(b5.1) )
( loo_b5.2 <- loo(b5.2) )
( loo_b5.3 <- loo(b5.3) )

loo_compare(loo_b5.1, loo_b5.2, loo_b5.3) %>% 
  print(simplify = F)


loo(b5.3) %>% 
  pareto_k_ids(threshold = 0.5)

d %>% 
  slice(13) %>% 
  dplyr::select(Location:Loc)

pareto_k_values(loo(b5.3))[13]

loo_b5.3$diagnostics$pareto_k[13]


# Figure 7.10 SR pg. 232
b5.3 <- brm(D ~ 1 + M + A, family=gaussian, data=d,
            prior = c(prior(normal(0, 0.2), class = Intercept),
                      prior(normal(0, 0.5), class = b),
                      prior(exponential(1), class = sigma)),
            iter = 2000, warmup = 1000, chains = 4, cores = 4,
            seed = 5)
b5.3 <- add_criterion(b5.3, "loo")
b5.3 <- add_criterion(b5.3, "waic")
tibble(pareto_k = b5.3$criteria$loo$diagnostics$pareto_k,
       p_waic   = b5.3$criteria$waic$pointwise[, "p_waic"],
       Loc      = pull(d, Loc)) %>% 
  
  ggplot(aes(x = pareto_k, y = p_waic, color = Loc == "ID")) +
  geom_vline(xintercept = .5, linetype = 2, color = "black", alpha = 1/2) +
  geom_point(aes(shape = Loc == "ID")) +
  geom_text(data = . %>% filter(p_waic > 0.5),
            aes(x = pareto_k - 0.03, label = Loc),
            hjust = 1) +
  scale_color_manual(values = carto_pal(7, "BurgYl")[c(5, 7)]) +
  scale_shape_manual(values = c(1, 19)) +
  labs(subtitle = "Gaussian model (b5.3)") +
  theme(legend.position = "none")


b5.3t <- brm(bf(D ~ 1 + M + A, nu = 2), family = student, data = d, refresh=0)
      
( loo_b5.3t <- loo(b5.3t) )

loo_compare(loo_b5.3, loo_b5.3t) %>% 
  print(simplify = F)

