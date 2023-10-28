library(strengejacke)
library(HSAUR)
library(MASS)
library(Matrix)
library(mvtnorm)
library(arm)
library(rstan)
library(brms)
library(rethinking)
library(rstanarm)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(loo)
library(ggplot2)
library(ggeffects)
library(ggbiplot)
library(grid)
library(gridExtra)
library(ellipse)
library(plot3D)
library(broom)
library(broom.mixed)
library(tidyverse)




# Univariate (aka one-dimensional) Gaussian / normal distribution
x <- seq(from = -5, to = 5, length.out = 1000)  # the interval [-5 5]
f <- dnorm(x)                                   # normal with mean 0 and sd 1
ggplot(data.frame(x=x, y=f), aes(x=x, y=y)) + 
  geom_line() +
  labs(y = "density", title = "Normal(0,1) distribution") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))



# Plot three one-dimensional normal distributions
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



# Bivariate uncorrelated normal(0,1) distribution
n <- 1e3

Mu <- rep(0, 2)
Mu

Sigma <- matrix(c(1,0,0,1), 2, 2)
Sigma

X <- mvrnorm(n = n, mu = Mu, Sigma = Sigma)

plot( X[,1] , X[,2] , col="lightblue" ,
      xlab="dimension 1" , ylab="dimension 2",
      main="MVNormal(mu=[0,0]) , Sigma=[1.0 0.0 0.0 1.0]")

# overlay population distribution
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma,centre=Mu,level=l),col=col.alpha("black",0.2))




# Two bivariate normal distributions based on the 
# code in SR pg. 437

par(mfrow=c(1,2))

#####################################################################

# Plot 1

## R code 14.1
a <- 3.5            # average morning wait time
b <- (-1)           # average difference afternoon wait time
sigma_a <- 1        # std dev in intercepts
sigma_b <- 0.5      # std dev in slopes
rho <- (-0.7)       # correlation between intercepts and slopes

## R code 14.2
Mu <- c( a , b )
Mu

# Option 1: to build variance-covariance matrix
## R code 14.3
# covariance between a and b
cov_ab <- sigma_a*sigma_b*rho
cov_ab

# variance-covariance matrix
Sigma <- matrix( c(sigma_a^2, cov_ab, cov_ab, sigma_b^2) , ncol=2 )
Sigma

# # Option 2: to build variance-covariance matrix
## R code 14.5
sigmas <- c(sigma_a,sigma_b) # standard deviations

Rho <- matrix( c(1,rho,rho,1) , nrow=2 ) # correlation matrix
Rho

# now matrix multiply to get covariance matrix
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)

# Let's simulate some cafes, each with its own intercept and slope,
# drawn from the multivariate Gaussian distribution defined by Mu and Sigma

## R code 14.6
N_cafes <- 500

## R code 14.7
library(MASS)
set.seed(1) # used to replicate example
vary_effects <- mvrnorm( N_cafes , Mu , Sigma )

## R code 14.8
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]

## R code 14.9
plot( a_cafe , b_cafe , col=rangi2 ,
      xlab="dimension 1" , ylab="dimension 2",
      main="MVNormal(mu=[3.5,-1.0]) , Sigma=[1.0 -0.35 -0.35 0.25]")

# overlay population distribution
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma,centre=Mu,level=l),col=col.alpha("black",0.2))


# Plot 2

## R code 14.1
a <- 3.5            # average morning wait time
b <- (-1)           # average difference afternoon wait time
sigma_a <- 1        # std dev in intercepts
sigma_b <- 0.5      # std dev in slopes
rho <- 0.7          # correlation between intercepts and slopes

## R code 14.2
Mu <- c( a , b )
Mu

# Option 1: to build variance-covariance matrix
## R code 14.3
# covariance between a and b
cov_ab <- sigma_a*sigma_b*rho
cov_ab

# variance-covariance matrix
Sigma <- matrix( c(sigma_a^2, cov_ab, cov_ab, sigma_b^2) , ncol=2 )
Sigma

# # Option 2: to build variance-covariance matrix
## R code 14.5
sigmas <- c(sigma_a,sigma_b) # standard deviations

Rho <- matrix( c(1,rho,rho,1) , nrow=2 ) # correlation matrix
Rho

# now matrix multiply to get covariance matrix
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)

# Let's simulate some cafes, each with its own intercept and slope,
# drawn from the multivariate Gaussian distribution defined by Mu and Sigma

## R code 14.6
N_cafes <- 500

## R code 14.7
library(MASS)
set.seed(1) # used to replicate example
vary_effects <- mvrnorm( N_cafes , Mu , Sigma )

## R code 14.8
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]

## R code 14.9
plot( a_cafe , b_cafe , col=rangi2 ,
      xlab="dimension 1" , ylab="dimension 2",
      main="MVNormal(mu=[3.5,-1.0]) , Sigma=[1.0 0.35 0.35 0.25]" )

# overlay population distribution
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma,centre=Mu,level=l),col=col.alpha("black",0.2))

#####################################################################

par(mfrow=c(1,1))




# Plot bivariate normal(0,1) distributions, with
# covariance between the two dimensions
# ranging from -0.9 to 0.9, by 0.1s
r.seq <- seq(from=-0.9, to=0.9, by=0.1)
r.seq
r <- 1

par(mfrow=c(5,4))

n <- 1e3
mu <- rep(0, 2)

for (r in seq_along(r.seq)) {  
  # print(r.seq[r])
  Sigma <- matrix( c(1, r.seq[r], 
                     r.seq[r], 1) , 
                   2 , 2 )
  X <- mvrnorm( n = n , mu = mu , Sigma = Sigma )
  #print( round( cov(X) , 2 ) )
  
  plot(X[,1], X[,2], cex = 0.25,
       xlim = c(-5,5), ylim = c(-5,5), 
       main = paste0("bivariate normal (var/stddev 1, covariance ", r.seq[r], ")"),
       xlab = "dimension 1", ylab = "dimension 2")
}
par(mfrow=c(1,1))



# Plot bivariate normal(0,1) distributions, with
# covariance between the two dimensions
# ranging from -0.9 to 0.9, by 0.2s
r.seq <- seq(from=-0.9, to=0.9, by=0.2)
r.seq
r <- 1

par(mfrow=c(2,5))

n <- 750
mu <- rep(0, 2)

for (r in seq_along(r.seq)) {  
  # print(r.seq[r])
  Sigma <- matrix( c(1, r.seq[r], 
                     r.seq[r], 1) , 
                   2 , 2 )
  X <- mvrnorm( n = n , mu = mu , Sigma = Sigma )
  plot(X[,1], X[,2], cex = 0.35,
       xlim = c(-5,5), ylim = c(-5,5), 
       main = paste0("bivariate normal (var/stddev 1, covariance ", r.seq[r], ")"),
       xlab = "dimension 1", ylab = "dimension 2")
}
par(mfrow=c(1,1))




# Plot bivariate normal(100,2) distributions, with
# covariance between the two dimensions
# ranging from -0.9 to 0.9, by 0.2s
r.seq <- seq(from=-0.9, to=0.9, by=0.2)
r.seq
r <- 1

par(mfrow=c(2,5))

n <- 750
mu <- rep(100, 2)

for (r in seq_along(r.seq)) {  
  # print(r.seq[r])
  Sigma <- matrix( c(2, r.seq[r], 
                     r.seq[r], 2) , 
                   2 , 2 )
  X <- mvrnorm( n = n , mu = mu , Sigma = Sigma )
  plot(X[,1], X[,2], cex = 0.35,
       xlim = c(94,106), ylim = c(94,106), 
       main = paste0("bivariate normal(100, 2) with covariance: ", r.seq[r], ")"),
       xlab = "dimension 1", ylab = "dimension 2")
}
par(mfrow=c(1,1))




# ROS pg. 179
# Generate fake data
# 2000×5 matrix representing 5 predictors that are random but not independent; 
# rather, we draw them from a multivariate normal distribution with correlations 0.8
set.seed(123)

n <- 2000

k <- 5

rho <- 0.8

Sigma <- rho*array(1, c(k,k)) + (1-rho)*diag(k)
Sigma

X <- mvrnorm(n=n, mu=rep(0,k), Sigma=Sigma)
dim(X)
head(X)


# Another way to build the variance-covariance matrix
# SR code 14.5
sigmas <- c(1, 1, 1, 1, 1) # standard deviations

Rho <- rho*array(1, c(k,k)) + (1-rho)*diag(k) # correlation matrix
Rho

# now matrix multiply to get covariance matrix
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)
Sigma


# Plot each dimension separately to show 
# that each dimension is Gaussian
par(mfrow=c(1,5))
dens(X[,1])
dens(X[,2])
dens(X[,3])
dens(X[,4])
dens(X[,5])
par(mfrow=c(1,1))


# Plot dimension 1 against each of the 4 remaining dimensions
# to show that they are bivariate Gaussian with correlation 0.8
par(mfrow=c(1,4))

plot(X[,1], X[,2], cex = 0.35,
     #xlim = c(94,106), ylim = c(94,106), 
     #main = paste0("bivariate normal(100, 2) with covariance: ", r.seq[r], ")"),
     xlab = "dimension 1", ylab = "dimension 2")
# overlay population distribution
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma[,c(1,2)],centre=X[,c(1,2)],level=l),col=col.alpha("black",0.2))

plot(X[,1], X[,3], cex = 0.35,
     #xlim = c(94,106), ylim = c(94,106), 
     #main = paste0("bivariate normal(100, 2) with covariance: ", r.seq[r], ")"),
     xlab = "dimension 1", ylab = "dimension 3")
# overlay population distribution
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma[,c(1,3)],centre=X[,c(1,3)],level=l),col=col.alpha("black",0.2))

plot(X[,1], X[,4], cex = 0.35,
     #xlim = c(94,106), ylim = c(94,106), 
     #main = paste0("bivariate normal(100, 2) with covariance: ", r.seq[r], ")"),
     xlab = "dimension 1", ylab = "dimension 4")
# overlay population distribution
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma[,c(1,4)],centre=X[,c(1,4)],level=l),col=col.alpha("black",0.2))

plot(X[,1], X[,5], cex = 0.35,
     #xlim = c(94,106), ylim = c(94,106), 
     #main = paste0("bivariate normal(100, 2) with covariance: ", r.seq[r], ")"),
     xlab = "dimension 1", ylab = "dimension 5")
# overlay population distribution
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma[,c(1,5)],centre=X[,c(1,5)],level=l),col=col.alpha("black",0.2))

par(mfrow=c(1,1))




# SR pg. 440
# variance, covariance, correlation
sigma_a <- 1     # std dev in intercepts
sigma_b <- 0.5   # std dev in slopes
rho <- 0.7       # correlation between intercepts and slopes

# covariance between a and b
cov_ab <- sigma_a * sigma_b * rho
cov_ab

# variance of a
var_a <- sigma_a * sigma_a
var_a

# variance of b
var_b <- sigma_b * sigma_b
var_b

# correlation between a and b
cor_ab <- cov_ab / sqrt(var_a * var_b)
cor_ab

# variance-covariance matrix
Sigma <- matrix( c(sigma_a^2, cov_ab, cov_ab, sigma_b^2) , ncol=2 )
Sigma

# correlation matrix
cov2cor(Sigma)




# SR pg. 82
## R code 4.7
library(rethinking)
data(Howell1)
d <- Howell1

## R code 4.8
str( d )
## R code 4.9
precis( d )
## R code 4.10
d$height
## R code 4.11
d2 <- d[ d$age >= 18 , ]
dens(d2$height)
## R code 4.12
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )
## R code 4.13
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

## R code 4.14
sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

## R code 4.16
mu.list <- seq( from=150, to=160 , length.out=100 )
sigma.list <- seq( from=7 , to=9 , length.out=100 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum(
  dnorm( d2$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

## R code 4.17
contour_xyz( post$mu , post$sigma , post$prob )

## R code 4.18
image_xyz( post$mu , post$sigma , post$prob )

## R code 4.19
sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

## R code 4.20
jitt <- runif(n = length(sample.rows), min = -0.075, max = 0.075)
plot( sample.mu + jitt , sample.sigma + jitt , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.25),
      xlab = "sample.mu", ylab = "sample.sigma")

## R code 4.21
dens( sample.mu )
dens( sample.sigma , norm.comp = TRUE )

## R code 4.22
PI( sample.mu )
PI( sample.sigma )


# Overthinking: Sample size and the normality of sigma's posterior
## R code 4.23
d3 <- sample( d2$height , size=20 )

## R code 4.24
mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
  sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
              log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
  dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
                        prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu + jitt , sample2.sigma + jitt , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="sample.mu" , ylab="sample.sigma" , pch=16 )

## R code 4.25
dens( sample2.sigma , norm.comp=TRUE )




# SR pg. 90
# Quadratic approximation to a posterior distribution
# with more than one parameter distribution -- mu and sigma 
# each contribute one dimension -- is a multi-dimensional
# Gaussian distribution
## R code 4.26

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

m4.1 <- quap( alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 50 )
) , data=d2 )

precis(m4.1)


## R code 4.32
# variance-covariance matrix
vcov( m4.1 )

rethinking::pairs( m4.1 )


## R code 4.33
# a vector of variances for the parameters
diag( vcov( m4.1 ) )


# a correlation matrix that tells how changes
# in any parameter lead to correlated changes in the others
cov2cor( vcov( m4.1 ) )


## R code 4.34
library(rethinking)
post <- extract.samples( m4.1 , n=1e4 )
head(post)


# SR pg. 91
# use plot(post) to see how much these samples resemble
# the samples from the grid approximation
# these samples also preserve the covariance between mu and sigma
jitt <- runif(n = dim(post)[1], min = -0.075, max = 0.075)
plot( post$mu + jitt , post$sigma + jitt , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="post.mu" , ylab="post.sigma" , pch=16 )


## R code 4.35
# compare these values to the output from precis(m4.1)
precis(post)

## R code 4.36
# extract.samples uses mvrnorm
library(MASS)
post <- mvrnorm( n=1e4 , mu=coef(m4.1) , Sigma=vcov(m4.1) )
head(post)


### FOR PRINTING ###

# vector of means
coef(m4.1)

# variance-covariance matrix
vcov(m4.1)

# draw samples from a multi-dimensional Gaussian distribution
# with means and variance-covariance matrix based on the model
library(rethinking)
post <- extract.samples( m4.1 , n=1e4 )
head(post)

# plot the samples from the multi-dimensional Gaussian distribution
jitt <- runif(n = dim(post)[1], min = -0.075, max = 0.075)
plot( post$mu + jitt , post$sigma + jitt , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="post.mu" , ylab="post.sigma" , pch=16 )

### END FOR PRINTING ###




# rstanarm version
b4.1 <- stan_glm(height ~ 1, data = d2,
                 prior_intercept = normal( location = c(178) , scale = c(20) ) ,
                 prior_aux = NULL, refresh = 0)
summary(b4.1)

# extract posterior distribution as a matrix
b4.1_post <- as.matrix(b4.1)
dim( b4.1_post)
head(b4.1_post)


# variance-covariance matrix
var(b4.1_post)


# a vector of variances for the parameters
diag( var(b4.1_post) )


# a correlation matrix that tells how changes
# in any parameter lead to correlated changes in the others
cov2cor( var(b4.1_post) )


# "by hand" variance and standard deviation of mu
var(b4.1_post[,"(Intercept)"])
sqrt(var(b4.1_post[,"(Intercept)"]))

# "by hand" variance and standard deviation of sigma
var(b4.1_post[,"sigma"])
sqrt(var(b4.1_post[,"sigma"]))

# "by hand" covariance between mu and sigma
cov(b4.1_post[,"(Intercept)"], b4.1_post[,"sigma"])

# "by hand" correlation between mu and sigma
cor(b4.1_post[,"(Intercept)"], b4.1_post[,"sigma"])


### FOR PRINTING ###

# vector of means
b4.1_post <- as.matrix(b4.1)
colMeans(b4.1_post)

# variance-covariance matrix
var(b4.1_post)

# draw samples from a multi-dimensional Gaussian distribution
# with means and variance-covariance matrix based on the model
library(MASS)
post <- mvrnorm( n=1e4 , mu=colMeans(b4.1_post) , Sigma=var(b4.1_post) )
head(post)

# plot the samples from the multi-dimensional Gaussian distribution
jitt <- runif(n = dim(post)[1], min = -0.075, max = 0.075)
plot( post[,"(Intercept)"] + jitt , post[,"sigma"] + jitt , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="post.mu" , ylab="post.sigma" , pch=16 )

### END FOR PRINTING ###




# SR pg. 92
# Quadratic approximation to a posterior distribution
# with more than one parameter distribution -- alpha and beta 
# each contribute one dimension -- is a multi-dimensional
# Gaussian distribution

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

plot( d2$height ~ d2$weight , xlab = "weight (kg)" , ylab = "height (cm)")

xbar <- mean(d2$weight)

m4.3 <- quap( alist(
  height ~ dnorm( mu , sigma ) ,
  mu <- a + b*( weight - xbar ) ,
  a ~ dnorm( 178 , 20 ) ,
  b ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 50 )
) , data=d2 )

precis(m4.3)


options(scipen = 5)

## R code 4.32
# variance-covariance matrix
round( vcov( m4.3 ) , 3)

rethinking::pairs( m4.3 )


## R code 4.33
# a vector of variances for the parameters
round( diag( vcov( m4.3 ) ) , 3 )


# a correlation matrix that tells how changes
# in any parameter lead to correlated changes in the others
round( cov2cor( vcov( m4.3 ) ) , 3 )


## R code 4.34
library(rethinking)
post <- extract.samples( m4.3 , n=1e4 )
head(post)


# SR pg. 91
# use plot(post) to see how much these samples resemble
# the samples from the grid approximation
# these samples also preserve the covariance between mu and sigma
jitt <- runif(n = dim(post)[1], min = -0.075, max = 0.075)
plot( post$a + jitt , post$b + jitt , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="post.alpha" , ylab="post.beta" , pch=16 )


## R code 4.35
# compare these values to the output from precis(m4.3)
precis(post)

## R code 4.36
# extract.samples uses mvrnorm
library(MASS)
post <- mvrnorm( n=1e4 , mu=coef(m4.3) , Sigma=vcov(m4.3) )
head(post)


### FOR PRINTING ###

# vector of means
round( coef(m4.3) , 2)

# variance-covariance matrix
round( vcov(m4.3) , 3 )

# draw samples from a multi-dimensional Gaussian distribution
# with means and variance-covariance matrix based on the model
library(rethinking)
post <- extract.samples( m4.3 , n=1e4 )
head(post)

# plot the samples from the multi-dimensional Gaussian distribution
jitt <- runif(n = dim(post)[1], min = -0.075, max = 0.075)
plot( post$a + jitt , post$b + jitt , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="post.alpha" , ylab="post.beta" , pch=16 )

### END FOR PRINTING ###




# rstanarm version
weight_c <- d2$weight - mean(d2$weight)

b4.3 <- stan_glm(height ~ 1 + weight_c, data = d2,
                 prior_intercept = normal( location = c(178) , scale = c(20) ) ,
                 prior = normal( location = c(0) , scale = c(1) ) ,
                 prior_aux = NULL, refresh = 0)
summary(b4.3)

# extract posterior distribution as a matrix
b4.3_post <- as.matrix(b4.3)
colMeans(b4.3_post)
dim( b4.3_post)
head(b4.3_post)


# variance-covariance matrix
round( var(b4.3_post) , 3 )


# a vector of variances for the parameters
round( diag( var(b4.3_post) ) , 4 )


# a correlation matrix that tells how changes
# in any parameter lead to correlated changes in the others
round( cov2cor( var(b4.3_post) ) , 4 )


# "by hand" variance and standard deviation of alpha (Intercept)
var(b4.3_post[,"(Intercept)"])
sqrt(var(b4.3_post[,"(Intercept)"]))

# "by hand" variance and standard deviation of beta (weight_c)
var(b4.3_post[,"weight_c"])
sqrt(var(b4.3_post[,"weight_c"]))

# "by hand" variance and standard deviation of sigma (sigma)
var(b4.3_post[,"sigma"])
sqrt(var(b4.3_post[,"sigma"]))

# "by hand" covariance between alpha and beta
cov(b4.3_post[,"(Intercept)"], b4.3_post[,"weight_c"])

# "by hand" correlation between alpha and beta
cor(b4.3_post[,"(Intercept)"], b4.3_post[,"weight_c"])


### FOR PRINTING ###

# vector of means
b4.3_post <- as.matrix(b4.3)
colMeans(b4.3_post)

# variance-covariance matrix
var(b4.3_post)

# draw samples from a multi-dimensional Gaussian distribution
# with means and variance-covariance matrix based on the model
library(MASS)
post <- mvrnorm( n=1e4 , mu=colMeans(b4.3_post) , Sigma=var(b4.3_post) )
head(post)

# plot the samples from the multi-dimensional Gaussian distribution
jitt <- runif(n = dim(post)[1], min = -0.075, max = 0.075)
plot( post[,"(Intercept)"] + jitt , post[,"weight_c"] + jitt , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="post.alpha" , ylab="post.beta" , pch=16 )

### END FOR PRINTING ###



# Classical `lm` version
lm4.3 <- lm(height ~ 1 + weight_c, data = d2)
summary(lm4.3)

# variance-covariance matrix
round( vcov( lm4.3 ) , 3)

rethinking::pairs( lm4.3 )


# a vector of variances for the parameters
round( diag( vcov( lm4.3 ) ) , 3 )


# a correlation matrix that tells how changes
# in any parameter lead to correlated changes in the others
round( cov2cor( vcov( lm4.3 ) ) , 3 )




# SR pg. 121
# Practice problem 4M7
# Refit model m4.3 but omit the mean weight xbar this time
# Compare the new model's posterior to that of the original model
# In particular, look at the covariance among the parameters

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

plot( d2$height ~ d2$weight , xlab = "weight (kg)" , ylab = "height (cm)")

m4M7 <- quap( 
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b * weight ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
) , data=d2 )

precis(m4M7)


## R code 4.32
# variance-covariance matrix
round( vcov( m4M7 ) , 3)

rethinking::pairs( m4M7 )


## R code 4.33
# a vector of variances for the parameters
round( diag( vcov( m4M7 ) ) , 3 )


# a correlation matrix that tells how changes
# in any parameter lead to correlated changes in the others
round( cov2cor( vcov( m4M7 ) ) , 3 )


## R code 4.34
library(rethinking)
post <- extract.samples( m4M7 , n=1e4 )
head(post)


# SR pg. 91
# use plot(post) to see how much these samples resemble
# the samples from the grid approximation
# these samples also preserve the covariance between mu and sigma
jitt <- runif(n = dim(post)[1], min = -0.075, max = 0.075)
plot( post$a + jitt , post$b + jitt , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="post.alpha" , ylab="post.beta" , pch=16 )


## R code 4.35
# compare these values to the output from precis(m4M7)
precis(post)

## R code 4.36
# extract.samples uses mvrnorm
library(MASS)
post <- mvrnorm( n=1e4 , mu=coef(m4M7) , Sigma=vcov(m4M7) )
head(post)


### FOR PRINTING ###

# vector of means
round( coef(m4M7) , 2 )

# variance-covariance matrix
round( vcov(m4M7) , 3 )

# draw samples from a multi-dimensional Gaussian distribution
# with means and variance-covariance matrix based on the model
library(rethinking)
post <- extract.samples( m4M7 , n=1e4 )
head(post)

# plot the samples from the multi-dimensional Gaussian distribution
jitt <- runif(n = dim(post)[1], min = -0.075, max = 0.075)
plot( post$a + jitt , post$b + jitt , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="post.alpha" , ylab="post.beta" , pch=16 )

### END FOR PRINTING ###




# rstanarm version
b4M7 <- stan_glm(height ~ 1 + weight, data = d2,
                 prior_intercept = normal( location = c(178) , scale = c(20) ) ,
                 prior = normal( location = c(0) , scale = c(1) ) ,
                 prior_aux = NULL, refresh = 0)
summary(b4M7)

# extract posterior distribution as a matrix
b4M7_post <- as.matrix(b4M7)

colMeans(b4M7_post)
dim(b4M7_post)
head(b4M7_post)


# variance-covariance matrix
round( var(b4M7_post) , 3 )


# a vector of variances for the parameters
round( diag( var(b4M7_post) ) , 4 )


# a correlation matrix that tells how changes
# in any parameter lead to correlated changes in the others
round( cov2cor( var(b4M7_post) ) , 4 )


# "by hand" variance and standard deviation of alpha (Intercept)
var(b4M7_post[,"(Intercept)"])
sqrt(var(b4M7_post[,"(Intercept)"]))

# "by hand" variance and standard deviation of beta (weight)
var(b4M7_post[,"weight"])
sqrt(var(b4M7_post[,"weight"]))

# "by hand" variance and standard deviation of sigma (sigma)
var(b4M7_post[,"sigma"])
sqrt(var(b4M7_post[,"sigma"]))

# "by hand" covariance between alpha and beta
cov(b4M7_post[,"(Intercept)"], b4M7_post[,"weight"])

# "by hand" correlation between alpha and beta
cor(b4M7_post[,"(Intercept)"], b4M7_post[,"weight"])


### FOR PRINTING ###

# vector of means
b4M7_post <- as.matrix(b4M7)
round( colMeans(b4M7_post) , 2 )

# variance-covariance matrix
round( var(b4M7_post) , 3 )

# draw samples from a multi-dimensional Gaussian distribution
# with means and variance-covariance matrix based on the model
library(MASS)
post <- mvrnorm( n=1e4 , mu=colMeans(b4M7_post) , Sigma=var(b4M7_post) )
head(post)

# plot the samples from the multi-dimensional Gaussian distribution
jitt <- runif(n = dim(post)[1], min = -0.075, max = 0.075)
plot( post[,"(Intercept)"] + jitt , post[,"weight"] + jitt , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="post.alpha" , ylab="post.beta" , pch=16 )

### END FOR PRINTING ###




# Classical `lm` version
lm4M7 <- lm(height ~ 1 + weight, data = d2)
summary(lm4M7)

# variance-covariance matrix
round( vcov( lm4M7 ) , 3)

rethinking::pairs( lm4M7 )


# a vector of variances for the parameters
round( diag( vcov( lm4M7 ) ) , 3 )


# a correlation matrix that tells how changes
# in any parameter lead to correlated changes in the others
round( cov2cor( vcov( lm4M7 ) ) , 3 )










# SR pg. 437
# Varying intercepts, varying slopes by construction
## R code 14.1
a <- 3.5            # average morning wait time
b <- (-1)           # average difference afternoon wait time
sigma_a <- 1        # std dev in intercepts
sigma_b <- 0.5      # std dev in slopes
rho <- (-0.7)       # correlation between intercepts and slopes

## R code 14.2
Mu <- c( a , b )
Mu

# Option 1: to build variance-covariance matrix
## R code 14.3
# covariance between a and b
cov_ab <- sigma_a*sigma_b*rho
cov_ab

# variance-covariance matrix
Sigma <- matrix( c(sigma_a^2, cov_ab, cov_ab, sigma_b^2) , ncol=2 )
Sigma


# # Option 2: to build variance-covariance matrix
## R code 14.5
sigmas <- c(sigma_a,sigma_b) # standard deviations

Rho <- matrix( c(1,rho,rho,1) , nrow=2 ) # correlation matrix
Rho

# now matrix multiply to get covariance matrix
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)
Sigma



# Let's simulate some cafes, each with its own intercept and slope,
# drawn from the multivariate Gaussian distribution defined by Mu and Sigma

## R code 14.6
N_cafes <- 20

## R code 14.7
library(MASS)
set.seed(5) # used to replicate example
vary_effects <- mvrnorm( N_cafes , Mu , Sigma )

## R code 14.8
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]

## R code 14.9
plot( a_cafe , b_cafe , col=rangi2 ,
      xlab="intercepts (a_cafe)" , ylab="slopes (b_cafe)" )

# overlay population distribution
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma,centre=Mu,level=l),col=col.alpha("black",0.2))




# SR Lecture 17 minute 12:00
# Population of cafes
Sigma <- matrix( c(1, -0.8, -0.8, 1), 2, 2 )
Sigma

pop <- mvrnorm(n = 500, rep(0, 2), Sigma)

plot( pop[,1] , pop[,2] , col=rangi2 , xlab="intercept" , ylab="slope" )

for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma,centre=rep(0, 2),level=l),col=col.alpha("black",0.2))




# SR code 14.11
R1 <- rlkjcorr( 1e4 , K=2 , eta=1 )
R2 <- rlkjcorr( 1e4 , K=2 , eta=2 )
R4 <- rlkjcorr( 1e4 , K=2 , eta=4 )
dens( R4[,1,2] , xlab="correlation" )
dens( R2[,1,2] , add = TRUE )
dens( R1[,1,2] , add = TRUE )
text(x = 0 , y = 0.6 , labels = c("eta = 1"))
text(x = 0 , y = 0.8 , labels = c("eta = 2"))
text(x = 0.2 , y = 1.1 , labels = c("eta = 4"))




# SR code 14.10
# Simulate observations
# i.e. simulate our robot visiting 
# these cafes and collecting data
set.seed(22)
N_visits <- 10
afternoon <- rep(0:1,N_visits*N_cafes/2)
cafe_id <- rep( 1:N_cafes , each=N_visits )
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- 0.5  # std dev within cafes
wait <- rnorm( N_visits*N_cafes , mu , sigma )
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )
head(d)
tail(d)
str(d)


# Fit a complete pooling model with `rethinking::quap()`
m14.1_pool <- quap(
  alist(
    wait ~ dnorm( mu , sigma ),
    mu <- a + b*afternoon,
    a ~ dnorm(5, 2),
    b ~ dnorm(-1, 0.5),
    sigma ~ dexp(1)
  ) , data=d )

precis(m14.1_pool)


# Fit a no pooling model with `rethinking::quap()`
m14.1_no_pool <- quap(
  alist(
    wait ~ dnorm( mu , sigma ),
    mu <- a[cafe] + b*afternoon,
    a[cafe] ~ dnorm(5, 2),
    b ~ dnorm(-1, 0.5),
    sigma ~ dexp(1)
  ) , data=d )

precis(m14.1_no_pool)

precis(m14.1_no_pool, depth=2)


# Fit a multilevel model with `rethinking::ulam()`
m14.1 <- ulam(
  alist(
    wait ~ normal( mu , sigma ),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    c(a_cafe,b_cafe)[cafe] ~ multi_normal( c(a,b) , Rho , sigma_cafe ),
    a ~ normal(5,2),
    b ~ normal(-1,0.5),
    sigma_cafe ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkjcorr(2)
  ) , data=d , chains=4 , cores=4, iter=2000 , 
  control=list(adapt_delta=0.99) , 
  cmdstan=TRUE , messages=FALSE )

precis(m14.1)

precis(m14.1, depth=2)

precis(m14.1, depth=3)


# SR code 14.13
post <- extract.samples(m14.1)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 )    # prior
dens( R[,1,2] , add=TRUE , lty=2 )

# SR code 14.14
# compute unpooled estimates directly from data
a1 <- sapply( 1:N_cafes ,
              function(i) mean(wait[cafe_id==i & afternoon==0]) )
b1 <- sapply( 1:N_cafes ,
              function(i) mean(wait[cafe_id==i & afternoon==1]) ) - a1

# extract posterior means of partially pooled estimates
post <- extract.samples(m14.1)
a2 <- apply( post$a_cafe , 2 , mean )
b2 <- apply( post$b_cafe , 2 , mean )

# plot both and connect with lines
plot( a1 , b1 , xlab="intercept" , ylab="slope" ,
      pch=16 , col=rangi2 , ylim=c( min(b1)-0.1 , max(b1)+0.1 ) ,
      xlim=c( min(a1)-0.1 , max(a1)+0.1 ) )
points( a2 , b2 , pch=1 )
for ( i in 1:N_cafes ) lines( c(a1[i],a2[i]) , c(b1[i],b2[i]) )

# SR code 14.15
# compute posterior mean bivariate Gaussian
Mu_est <- c( mean(post$a) , mean(post$b) )
rho_est <- mean( post$Rho[,1,2] )
sa_est <- mean( post$sigma_cafe[,1] )
sb_est <- mean( post$sigma_cafe[,2] )
cov_ab <- sa_est*sb_est*rho_est
Sigma_est <- matrix( c(sa_est^2,cov_ab,cov_ab,sb_est^2) , ncol=2 )

# draw contours
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma_est,centre=Mu_est,level=l),
        col=col.alpha("black",0.2))

## R code 14.16
# convert varying effects to waiting times
wait_morning_1 <- (a1)
wait_afternoon_1 <- (a1 + b1)
wait_morning_2 <- (a2)
wait_afternoon_2 <- (a2 + b2)

# plot both and connect with lines
plot( wait_morning_1 , wait_afternoon_1 , xlab="morning wait" ,
      ylab="afternoon wait" , pch=16 , col=rangi2 ,
      ylim=c( min(wait_afternoon_1)-0.1 , max(wait_afternoon_1)+0.1 ) ,
      xlim=c( min(wait_morning_1)-0.1 , max(wait_morning_1)+0.1 ) )
points( wait_morning_2 , wait_afternoon_2 , pch=1 )
for ( i in 1:N_cafes )
  lines( c(wait_morning_1[i],wait_morning_2[i]) ,
         c(wait_afternoon_1[i],wait_afternoon_2[i]) )
abline( a=0 , b=1 , lty=2 )

## R code 14.17
# now shrinkage distribution by simulation
v <- mvrnorm( 1e4 , Mu_est , Sigma_est )
v[,2] <- v[,1] + v[,2] # calculate afternoon wait
Sigma_est2 <- cov(v)
Mu_est2 <- Mu_est
Mu_est2[2] <- Mu_est[1]+Mu_est[2]

# draw contours
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma_est2,centre=Mu_est2,level=l),
        col=col.alpha("black",0.5))



# Fit a complete pooling model with `lm`
arm::display( lm( wait ~ 1 + afternoon , data = d) )


# Fit a no pooling model with `lm`
arm::display( lm( wait ~ afternoon + factor(cafe_id) - 1 , data = d) )


# Fit a multilevel model with `Matrix::lmer()`
lmer14.1 <- lmer( wait ~ 1 + afternoon + (1 + afternoon | cafe_id) , data = d)
arm::display(lmer14.1)


# view the estimated model within each cafe
coef(lmer14.1)


# view the estimated model averaging over the cafes ("fixed effects")
fixef(lmer14.1)

se.fixef(lmer14.1)

# view the cafe-level errors ("random effects")
ranef(lmer14.1)

se.ranef(lmer14.1)




# Fit a complete pooling model with `rstanarm::stan_glm()`
b14.1_pool <- stan_glm( wait ~ 1 + afternoon , 
                        data = d , refresh=0)
summary( b14.1_pool , digits=2 )
tidy(b14.1_pool)
glance(b14.1_pool)


# Fit a no pooling model with `rstanarm::stan_glm()`
b14.1_no_pool <- stan_glm( wait ~ afternoon + factor(cafe_id) - 1 , 
                           data = d , refresh=0)
summary( b14.1_no_pool , digits=2 )
tidy(b14.1_no_pool)
glance(b14.1_no_pool)


# Fit a multilevel model with `rstanarm::stan_glm()`
b14.1 <- stan_glmer( wait ~ 1 + afternoon + (1 + afternoon | factor(cafe_id)) , 
                     prior_covariance = decov(regularization = 2) ,
                     data = d , refresh=0)
summary( b14.1 , digits=2 )
tidy(b14.1)
glance(b14.1)




# Source: https://www.r-bloggers.com/2011/03/five-ways-to-visualize-your-pairwise-comparisons/
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}




# PCA and plots of the USArrests dataset
# https://uc-r.github.io/pca
data("USArrests")
head(USArrests)

# compute the variance of each variable
round( apply(USArrests , 2 , var) , 2 )

# plot all pairwise relationships
pairs(USArrests)

pairs(USArrests, lower.panel=panel.smooth, upper.panel=panel.cor)


# create new data frame with centered variables
USArrests_scaled <- apply(USArrests , 2 , scale)
head(USArrests_scaled)

# compute the variance of each variable
apply(USArrests_scaled , 2 , var)

# plot all pairwise relationships
pairs(USArrests_scaled)

pairs(USArrests_scaled, lower.panel=panel.smooth, upper.panel=panel.cor)


# calculate the covariance matrix
arrests_cov <- cov(USArrests_scaled)
arrests_cov

# calculate eigenvalues & eigenvectors
arrests_eigen <- eigen(arrests_cov)
arrests_eigen

str(arrests_eigen)


# extract the first two sets of loadings
phi <- arrests_eigen$vectors[, 1:2]
phi


# eigenvectors that are calculated in any software 
# package are unique up to a sign flip
# by default, eigenvectors in R point in the negative direction

# for this example, we would prefer the eigenvectors 
# point in the positive direction because it leads to 
# more logical interpretation of graphical results

# to use the positive-pointing vector,
# multiply the default loadings by -1
phi <- -phi
row.names(phi) <- c("Murder", "Assault", "UrbanPop", "Rape")
colnames(phi) <- c("PC1", "PC2")
phi


# calculate principal components scores
PC1 <- as.matrix(USArrests_scaled) %*% phi[,1]
PC2 <- as.matrix(USArrests_scaled) %*% phi[,2]


# create a data frame with principal components scores
PC <- data.frame(State = row.names(USArrests), PC1, PC2)
head(PC)


# plot principal components for each state
ggplot(PC, aes(PC1, PC2)) + 
  modelr::geom_ref_line(h = 0, colour = "grey") +
  modelr::geom_ref_line(v = 0, colour = "grey") +
  geom_text(aes(label = State), size = 3) +
  xlab("First Principal Component") + 
  ylab("Second Principal Component") + 
  ggtitle("First Two Principal Components of USArrests Data") +
  theme_light() +
  theme(plot.title=element_text(hjust=0.5))


# calculate the proportion of variance explained (PVE)
PVE <- arrests_eigen$values / sum(arrests_eigen$values)
round(PVE, 2)


# PVE (aka scree) plot
PVEplot <- qplot(c(1:4), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1) +
  theme_light() +
  theme(plot.title=element_text(hjust=0.5))

# Cumulative PVE plot
cumPVE <- qplot(c(1:4), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1) +
  theme_light() +
  theme(plot.title=element_text(hjust=0.5))

grid.arrange(PVEplot, cumPVE, ncol = 2)


# conduct a principal component analysis using the prcomp function
USArrests_pca <- prcomp(USArrests, scale = TRUE)
names(USArrests_pca)

# means
USArrests_pca$center

# standard deviations
USArrests_pca$scale

# principal component loadings
USArrests_pca$rotation

USArrests_pca$rotation <- -USArrests_pca$rotation
USArrests_pca$rotation

# principal component scores
USArrests_pca$x

USArrests_pca$x <- -USArrests_pca$x
USArrests_pca$x

# standard deviation of each principal component
USArrests_pca$sdev

# variance explained by each principal component
( VE <- USArrests_pca$sdev^2 )

# proportion of variance explained by each principal component
PVE <- VE / sum(VE)
round(PVE, 2)


# plot principal components using biplot
# include choices = 3:4 to plot principal components 3 vs 4
# scale = 0 ensures the arrows are scaled to represent the loadings
biplot(USArrests_pca, scale = 0)




# Principal Component Analysis (PCA)
# HSAUR pg.215
data("heptathlon", package = "HSAUR")

# it will help to score all the seven events in the same direction
# so that "large" values are "good"
# recode the running events to achieve this
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m

# view scatterplot matrix of the results 
# from the 25 competitors on the 7 events
score <- which(colnames(heptathlon) == "score")

heptathlon[, -score]

plot(heptathlon[, -score])

# view correlations between the events
round( cor(heptathlon[, -score]) , 2 )

# perform principal component analysis of the data
heptathlon_pca <- prcomp(heptathlon[, -score], center=TRUE, scale=TRUE)

print(heptathlon_pca)

summary(heptathlon_pca)

str(heptathlon_pca)

# linear combination for the first principal component
a1 <- heptathlon_pca$rotation[, 1]
a1

# to compute the first principal component, the data need to be rescaled
# extract the centering and scaling used by `prcomp` internally
center <- heptathlon_pca$center
center

scale <- heptathlon_pca$scale
scale

# compute the first principal component score for each competitor
# apply the `scale` function to the data and multiply by the loadings matrix
hm <- as.matrix(heptathlon[, -score])
scale(hm, center=center, scale=scale) %*% heptathlon_pca$rotation[, 1]

drop( scale(hm, center=center, scale=scale) %*% heptathlon_pca$rotation[, 1] )

# more conveniently, extract the first principal component score
# for each competitor from all precomputed principal components
predict(heptathlon_pca)[, 1]

# barplot of the variances explained by the principal components
plot(heptathlon_pca)

# biplot of the (scaled) first two principal components
biplot( heptathlon_pca , col = c("grey", "black") )

athlete.country <- c("USA", "GDR", "GDR", "URS", "URS", 
                     "GDR", "AUS", "USA", "CZE", "URS", 
                     "HOL", "BUL", "SWI", "FRG", "FIN", 
                     "CHN", "GB", "USA", "GB", "BEL", 
                     "FIN", "BRA", "TAI", "KOR", "PNG")
length(unique(athlete.country))

ggbiplot::ggbiplot( heptathlon_pca , labels = rownames(heptathlon) , groups = athlete.country ) +
  scale_color_hue()
  #scale_color_brewer(palette="Set3")

# biplot of the (scaled) third and fourth principal components
ggbiplot::ggbiplot( heptathlon_pca , labels = rownames(heptathlon) , choices = c(3,4) ) + 
  ggtitle("PCA of heptathlon dataset") +
  theme_light() +
  theme(plot.title=element_text(hjust=0.5))

# correlation between the score given to each athlete 
# by the standard scoring system and the first principal component
round( cor(heptathlon$score, heptathlon_pca$x[, 1]) , 3 )
# this correlation implies the first principal component is in
# good agreement with the score assigned by the official Olympic rules

# scatterplot of the score assigned to each athlete in 1988 
# and the first principal component
plot(heptathlon$score, heptathlon_pca$x[, 1])




# Version for presentation slides
data("heptathlon", package = "HSAUR")

# it will help to score all the seven events in the same direction
# so that "large" values are "good"
# recode the running events to achieve this
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m

# view scatterplot matrix of the results 
# from the 25 competitors on the 7 events
score <- which(colnames(heptathlon) == "score")

heptathlon[, -score]

# compute the variance of each variable
round( apply(heptathlon[, -score] , 2 , var) , 2 )

# plot all pairwise relationships
pairs(heptathlon[, -score])

pairs(heptathlon[, -score], lower.panel=panel.smooth, upper.panel=panel.cor)


# create new data frame with centered variables
heptathlon_scaled <- apply(heptathlon[, -score] , 2 , scale)
head(heptathlon_scaled)

# compute the variance of each variable
apply(heptathlon_scaled , 2 , var)

# plot all pairwise relationships
pairs(heptathlon_scaled)

pairs(heptathlon_scaled, lower.panel=panel.smooth, upper.panel=panel.cor)

# create new data frame with centered variables
heptathlon_scaled <- apply(heptathlon[, -score] , 2 , scale)

# calculate the covariance matrix
heptathlon_cov <- cov(heptathlon_scaled)
heptathlon_cov

# calculate eigenvalues & eigenvectors
heptathlon_eigen <- eigen(heptathlon_cov)
heptathlon_eigen

str(heptathlon_eigen)


# extract the first two sets of loadings
phi <- heptathlon_eigen$vectors[, 1:2]
phi


# eigenvectors that are calculated in any software 
# package are unique up to a sign flip
# by default, eigenvectors in R point in the negative direction

# for this example, we would prefer the eigenvectors 
# point in the positive direction because it leads to 
# more logical interpretation of graphical results

# to use the positive-pointing vector,
# multiply the default loadings by -1
phi <- -phi
row.names(phi) <- c("hurdles", "highjump", "shot", "run200m", 
                    "longjump", "javelin", "run800m")
colnames(phi) <- c("PC1", "PC2")
phi


# calculate principal components scores
PC1 <- as.matrix(heptathlon_scaled) %*% phi[,1]
PC2 <- as.matrix(heptathlon_scaled) %*% phi[,2]


# create a data frame with principal components scores
PC <- data.frame(Competitor = row.names(heptathlon), PC1, PC2)
head(PC)


# plot principal components for each competitor
ggplot(PC, aes(PC1, PC2)) + 
  modelr::geom_ref_line(h = 0, colour = "grey") +
  modelr::geom_ref_line(v = 0, colour = "grey") +
  geom_text(aes(label = Competitor), size = 3) +
  xlab("First Principal Component") + 
  ylab("Second Principal Component") + 
  ggtitle("First Two Principal Components of heptathlon Data") +
  theme_light() +
  theme(plot.title=element_text(hjust=0.5))


# calculate the proportion of variance explained (PVE)
PVE <- heptathlon_eigen$values / sum(heptathlon_eigen$values)
round(PVE, 2)


# PVE (aka scree) plot
PVEplot <- qplot(c(1:7), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1) +
  theme_light() +
  theme(plot.title=element_text(hjust=0.5))

# Cumulative PVE plot
cumPVE <- qplot(c(1:7), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1) +
  theme_light() +
  theme(plot.title=element_text(hjust=0.5))

grid.arrange(PVEplot, cumPVE, ncol = 2)


# conduct a principal component analysis using the prcomp function
heptathlon_pca <- prcomp(heptathlon[, -score], scale = TRUE)
names(heptathlon_pca)

# means
heptathlon_pca$center

# standard deviations
heptathlon_pca$scale

# principal component loadings
heptathlon_pca$rotation

heptathlon_pca$rotation <- -heptathlon_pca$rotation
heptathlon_pca$rotation

# principal component scores
round( heptathlon_pca$x , 2 )

heptathlon_pca$x <- -heptathlon_pca$x
heptathlon_pca$x

# standard deviation of each principal component
heptathlon_pca$sdev

# variance explained by each principal component
( VE <- heptathlon_pca$sdev^2 )

# proportion of variance explained by each principal component
PVE <- VE / sum(VE)
round(PVE, 2)


# plot principal components using biplot
# include choices = 3:4 to plot principal components 3 vs 4
# scale = 0 ensures the arrows are scaled to represent the loadings
biplot(heptathlon_pca, scale = 0)




# PCA and biplot of the wine dataset
# ggbiplot {ggbiplot} Help Page
data(wine)
head(wine)
wine.pca <- prcomp(wine, scale = TRUE)
print(ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = wine.class, ellipse = TRUE, circle = TRUE))






### EXTRA ###

# MASS::mvrnorm
# Simulate from a Multivariate Normal Distribution
# Produces one or more samples from 
# the specified multivariate normal distribution
# mvrnorm(n = 1, mu, Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)

# Examples
Sigma <- matrix(c(10,3,3,2),2,2)
Sigma

var( mvrnorm(n = 1000, rep(0, 2), Sigma) )

var( mvrnorm(n = 1000, rep(0, 2), Sigma, empirical = TRUE) )




# Generate a sample X = (X1, X2) from a bivariate normal 
# in which the marginals are N(5,5) and N(10,1) and
# the correlation between X1 and X2 is 0.0
# Define the properties of the normal random variables
# https://stackoverflow.com/questions/51336011/how-to-generate-2-uncorrelated-random-normal-variables-with-different-means-u
# Means
m1 <- 5
m2 <- 10
# Variances
s1 <- 5
s2 <- 1
# Correlations
X1 <- 0

# Create the normal random variables
set.seed(123)
dat <- MASS::mvrnorm(20, 
                     mu = c(m1, m2),
                     Sigma = matrix(c(s1, X1,
                                      X1, s2),
                                    ncol = 2, byrow = TRUE), 
                     empirical = TRUE)
dat

# Examine the correlations
dat %>% cor()




# Generate a sample X = (X1, X2) from a bivariate normal 
# in which each marginal is N(0,1) and 
# the correlation between X1 and X2 is 0.5
# https://stackoverflow.com/questions/59997925/how-to-generate-multivariate-normal-data-in-r
n <- 1e3
R <- matrix(c(1, 0.5,
              0.5, 1), 
            nrow = 2, ncol = 2)

mu <- c(X=0, Y=0)
#mvtnorm::rmvnorm(n, mean = mu, sigma = R)
set.seed(123)
dat.n <- MASS::mvrnorm(n, mu = mu, Sigma = R, empirical = TRUE)
dat.n

# Examine the correlations
dat.n %>% cor()


# ROS pg. 188
# Plot principal component line
par(mfrow=c(1,2))

plot(dat.n[,1], dat.n[,2], xlab="x", ylab="y", main="principal component line")
abline(a=0, b=1)
text(-3, 3, paste("   y =", 0, "+", 1, "* x"), adj=0)

# Plot linear regression line
plot(dat.n[,1], dat.n[,2], xlab="x", ylab="y", main="linear regression line")
fit.n <- lm(dat.n[,2] ~ dat.n[,1], data=data.frame(dat.n))
abline(coef(fit.n))
text(-3, 3, paste("   y =", round(coef(fit.n)[1], 2), "+", round(coef(fit.n)[2], 2), "* x"), adj=0)

par(mfrow=c(1,1))




# Multivariate normal distributions
# https://stephens999.github.io/fiveMinuteStats/mvnorm.html

# Example 1
# Suppose Z1,...,Zn are independent random variables 
# each with a standard normal distribution N(0,1)
Z <- rnorm(3)
A <- rbind( c(1,1,0), c(1,0,1) )
( A %*% t(A) ) # show Sigma

( X <- A %*% Z )




# Example 2
X <- matrix(0, nrow=2, ncol=1000)
A <- rbind( c(1,1,0), c(1,0,1) )
for(i in 1:1000){
  Z <- rnorm(3)
  X[,i] = A %*% Z
}

plot(X[1,], X[2,], 
     main="bivariate normal with variance 2, covariance 1", 
     asp=1,
     xlim=c(-5,5),
     ylim=c(-5,5))

# check the sample covariances are close to the theoretical values
cov(t(X))




# General algorithm
# Cholesky decomposition finds a unique lower triangular matrix L such that LL′=Σ
Sigma <- rbind( c(2,1), c(1,2) )
L <- t(chol(Sigma))
L

# We can use this to generate a multivariate normal
# Here we use it to generate a bivariate normal with covariance matrix
#  1.0 0.9
#  0.9 1.0
my_rmvnorm <- function(mu, Sigma){
  r <- length(mu)
  L <- t(chol(Sigma)) 
  Z <- rnorm(r)
  return(L %*% Z + mu)
}

X = matrix(0, nrow=2, ncol=1000)
for(i in 1:1000){
  X[,i] <- my_rmvnorm( c(0,0) , rbind( c(1.0, 0.9), c(0.9,1.0) ) )
}

plot(X[1,], X[2,],
     main="bivariate normal with variance 1, covariance 0.9",
     asp=1)




# ROS pg. 179
# Demonstration of k-fold cross validation using simulated data
# https://avehtari.github.io/ROS-Examples/FakeKCV/fake_kcv.html

# Set random seed for reproducability
SEED <- 1754

# Generate fake data
# 60×30 matrix representing 30 predictors that are random but not independent; 
# rather, we draw them from a multivariate normal distribution with correlations 0.8
set.seed(SEED)

n <- 60

k <- 30

rho <- 0.8

Sigma <- rho*array(1, c(k,k)) + (1-rho)*diag(k)

X <- mvrnorm(n=n, mu=rep(0,k), Sigma=Sigma)

b <- c(c(-1, 1, 2), rep(0,k-3))

y <- X %*% b + rnorm(n)*2

fake <- data.frame(X, y)

# Weakly informative prior
fit_1 <- stan_glm(y ~ ., 
                  prior=normal(0, 10, autoscale=FALSE),
                  data=fake, seed=SEED, refresh=0)

( loo_1 <- loo(fit_1) )

# In this case, Pareto smoothed importance sampling LOO fails, 
# but the diagnostic recognizes this with many high Pareto k values
# We can run slower, but more robust K-fold-CV
( kfold_1 <- kfold(fit_1) )

# An alternative weakly informative prior
# The regularized horseshoe prior hs() is weakly informative, 
# stating that it is likely that only small number of 
# predictors are relevant, but we don’t know which ones
k0 <- 2 # prior guess for the number of relevant variables

tau0 <- k0/(k-k0) * 1/sqrt(n)

hs_prior <- hs(df=1, global_df=1, global_scale=tau0, slab_scale=3, slab_df=7)

fit_2 <- stan_glm(y ~ ., 
                  prior=hs_prior, data=fake, seed=SEED,
                  control=list(adapt_delta=0.995), refresh=200)

( loo_2 <- loo(fit_2) )

# PSIS-LOO performs better now, but there is still one bad diagnostic value, 
# and thus we run again slower, but more robust K-fold-CV
( kfold_2 <- kfold(fit_2) )

# Comparison of models
loo_compare(loo_1,loo_2)

loo_compare(kfold_1,kfold_2)
# As PSIS-LOO fails, PSIS-LOO comparison underestimates 
# the difference between the models. The Pareto k diagnostic 
# correctly identified the problem, and more robust K-fold-CV 
# shows that by using a better prior we can get better predictions




# SR pg. 170
# Simulating collinearity
# SR code 6.12
# Plot the standard deviation of the posterior distribution of perc.fat 
# as a function of its correlation with another predictor variable
# to illustrate how stronger collinearity increases the std dev
library(rethinking)
data(milk)
d <- milk

sim.coll <- function( r=0.9 ) {
  d$x <- rnorm( nrow(d) , mean=r*d$perc.fat ,
                sd=sqrt( (1-r^2)*var(d$perc.fat) ) )
  m <- lm( kcal.per.g ~ perc.fat + x , data=d )
  sqrt( diag( vcov(m) ) )[2] # stddev of parameter
}

rep.sim.coll <- function( r=0.9 , n=100 ) {
  stddev <- replicate( n , sim.coll(r) )
  mean(stddev)
}

r.seq <- seq(from=0,to=0.99,by=0.01)

stddev <- sapply( r.seq , function(z) rep.sim.coll(r=z,n=100) )

plot( stddev ~ r.seq , type="l" , col=rangi2, lwd=2 , xlab="correlation" )







