library("broom")
library("betareg")
library("tidyverse")
library("rethinking")
library("rstanarm")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library("ggplot2")
library("humanize")
library("scales")
library("patchwork")




# Probability vs Inference

# Probability: assume the probability that a baby is a girl is 48.8%
n_sims <- 1000
n_girls <- rbinom(n = n_sims, size = 400, prob = 0.488)
mean(n_girls) / 400
hist(n_girls)


# Inference: learn the probability that a baby is a girl based on data
# https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/
prop_girls <- n_girls / 400
glimpse(prop_girls)


# Model: fractional logistic regression
fit_frac <- glm(prop_girls ~ 1, 
                data = data.frame(prop_girls), 
                family = quasibinomial())
tidy(fit_frac)

# extract the Intercept and
# convert logit to a probability (or proportion in this case)
fit_frac %>%
  tidy() %>%
  filter(term == "(Intercept)") %>%
  pull(estimate) %>%
  plogis(.)


# Model: beta regression
fit_prop <- betareg(prop_girls ~ 1, 
                    data = data.frame(prop_girls), 
                    link = "logit")
tidy(fit_prop)

# extract the Intercept and
# convert logit to a probability (or proportion in this case) 
fit_prop %>%
  tidy() %>%
  filter(component == "mean", term == "(Intercept)") %>%
  pull(estimate) %>%
  plogis(.)




# Uniform distribution

# define x-axis (1,000 evenly-spaced numbers from -0.1 to 1.1)
x <- seq(-0.1, 1.1, length=1000)
# calculate uniform distribution probabilities
y <- dunif(x, min = 0, max = 1)
# plot uniform distribution
plot(x, y, type = 'l')
plot(x, y, type = 'l', lwd = 3, col = 'steelblue', xlab = 'x', ylab = 'Probability', main = 'Uniform [0,1] Distribution Plot')

# Area under the curve between 0.25 and 0.75
punif(0.75) - punif(0.25)

# x value corresponding to 75th percentile
qunif(0.75)

# 28 pseudo-random numbers from the uniform distribution
set.seed(3)
runif(28)

# Mean, variance, standard deviation
mean(runif(1e5)) # (a+b)/2
var(runif(1e5))  # (b-a)^2 / 12
1/12
sd(runif(1e5))   # sqrt((b-a)^2 / 12)
sqrt(1/12)




# Bernoulli distribution

# https://uw-statistics.github.io/Stat311Tutorial/discrete-distributions.html
# https://stats.idre.ucla.edu/r/codefragments/greek_letters/

par(mfrow=c(1,3))
barplot(names.arg = 0:1, 
        height = dbinom(0:1, size = 1, p = 0.3), ylim = c(0,1),
        main = expression(paste("Bernoulli PDF ", pi, " = 0.3")), xlab = 'X', ylab = 'Probability')
barplot(names.arg = 0:1, 
        height = dbinom(0:1, size = 1, p = 0.5), ylim = c(0,1),
        main = expression(paste("Bernoulli PDF ", pi, " = 0.5")), xlab = 'X', ylab = 'Probability')
barplot(names.arg = 0:1, 
        height = dbinom(0:1, size = 1, p = 0.7), ylim = c(0,1),
        main = expression(paste("Bernoulli PDF ", pi, " = 0.7")), xlab = 'X', ylab = 'Probability')
par(mfrow=c(1,1))


# Simulate from the Bernoulli with parameter pi
sims <- 1000
bernpi <- 0.25
u <- runif(sims)
y <- as.integer(u < bernpi)
y
sum(y) / sims


# Mean, variance, standard deviation
mean(rbinom(1e5, size = 1, p = 0.3))
var( rbinom(1e5, size = 1, p = 0.3))
sd(  rbinom(1e5, size = 1, p = 0.3))


# 162 draws from the Bernoulli distribution
# In R, Binomial(size = 1) equals Bernoulli
set.seed(3)
rbinom(162, size = 1, p = 0.3)




# Binomial distribution

# https://uw-statistics.github.io/Stat311Tutorial/discrete-distributions.html#the-binomial-distribution

par(mfrow = c(1,2))
barplot(height = dbinom(0:20, size = 20, p = 0.7), 
        names.arg = 0:20, 
        ylim = c(0,1),
        main = expression(paste("Binomial PDF (20 trials, ", pi, " = 0.7)")), xlab = 'X', ylab = 'Probability',
        col = c(rep("blue", 15), rep("gray", 8)))
barplot(height = pbinom(0:20, size = 20, p = 0.7), 
        names.arg = 0:20, 
        ylim = c(0,1),
        main = expression(paste("Binomial CDF (20 trials, ", pi, " = 0.7)")), xlab = 'X', ylab = 'Probability',
        col = c(rep("gray", 14), "blue", rep("gray", 6)))
par(mfrow = c(1,1))


# PDSwR Appendix B pg. 343
numflips <- 20
x <- 0:numflips


# probability of heads for several different coins
p <- c(0.05, 0.15, 0.5, 0.7)
plabels <- paste("p =", p)


# calculate the probability of seeing x heads in numflips flips for all the coins
flips <- NULL
for ( i in 1:length(p) ) {
  coin <- p[i]
  label <- plabels[i]
  tmp <- data.frame(number.of.heads = x,
                    probability = dbinom(x, numflips, coin),
                    coin.type = label)
  flips <- rbind(flips, tmp)
}


# plot it

# v1: lines
ggplot(flips, aes(x = number.of.heads, y = probability)) +
  geom_point(aes(color = coin.type, shape = coin.type)) +
  geom_line(aes(color = coin.type)) +
  theme_classic()

# v2: bars
flips %>%
  group_by(coin.type) %>%
  ggplot(., aes(x = number.of.heads, y = probability)) +
  geom_point(aes(color = coin.type, shape = coin.type, cex = 1.1)) +
  geom_segment(aes(xend = number.of.heads, yend = 0, color = coin.type)) +
  theme_classic() +
  guides(size = "none")


# Mean, variance, standard deviation
mean(rbinom(1e5, size = 10, p = 0.5)) # Np
var( rbinom(1e5, size = 10, p = 0.5)) # Np(1-p)
sd(  rbinom(1e5, size = 10, p = 0.5)) # sqrt( Np(1-p) )




# Normal distribution

x <- seq(from = -5, to = 5, length.out = 1000)  # the interval [-5 5]
f <- dnorm(x)                                   # normal with mean 0 and sd 1
ggplot(data.frame(x=x, y=f), aes(x=x, y=y)) + 
  geom_line()


# PDSwR Appendix B pg. 335
u <- rnorm(1000)
ggplot(data.frame(x=u), aes(x=x)) +
  geom_density() +
  geom_line(data = data.frame(x=x, y=f), aes(x=x, y=y), linetype = 2) +
  labs(title = "Empirical distribution of points from N(0,1) and theoretical normal distribution (dotted)") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))


# https://uw-statistics.github.io/Stat311Tutorial/continuous-distributions.html#the-normal-distribution
hist(u, freq = FALSE, main = "Histogram of Normal data")


plot(density(u), 
     xlab = "x", ylab = "Density",
     main = "Approximate Distribution")


curve(dnorm(x), 
      xlim = c(-3, 3),
      main = "The Standard Normal Distribution", ylab = "Density")


curve(dnorm(x, mean = 2, sd = 0.5), 
      xlim = c(-4, 4), col = "red",
      main = "Normal Distributions", ylab = "Density")
curve(dnorm(x, mean = -1, sd = 1),
      add = TRUE, 
      col = "blue")
text(x = c(-1, 2), y = c(0.2, 0.4),         # adds some text to the plot
     labels = c("N(-1, 1.0)", "N(2, 0.5)"),
     col = c("blue", "red"))


# PDSwR Appendix B pg. 337
# calculate the 75th percentile
qnorm(0.75)
pnorm(0.6744898)  # result of qnorm(0.75)


# Illustrate 75th percentile
x <- seq(from=-5, to=5, length.out=1000)
f <- dnorm(x)
nframe <- data.frame(x=x, y=f)

# calculate the 75th percentile
line <- qnorm(0.75)
xstr <- sprintf("qnorm(0.75) = %.3f", line) #sprintf("qnorm(0.75) = %1.3f", line)

# to the left of the 75th percentile
nframe75 <- subset(nframe, nframe$x < line)

# plot it
ggplot(nframe, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data = nframe75, aes(x=x, y=y), fill = "steelblue") +
  geom_vline(aes(xintercept = line), linetype = 2) +
  annotate(geom = "text", x = 0.55, y = 0, label = xstr, vjust = 1.5)


# 50% of the observations will be less than the mean
pnorm(0)


# About 2.3% of the observations are more than 2 
# standard deviations below the mean
pnorm(-2)


# About 95.4% of the observations are within 2
# standard deviations from the mean
pnorm(2) - pnorm(-2)




# Lognormal distribution

u <- rlnorm(1e5)

# the mean of u is higher than the median
mean(u)
median(u)


# the mean of log(u) is approx meanlog=0
mean(log(u))


# the sd of log(u) is approx sdlog=1
sd(log(u))


# generate the lognormal with meanlog=0 and sdlog=1
x <- seq(from=0, to=25, length.out=500)
f <- dlnorm(x)


# generate a normal with mean=0 and sd=1
x2 <- seq(from=-5, to=5, length.out=500)
f2 <- dnorm(x2)


# plot density plots with theoretical curves superimposed
lnormframe <- data.frame(x=x, y=f)
normframe <- data.frame(x=x2, y=f2)
dframe <- data.frame(u=u)

p1 <- ggplot(dframe, aes(x=u)) +
  geom_density() +
  geom_line(data = lnormframe, aes(x = x, y = y), linetype = 2) +
  labs(title = "Lognormal: meanlog = 0 , sdlog = 1") +
  theme(plot.title=element_text(hjust=0.5))

p2 <- ggplot(dframe, aes(x=log(u))) +
  geom_density() +
  geom_line(data = normframe, aes(x = x, y = y), linetype = 2) +
  labs(title = "Normal: mean = 0 , sd = 1") +
  theme(plot.title=element_text(hjust=0.5))

p1 + p2 +
  plot_layout(ncol = 1)


# the 50th percentile (median) of the lognormal
# with meanlog=0 and sdlog=1
qlnorm(0.5)


# the probability of seeing a value x at most 1
plnorm(1)


# the probability of seeing a value x at most 10
plnorm(10)


# calculate the 75th percentile
line <- qlnorm(0.75)
xstr <- sprintf("qlnorm(0.75) = %.3f", line)

# to the left of the 75th percentile
lnormframe75 <- subset(lnormframe, lnormframe$x < line)

# plot it
ggplot(lnormframe, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data = lnormframe75, aes(x=x, y=y), fill = "steelblue") +
  geom_vline(aes(xintercept = line), linetype = 2) +
  annotate(geom = "text", x = 3.75, y = 0, label = xstr, vjust = 1.5) +
  labs(title = "The 75th percentile of the lognormal with meanlog=0, sdlog=1") +
  theme(plot.title=element_text(hjust=0.5))




# Exponential distribution

# PDF
par(mfrow = c(1,3))
curve(dexp(x, rate = 0.1), 
      xlim = c(0, 50), ylim = c(0, 1),
      main = "PDF of Exp(rate = 0.1)", ylab = "Density")
curve(dexp(x, rate = 0.25), 
      xlim = c(0, 50), ylim = c(0, 1),
      main = "PDF of Exp(rate = 0.25)", ylab = "Density")
curve(dexp(x, rate = 1.0), 
      xlim = c(0, 50), ylim = c(0, 1),
      main = "PDF of Exp(rate = 1.0)", ylab = "Density")
par(mfrow = c(1,1))


# CDF
curve(pexp(x, rate = 0.2), 
      xlim = c(0, 50),
      main = "CDF of Exp(rate = 0.2)", ylab = "Probability")


# the 50th percentile (median) of Exponential(rate = 1)
qexp(0.5, rate = 1)
pexp(qexp(0.5, rate = 1), rate = 1)


# the probability of seeing a value x at most 2
pexp(2, rate = 1)


# the probability of seeing a value x at most 5
pexp(5, rate = 1)


# Example (mean = 4 minutes , rate = 1/4 = 0.25)
# https://courses.lumenlearning.com/introstats1/chapter/the-exponential-distribution/
line1 <- qexp(pexp(4, rate = 0.25), rate = 0.25)
line2 <- qexp(pexp(5, rate = 0.25), rate = 0.25)
prob_diff <- pexp(5, rate = 0.25) - pexp(4, rate = 0.25)
xstr <- sprintf("Shaded area represents P(4 < x < 5) = %.3f", prob_diff)

# to the left of the 75th percentile
x <- seq(from=0, to=20, length.out=500)
f <- dexp(x, rate = 0.25)
expframe <- data.frame(x=x, y=f)
expframe4to5 <- subset(expframe, expframe$x >= line1 & expframe$x <= line2)

# plot it
ggplot(expframe, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data = expframe4to5, aes(x=x, y=y), fill = "steelblue") +
  geom_vline(aes(xintercept = line1), linetype = 2) +
  geom_vline(aes(xintercept = line2), linetype = 2) +
  annotate(geom = "text", x = 8, y = 0, label = xstr, vjust = 1.5) +
  labs(title = "Exponential distribution (mean = 4 minutes , rate = 1/4 = 0.25)", 
       subtitle = xstr) +
  theme(plot.title=element_text(hjust=0.5), 
        plot.subtitle=element_text(hjust=0.5))




# Poisson distribution

# PDF
x <- seq.int(from = 0, to = 51, by = 1)
par(mfrow = c(3,1))
barplot(height = dpois(x, lambda = 1.8), 
        names.arg = 0:51, 
        main = expression(paste("dpois(y|", lambda, " = 1.8)")), 
        xlab = 'y', 
        ylab = 'p(y)')
barplot(height = dpois(x, lambda = 8.3), 
        names.arg = 0:51, 
        main = expression(paste("dpois(y|", lambda, " = 8.3)")), 
        xlab = 'y', 
        ylab = 'p(y)')
barplot(height = dpois(x, lambda = 32.1), 
        names.arg = 0:51, 
        main = expression(paste("dpois(y|", lambda, " = 32.1)")), 
        xlab = 'y', 
        ylab = 'p(y)')
par(mfrow = c(1,1))


# CDF
x <- seq.int(from = 0, to = 10, by = 1)
barplot(height = ppois(x, lambda = 1.8), 
        names.arg = 0:10, 
        ylim = c(0, 1),
        main = "CDF of Poisson(lambda = 1.8)", 
        ylab = "Probability")


# the 50th percentile (median) for Poisson(lambda = 1.8)
qpois(0.5, lambda = 1.8)


# the probability of 2 occurrences for Poisson(lambda = 1.8) 
dpois(2, lambda = 1.8)


# the probability of at most 2 occurrences for Poisson(lambda = 1.8)
ppois(2, lambda = 1.8)


# PDF and CDF of Poisson(lambda = 1.8)
x <- seq.int(from = 0, to = 10, by = 1)
par(mfrow = c(1,2))
barplot(height = dpois(x, lambda = 1.8), 
        names.arg = 0:10, 
        col = c(rep("blue", 3), rep("gray", 9)),
        ylim = c(0,1),
        main = expression(paste("dpois(y|", lambda, " = 1.8)")), 
        xlab = 'y', 
        ylab = 'p(y)')
barplot(height = ppois(x, lambda = 1.8), 
        names.arg = 0:10, 
        col = c(rep("gray", 2), "blue", rep("gray", 9)),
        ylim = c(0,1),
        main = expression(paste("dpois(y|", lambda, " = 1.8)")), 
        xlab = 'y', 
        ylab = 'p(y <= x)')
par(mfrow = c(1,1))




# Binomial distribution
# https://uw-statistics.github.io/Stat311Tutorial/discrete-distributions.html#the-binomial-distribution
ps <- seq(from=0.1, to=0.9, by=0.1)

par(mfrow = c(3,3))
for (p in ps) {
  barplot(height = dbinom(0:20, size = 20, p = p), 
          names.arg = 0:20, 
          #ylim = c(0,1), 
          main = paste("B(20," , p , ")", sep=""), 
          xlab = 'X', 
          ylab = 'Probability')
}
par(mfrow = c(1,1))


# Display table of k B(20,p) probability values for p=0.1 to p=0.9
# Create table
B20 <- tibble(0:20, dbinom(0:20, size = 20, p = 0.1)) %>% rename(k = `0:20`, `p=0.1` = `dbinom(0:20, size = 20, p = 0.1)`)

for (i in 2:length(ps)) B20 <- B20 %>% 
  add_column( tibble(0:20, dbinom(0:20, size = 20, p = ps[i])) %>% 
                dplyr::select(2) %>% 
                rename("p=0.{i}" := `dbinom(0:20, size = 20, p = ps[i])`) )
# Display table
B20 %>% arrange(desc(k)) %>% print(n=21)


# Display table of probabilities as a heatmap
B20 %>%
  pivot_longer(!k, names_to = "p", values_to = "probability") %>%
  ggplot(., aes(k, p)) +
  geom_tile(aes(fill = probability)) + 
  geom_text(size = 3, aes(label = round(probability, 3))) +
  scale_fill_gradient(low = "white", high = "blue")


# Plot binomial distribution and normal distribution overlay
# https://stackoverflow.com/questions/60546225/plotting-the-normal-and-binomial-distribution-in-same-plot
n <- 100
p <- 0.10

dev <- 4
mu <- n*p
sigma <- sqrt(n*p*(1 - p))

xmin <- round(max(mu - dev*sigma,0));
xmax <- round(min(mu + dev*sigma,n))
x <- seq(xmin, xmax)
y <- dbinom(x,n,p)

plot(x, y, type = 'h', lwd = 30, lend = 3, col = 'steelblue', las = 1, bty = 'l') # ylim = c(0, 0.08)  ann = FALSE,   yaxs = 'i',
title(main = sprintf('Binomial distribution, n=%s, p=%.2f', n, p))
lines(x, dnorm(x, mean = mu, sd = sigma), col = 'darkgray', lwd = 5)

xx <- seq(min(x), max(x), length.out = 1000)
lines(xx, dnorm(xx, mean = mu, sd = sigma), col = 'white')


# Alternatively
# IPS by Moore & McCabe pg. 385
# Normal approximation MORE accurate when near 0.5
n <- 100
p <- 0.50

dev <- 4
mu <- mean <- n*p
sd <-   sqrt(n*p*(1-p))
binwidth <-   0.005

xmin <- round(max(mu - dev*sigma,0));
xmax <- round(min(mu + dev*sigma,n))
x <- seq(xmin, xmax)
y <- dbinom(x,n,p)

df <- cbind.data.frame(x, y)

ggplot(df, aes(x = x, y = y)) +
  geom_bar(stat="identity", fill = 'dodgerblue3')+
  stat_function( 
    fun = function(x, mean, sd, n, bw){ 
      dnorm(x = x, mean = mean, sd = sd)
    }, col = "darkgray", size=I(1.4),  
    args = c(mean = mean, sd = sd, n = n, bw = binwidth)) +
  labs(title = "Probability histogram and normal approximation for binomial distribution, n=100, p=0.50", 
       x = "X", y = "Probability") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))


# Normal approximation LESS accurate when near 0 or 1
n <- 100
p <- 0.05

dev <- 4
mu <- mean <- n*p
sd <-   sqrt(n*p*(1-p))
binwidth <-   0.005

xmin <- round(max(mu - dev*sigma,0));
xmax <- round(min(mu + dev*sigma,n))
x <- seq(xmin, xmax)
y <- dbinom(x,n,p)

df <- cbind.data.frame(x, y)

ggplot(df, aes(x = x, y = y)) +
  geom_bar(stat="identity", fill = 'dodgerblue3')+
  stat_function( 
    fun = function(x, mean, sd, n, bw){ 
      dnorm(x = x, mean = mean, sd = sd)
    }, col = "darkgray", size=I(1.4),  
    args = c(mean = mean, sd = sd, n = n, bw = binwidth)) +
  labs(title = "Probability histogram and normal approximation for binomial distribution, n=100, p=0.05", 
       x = "X", y = "Probability") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))


# Averages are less variable than individual observations
xs <- seq(from=45, to=135, by=1)
f <- dnorm(xs, mean = 90, sd = 15)
ggplot(data.frame(x=xs, y=f), aes(x=xs, y=y)) +
  xlim(45,135) +
  geom_line()

normSamps1000 <- replicate(n = 1000, mean(rnorm(n = 25, mean = 90, sd = 15)))
samp_mean <- mean(normSamps1000)
samp_sd <- sd(normSamps1000)

n <- 1000
mu <- mean <- 90
sd <- 15
binwidth <- 0.005

# https://r-charts.com/distribution/histogram-density-ggplot2/
ggplot(data.frame(x = normSamps1000), aes(x = x)) +
  geom_histogram(aes(y = ..density..), fill = "steelblue", color = "white") +
  stat_function( 
    fun = function(x, mean, sd, n, bw){ 
      dnorm(x = x, mean = mean, sd = sd)
    }, col = "darkgray", size=I(1.4),  
    args = c(mean = mean, sd = sd, n = n, bw = binwidth)) +
  xlim(45,135) +
  labs(title = "Averages are less variable than individual observations",
       subtitle = "Population: N(mean=90,sd=15)\nDistribution of sample means from 1000 samples of size 25 (mean=90,sd=15/sqrt(25)=3)",
       x = "X", y = "Density") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))


# Averages are more normal than individual observations
e1000 <- rexp(n = 1e3, rate = 0.2)
hist(e1000, xlim = c(0, 30), 
     xlab = "x", main = "1,000 Exp(0.2) observations")
e1000bar <- replicate(n = 1000, mean(rexp(n = 1e3, rate = 0.2)))
hist(e1000bar, #breaks = 10,
     xlab = "x", main = "1000 means from samples of size 1,000")


# Build dataset with different distributions
data <- data.frame(
  Type = c( rep("Observations from exp(0.2) distribution", 1000), rep("Means from samples of size 1,000", 1000) ),
  Value = c( e1000, e1000bar )
)


# Represent it
ggplot(data, aes(x=Value, fill=Type)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, color="#e9ecef", alpha=1.0, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  xlim(0,15) +
  labs(y = "Density", title = "Averages are more normal than individual observations") +
  theme_minimal() +
  theme(legend.position="bottom",
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))




# Law of Large Numbers

# https://uw-statistics.github.io/Stat311Tutorial/limit-theorems.html

set.seed(1234)
flips <- sample(c("heads", "tails"), size = 2000, replace = TRUE)
plot(cumsum(flips == "heads") / (1:length(flips)), 
     type = "l", ylim = c(0,1),  
     main = "Coin Flips", 
     xlab = "Number of flips", ylab = "Proportion of heads")
abline(h = 0.5, col = "red")


# Means of rolls of a 6-sided die, increasing sample sizes
# https://stats.stackexchange.com/questions/496623/law-of-large-numbers
set.seed(15)
N <- 2000
m <- 6
vec.mean <- numeric(N)

for (i in 1:N) {
  vec.mean[i] <- mean(sample(1:m, i, replace = TRUE))
}
plot(vec.mean)
abline(h = 3.5, col = "red")


# Tosses of 6-sided die
n.sims <- 1000
Ns <- c(1e1, 1e2, 1e3, 1e4, 1e5)
avg.val <- matrix(NA, nrow = n.sims, ncol = length(Ns))
for (n in 1:length(Ns)) {
  for (s in 1:n.sims){
    vals <- sample(1:m, Ns[n], replace = TRUE)
    avg.val[s,n] <- mean(vals)
  }
}

par(mfrow = c(5,1), 
    mar=c(1, 1, 1, 1) + 0.1) # mar/oma: bottom, left, top, right
hist(avg.val[,1] , 
     main = sprintf("Mean of 1000 draws of %d 6-sided die", 
                    Ns[1]), xlim = c(1,6), xlab = "avg.val")
hist(avg.val[,2] , 
     main = sprintf("Mean of 1000 draws of %d 6-sided die", 
                    Ns[2]), xlim = c(1,6), xlab = "avg.val")
hist(avg.val[,3] , 
     main = sprintf("Mean of 1000 draws of %s 6-sided die", 
                    number_as_comma(Ns[3])), xlim = c(1,6), xlab = "avg.val")
hist(avg.val[,4] , 
     main = sprintf("Mean of 1000 draws of %s 6-sided die", 
                    number_as_comma(Ns[4])), xlim = c(1,6), xlab = "avg.val")
hist(avg.val[,5] , 
     main = sprintf("Mean of 1000 draws of %s 6-sided die", 
                    number_as_comma(Ns[5])), xlim = c(1,6), xlab = "avg.val")
par(mfrow = c(1,1))


# Coin flips: mean = 0.5
n.sims <- 1000
Ns <- c(1e1, 1e2, 1e3, 1e4, 1e5)
avg.val <- matrix(NA, nrow = n.sims, ncol = length(Ns))
for (n in 1:length(Ns)) {
  for (s in 1:n.sims){
    vals <- rbinom(n = Ns[n], size = 1, prob = 0.5)
    avg.val[s,n] <- mean(vals)
  }
}

par(mfrow = c(5,1), 
    mar=c(1, 1, 1, 1) + 0.1) # mar/oma: bottom, left, top, right
hist(avg.val[,1] , 
     main = sprintf("Mean of 1000 draws of %d bernoulli random variables", 
                    Ns[1]), xlim = c(0,1), xlab = "avg.val")
hist(avg.val[,2] , 
     main = sprintf("Mean of 1000 draws of %d bernoulli random variables", 
                    Ns[2]), xlim = c(0,1), xlab = "avg.val")
hist(avg.val[,3] , 
     main = sprintf("Mean of 1000 draws of %s bernoulli random variables", 
                    number_as_comma(Ns[3])), xlim = c(0,1), xlab = "avg.val")
hist(avg.val[,4] , 
     main = sprintf("Mean of 1000 draws of %s bernoulli random variables", 
                    number_as_comma(Ns[4])), xlim = c(0,1), xlab = "avg.val")
hist(avg.val[,5] , 
     main = sprintf("Mean of 1000 draws of %s bernoulli random variables", 
                    number_as_comma(Ns[5])), xlim = c(0,1), xlab = "avg.val")
par(mfrow = c(1,1), mar=c(5, 4, 4, 2) + 0.1)




# Central Limit Theorem

# https://uw-statistics.github.io/Stat311Tutorial/limit-theorems.html

set.seed(1727498)
e10   <- rexp(n = 1e1, rate = 0.2)
e100  <- rexp(n = 1e2, rate = 0.2)
e1000 <- rexp(n = 1e3, rate = 0.2)


# exponential distributions not normally distributed
par(mfrow = c(1, 3))
hist(e10, xlim = c(0, 30), 
     xlab = "x", main = "10 Exp(0.2) observations")
hist(e100, xlim = c(0, 30), 
     xlab = "x", main = "100 Exp(0.2) observations")
hist(e1000, xlim = c(0, 30), 
     xlab = "x", main = "1,000 Exp(0.2) observations")


# sampling distributions of the sample mean of the 
# exponential distributions are normally distributed
set.seed(81793)
e10bar <- replicate(n = 1000, mean(rexp(n = 1e1, rate = 0.2)))
e100bar <- replicate(n = 1000, mean(rexp(n = 1e2, rate = 0.2)))
e1000bar <- replicate(n = 1000, mean(rexp(n = 1e3, rate = 0.2)))

par(mfrow = c(1, 3))
hist(e10bar, #breaks = 10,
     xlab = "x", main = "1000 means from samples of size 10")
hist(e100bar, #breaks = 10,
     xlab = "x", main = "1000 means from samples of size 100")
hist(e1000bar, #breaks = 10,
     xlab = "x", main = "1000 means from samples of size 1,000")


# test/confirm normally distributed
par(mfrow = c(1, 3))
qqnorm(e10bar, pch = 16, col = rgb(0, 0, 0, alpha = 0.5),
       main = "Sample size = 10",
       sub = "1000 repetitions")
qqline(e10bar, col = rgb(1, 0, 0, 0.6))
qqnorm(e100bar, pch = 16, col = rgb(0, 0, 0, alpha = 0.5),
       main = "Sample size = 100",
       sub = "1000 repetitions")
qqline(e100bar, col = rgb(1, 0, 0, 0.6))
qqnorm(e1000bar, pch = 16, col = rgb(0, 0, 0, alpha = 0.5),
       main = "Sample size = 1,000",
       sub = "1000 repetitions")
qqline(e1000bar, col = rgb(1, 0, 0, 0.6))
par(mfrow = c(1, 1))




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




# Sampling and Simulation

# Marbles
marbles = c('red', 'blue', 'green')
sample(x = marbles, size = 2, replace = FALSE)

# Dice rolls
# 1 die
sample(x = 1:6, size = 1, replace = TRUE)
# 2 dice
sample(x = 1:6, size = 2, replace = TRUE)
# sum of 2 dice
sum(sample(x = 1:6, size = 2, replace = TRUE))

# http://ditraglia.com/Econ103Public/Rtutorials/Rtutorial4.html
my.dice.sum <- function(n.dice, n.sides){
  dice <- sample(x = 1:n.sides, size = n.dice, replace = TRUE)
  return(sum(dice))
}

sims <- replicate(1e3, my.dice.sum(n.dice = 2, n.sides = 6))

table(sims)

table(sims) / length(sims)

plot(table(sims), xlab = "Sum", ylab = "Frequency", main = "1,000 Rolls of 2 Fair Dice")


# PDSwR Appendix B pg. 345

p = 0.5  # the percentage of females in this student population
class.size <- 20  # size of a classroom
num.classes <- 100  # how many classrooms we observe

# what might a typical outcome look like?
num.females <- rbinom(n = num.classes, size = class.size, prob = p)
num.females

# the theoretical counts (not necessarily integral)
probs <- dbinom(x = 0:class.size, size = class.size, prob = p)
tcount <- num.classes*probs

# plot it
zero <- function(x) {0}  # a dummy function that returns only 0

ggplot(data = data.frame(number.of.females = num.females, dummy = 1),
       aes(x = number.of.females, y = dummy)) +
  stat_summary(fun = "sum", geom = "point", size = 2) +
  stat_summary(fun.max = "sum", fun.min = "zero", geom = "linerange") +
  # superimpose the theoretical number of times you see x heads
  geom_line(data = data.frame(x = 0:class.size, y = probs),
            aes(x = x, y = tcount), linetype = 2) +
  scale_x_continuous(breaks = 0:class.size, labels = 0:class.size) +
  scale_y_continuous("number of classrooms") +
  labs(title = "Simulated and theoretical (dashed line) distributions of\nnumber of females in 100 20-person classrooms\nwhen P(female)=0.5") +
  theme(plot.title=element_text(hjust=0.5))


# SR pg. 52
# Sampling from a grid-approximate posterior
p_grid <- seq( from = 0 , to = 1 , length.out = 1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( x = 6 , size = 9 , prob = p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

samples <- sample( x = p_grid , size = 1e4 , prob = posterior , replace = TRUE )

par(mfrow = c(1, 2))
plot( samples , ylim = c(0,1) , col = "steelblue" , xlab = "sample number" , ylab = "proportion water (p)" , main = "10,000 samples from posterior implied by data and model" )
dens( samples , xlim = c(0,1) , lwd = 2 , col = "steelblue" , xlab = "proportion water (p)" , main = "Density of samples (vertical) at each parameter value (horizontal)" )
par(mfrow = c(1, 1))


# MHM pg. 137
# using a loop
n.sims <- 1000
n.girls <- rep(NA, n.sims)
for (s in 1:n.sims){ n.girls[s] <- rbinom(n = 1, size = 400, prob = 0.488) }
hist(n.girls, col = "steelblue", main = "")

# using the binomial distribution directly
n.girls <- rbinom(n = n.sims, size = 400, prob = 0.488)
hist(n.girls, col = "steelblue", main = "Distribution of number of girls in 400 births\nbased on 1,000 simulations, where P(girl) = 0.488")

# Distribution of average height pg. 139
n.sims <- 1000
avg.height <- rep(NA, n.sims)
max.height <- rep(NA, n.sims)
for (s in 1:n.sims){
  female <- rbinom(n = 10, size = 1, prob = 0.52)
  height <- ifelse(female==1, rnorm(n = 1, mean = 63.7, sd = 2.7), rnorm(n = 1, mean = 69.1, sd = 2.9))
  avg.height[s] <- mean(height)
  max.height[s] <-  max(height)
}
hist(avg.height, col = "steelblue", main = "Distribution of average height of 10 adults in the USA")

# simulation with custom function pg. 139
Height.sim <- function(n.adults){
  female <- rbinom(n = 10, size = 1, prob = 0.52)
  height <- ifelse(female==0, rnorm(n = 1, mean = 69.1, sd = 2.9), rnorm(n = 1, mean = 63.7, sd = 2.7))
  return(mean(height))
}
avg.height <- replicate(n = 1000, expr = Height.sim(n.adults = 10))
hist(avg.height, col = "steelblue", main = "Distribution of average height of 10 adults in the USA")


# Monte Hall's Let's Make a Deal
# 3. Data Generation Process (minute 18)
sims <- 1000
win_no_switch <- 0
win_switch <- 0
doors <- c(1, 2, 3)

for (i in 1:sims) {
  win_door <- sample(x = doors, size = 1)
  choice   <- sample(x = doors, size = 1)
  if (win_door == choice) 
    win_no_switch <- win_no_switch + 1
  doors_remaining <- doors[doors != choice]
  if( any(doors_remaining == win_door) ) 
    win_switch <- win_switch + 1
}

cat("Prob(Car | no switch) =" , win_no_switch / sims , "\n")
cat("Prob(Car | switch) =" , win_switch / sims , "\n")

# replicate: generate distribution for probability of winning if switch
Monte.Hall <- function(n.sims) {
  sims <- n.sims
  win_no_switch <- 0
  win_switch <- 0
  doors <- c(1, 2, 3)
  
  for (i in 1:sims) {
    win_door <- sample(x = doors, size = 1)
    choice   <- sample(x = doors, size = 1)
    if (win_door == choice) 
      win_no_switch <- win_no_switch + 1
    doors_remaining <- doors[doors != choice]
    if( any(doors_remaining == win_door) ) 
      win_switch <- win_switch + 1
  }
  
  return(win_switch / sims)
}

win_switch <- replicate(n = 500, expr = Monte.Hall(n.sims = 1000))
hist(win_switch, col = "steelblue", main = "Distribution of probability of winning if switch")


# The Birthday Problem
# 3. Data Generation Process (minute 24)
sims <- 1000
people <- 24
all_days <- seq(1, 365, 1)
same_day <- 0

for (i in 1:sims) {
  room <- sample(x = all_days, size = people, replace = TRUE)
  if( length( unique(room) ) < people ) same_day <- same_day + 1 
}

cat("Prob(at least two with same birthday):", same_day / sims, "\n")

# replicate: generate distribution for probability of >=2 same birthday in room of 24 people
Birthday.Problem <- function(n.sims) {
  sims <- n.sims
  people <- 24
  all_days <- seq(1, 365, 1)
  same_day <- 0
  
  for (i in 1:sims) {
    room <- sample(x = all_days, size = people, replace = TRUE)
    if( length( unique(room) ) < people ) same_day <- same_day + 1 
  }
  
  return(same_day / sims)
}

same_day <- replicate(n = 500, expr = Birthday.Problem(n.sims = 1000))
hist(same_day, col = "steelblue", main = "Distribution of probability of >=2 same birthday in room of 24 people")


# calculate the probability of >=2 people having the same birthday
# as the number of people in the room increases from 2 to 50
n.sims <- 1000
all.days <- seq(1, 365, 1)
num.people <- seq(2, 50, 1)
avg.prob.same.day <- rep(NA, nrow = length(num.people))
for (n in 1:length(num.people)) {
  same_day <- 0
  for (s in 1:n.sims){
    room <- sample(x = all.days, size = n, replace = TRUE)
    if( length( unique(room) ) < n ) same_day <- same_day + 1 
  }
  avg.prob.same.day[n] <- same_day / n.sims
}

plot(x = num.people, y = avg.prob.same.day, type = "b", col = "steelblue", 
     xlim = c(2, 50), ylim = c(0.0, 1.0),
     xlab = "Number of people in room", ylab = "Avg probability of 2+ people with same birthday", 
     main = "Average probablity of 2+ people with same birthday\nby number of people in room")
axis(1, seq(2, 50, 2))
abline(v=24,  lwd = 2, lty = "dashed", col="grey")
abline(h=0.5, lwd = 2, lty = "dashed", col="grey")




# Mean and standard deviation for prior
kidiq <- read.csv(url("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv"))
head(kidiq)

paste("mean(kid_score) =", mean(kidiq$kid_score))
paste("sd(kid_score) =",   sd(kidiq$kid_score))

paste("mean(mom_iq) =", mean(kidiq$mom_iq))
paste("sd(mom_iq) =",   sd(kidiq$mom_iq))


# SR pg. 89 Simple linear regression
mom_iq_mean <- mean(kidiq$mom_iq)
mom_iq_centered = kidiq$mom_iq - mom_iq_mean

fit_lognormal <- quap( 
  alist(
    kid_score ~ dnorm( mu , sigma ) ,
    mu <- a + b*( mom_iq - mom_iq_mean ) ,
    a ~ dnorm( 87 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dexp( 1 )
  ) , data = kidiq)

precis(fit_lognormal, prob = 0.89, digits = 2)


# SR pg. 95 (Prior predictive simulation)
a_prior <- rnorm(n = 1e6 , mean = 87 , sd = 20)
dens( a_prior , xlim = c(10,165) , xlab = "Prior on intercept" )  # norm.comp = TRUE

b_prior <- rlnorm(n = 1e6 , meanlog = 0 , sdlog = 1)
dens( b_prior , xlim = c(0,5) , adj = 0.1 , xlab = "Prior on slope coefficient" )

sigma_prior <- rexp(n = 1e6 , rate = 1 )
dens( sigma_prior , xlim = c(0,10) , xlab = "Prior on sigma" )

set.seed(2971)
N <- 100  # 100 lines
a <- rnorm( N , 87 , 20 )
b <- rlnorm( N , 0 , 1 )
plot( NULL , xlim = range(kidiq$mom_iq) , ylim = c(-100,400) , xlab = "mom_iq" , ylab = "kid_score" )
abline( h = min(kidiq$kid_score) , lty = 2 )
text( 72 , min(kidiq$kid_score)+7 , "min(kid_score)" , cex = .8 )
abline( h = max(kidiq$kid_score) , lty = 2 )
text( 72 , max(kidiq$kid_score)+7 , "max(kid_score)" , cex = .8 )
mtext( "Prior predictive simulation:  log(b) ~ dnorm(0,1)")
xbar <- mean(kidiq$mom_iq)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) , 
                        from = min(kidiq$mom_iq) , to = max(kidiq$mom_iq) , add = TRUE ,
                        col = col.alpha("black", 0.2) )


# SR pg. 105-106
mom_iq_seq <- seq( from = min(kidiq$mom_iq) , to = max(kidiq$mom_iq) , by = 1 )
mu <- link( fit_lognormal , data = data.frame(mom_iq = mom_iq_seq) )
mu_mean <- apply( mu , 2 , mean )
mu_PI <- apply( mu , 2 , PI , prob = 0.89 )
plot( kid_score ~ mom_iq , data = kidiq , col = col.alpha(rangi2 , 0.5 ) )
lines( mom_iq_seq , mu_mean )
shade( mu_PI , mom_iq_seq )


# a: Normal distribution
x <- seq(from = -5, to = 205, length.out = 1000)  # the interval [-5 205]
f <- dnorm(x, 87 , 20)                            # normal with mean 87 and sd 20
ggplot(data.frame(x=x, y=f), aes(x=x, y=y)) +
  geom_line() +
  labs(title = "Normal distribution (mean = 87 , sd = 20)") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))

# b: Normal distribution
x <- seq(from = -35, to = 35, length.out = 1000)  # the interval [-35 35]
f <- dnorm(x, 0 , 10)                            # normal with mean 0 and sd 10
ggplot(data.frame(x=x, y=f), aes(x=x, y=y)) +
  geom_line() +
  labs(title = "Normal distribution (mean = 0 , sd = 10)") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))

# sigma: Exponential distribution
x <- seq(from = -1, to = 10, length.out = 1000)   # the interval [-1 10]
f <- dexp(x, rate = 1)                            # exponential with rate 1
ggplot(data.frame(x=x, y=f), aes(x=x, y=y)) +
  geom_line() +
  labs(title = "Exponential distribution (rate = 1)") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))


# ROS pg. 132 (Specified priors)
prior_summary(stan_glm(kid_score ~ mom_iq_centered, data=kidiq, refresh = 0 ,
                       prior_intercept = normal( location = c(87) , scale = c(20) ) ,
                       prior = normal( location = c(0) , scale = c(1) ) ,
                       prior_aux = exponential( 1 ) ) )

fit_2b <- stan_glm(kid_score ~ mom_iq_centered, data=kidiq, refresh = 0,
                   prior_intercept = normal( location = c(87) , scale = c(20) ) ,
                   prior = normal( location = c(0) , scale = c(1) ) ,
                   prior_aux = exponential( 1 ) )
print(fit_2b)


# Draw samples from the posterior
post <- extract.samples(fit_lognormal)
post[1:10,]


# Plot the posterior distribution of a
ggplot(data.frame(x=post$a), aes(x=x)) +
  geom_density() +
  labs(title = "Posterior distribution of a") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))

# Plot the posterior distribution of b
ggplot(data.frame(x=post$b), aes(x=x)) +
  geom_density() +
  labs(title = "Posterior distribution of b") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))

# Plot the posterior distribution of sigma
ggplot(data.frame(x=post$sigma), aes(x=x)) +
  geom_density() +
  labs(title = "Posterior distribution of sigma") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))




# Intervals of defined boundaries

# SR pg. 53
samples <- extract.samples(fit_lognormal)

sum( samples$b < 0.55 ) / length(samples$b)

sum( samples$b > 0.5 & samples$b < 0.7 ) / length(samples$b)


# PDSwR Appendix B pg. 337
b_mean <- mean(samples$b)
b_sd   <-   sd(samples$b)

# calculate probability less than parameter value 0.55
pnorm(0.55, b_mean, b_sd)
p0.55 <- pnorm(0.55, b_mean, b_sd)

# calculate parameter value at specific probability
qnorm(p0.55, b_mean, b_sd)

# plot percentile
x <- seq(from=0.3, to=0.9, length.out=1000)
f <- dnorm(x, b_mean, b_sd)
nframe <- data.frame(x=x, y=f)

# calculate the percentile
line <- qnorm(p0.55, b_mean, b_sd)
xstr <- sprintf("pnorm(0.55) = %.3f", p0.55) #sprintf("qnorm(0.75) = %1.3f", line)

# to the left of the p0.55
nframe55 <- subset(nframe, nframe$x < line)

# plot it
ggplot(nframe, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data = nframe55, aes(x=x, y=y), fill = "steelblue") +
  geom_vline(aes(xintercept = line), linetype = 2) +
  annotate(geom = "text", x = 0.5, y = 0, label = xstr, vjust = 1.5) +
  theme_classic()


# plot percentile
# calculate probability at two parameter values
pnorm(0.5, b_mean, b_sd)
p0.5 <- pnorm(0.5, b_mean, b_sd)

pnorm(0.7, b_mean, b_sd)
p0.7 <- pnorm(0.7, b_mean, b_sd)

x <- seq(from=0.3, to=0.9, length.out=1000)
f <- dnorm(x, b_mean, b_sd)
nframe <- data.frame(x=x, y=f)

# calculate the percentile
line5 <- qnorm(p0.5, b_mean, b_sd)
line7 <- qnorm(p0.7, b_mean, b_sd)
xstr5 <- sprintf("pnorm(0.5) = %.3f", p0.5) #sprintf("qnorm(0.75) = %1.3f", line)
xstr7 <- sprintf("pnorm(0.7) = %.3f", p0.7)

# between p0.5 and p0.7
nframe57 <- subset(nframe, (nframe$x > line5) & (nframe$x < line7) )

# plot it
ggplot(nframe, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data = nframe57, aes(x=x, y=y), fill = "steelblue") +
  geom_vline(aes(xintercept = line5), linetype = 2) +
  annotate(geom = "text", x = 0.45, y = 0, label = xstr5, vjust = 1.5) +
  geom_vline(aes(xintercept = line7), linetype = 2) +
  annotate(geom = "text", x = 0.65, y = 0, label = xstr7, vjust = 1.5) +
  theme_classic()




# Intervals of defined mass

quantile( samples$b , c( 0.1 , 0.9 ) )

# plot central mass
# calculate probability at two parameter values
qnorm(0.1, b_mean, b_sd)
p0.1 <- qnorm(0.1, b_mean, b_sd)

qnorm(0.9, b_mean, b_sd)
p0.9 <- qnorm(0.9, b_mean, b_sd)

x <- seq(from=0.3, to=0.9, length.out=1000)
f <- dnorm(x, b_mean, b_sd)
nframe <- data.frame(x=x, y=f)

# calculate the percentile
line1 <- qnorm(0.1, b_mean, b_sd)
line9 <- qnorm(0.9, b_mean, b_sd)
xstr1 <- sprintf("qnorm(0.1) = %.3f", line1) #sprintf("qnorm(0.75) = %1.3f", line)
xstr9 <- sprintf("qnorm(0.9) = %.3f", line9)

# between p0.1 and p0.9
nframe19 <- subset(nframe, (nframe$x > line1) & (nframe$x < line9) )

# plot it
ggplot(nframe, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data = nframe19, aes(x=x, y=y), fill = "steelblue") +
  scale_x_continuous(breaks = seq(0.3, 0.9, by=0.05)) +
  geom_vline(aes(xintercept = line1), linetype = 2) +
  annotate(geom = "text", x = 0.48, y = 0, label = xstr1, vjust = 1.5) +
  geom_vline(aes(xintercept = line9), linetype = 2) +
  annotate(geom = "text", x = 0.73, y = 0, label = xstr9, vjust = 1.5) +
  theme_classic()




# Point estimates

round( chainmode( samples$b ) , 3)

round( median( samples$b ) , 3)
round( mad( samples$b) , 3)

round( mean( samples$b ) , 3)
round( sd( samples$b) , 3)


# Plot them
pnorm( chainmode(samples$b) , b_mean , b_sd)
p_mode <- pnorm( chainmode(samples$b) , b_mean , b_sd)

pnorm( median(samples$b) , b_mean , b_sd)
p_median <- pnorm(median(samples$b) , b_mean , b_sd)

pnorm( mean(samples$b) , b_mean , b_sd)
p_mean <- pnorm(mean(samples$b) , b_mean , b_sd)

line_mode   <- qnorm(p_mode, b_mean, b_sd)
line_median <- qnorm(p_median, b_mean, b_sd)
line_mean   <- qnorm(p_mean, b_mean, b_sd)

xstr_mode   <- sprintf("qnorm(b_mode) = %.3f", line_mode) #sprintf("qnorm(0.75) = %1.3f", line)
xstr_median <- sprintf("qnorm(b_median) = %.3f", line_median)
xstr_mean   <- sprintf("qnorm(b_mean) = %.3f", line_mean)


x <- seq(from=0.3, to=0.9, length.out=1000)
f <- dnorm(x, b_mean, b_sd)
nframe <- data.frame(x=x, y=f)

# plot it
ggplot(nframe, aes(x=x, y=y)) +
  geom_line() +
  geom_vline(aes(xintercept = line_mode), linetype = 2) +
  annotate(geom = "text", x = 0.61, y = 0, label = xstr_mode, hjust = 0, vjust = 2) +
  geom_vline(aes(xintercept = line_median), linetype = 2) +
  annotate(geom = "text", x = 0.612, y = 0, label = xstr_median, hjust = 0, vjust = 0) +
  geom_vline(aes(xintercept = line_mean), linetype = 2) +
  annotate(geom = "text", x = 0.614, y = 0, label = xstr_mean, hjust = 0, vjust = -2) +
  theme_classic()

