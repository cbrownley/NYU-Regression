library("arm")
library("brms")
library("dplyr")
library("ggplot2")
library("ggridges")
library("lattice")
library("emmeans")
library("patchwork")
library("rstanarm")
library("rethinking")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library("tidyverse")
library("tidybayes")
library("bayesplot")




### Version 1: Tukey Honest Significant Differences ###
?TukeyHSD
head(warpbreaks)
unique(warpbreaks$wool)
unique(warpbreaks$tension)

# mean and sd of breaks by wool and tension
round( tapply(warpbreaks$breaks, list(warpbreaks$wool, warpbreaks$tension), mean), 2)
round( tapply(warpbreaks$breaks, list(warpbreaks$wool, warpbreaks$tension), sd), 2)

round( tapply(warpbreaks$breaks, list(warpbreaks$wool), mean), 2)
round( tapply(warpbreaks$breaks, list(warpbreaks$tension), mean), 2)

# plot of these means
plot.design(warpbreaks)

interaction.plot(warpbreaks$wool, warpbreaks$tension, warpbreaks$breaks)

# differences in means of breaks by tension
mean(warpbreaks[warpbreaks$tension == "M", "breaks"] - warpbreaks[warpbreaks$tension == "H", "breaks"])
mean(warpbreaks[warpbreaks$tension == "L", "breaks"] - warpbreaks[warpbreaks$tension == "H", "breaks"])
mean(warpbreaks[warpbreaks$tension == "L", "breaks"] - warpbreaks[warpbreaks$tension == "M", "breaks"])

# fit an analysis of variance (ANOVA) model
summary(fm1 <- aov(breaks ~ wool + tension, data = warpbreaks))
coef(fm1)

# compute and plot Tukey honest significant differences
TukeyHSD(fm1, "tension", ordered = TRUE, conf.level = 0.95)
plot(TukeyHSD(fm1, "tension"))




# Version 2: ANOVA as classical linear regression (contrasts with emmeans)
arm::display(lm1 <- lm(breaks ~ wool + tension, data = warpbreaks))
lm1_emm <- emmeans(lm1, pairwise ~ tension)
lm1_emm
pairs(lm1_emm)




# Version 3: ANOVA as Bayesian linear regressions (rstanarm & contrasts by-hand)
b1 <- stan_aov(breaks ~ wool + tension, data = warpbreaks, 
               prior = R2(location = 0.5), adapt_delta = 0.999, 
               seed = 12345)
summary(b1)


b2 <- stan_lmer(breaks ~ 1 + (1|wool) + (1|tension), 
                data = warpbreaks, 
                prior_intercept = cauchy(), 
                prior_covariance = decov(shape = 2, scale = 2), 
                adapt_delta = 0.999, seed = 12345)
summary(b2)


b3 <- stan_glm(breaks ~ wool + tension, data = warpbreaks, 
               prior_intercept=NULL, prior=NULL, prior_aux=NULL)
summary(b3)

postb3 <- as.matrix(b3)
dim(postb3)
head(postb3)

# compute contrasts
M_H <-  postb3[ , "tensionM"] - postb3[ , "tensionH"]
L_H <- -postb3[ , "tensionH"] # note the minus sign
L_M <- -postb3[ , "tensionM"] # note the minus sign
postb3 <- cbind(postb3, M_H)
postb3 <- cbind(postb3, L_H)
postb3 <- cbind(postb3, L_M)
head(postb3)

data.frame(postb3) %>%
  dplyr::select(M_H, L_H, L_M) %>%
  precis(. , prob = 0.95, digits = 2)

data.frame(postb3) %>%
  dplyr::select(M_H, L_H, L_M) %>%
  mcmc_areas(point_est = "mean", prob_outer = 0.95) +
  labs(x="Difference in breaks between tensions")

  
  

# Statistical Rethinking, 2nd Edition
summary(warpbreaks$breaks)

d <- warpbreaks
d$wool <- as.integer(warpbreaks$wool)
d$tension <- as.integer(warpbreaks$tension)

d$condition <- ifelse((warpbreaks$wool == "A") & (warpbreaks$tension == "L"), 1,
               ifelse((warpbreaks$wool == "A") & (warpbreaks$tension == "M"), 2,
               ifelse((warpbreaks$wool == "A") & (warpbreaks$tension == "H"), 3,
               ifelse((warpbreaks$wool == "B") & (warpbreaks$tension == "L"), 4,
               ifelse((warpbreaks$wool == "B") & (warpbreaks$tension == "M"), 5, 6)))))

m5.10 <- quap(
  alist(
    breaks ~ dnorm( mu , sigma ),
    mu <- a[condition],
    a[condition] ~ dnorm( 30 , 30 ),
    # mu <- w[wool] + t[tension],
    # w[wool]    ~ dnorm( 30 , 30 ),
    # t[tension] ~ dnorm( 30, 30 ),
    sigma ~ dunif( 0 , 50 )
  ), data=d )

precis(m5.10, prob = 0.95, depth = 2)

post <- extract.samples(m5.10)
names(post)
lapply(post, dim)

# compute contrasts
# post$M_H <- post$t[,2] - post$t[,3]
# post$L_H <- post$t[,1] - post$t[,3]
# post$L_M <- post$t[,1] - post$t[,2]
# precis( post , pars = c("M_H", "L_H", "L_M") , prob = 0.95 , digits = 2 )

mean(c(post$a[,2], post$a[,5])) - mean(c(post$a[,3], post$a[,6])) # M - H
mean(c(post$a[,1], post$a[,4])) - mean(c(post$a[,3], post$a[,6])) # L - H
mean(c(post$a[,1], post$a[,4])) - mean(c(post$a[,2], post$a[,5])) # L - M




# Doing Bayesian Data Analysis, 2nd Edition
# https://bookdown.org/content/3686/metric-predicted-variable-with-one-nominal-predictor.html

fruitfly <- read_csv("/Users/clinton/Downloads/DBDA2Eprograms/FruitflyDataReduced.csv")
glimpse(fruitfly)

# view distributions of Longevity by CompanionNumber
fruitfly %>% 
  group_by(CompanionNumber) %>% 
  mutate(group_mean = mean(Longevity)) %>% 
  ungroup() %>% 
  mutate(CompanionNumber = fct_reorder(CompanionNumber, group_mean)) %>% 
  
  ggplot(aes(x = Longevity, y = CompanionNumber, fill = group_mean)) +
  geom_density_ridges(scale = 3/2, size = .2) +
  scale_fill_gradient() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_y_discrete(NULL, expand = expansion(mult = c(0, 0.4))) +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank(),
        legend.position = "none")

# fit hierarchical Bayesian alternative to an ANOVA
fit19.1 <-
  brm(data = fruitfly,
      family = gaussian,
      Longevity ~ 1 + (1 | CompanionNumber),
      iter = 4000, warmup = 1000, chains = 4, cores = 4,
      seed = 19, control = list(adapt_delta = 0.99))

# check mcmc sampling
plot(fit19.1, widths = c(2, 3))

draws19.1 <- as_draws_df(fit19.1)

# view model summaries
print(fit19.1)

ranef(fit19.1) # group-specific deflections

coef(fit19.1) # group-specific means (not deflections)

posterior_summary(fit19.1)["sigma", ] # all groups have same sigma_y

# Figure 19.3: Data with Posterior Predictive Distributions
densities <-
  fruitfly %>% 
  distinct(CompanionNumber) %>% 
  add_epred_draws(fit19.1, ndraws = 20, seed = 19, dpar = c("mu", "sigma"))

densities <-
  densities %>% 
  mutate(ll = qnorm(.025, mean = mu, sd = sigma),
         ul = qnorm(.975, mean = mu, sd = sigma)) %>% 
  mutate(Longevity = map2(ll, ul, seq, length.out = 100)) %>% 
  unnest(Longevity) %>% 
  mutate(density = dnorm(Longevity, mu, sigma))

densities %>% 
  ggplot(aes(x = Longevity, y = CompanionNumber)) +
  # here we make our density lines
  geom_ridgeline(aes(height = density, group = interaction(CompanionNumber, .draw)),
                 fill = NA, color = adjustcolor("steelblue", alpha.f = 2/3),
                 size = 1/3, scale = 25) +
  # the original data with little jitter thrown in
  geom_jitter(data = fruitfly,
              height = .04, alpha = 3/4, color = adjustcolor("black", alpha.f = 1/3)) +
  # pretty much everything below this line is aesthetic fluff
  scale_x_continuous(breaks = 0:4 * 25, limits = c(0, 110), 
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Data with Posterior Predictive Distrib.", 
       y = NULL) +
  coord_cartesian(ylim = c(1.25, 5.25)) +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank()) +
  coord_flip()

# All pairwise comparisons
fit19.1 %>%
  # these two lines are where the magic is at
  spread_draws(r_CompanionNumber[CompanionNumber,]) %>%
  compare_levels(r_CompanionNumber, by = CompanionNumber) %>%
  
  ggplot(aes(x = r_CompanionNumber, y = CompanionNumber)) +
  geom_vline(xintercept = 0, color = "grey", lty = 2) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black", 
                    slab_size = 0, quantiles = 100) +
  labs(x = "Contrast",
       y = NULL) +
  coord_cartesian(ylim = c(1.5, 10.5)) +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 0))




## Include a metric predictor (ANCOVA) ##

# Since Thorax is moderately correlated with Longevity, 
# including Thorax in the statistical model should help shrink that  
# σy estimate, making it easier to compare group means
as_draws_df(fit19.1) %>% 
  ggplot(aes(x = sigma, y = 0)) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black", 
                    slab_size = 0, quantiles = 100) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab(expression(sigma[italic(y)])) +
  theme(panel.grid = element_blank())


# mean-center the covariate, Thorax
fruitfly <-
  fruitfly %>% 
  mutate(thorax_c = Thorax - mean(Thorax))

# fit hierarchical Bayesian alternative to an ANCOVA
fit19.5 <-
  brm(data = fruitfly,
      family = gaussian,
      Longevity ~ 1 + thorax_c + (1 | CompanionNumber),
      iter = 4000, warmup = 1000, chains = 4, cores = 4,
      seed = 19, control = list(adapt_delta = 0.99))

# check mcmc sampling
plot(fit19.5, widths = c(2, 3))

draws19.5 <- as_draws_df(fit19.5)

# view model summaries
print(fit19.5)

ranef(fit19.5) # group-specific deflections

coef(fit19.5) # group-specific means (not deflections)

posterior_summary(fit19.5)["sigma", ] # all groups have same sigma_y

# did σy shrink? yes, it did.
as_draws_df(fit19.5) %>% 
  ggplot(aes(x = sigma, y = 0)) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black", 
                    slab_size = 0, quantiles = 100) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab(expression(sigma[italic(y)]))


# Figure 19.5: Data with Posterior Predictive Distributions
(r <- range(fruitfly$Thorax))

n_draws <- 80

densities <-
  fruitfly %>% 
  distinct(CompanionNumber) %>% 
  expand_grid(Thorax = c(r[1], mean(r), r[2])) %>% 
  mutate(thorax_c  = Thorax - mean(fruitfly$Thorax)) %>% 
  add_epred_draws(fit19.5, ndraws = n_draws, seed = 19, dpar = c("mu", "sigma")) %>% 
  mutate(ll = qnorm(.025, mean = mu, sd = sigma),
         ul = qnorm(.975, mean = mu, sd = sigma)) %>% 
  mutate(Longevity = map2(ll, ul, seq, length.out = 100)) %>% 
  unnest(Longevity) %>% 
  mutate(density = dnorm(Longevity, mu, sigma))

f <-
  fruitfly %>% 
  distinct(CompanionNumber) %>% 
  expand_grid(Thorax = c(r[1], mean(r), r[2])) %>% 
  mutate(thorax_c = Thorax - mean(fruitfly$Thorax)) %>% 
  add_epred_draws(fit19.5, ndraws = n_draws, seed = 19, value = "Longevity")

densities %>% 
  ggplot(aes(x = Longevity, y = Thorax)) +
  # the Gaussians
  geom_ridgeline(aes(height = -density, group = interaction(Thorax, .draw)),
                 fill = NA, size = 1/5, scale = 5/3,
                 color = adjustcolor("steelblue", alpha.f = 1/5),
                 min_height = NA) +
  # the vertical lines below the Gaussians
  geom_line(aes(group = interaction(Thorax, .draw)),
            alpha = 1/5, linewidth = 1/5, color = "black") +
  # the regression lines
  geom_line(data = f,
            aes(group = .draw),
            alpha = 1/5, linewidth = 1/5, color = "black") +
  # the data
  geom_point(data = fruitfly, pch = 1,
             alpha = 3/4, color = "black") +
  coord_flip(xlim = c(0, 110),
             ylim = c(.58, 1)) +
  facet_wrap(~ CompanionNumber, ncol = 5)


# specific differences and effect sizes
draws19.5 <- as_draws_df(fit19.5)

differences <-
  draws19.5 %>% 
  transmute(`Pregnant1.Pregnant8 vs None0` = (`r_CompanionNumber[Pregnant1,Intercept]` + `r_CompanionNumber[Pregnant1,Intercept]`) / 2 - `r_CompanionNumber[None0,Intercept]`,
            
            `Pregnant1.Pregnant8.None0 vs Virgin1` = (`r_CompanionNumber[Pregnant1,Intercept]` + `r_CompanionNumber[Pregnant1,Intercept]` + `r_CompanionNumber[None0,Intercept]`) / 3 - `r_CompanionNumber[Virgin1,Intercept]`,
            
            `Virgin1 vs Virgin8` = `r_CompanionNumber[Virgin1,Intercept]` - `r_CompanionNumber[Virgin8,Intercept]`,
            
            `Pregnant1.Pregnant8.None0 vs Virgin1.Virgin8` = (`r_CompanionNumber[Pregnant1,Intercept]` + `r_CompanionNumber[Pregnant1,Intercept]` + `r_CompanionNumber[None0,Intercept]`) / 3 - (`r_CompanionNumber[Virgin1,Intercept]` + `r_CompanionNumber[Virgin8,Intercept]`) / 2)

p1 <-
  differences %>% 
  pivot_longer(everything()) %>%   
  
  ggplot(aes(x = value, y = 0)) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black", 
                    slab_size = 0, quantiles = 100) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab("Difference") +
  theme(strip.text = element_text(size = 6.4)) +
  facet_wrap(~ name, scales = "free_x", ncol = 4)

p2 <-
  differences %>% 
  mutate_all(.funs = ~. / draws19.5$sigma) %>% 
  pivot_longer(everything()) %>%   
  
  ggplot(aes(x = value, y = 0)) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black", 
                    slab_size = 0, quantiles = 100) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab("Effect Size") +
  theme(strip.text = element_text(size = 6.4)) +
  facet_wrap(~ name, scales = "free_x", ncol = 4)

# combine
p1 / p2




## 19.5.1 Example: Contrast of means with DIFFERENT VARIANCES ##
nonhomogvar <- read_csv("/Users/clinton/Downloads/DBDA2Eprograms/NonhomogVarData.csv")
glimpse(nonhomogvar)

# mean and sd for each group
nonhomogvar %>% 
  group_by(Group) %>% 
  summarise(mean = mean(Y),
            sd   = sd(Y))

# fit ANOVA-like homogeneous-variances model
fit19.6 <-
  brm(data = nonhomogvar,
      family = gaussian,
      Y ~ 1 + (1 | Group),
      iter = 4000, warmup = 1000, chains = 4, cores = 4,
      seed = 19, control = list(adapt_delta = 0.995))

print(fit19.6)

# Figure 19.7: Data with Posterior Predictive Distributions
n_draws <- 20

densities <-
  nonhomogvar %>% 
  distinct(Group) %>% 
  add_epred_draws(fit19.6, ndraws = n_draws, seed = 19, dpar = c("mu", "sigma")) %>% 
  mutate(ll = qnorm(.025, mean = mu, sd = sigma),
         ul = qnorm(.975, mean = mu, sd = sigma)) %>% 
  mutate(Y = map2(ll, ul, seq, length.out = 100)) %>% 
  unnest(Y) %>% 
  mutate(density = dnorm(Y, mu, sigma)) %>% 
  group_by(.draw) %>% 
  mutate(density = density / max(density))

densities %>% 
  ggplot(aes(x = Y, y = Group)) +
  geom_ridgeline(aes(height = density, group = interaction(Group, .draw)),
                 fill = NA, color = adjustcolor("steelblue", alpha.f = 2/3),
                 size = 1/3, scale = 3/4) +
  geom_jitter(data = nonhomogvar,
              height = .04, alpha = 3/4, color = "black") +
  scale_x_continuous(breaks = seq(from = 80, to = 120, by = 10)) +
  labs(title = "Data with Posterior Predictive Distrib.", 
       y = NULL) +
  coord_cartesian(xlim = c(75, 125),
                  ylim = c(1.25, 4.5)) +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank())

# all contrasts
fit19.6 %>%
  spread_draws(r_Group[Group,]) %>%
  compare_levels(r_Group, by = Group) %>%
  # these next two lines allow us to reorder the contrasts along the y
  ungroup() %>% 
  mutate(Group = reorder(Group, r_Group)) %>%
  
  ggplot(aes(x = r_Group, y = Group)) +
  geom_vline(xintercept = 0, color = "black", lty = 2) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black",
                    slab_size = 0, quantiles = 100) +
  labs(x = "Contrast",
       y = NULL) +
  coord_cartesian(ylim = c(1.5, 6.5)) +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank())


## fit robust, better-than-ANOVA heterogeneous-variances model ##
# create stanvars
gamma_a_b_from_omega_sigma <- function(mode, sd) {
  if (mode <= 0) stop("mode must be > 0")
  if (sd   <= 0) stop("sd must be > 0")
  rate <- (mode + sqrt(mode^2 + 4 * sd^2)) / (2 * sd^2)
  shape <- 1 + mode * rate
  return(list(shape = shape, rate = rate))
}

(mean_y <- mean(nonhomogvar$Y))
(sd_y <- sd(nonhomogvar$Y))

omega <- sd_y / 2
sigma <- 2 * sd_y

(s_r <- gamma_a_b_from_omega_sigma(mode = omega, sd = sigma))

stanvars <- 
  stanvar(mean_y,    name = "mean_y") + 
  stanvar(sd_y,      name = "sd_y") +
  stanvar(s_r$shape, name = "alpha") +
  stanvar(s_r$rate,  name = "beta") +
  stanvar(1/29,      name = "one_over_twentynine")

fit19.7 <-
  brm(data = nonhomogvar,
      family = student,
      bf(Y     ~ 1 + (1 | Group), 
         sigma ~ 1 + (1 | Group)),
      prior = c(# grand means
        prior(normal(mean_y, sd_y * 10), class = Intercept),
        prior(normal(log(sd_y), 1), class = Intercept, dpar = sigma),
        
        # the priors controlling the spread for our hierarchical deflections
        prior(gamma(alpha, beta), class = sd),
        prior(normal(0, 1), class = sd, dpar = sigma),
        
        # don't forget our student-t nu
        prior(exponential(one_over_twentynine), class = nu)),
      iter = 4000, warmup = 1000, chains = 4, cores = 4,
      seed = 19,
      control = list(adapt_delta = 0.99,
                     max_treedepth = 12),
      stanvars = stanvars)

plot(fit19.7, widths = c(2, 3)) # check chains

print(fit19.7) # check parameter summary

# Figure 19.8: Data with Posterior Predictive Distributions
densities <-
  nonhomogvar %>% 
  distinct(Group) %>% 
  add_epred_draws(fit19.7, ndraws = n_draws, seed = 19, dpar = c("mu", "sigma", "nu")) %>% 
  mutate(ll = qt(.025, df = nu),
         ul = qt(.975, df = nu)) %>% 
  mutate(Y = map2(ll, ul, seq, length.out = 100)) %>% 
  unnest(Y) %>%
  mutate(density = dt(Y, nu)) %>% 
  # notice the conversion
  mutate(Y = mu + Y * sigma) %>% 
  group_by(.draw) %>% 
  mutate(density = density / max(density))

# top panel
p1 <-
  densities %>% 
  ggplot(aes(x = Y, y = Group)) +
  geom_ridgeline(aes(height = density, group = interaction(Group, .draw)),
                 fill = NA, color = adjustcolor("steelblue", alpha.f = 2/3),
                 size = 1/3, scale = 3/4) +
  geom_jitter(data = nonhomogvar,
              height = .04, alpha = 3/4, color = "black") +
  scale_x_continuous(breaks = seq(from = 80, to = 120, by = 10)) +
  labs(title = "Data with Posterior Predictive Distrib.", 
       y = NULL) +
  coord_cartesian(xlim = c(75, 125),
                  ylim = c(1.25, 4.5)) +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank())

# difference distributions
draws19.7 <- as_draws_df(fit19.7)

p2 <-
  draws19.7 %>% 
  transmute(`D vs A` = `r_Group[D,Intercept]` - `r_Group[A,Intercept]`,
            `C vs B` = `r_Group[C,Intercept]` - `r_Group[B,Intercept]`) %>% 
  pivot_longer(everything()) %>%
  mutate(name = factor(name, levels = c("D vs A", "C vs B"))) %>% 
  
  ggplot(aes(x = value, y = 0)) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black",
                    slab_size = 0, quantiles = 100) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab("Difference") +
  facet_wrap(~ name, scales = "free_x", ncol = 4)

# first compute and save the sigma_j's, which will come in handy later 
draws19.7 <-
  draws19.7 %>% 
  mutate(sigma_A = exp(b_sigma_Intercept + `r_Group__sigma[A,Intercept]`),
         sigma_B = exp(b_sigma_Intercept + `r_Group__sigma[B,Intercept]`),
         sigma_C = exp(b_sigma_Intercept + `r_Group__sigma[C,Intercept]`),
         sigma_D = exp(b_sigma_Intercept + `r_Group__sigma[D,Intercept]`))

p3 <-
  draws19.7 %>% 
  # note we're using pooled standard deviations to standardize our effect sizes, here
  transmute(`D vs A` = (`r_Group[D,Intercept]` - `r_Group[A,Intercept]`) / sqrt((sigma_A^2 + sigma_D^2) / 2),
            `C vs B` = (`r_Group[C,Intercept]` - `r_Group[B,Intercept]`) / sqrt((sigma_B^2 + sigma_C^2) / 2)) %>% 
  pivot_longer(everything()) %>%
  mutate(name = factor(name, levels = c("D vs A", "C vs B"))) %>% 
  
  ggplot(aes(x = value, y = 0)) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black",
                    slab_size = 0, quantiles = 100) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab("Effect Size") +
  facet_wrap(~ name, scales = "free_x", ncol = 4)


# Combine them all and plot
p1 / p2 / p3 + plot_layout(heights = c(2, 1, 1))


# finally, because each group has its own estimated scale (i.e., σj), 
# we can investigate differences in scales across groups
# recall we computed the sigma_j's a couple blocks up;
# now we put them to use
draws19.7 %>% 
  transmute(`D vs A` = sigma_D - sigma_A,
            `C vs B` = sigma_C - sigma_B,
            `D vs C` = sigma_D - sigma_C,
            `B vs A` = sigma_B - sigma_A) %>% 
  pivot_longer(everything()) %>%
  mutate(name = factor(name, levels = c("D vs A", "C vs B", "D vs C", "B vs A"))) %>% 
  
  ggplot(aes(x = value, y = 0)) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black",
                    slab_size = 0, quantiles = 100) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab(expression(Differences~'in'~sigma[italic(j)])) +
  facet_wrap(~ name, scales = "free", ncol = 4)








# Doing Bayesian Data Analysis, 2nd Edition
# https://bookdown.org/content/3686/metric-predicted-variable-with-one-nominal-predictor.html

fruitfly <- read_csv("FruitflyDataReduced.csv")
glimpse(fruitfly)

# view distributions of Longevity by CompanionNumber
fruitfly %>% 
  group_by(CompanionNumber) %>% 
  mutate(group_mean = mean(Longevity)) %>% 
  ungroup() %>% 
  mutate(CompanionNumber = fct_reorder(CompanionNumber, group_mean)) %>% 
  
  ggplot(aes(x = Longevity, y = CompanionNumber, fill = group_mean)) +
  geom_density_ridges(scale = 3/2, size = .2) +
  scale_fill_gradient() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_y_discrete(NULL, expand = expansion(mult = c(0, 0.4))) +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank(),
        legend.position = "none")

# create stanvars
gamma_a_b_from_omega_sigma <- function(mode, sd) {
  if (mode <= 0) stop("mode must be > 0")
  if (sd   <= 0) stop("sd must be > 0")
  rate <- (mode + sqrt(mode^2 + 4 * sd^2)) / (2 * sd^2)
  shape <- 1 + mode * rate
  return(list(shape = shape, rate = rate))
}

(mean_y <- mean(fruitfly$Longevity))
(sd_y <- sd(fruitfly$Longevity))

omega <- sd_y / 2
sigma <- 2 * sd_y

(s_r <- gamma_a_b_from_omega_sigma(mode = omega, sd = sigma))

stanvars <- 
  stanvar(mean_y,    name = "mean_y") + 
  stanvar(sd_y,      name = "sd_y") +
  stanvar(s_r$shape, name = "alpha") +
  stanvar(s_r$rate,  name = "beta")

# fit hierarchical Bayesian alternative to an ANOVA
fit19.1 <-
  brm(data = fruitfly,
      family = gaussian,
      Longevity ~ 1 + (1 | CompanionNumber),
      prior = c(prior(normal(mean_y, sd_y * 5), class = Intercept),
                prior(gamma(alpha, beta), class = sd),
                prior(cauchy(0, sd_y), class = sigma)),
      iter = 4000, warmup = 1000, chains = 4, cores = 4,
      seed = 19,
      control = list(adapt_delta = 0.99),
      stanvars = stanvars)

# check mcmc sampling
plot(fit19.1, widths = c(2, 3))

draws <- as_draws_df(fit19.1)

draws %>% 
  mutate(chain = .chain) %>% 
  mcmc_acf(pars = vars(b_Intercept:sigma), lags = 10)

# view model summaries
print(fit19.1)

ranef(fit19.1) # group-specific deflections

coef(fit19.1) # group-specific means (not deflections)

posterior_summary(fit19.1)["sigma", ] # all groups have same sigma_y


# Figure 19.3: Data with Posterior Predictive Distributions
densities <-
  fruitfly %>% 
  distinct(CompanionNumber) %>% 
  add_epred_draws(fit19.1, ndraws = 20, seed = 19, dpar = c("mu", "sigma"))

glimpse(densities)

densities <-
  densities %>% 
  mutate(ll = qnorm(.025, mean = mu, sd = sigma),
         ul = qnorm(.975, mean = mu, sd = sigma)) %>% 
  mutate(Longevity = map2(ll, ul, seq, length.out = 100)) %>% 
  unnest(Longevity) %>% 
  mutate(density = dnorm(Longevity, mu, sigma))

glimpse(densities)

densities %>% 
  ggplot(aes(x = Longevity, y = CompanionNumber)) +
  # here we make our density lines
  geom_ridgeline(aes(height = density, group = interaction(CompanionNumber, .draw)),
                 fill = NA, color = adjustcolor("steelblue", alpha.f = 2/3),
                 size = 1/3, scale = 25) +
  # the original data with little jitter thrown in
  geom_jitter(data = fruitfly,
              height = .04, alpha = 3/4, color = adjustcolor("black", alpha.f = 1/3)) +
  # pretty much everything below this line is aesthetic fluff
  scale_x_continuous(breaks = 0:4 * 25, limits = c(0, 110), 
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Data with Posterior Predictive Distrib.", 
       y = NULL) +
  coord_cartesian(ylim = c(1.25, 5.25)) +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank())


# Figure 19.3: Contrast: One Difference: Pregnant1.Pregnant8 vs None0
draws %>% 
  transmute(c = (`r_CompanionNumber[Pregnant1,Intercept]` + `r_CompanionNumber[Pregnant8,Intercept]`) / 2 - `r_CompanionNumber[None0,Intercept]`) %>% 
  
  ggplot(aes(x = c, y = 0)) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_color = "steelblue", slab_fill = "steelblue", color = "black") +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = "Pregnant1.Pregnant8 vs None0",
       x = expression(Difference~(mu[1]+mu[2])/2-mu[3]))


# Figure 19.3: Contrast: One Effect Size: Pregnant1.Pregnant8 vs None0
draws %>% 
  transmute(es = ((`r_CompanionNumber[Pregnant1,Intercept]` + `r_CompanionNumber[Pregnant8,Intercept]`) / 2 - `r_CompanionNumber[None0,Intercept]`) / sigma) %>% 
  
  ggplot(aes(x = es, y = 0)) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black", 
                    slab_size = 0, quantiles = 100) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = "Pregnant1.Pregnant8 vs None0",
       x = expression(Effect~Size~(Difference/sigma[italic(y)])))


# Figure 19.3: Three Differences
differences <-
  draws %>% 
  transmute(`Pregnant1.Pregnant8.None0 vs Virgin1` = (`r_CompanionNumber[Pregnant1,Intercept]` + `r_CompanionNumber[Pregnant8,Intercept]` + `r_CompanionNumber[None0,Intercept]`) / 3 - `r_CompanionNumber[Virgin1,Intercept]`,
            
            `Virgin1 vs Virgin8` = `r_CompanionNumber[Virgin1,Intercept]` - `r_CompanionNumber[Virgin8,Intercept]`,
            
            `Pregnant1.Pregnant8.None0 vs Virgin1.Virgin8` = (`r_CompanionNumber[Pregnant1,Intercept]` + `r_CompanionNumber[Pregnant8,Intercept]` + `r_CompanionNumber[None0,Intercept]`) / 3 - (`r_CompanionNumber[Virgin1,Intercept]` + `r_CompanionNumber[Virgin8,Intercept]`) / 2)


differences %>% 
  pivot_longer(everything()) %>% 
  
  ggplot(aes(x = value, y = 0)) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black", 
                    slab_size = 0, quantiles = 100) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab("Difference") +
  facet_wrap(~ name, scales = "free")


# Figure 19.3: Three Effect Sizes
differences %>% 
  mutate_all(.funs = ~ . / draws$sigma) %>% 
  pivot_longer(everything()) %>% 
  
  ggplot(aes(x = value, y = 0)) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black", 
                    slab_size = 0, quantiles = 100) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab("Effect Size (Standardized mean difference)") +
  facet_wrap(~ name, scales = "free_x")


# All pairwise comparisons
fit19.1 %>%
  # these two lines are where the magic is at
  spread_draws(r_CompanionNumber[CompanionNumber,]) %>%
  compare_levels(r_CompanionNumber, by = CompanionNumber) %>%
  
  ggplot(aes(x = r_CompanionNumber, y = CompanionNumber)) +
  geom_vline(xintercept = 0, color = "grey", lty = 2) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black", 
                    slab_size = 0, quantiles = 100) +
  labs(x = "Contrast",
       y = NULL) +
  coord_cartesian(ylim = c(1.5, 10.5)) +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 0))


# Rough analogue of omnibus test
fit19.2 <-
  brm(data = fruitfly,
      family = gaussian,
      Longevity ~ 1,
      prior = c(prior(normal(mean_y, sd_y * 5), class = Intercept),
                prior(cauchy(0, sd_y), class = sigma)),
      iter = 4000, warmup = 1000, chains = 4, cores = 4,
      seed = 19,
      control = list(adapt_delta = 0.99),
      stanvars = stanvars)


# Compare the two models with loo
fit19.1 <- add_criterion(fit19.1, criterion = "loo")
fit19.2 <- add_criterion(fit19.2, criterion = "loo")

loo_compare(fit19.1, fit19.2) %>% 
  print(simplify = F)

mw <- model_weights(fit19.1, fit19.2) # stacking-based model weights
mw %>% 
  round(digits = 3)


# Is zero a credible value for sigma_beta? Nope!
draws %>% 
  ggplot(aes(x = sd_CompanionNumber__Intercept, y = 0)) +
  stat_dotsinterval(point_interval = mode_hdi, .width = .95,
                    slab_fill = "steelblue", color = "black", 
                    slab_size = 0, quantiles = 100) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = c(0, 50)) +
  labs(title = expression("Behold the fit19.1 posterior for "*sigma[beta]*"."),
       subtitle = "This parameter's many things, but zero isn't one of them.",
       x = NULL)




# Simulation: Morphological awareness and reading comprehension

# students
n <- 1000
english_only <- rep(1, n)
spanish_english <- rep(2, n)
chinese_english <- rep(3, n)
student_type <- c(english_only, spanish_english, chinese_english)

# morphological awareness
eo_ma <- rnorm(n=n, mean=80, sd=2)
se_ma <- rnorm(n=n, mean=70, sd=2)
ce_ma <- rnorm(n=n, mean=60, sd=2)
morph_awareness <- c(eo_ma, se_ma, ce_ma)

# relationship between reading comprehension and morphological awareness
eo_rc <- 0 + 6 * eo_ma + rnorm(n)
se_rc <- 0 + 4 * se_ma + rnorm(n)
ce_rc <- 0 + 2 * ce_ma + rnorm(n)
reading_score <- c(eo_rc, se_rc, ce_rc)

# combine data into data.frame
d <- data.frame(cbind(reading_score, morph_awareness, student_type))
head(d)


# plot the data
ggplot(d, aes(x=morph_awareness, y=reading_score, 
              group=factor(student_type), colour=factor(student_type))) +
  geom_point() +
  labs(x = "Morphological Awareness", y = "Reading Comprehension", color = "Student Type", 
       title = "Relationship between MA and RC for 3 Types of Students") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))


lattice::xyplot(reading_score ~ morph_awareness | factor(student_type), data=d, 
                panel = function(x, y, ...) {
                  panel.xyplot(x, y, ...)
                  panel.lmline(x, y, col = "red")
                })


# does relationship between morphological awareness and reading comprehension
# vary by student type?
fit1 <- lm(reading_score ~ morph_awareness * factor(student_type), data = d)
arm::display(fit1)

# english-only
approx. 0 + 6 * morph_awareness
# spanish-english ( factor(student_type)2 == 1 )
approx. 0 + (6-2) = 4 * morph_awareness
# chinese-english ( factor(student_type)3 == 1 )
approx. 0 + (6-4) = 2 * morph_awareness


# plot data and fitted lines (automatically with ggplot)
ggplot(d, aes(x=morph_awareness, y=reading_score, 
              group=factor(student_type), colour=factor(student_type))) +
  geom_point(shape = 1, size = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 1) +
  labs(x = "Morphological Awareness", y = "Reading Comprehension", color = "Student Type", 
       title = "Relationship between MA and RC for 3 Types of Students") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))


# plot data and fitted lines (manually with 3 `abline`s)
b <- coef(fit1)

ggplot(d, aes(x=morph_awareness, y=reading_score, 
              group=factor(student_type), colour=factor(student_type))) +
  geom_point(shape = 1, size = 1) +
  geom_abline(intercept = b[[1]], slope = b[[2]]) + # english-only
  geom_abline(intercept = b[[1]] + b[[3]], slope = b[[2]] + b[[5]]) + # spanish-english
  geom_abline(intercept = b[[1]] + b[[4]], slope = b[[2]] + b[[6]]) + # chinese-english
  labs(x = "Morphological Awareness", y = "Reading Comprehension", color = "Student Type", 
       title = "Relationship between MA and RC for 3 Types of Students") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))



# now re-level student_type to make spanish-english students
# the base, comparison category
d$student_type <- factor(d$student_type, levels = c(2, 1, 3))


# does relationship between morphological awareness and reading comprehension
# vary by student type?
fit2 <- lm(reading_score ~ morph_awareness * factor(student_type), data = d)
summary(fit2)

# spanish-english
approx. 0 + 4 * morph_awareness
# english-only ( factor(student_type)1 == 1 )
approx. 0 + (4+2) = 6 * morph_awareness
# chinese-english ( factor(student_type)3 == 1 )
approx. 0 + (4-2) = 2 * morph_awareness
