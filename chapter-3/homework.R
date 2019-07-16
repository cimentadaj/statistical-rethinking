library(ggplot2)
library(rethinking)

# homework
n <- 1e3
p_grid <- seq(0, 1, length.out = n)
prior <- rep(1, n)
likelihood <- dbinom(6, 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, size = 1e4, prob = posterior, replace = TRUE)

## 3E1
options(scipen = 9999)
round(mean(samples < .2), 5)

## 3E2
round(mean(samples > .8), 5)

## 3E3
round(mean(samples > .2 & samples < .8), 5)

## 3E4
quantile(samples, .2) # 0.5185

## 3E5
quantile(samples, .8) # 0.755

## 3E6
HPDI(samples, prob = .66) # 0.5085085 0.7737738 

## 3E7
PI(samples, prob = .66) # 0.5025025 0.7697698 


## The distribution must be normal or close to normal because both intervals are
## practically equal.
hist(samples)


## 3M1
n <- 1e3
p_grid <- seq(0, 1, length.out = n)
prior <- rep(1, n)
likelihood <- dbinom(8, 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

## 3M2
set.seed(123)
post_sample <- sample(p_grid, size = 1e4, prob = posterior, replace = TRUE)
HPDI(post_sample, prob = .9) # 0.3343343 0.7217217 
mean(post_sample) #  0.532214
median(post_sample) #  0.5325325

## 3M3
post_check <- rbinom(1e4, 15, prob = post_sample)
mean(post_check == 8) # 0.1403

simplehist(post_check)

## 3M4
post_check <- rbinom(1e4, 9, prob = post_sample)
mean(post_check == 6) # 0.1756
simplehist(post_check)


## 3M5
n <- 1e3
p_grid <- seq(0, 1, length.out = n)
# I want to check the difference in the distribution of the
# posterior before/after the new prior
# Original prior
prior <- rep(1, n)
likelihood <- dbinom(8, 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot(p_grid, posterior)
p_grid[which.max(posterior)]

# New prior
prior <- ifelse(p_grid <= 0.5, 0, .7)
likelihood <- dbinom(8, 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
# The prior eliminated any probability that it is below 0
plot(p_grid, posterior)
p_grid[which.max(posterior)]

## 5.1)
set.seed(123)
post_sample <- sample(p_grid, size = 1e4, prob = posterior, replace = TRUE)
HPDI(post_sample, prob = .9) # 0.5005005 0.7157157 
# The prior shifted the lower bound from .30 to .50
# This is because, as can be seen from the plot above,
# the probs below 0.5 are now 0 due to the prior

mean(post_sample)
median(post_sample)
# The prior shifted the average/median from 0.5322 to .60.
# I think the change is much smaller for the mean/median
# than for the intervals because it was mostly concentrated
# in the first place at above 0.5, so it is less affected by it.

## 5.2)
post_check <- rbinom(1e4, 15, prob = post_sample)
mean(post_check == 8) # 0.1561

simplehist(post_check)

## 5.3)
post_check <- rbinom(1e4, 9, prob = post_sample)
mean(post_check == 6) # 0.2343
simplehist(post_check)


## 3H1
data(homeworkch3)

n <- 1000
posterior_grid <- seq(0, 1, length.out = n)
prior <- rep(1, rep = n)

# Data
order_births <- data.frame(first_birth = birth1, second_birth = birth2)
total_boys <- sum(order_births$first_birth) + sum(order_births$second_birth)
total_births <- nrow(order_births) * 2 # Because there are two columns

likelihood <- dbinom(total_boys, total_births, prob = posterior_grid)

posterior  <- likelihood * prior
posterior <- posterior / sum(posterior)

posterior_grid[which.max(posterior)]


## 3H2
n <- 1e5
sample_posterior <- sample(p_grid, n, prob = posterior, replace = TRUE)

# 50% of the data is contained between these two values
hpi_50 <- HPDI(sample_posterior, prob = 0.5)

# 89% of the data is contained between these two values
hpi_89 <- HPDI(sample_posterior, prob = 0.89)

# 97% of the data is contained between these two values
hpi_97 <- HPDI(sample_posterior, prob = 0.97)


## 3H3

replicate_births <- rbinom(n, 200, prob = sample_posterior)

## The posterior seems to match the data very well as both lines overlap almost
## perfectly

qplot(replicate_births, geom = "histogram", bins = 100) +
  geom_vline(xintercept = mean(replicate_births), color = 'red', alpha = 1/2) +
  geom_vline(xintercept = total_boys, color = 'blue', alpha = 1/2)

## In another way:
mean(replicate_births) ## 110.89
total_boys ## 111


## 3H4
total_boys_first_births <- sum(order_births$first_birth)
total_first_births <- length(order_births$first_birth)
replicate_births_first <- rbinom(n, 100, prob = sample_posterior)

qplot(replicate_births_first, geom = "histogram", bins = 100) +
  geom_vline(xintercept = mean(replicate_births_first),
             color = 'red') +
  geom_vline(xintercept = total_boys_first_births,
             color = 'blue')


## In another way:
mean(replicate_births_first) ## 55.45
total_boys_first_births ## 51

## 3H5

only_female_first_birth <- order_births[order_births$first_birth == 0, ]
total_female_first_birth <- nrow(only_female_first_birth)
second_birth_boy <- sum(only_female_first_birth$second_birth)
post_boys <- rbinom(n, total_female_first_birth, prob = sample_posterior)

qplot(post_boys, geom = "histogram", bins = 100) +
  geom_vline(xintercept = mean(post_boys),
             color = 'red') +
  geom_vline(xintercept = second_birth_boy,
             color = 'blue')


mean(post_boys) ## 27.18
median(post_boys)  ## 27

second_birth_day ## 39


## It looks like the model is under predicting boys followed after first birth
## girls. It looks like the second birth is not independent from the first
## so the model should take this into account.




tst































## 3M1
w <- 8
n <- 15
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(x = 1, length(p_grid))
likelihood <- dbinom(x = w, size = n, prob = p_grid)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
plot(posterior ~ p_grid, type = "l")

## 3M2
trials <- 1e4
samples <- sample(x = p_grid, size = trials, prob = posterior, replace = TRUE)
HPDI(samples = samples, prob = .9)

## 3M3
posterior.predictive.distribution <- rbinom(n = trials, size = n, prob = samples)
mean(posterior.predictive.distribution == 8)

## 3M4
posterior.predictive.distribution <- rbinom(n = trials, size = 9, prob = samples)
mean(posterior.predictive.distribution == 6)

## 3M5

# 3M1
w <- 8
n <- 15
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- ifelse(test = p_grid < .5, yes = 0, no = 1)
likelihood <- dbinom(x = w, size = n, prob = p_grid)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
plot(posterior ~ p_grid, type = "l")

# 3M2
trials <- 1e4
samples <- sample(x = p_grid, size = trials, prob = posterior, replace = TRUE)
HPDI(samples = samples, prob = .9)

# 3M3
posterior.predictive.distribution <- rbinom(n = trials, size = n, prob = samples)
mean(posterior.predictive.distribution == 8)

# 3M4
posterior.predictive.distribution <- rbinom(n = trials, size = 9, prob = samples)
mean(posterior.predictive.distribution == 6)

## 3H1
library(rethinking)
data(homeworkch3)

total.births <- length(birth1) + length(birth2)
boys.born <- sum(birth1 + birth2)
girls.born <- total.births - boys.born

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(x = 1, length(p_grid))
likelihood <- dbinom(x = boys.born, size = total.births, prob = p_grid)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
plot(posterior ~ p_grid, type = "l")

p_grid[which.max(posterior)]

## 3H2
trials <- 1e4
samples <- sample(x = p_grid, size = trials, prob = posterior, replace = TRUE)
HPDI(samples = samples, prob = c(.5, .89, .97))

## 3H3
n <- total.births
posterior.predictive.distribution <- rbinom(n = trials, size = n, prob = samples)
dens(posterior.predictive.distribution, adj = .1)
abline(v = boys.born, col = "red")

## 3H4
n <- 100
sum(birth1)
posterior.predictive.distribution <- rbinom(n = trials, size = n, prob = samples)
dens(posterior.predictive.distribution, adj = .1)
abline(v = sum(birth1), col = "red" )

## 3H5
boys.born.after.girls <- birth2[birth1 == 0]
posterior.predictive.distribution <- rbinom(n = trials, size = length(boys.born.after.girls), prob = samples)
dens(posterior.predictive.distribution, adj = .1)
abline(v = sum(boys.born.after.girls), col = "red")
