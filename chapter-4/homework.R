# homework

library(rethinking)

## 4E1

## In this particular case it is not straight forward because all parameters
## are estimated from prior distributions and there is no hint of data anywhere.
## for examplein page 42 of the second edition of the book, he shows this model:

## alist(
##   W ~ dbinom(W + L, p),
##   p ~ dunif(0, 1)
## )

## Which is very similar to the one defined in the example. However, from this
## example is becomes very clear which one is the likelihood and the prior.
## W + L are parameters defined in the data, and their likelihood comes from p
## which is the prior. In this specific case, the likelihood is the first term
## and the prior the second. In our particular example, there is no evidence of
## data anywhere but just definitions of priors.

## after reading page 78-79 it became clear that the likelihood is the first line.
## why? because the first line estimate a normal distribution where mu is now
## another distribution with different mean values and sigma is another
## distribution with different values as well. this first line estimates
## the likelihood of each combination of different values from the distributions
## of mu and sigma. i always thought that the likelihood must contain the data
## but i think i understand the rationale for the likelihood being the first
## line.

## 4E2
## Two, mu and sigma. You can get samples from posterior from these two
## because these are the only ones you're estimating

## 4E3

posterior =
  # Likelihood      *  Prior
  Normal(mu, sigma) * (normal(0, 10) * uniform(0, 10)) /
  # Divided by probability of data
  (normal(0, 10) * uniform(0, 10))


## 4E4
## The second line because it defines the intercept and slope which by definition
## are the linear components of the model. That is, we could define the first
## line (y_i ~ Normal(mu, sigma)) and then define some other type of model
## in mu. For that reason, mu_i is the definition of the linear model

## 4E5
## Three parameters: alpha, beta and sigma


## 4M1
N <- 1e4
y <- rnorm(N, rnorm(N, 0, 10), runif(N, 0, 10))

## 4M2
quap_formula <-
  alist(
    y ~ dnorm(mu, sigma),
    mu ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  )

## 4M3

## It's the same as the model defined in the exercise

## We're predicting y_i which is normally distributed with mean mu and standard
## deviation sigma.
y_i ~ dnorm(mu, sigma)

## mu is define as having an intercept and a beta term which represents
## a slope
mu <- a + b_1 * x_i

## The intercept has a normal distribution with mean 0 and sd 50
a ~ dnorm(0, 50)

## The slope has a uniform distributuion between 0 and 10
b ~ dunif(0, 10)

## The uncertainty of the model is between 0 and 50 with equal outcomes
sigma ~ dunif(0, 50)


## 4M4

## We're predicting heights as normally distributed with mean mu and standard
## deviation sigma.
height_i ~ dnorm(mu, sigma)

## mu is the intercept + a slope for the year multiplied by the year column
## for each person
mu = intercept + beta_1 * x_i

## Because the intercept is the mean height for the average of the years,
## it will always have lower height then the last year assuming that students
## increase height linearly. Moreover, since it is the middle year and some
## students might've increased much more than other I provide a mean
## height of 100cm but a standard deviation of around 30 cm
intercept ~ dnorm(100, 30)

## Students cannot decrease height over time so it must be strictly positive
## For that I use a log-normal distribution. Asking around my workplace people
## think that students increase about 1-2cm each year with some outliers of 3 cm.
## I came up with a log-normal distribution with a mean of 0.03 and sd a 0.3
## which shows a mean of 1.02cm with a maximum of 4cm:

dist <- rlnorm(1e4, 0.03, 0.4)
summary(dist)

# However, only 0.004% a person is allowed to have a height increase of 3cm or more
mean(dist >= 3)

## The lower bound is particular high as it allows an increase below 1cm 47% of
## the time and I find that too much. 

mean(dist <= 1)

## However, I don't have internet to search for different distributions
## (I don't know many), but the ideal distribution would be one where the variation
## will increase to the right but be very concentrated on 1cm.

beta_1 ~ dlnorm(0.03, 0.4)

## Finally, sigma will be left out with a uniform above zero up to 5 as we find it
## hard to believe the there is an increase of higher of 5cm

sigma ~ dunif(0, 5)


## 4M5
## Yes, it leads me to change the intercept to an average of 122 because
## the intercept is the average height in the middle year, which must be
## higher than the first year, which was 120. I also made the standard
## deviation much smaller because I thought again and everyone must be very
## close to 120, maybe 5 cm around.

## The beta prior should not change because we still thinking there is always
## an increase per year and it doesn't say aynthing about how much they increase

height_i ~ dnorm(mu, sigma)
mu = intercept + beta_1 * x_i
intercept ~ dnorm(122, 5)
beta_1 ~ dlnorm(0.03, 0.4)
sigma ~ dunif(0, 5)

## 4M6
## This would impact the variance of the intercept to make it much higher
## and probably force any number higher than 64cm as zero. Having said all of that
## this model needs to account for the within-year clustering of students.


## 4H1
data(Howell1)
d <- Howell1
avg_weight <- mean(d$weight)
sd_weight <- sd(d$weight)

## Standardize weight so intercept is mean height on mean weight
d$weight <- (d$weight - avg_weight) / sd_weight

sim_height <- data.frame(unstd.weight = c(46.95, 43.72, 64.78, 32.59, 54.63))
## standardize the manual weights
sim_height$weight <-
  with(sim_height, (unstd.weight - avg_weight) / sd_weight)

form_height <-
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight,
    a ~ dnorm(178, 20),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  )

m1 <- quap(form_height, data = list(height = d$height, weight = d$weight))

post_height <- sim(m1, data = sim_height)
avg_height <- apply(post_height, 2, mean)
avg_hpdi <- apply(post_height, 2, HPDI)

sim_height$height <- avg_height
sim_height$hpdi <- lapply(1:ncol(avg_hpdi), function(i) avg_hpdi[, i])
sim_height$weight <- NULL
sim_height

## I've contrasted the previos with other solutions and all are different.
## This is because of the prior on a and b.

## 4H2

d2 <- subset(Howell1, age < 18)

## a)
avg_weight <- mean(d2$weight)
sd_weight <- sd(d2$weight)

## Standardize weight so intercept is mean height on mean weight
d2$std.weight <- (d2$weight - avg_weight) / sd_weight

m2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (std.weight / 0.05), # 1SD increase is too much, so this would be a 0.05 increase
    a ~ dnorm(100, 20),
    b ~ dlnorm(0, 0.5),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

precis(m2)

## The average height on the average withgt is 108cm. A 0.05 standard deviation
## increase in weight is associated with a 1.205cm increase in height.
## Even though this samples from very young people, I find the
## increase very very high.

## b)

range_weight <- range(d2$weight)
pred_weight <-
  seq(range_weight[1],
      range_weight[2],
      length.out = 50)

pred_weight_std <- (pred_weight - avg_weight) / sd_weight
posterior_height <- sim(m2, data = data.frame(std.weight = pred_weight_std))
pred_height <- apply(posterior_height, 2, mean)
pred_hpdi <- apply(posterior_height, 2, HPDI)

sim_df <- data.frame(height = pred_height,
                     weight = pred_weight,
                     lower_bound = pred_hpdi[1, ],
                     upper_bound = pred_hpdi[2, ])

library(ggplot2)

ggplot(d2, aes(weight, height)) +
  geom_point() +
  geom_smooth(data = sim_df, aes(weight, height)) +
  geom_ribbon(data = sim_df, aes(ymin = lower_bound, ymax = upper_bound),
              alpha = 1/3)


## c)
## Add splines/polynomials and perhaps play around with log scales



## 4H3
d3 <- Howell1

## a)
m3 <-
  quap(
    alist(
      height ~ dnorm(mu, sigma),
      mu <- a + b * log(weight),
      a ~ dnorm(178, 100),
      b ~ dnorm(0, 100),
      sigma ~ dunif(0, 50)
    ),
    data = d3
  )

## Difficult to interpret
summary(m3)

## Let's plot it

# Get the predicted heights
sim_weight <- seq(4, 65, length.out = 60)
sim_height <- sim(m3, data = data.frame(weight = sim_weight))
pred_height <- apply(sim_height, 2, mean)
pred_hpdi <- apply(sim_height, 2, HPDI)

# Simulate predicted heights
n <- 1e5
posterior.samples <- data.frame( mvrnorm(n = n, mu = coef(m3), Sigma = vcov(m3)) )
mu.link <- function(weight) posterior.samples$a + posterior.samples$b * log(weight)
mu <- sapply(X = sim_weight, FUN = mu.link)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.hpdi <- apply(X = mu, MARGIN = 2, FUN = HPDI, prob = .89)


sim_df <- data.frame(height = pred_height,
                     weight = sim_weight,
                     lower_bound = pred_hpdi[1, ],
                     upper_bound = pred_hpdi[2, ],
                     height_mu = mu.mean,
                     lower_bound_mu = mu.hpdi[1, ],
                     upper_bound_mu = mu.hpdi[2, ]
                     )

ggplot(d3, aes(weight, height)) +
  geom_point(alpha = 1/3) +
  geom_line(data = sim_df, aes(weight, height), color = "blue") +
  ## uncertainty interval around the predicted height
  geom_ribbon(data = sim_df, aes(ymin = lower_bound, ymax = upper_bound),
              alpha = 1/3) +
  ## uncertainty interval around the mean simulated height
  geom_ribbon(data = sim_df, aes(ymin = lower_bound_mu, ymax = upper_bound_mu),
              alpha = 1/3, color = "red") +
  theme_minimal()
