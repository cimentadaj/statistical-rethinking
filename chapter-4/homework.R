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


## 4H2
data(Howell1)
d <- Howell1

## Standardize weight so intercept is mean height on mean weight
d$weight <- (d$weight - mean(d$weight)) / sd(d$weight)

sim_height <- data.frame(unstd.weight = c(46.95, 43.72, 64.78, 32.59, 54.63))
## standardize the manual weights
sim_height$weight <-
  with(sim_height, (unstd.weight - mean(unstd.weight)) / sd(unstd.weight))

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







tst

















## 4M1
trials <- 1e3
mu.prior.samples <- rnorm(n = trials, mean = 0, sd = 10)
sigma.prior.samples <- runif(n = trials, min = 0, max = 10)
simulated.heights.from.prior <- rnorm(n = trials,
                                      mean = mu.prior.samples,
                                      sd = sigma.prior.samples)

## 4M2
formula.list <- alist(
  y ~ dnorm( mu , sigma ),
  mu ~ dnorm( 0 , 10 ),
  sigma ~ dunif( 0 , 10 )
)

## 4H1

# load data
data(Howell1)
d <- Howell1
d$weight.centered <- (d$weight - mean(d$weight)) / sd(d$weight)

# build model
model <- map(
  alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta*weight.centered,
    alpha ~ dnorm(mean = 0, sd = 100),
    beta ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 64)
  ),
  data = d
)

# simulate heights from model
individual.weights <- c(46.95, 43.72, 64.78, 32.59, 54.63)
individual.weights.centered <- (individual.weights - mean(d$weight)) / sd(d$weight)
simulated.heights <- sim(model, data = list(weight.centered = individual.weights.centered))

# summarize results
simulated.heights.mean <- apply(X = simulated.heights, MARGIN = 2, FUN = mean)
simulated.heights.PI <- apply(X = simulated.heights, MARGIN = 2, FUN = PI, prob = .89)

# try it manually for the first individual
posterior.samples <- extract.samples(model)
simulated.heights.first.individual <- rnorm(n = trials, mean = posterior.samples$alpha + posterior.samples$beta*individual.weights.centered[1], sd = posterior.samples$sigma)
simulated.heights.first.individual.mean <- mean(simulated.heights.first.individual)
simulated.heights.first.individual.PI <- PI(samples = simulated.heights.first.individual, prob = .89)

## 4H2
data(Howell1)
d <- Howell1
d <- d[d$age < 18,]

# scale weight column
d$weight.standardized <- d$weight

# a)
model <- map(
  alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta*weight.standardized,
    alpha ~ dnorm(mean = 100, sd = 100),
    beta ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 50)
  ),
  data = d
)

precis(model)

# b)
library(MASS)
trials <- 1e5

weight.seq <- seq(from = 1, to = 45, length.out = 50)

# simulate mu then compute mean and hpdi
posterior.samples <- data.frame( mvrnorm(n = trials, mu = coef(model), Sigma = vcov(model)) )
mu.link <- function(weight) posterior.samples$alpha + posterior.samples$beta * weight
mu <- sapply(X = weight.seq, FUN = mu.link)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.hpdi <- apply(X = mu, MARGIN = 2, FUN = HPDI, prob = .89)

# simulate heights then compute hpdi
height.link <- function(weight) rnorm(n = nrow(posterior.samples), mean = mu.link(weight), sd = posterior.samples$sigma)
height.samples <- sapply(X = weight.seq, FUN = height.link)
height.hpdi <- apply(X = height.samples, MARGIN = 2, FUN = HPDI, prob = .89)

# plot results
plot(height ~ weight.standardized, data = d, col = col.alpha(rangi2, .5))
lines(x = weight.seq, y = mu.mean)
shade(object = mu.hpdi, lim = weight.seq)
shade(object = height.hpdi, lim = weight.seq)

## 4H3
data(Howell1)
d <- Howell1
trials <- 1e5

model <- map(
  alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta*log(weight),
    alpha ~ dnorm(mean = 178, sd = 100),
    beta ~ dnorm(mean = 0, sd = 100),
    sigma ~ dunif(min = 0, max = 50)
  ),
  data = d
)

# simulate mu then compute mean and hpdi
weight.seq <- seq(from = 1, to = 70, length.out = 100)
posterior.samples <- data.frame( mvrnorm(n = trials, mu = coef(model), Sigma = vcov(model)) )
mu.link <- function(weight) posterior.samples$alpha + posterior.samples$beta * log(weight)
mu <- sapply(X = weight.seq, FUN = mu.link)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.hpdi <- apply(X = mu, MARGIN = 2, FUN = HPDI, prob = .89)

# simulate heights then compute hpdi
height.link <- function(weight) rnorm(n = nrow(posterior.samples), mean = mu.link(weight), sd = posterior.samples$sigma)
height.samples <- sapply(X = weight.seq, FUN = height.link)
height.hpdi <- apply(X = height.samples, MARGIN = 2, FUN = HPDI, prob = .89)

# plot results
plot(height ~ weight, data = d, col = col.alpha(rangi2, .4))
lines(x = weight.seq, y = mu.mean)
shade(object = mu.hpdi, lim = weight.seq)
shade(object = height.hpdi, lim = weight.seq)
