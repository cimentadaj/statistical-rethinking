# notes

## 5.1 - model divorce rate as a function of the (standarized) median age of marriage
library(rethinking)
library(tidyverse)
library(rstan)

data("WaffleDivorce")
d <- WaffleDivorce

# McElreaths way
m5.1 <-
  quap(
    alist(
      D ~ dnorm(mu, sigma),
      mu <- a + bA * A,
      a ~ dnorm(0, 0.2),
      bA ~ dnorm(0, 0.5),
      sigma ~ dexp(1)
    ),
    data = d
  )



set.seed(10)
prior <- extract.prior(m5.1)
mu <- link(m5.1, post = prior, data = list(A = c(-2, 2)))
plot(NULL, xlim = c(-2, 2), ylim = c(-2, 2))
for (i in 1:50) lines(c(-2, 2), mu[i, ], col = col.alpha("black", 0.4))

A_seq <- seq(from = -3, to = 3.2, length.out = 30)
mu <- link(m5.1, data = list(A = A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

pred <- sim(m5.1, data = list(A = A_seq))
pred_mean <- apply(pred, 2, mean)
pred_PI <- apply(pred, 2, PI)

plot(D ~ A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)
lines(A_seq, pred_mean, col = "red")
shade(pred_PI, A_seq)


stan_m5_2 <- "
data {
  int<lower=0> N;
  vector[N] divorce;
  vector[N] marriage;
}
parameters {
  real a;
  real bM;
  real<lower=0, upper=10> sigma;
}
model {
  vector[N] mu = a + marriage * bM;
  target += normal_lpdf(divorce | mu, sigma);
  target += normal_lpdf(a | 0, 0.2);
  target += normal_lpdf(bM | 0, 0.5);
  target += exponential_lpdf(sigma | 1);
}
"

m <- stan_model(model_code = stan_m5_2)

dat <-
  list(
    N = nrow(d),
    divorce = d$Divorce,
    marriage = d$Marriage_z
  )

fit05_2 <- sampling(m, data = dat, iter = 1000, chains = 2, cores = 2)

print(fit05_2, probs = c(.1, .5, .9))

# McElreaths way
d$M <- scale(d$Marriage)
m5.2 <-
  quap(
    alist(
      D ~ dnorm(mu, sigma),
      mu <- a + bM * M,
      a ~ dnorm(0, 0.2),
      bM ~ dnorm(0, 0.5),
      sigma ~ dexp(1)
    ),
    data = d
  )

M_seq <- seq(-1.7, 2.7, length.out = 50)
pred <- link(m5.2, data = list(M = M_seq))
pred_mean <- apply(pred, 2, mean)
pred_PI <- apply(pred, 2, PI)

plot(D ~ M, data = d, col = rangi2)
lines(M_seq, pred_mean, lwd = 2)
shade(pred_PI, M_seq)


post <- as.data.frame(fit05_1)

f_mu <- function(x) post$a + post$bA * x
A_z_new <- seq(-3, 3)

mu <-
  sapply(A_z_new, f_mu) %>%
  as_tibble(.name_repair = "unique") %>%
  set_names(A_z_new) %>% 
  mutate(Iter = row_number()) %>%
  gather(A_z, divorce, -Iter) %>%
  group_by(A_z) %>%
  mutate(mu = mean(divorce)) %>%
  ungroup() %>%
  mutate(A_z = as.numeric(A_z))

# plot raw data and model estimate of mu
p <- ggplot() 
p1 <- p + 
  geom_point(data = d,
             aes(MedianAgeMarriage_z, Divorce), 
             shape = 1, color = 'dodgerblue') +
  geom_line(data = mu,
            aes(x = A_z, y = mu))


m5.3 <-
  quap(
    alist(
      D ~ dnorm(mu, sigma),
      mu <- a + bA * A + bM * M,
      a ~ dnorm(0, 0.2),
      bA ~ dnorm(0, 0.5),
      bM ~ dnorm(0, 0.5),
      sigma ~ dexp(1)
    ),
    data = d
  )







plot(coeftab(m5.1, m5.2, m5.3), par = c("bA", "bM"))


m5.4 <-
  quap(
    alist(
      M ~ dnorm(mu, sigma),
      mu <- a + bAM * A,
      a ~ dnorm(0, 0.2),
      bAM ~ dnorm(0, 0.5),
      sigma ~ dexp(1)
    ),
    data = d
  )

mu <- link(m5.4)
mu_mean <- apply(mu, 2, mean)
mu_resid <- d$M - mu_mean

M_seq <- seq(-2, 3, length.out = 30)

pred_data <- data.frame(M = M_seq, A = 0)

mu <- link(m5.3, data = pred_data)
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

D_sim <- sim(m5.3, data = pred_data, n = 1e4)
D_PI <- apply(D_sim, 2, PI)

plot(D ~ M, data = d)
lines(M_seq, mu_mean)
shade(mu_PI, M_seq)
shade(D_PI, M_seq)


mu <- link(m5.3)
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

D_sim <- sim(m5.3, n = 1e4)
D_PI <- apply(D_sim, 2, PI)

N <- 100
x_real <- rnorm(N)
x_spur <- rnorm(N, x_real)
y <- rnorm(N, x_real)
d_tst <- data.frame(y, x_real, x_spur)

pairs(d_tst)


data(milk)
d <- milk
str(d)

d$K <- my_scale(d$kcal.per.g)
d$N <- my_scale(d$neocortex.perc)
d$M <- my_scale(log(d$mass))

dcc <- d[complete.cases(d$K, d$N, d$M), ]

m5.5_draft <-
  quap(
    alist(
      K ~ dnorm(mu, sigma),
      mu <- a + bN * N,
      a ~ dnorm(0, 1),
      bN ~ dnorm(0, 1),
      sigma ~ dexp(1)
    ),
    data = dcc
  )

prior_plots <- function(model, name, start_vals) {
  data <- setNames(list(start_vals), name)
  prior <- extract.prior(model)
  mu <- link(model, post = prior, data = data)
  plot(NULL, xlim = range(start_vals), ylim = range(start_vals))
  for (i in seq_len(nrow(mu))) {
    lines(start_vals, mu[i, ], col = col.alpha("black", 0.3))
  }
}

prior_plots(m5.5_draft, "N", c(-2, 0, 2))

m5.5 <-
  quap(
    alist(
      K ~ dnorm(mu, sigma),
      mu <- a + bN * N,
      a ~ dnorm(0, 0.2),
      bN ~ dnorm(0.5, 0.5),
      sigma ~ dexp(1)
    ),
    data = dcc
  )

prior_plots(m5.5, "N", c(-2, 0, 2))

precis(m5.5)

start_vals <- c(-2, 2)
N <- 1e3
a <- rnorm(N, 0, 0.2)
bN <- rnorm(N, 0, 0.5)
mu <- matrix(c(a, bN), nrow = N, ncol = 2)

plot(NULL, xlim = range(start_vals), ylim = range(start_vals))
for (i in seq_len(nrow(mu))) {
  lines(start_vals, mu[i, ], col = col.alpha("black", 0.3))
}





## From this moment I will use the brms package because it's more standard
## in Bayesian estimation and has many tools.

library(rethinking)
data(Howell1)
d <- Howell1
d$sex <- ifelse(d$male == 1, "male", "female")

m5.8 <-
  brm(height ~ 0 + sex,
      family = gaussian,
      prior = c(prior(cauchy(0, 2), class = sigma),
                prior(normal(178, 20), class = b)
                ),
      data = d,
      iter = 2000,
      warmup = 500,
      chains = 4,
      cores = 4
      )

post <- posterior_samples(m5.8)
post$diff_fm <- post[, 2] - post[, 1]
posterior_summary(post)


data(milk)
d <- milk
unique(d$clade)


d$K <- my_scale(d$kcal.per.g)

m5.9 <-
  brm(K ~ 0 + clade,
      family = gaussian,
      prior = c(prior(exponential(1), class = sigma),
                prior(normal(0, 0.5), class = b)
                ),
      data = d,
      iter = 2000,
      warmup = 500,
      chains = 4,
      cores = 4
      )




mcmc_intervals(m5.9, regex_pars = "^b_") +
  geom_vline(xintercept = 0)

set.seed(63)
d$house <- as.character(sample(rep(2:5, each = 8), size = nrow(d)))

m5.10 <-
  update(m5.9,
         formula = K ~ 0 + clade + house,
         newdata = d)

mcmc_intervals(m5.10, regex_pars = "b_")


as_tibble(posterior_samples(m5.10))


tst
























## 5.2 - plot the confidence interval around the mean of the Guassian
median.age.marriage.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(m5.1, data = data.frame(MedianAgeMarriage.standardized=median.age.marriage.seq))
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI)

plot(Divorce ~ MedianAgeMarriage.standardized, data = d, col=rangi2)
abline(m5.1)
shade(object = mu.PI, lim = median.age.marriage.seq)

## 5.3 - model divorce rate as a function of the (standardized) marriage rate
d$Marriage.standardized <- (d$Marriage - mean(d$Marriage)) / sd(d$Marriage)
m5.2 <- map(
  alist(
    Divorce ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.R * Marriage.standardized,
    alpha ~ dnorm(mean = 10, sd = 10),
    beta.R ~ dnorm(mean = 0, sd = 1),
    sigma ~ dunif(min = 0, max = 10)
  ),
  data = d
)

## 5.4 - model divorce rate as a function of both marriage rate and median age of marriage
m5.3 <- map(
  alist(
    Divorce ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.median.age.marriage * MedianAgeMarriage.standardized + beta.marriage.rate * Marriage.standardized,
    alpha ~ dnorm(mean = 10, sd = 10),
    beta.median.age.marriage ~ dnorm(mean = 0, sd = 1),
    beta.marriage.rate ~ dnorm(mean = 0, sd = 1),
    sigma ~ dunif(min = 0, max = 10)
  ),
  data = d
)

## 5.6 - model marriage rate as a function of median age of marriage
m5.4 <- map(
  alist(
    Marriage.standardized ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.median.age.marriage * MedianAgeMarriage.standardized,
    alpha ~ dnorm(mean = 0, sd = 10),
    beta.median.age.marriage ~ dnorm(mean = 0, sd = 1),
    sigma ~ dunif(min = 0, max = 10)
  ),
  data = d
)

## 5.7 - compute marriage rate residuals
mu <- coef(m5.4)['alpha'] + coef(m5.4)['beta.median.age.marriage'] * d$MedianAgeMarriage.standardized
marriage.rate.residuals <- d$Marriage.standardized - mu

## 5.8 - plot residuals
plot(Marriage.standardized ~ MedianAgeMarriage.standardized, data = d, col=rangi2)
abline(m5.4)
for (i in 1:length(marriage.rate.residuals)) {
  x <- d$MedianAgeMarriage.standardized[i]
  y <- d$Marriage.standardized[i]
  lines( c(x, x), c(mu[i], y), lwd = .5, col = col.alpha("black", .7))
}

## 5.9 - create counterfactual plot for standardized marriage rate vs. divorce rate

# prepare new counterfactual data
median.age.marriage.average <- mean(d$MedianAgeMarriage.standardized)
marriage.rate.seq <- seq(from = -3, to = 3, length.out = 30)
pred.data <- data.frame(Marriage.standardized = marriage.rate.seq, MedianAgeMarriage.standardized = median.age.marriage.average)

# compute counterfactual mean divorce rate
mu <- link(m5.3, data=pred.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI)

# simulate counterfactual divorce rate outcomes
divorce.rate.simulations <- sim(m5.3, data = pred.data, n = 1e4)
divorce.rate.simulations.PI <- apply(X = divorce.rate.simulations, MARGIN = 2, FUN = PI)

# plot results
plot(Divorce ~ Marriage.standardized, data = d, type = "n")
mtext("MedianAgeMarriage.standardized = 0")
lines(marriage.rate.seq, mu.mean)
shade(object = mu.PI, lim = marriage.rate.seq)
shade(object = divorce.rate.simulations.PI, lim = marriage.rate.seq)

## 5.10 - create counterfactual plot for standardized median age of marriage vs. divorce rate

# prepare new counterfactual data
marriage.rate.average <- mean(d$MedianAgeMarriage.standardized)
median.age.marriage.seq <- seq(from = -3, to = 3, length.out = 30)
pred.data <- data.frame(Marriage.standardized = marriage.rate.average, MedianAgeMarriage.standardized = median.age.marriage.seq)

# compute counterfactual mean divorce rate
mu <- link(m5.3, data = pred.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI)

# simulate counterfactual divorce rate outcomes
divorce.rate.simulations <- sim(m5.3, data = pred.data, n = 1e4)
divorce.rate.simulations.PI <- apply(X = divorce.rate.simulations, MARGIN = 2, FUN = PI)

# plot results
plot(Divorce ~ MedianAgeMarriage.standardized, data = d, type = "n")
mtext("MedianAgeMarriage.standardized = 0")
lines(median.age.marriage.seq, mu.mean)
shade(object = mu.PI, lim = median.age.marriage.seq)
shade(object = divorce.rate.simulations.PI, lim = median.age.marriage.seq)

## 5.11 - simulate divorce rates using our original data
mu <- link(m5.3)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI)
divorce.rate.simulations <- sim(m5.3, n = 1e4)
divorce.rate.simulations.PI <- apply(X = divorce.rate.simulations, MARGIN = 2, FUN = PI)

## 5.12 - plot actual divorce rates vs. observed divorce rates
plot( mu.mean ~ d$Divorce , col=rangi2 , ylim=range(mu.PI) ,
      xlab="Observed divorce" , ylab="Predicted divorce" )
abline( a=0 , b=1 , lty=2 )
for ( i in 1:nrow(d) )
  lines( rep(d$Divorce[i],2) , c(mu.PI[1,i],mu.PI[2,i]) ,
         col=rangi2 )

## 5.13
identify(x = d$Divorce, y = mu.mean, labels = d$Loc, cex = .8)

## 5.14 - compute and plot model residuals
divorce.rate.residuals <- d$Divorce - mu.mean
o <- order(divorce.rate.residuals)
dotchart( divorce.rate.residuals[o] , labels=d$Loc[o] , xlim=c(-6,5) , cex=0.6 )
abline( v=0 , col=col.alpha("black",0.2) )
for ( i in 1:nrow(d) ) {
  j <- o[i] # which State in order
  lines( d$Divorce[j]-c(mu.PI[1,j],mu.PI[2,j]) , rep(i,2) )
  points( d$Divorce[j]-c(divorce.rate.simulations.PI[1,j], divorce.rate.simulations.PI[2,j]), rep(i,2), pch=3 , cex=0.6 , col="gray" )
}

## 5.16
data(milk)
d <- milk
str(d)

## 5.62
data(cars)
glimmer(dist ~ speed, data = cars)
