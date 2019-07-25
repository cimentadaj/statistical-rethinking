library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18, ]

flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)

m4.1 <- quap(flist, data = d2)

precis(m4.1)

post <- extract.samples(m4.1, n = 1e4)

set.seed(2971)
N <- 100
a <- rnorm(N, 178, 20)
b <- rlnorm(N, 0, 1)

plot(NULL, xlim = range(d2$weight), ylim = c(-100, 400))
xbar <- mean(d2$weight)
abline(h = 0, lty = 2)
abline(h = 272, lty = 1, lwd = 0.5)

for (i in 1:N) {
  curve(
    a[i] + b[i] * (x - xbar),
    from = min(d2$weight),
    to = max(d2$weight),
    add = TRUE,
    col = col.alpha("black", 0.2)
  )
}


flist <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b*(weight - xbar),
  a ~ dnorm(178, 20),
  b ~ dlnorm(0, 1),
  sigma ~ dunif(0, 50)
)

m4.3 <- quap(flist, data = d2)

round(vcov(m4.3), 3)

pairs(m4.3)

plot(height ~ weight, data = d2, col = rangi2)
post <- extract.samples(m4.3)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve(a_map + b_map * (x - xbar), add = TRUE)

plot(height ~ weight, data = d2, col = rangi2)
post <- extract.samples(m4.3)
a_map <- sample(post$a, 100)
b_map <- sample(post$b, 100)

for (i in seq_along(a_map)) {
  curve(a_map[i] + b_map[i] * (x - xbar), add = TRUE,
        col = col.alpha("black", 0.1))
}

post <- extract.samples(m4.3)
mu_at_50 <- post$a + post$b * (50 - xbar)

dens(mu_at_50, col = rangi2, lwd = 2, xlab = "mu|weight=50")

mu <- link(m4.3)
weight.seq <- seq(from = 25, to = 70, by = 1)
mu <- link(m4.3, data = data.frame(weight = weight.seq))

plot(height ~ weight, d2, type = "n")
for (i in 1:100) points(weight.seq, mu[i, ], pch = 16,
                        col = col.alpha(rangi2, 0.1))

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)

plot(height ~ weight, d2, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean, add = TRUE)
shade(mu.HPDI, weight.seq)

samples <- extract.samples(m4.3)
mu.link <- function(post, weight, xbar) post$a + post$b * (weight - xbar)
weight.seq <- seq(25, 70, 1)

res <- sapply(weight.seq, mu.link, post = samples, xbar = xbar)

mu.mean <- apply(res, 2, mean)
mu.HPDI <- apply(res, 2, HPDI)

plot(NULL)
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)

sim.height <- sim(m4.3, data = list(weight = weight.seq), n = 1e4)

height.PI <- apply(sim.height, 2, PI, prob = 0.89)

plot(height ~ weight, d2, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)
shade(height.PI, weight.seq)

plot(height ~ weight, data = d)

d$weight_s <- (d$weight - mean(d$weight)) / sd(d$weight)
d$weight_s2 <- d$weight_s^2

m4.5 <-
  quap(
    alist(
      height ~ dnorm(mu, sigma),
      mu <- a + b1 * weight_s + b2 * weight_s2,
      a ~ dnorm(178, 20),
      b1 ~ dlnorm(0, 1),
      b2 ~ dnorm(0, 1),
      sigma ~ dunif(0, 50)
    ),
    data = d
  )

precis(m4.5)


# The metric changed because it's in sd now
weight.seq <- seq(-2.2, 2, length.out = 30)
weight_df <- data.frame(weight_s = weight.seq, weight_s2 = weight.seq^2)
mu <- link(m4.5, data = weight_df)

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI)

sim_mod <- sim(m4.5, data = weight_df)
overall.mean <- apply(sim_mod, 2, mean)
overall.HPDI <- apply(sim_mod, 2, HPDI)

plot(height ~ weight_s, data = d, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)
shade(overall.HPDI, weight.seq)
lines(weight.seq, overall.mean)


d$weight_s3 <- d$weight_s^3

m4.6 <-
  quap(
    alist(
      height ~ dnorm(mu, sigma),
      mu <- a + b1 * weight_s + b2 * weight_s2 + b3 * weight_s3,
      a ~ dnorm(178, 20),
      b1 ~ dlnorm(0, 1),
      b2 ~ dnorm(0, 1),
      b3 ~ dnorm(0, 1),
      sigma ~ dunif(0, 50)
    ),
    data = d
  )

precis(m4.6)


# The metric changed because it's in sd now
weight.seq <- seq(-2.2, 2, length.out = 30)
weight_df <- data.frame(weight_s = weight.seq,
                        weight_s2 = weight.seq^2,
                        weight_s3 = weight.seq^3)

mu <- link(m4.6, data = weight_df)

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI)

sim_mod <- sim(m4.6, data = weight_df)
overall.mean <- apply(sim_mod, 2, mean)
overall.HPDI <- apply(sim_mod, 2, HPDI)

plot(height ~ weight_s, data = d, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)
shade(overall.HPDI, weight.seq)
lines(weight.seq, overall.mean)

plot(height ~ weight_s, d, col = col.alpha(rangi2, 0.5), xaxt = "n")

data(cherry_blossoms)
d <- cherry_blossoms

precis(d)

plot(temp ~ year, data = d)


d2 <- d[complete.cases(d$temp), ]
num_knots <- 15
knot_list <- quantile(d2$year, probs = seq(0, 1, length.out = num_knots))

library(splines)
B <- bs(d2$year,
        knots = knot_list[-c(1, num_knots)],
        degree = 3,
        intercept = TRUE)

plot(NULL,
     xlim = range(d2$year),
     ylim = c(0, 1),
     xlab = "year",
     ylab = "basis value")

for (i in 1:ncol(B)) lines(d2$year, B[, i])
