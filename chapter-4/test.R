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
