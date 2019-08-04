library(ggplot2)



start_vals <- seq(-2, 2, length.out = 100)
n <- length(start_vals)
intercept <- rnorm(n, 10, 10)
bA <- rnorm(n, 0, 5)
mu <- intercept + (bA * start_vals)
pred <- rnorm(n, mu, exp(1))

tst <- data.frame(intercept, bA, pred, start_vals)

tst %>%
  ggplot() +
  geom_point(aes(start_vals, pred), alpha = 0) +
  geom_abline(aes(intercept = intercept, slope = bA), alpha = .3) +
  coord_cartesian(ylim = range(pred))
