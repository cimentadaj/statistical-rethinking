library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

rm(WaffleDivorce)
unloadNamespace("rethinking")

library(brms)
library(tidyverse)
library(ggrepel)
library(bayesplot)

my_scale <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

d %>%
  ggplot(aes(WaffleHouses / Population, Divorce)) +
  geom_point(color = "firebrick", alpha = 1/2) +
  geom_smooth(method = 'lm', color = "firebrick", fill = "firebrick",
              alpha = .2) +
  labs(x = "Waffle houses per million",
       y = "Divorce rate") +
  geom_text(data = filter(d, Loc %in% c("ME", "OK", "AR", "AL", "GA", "SC")),
            aes(label = Loc)) +
  coord_cartesian(xlim = 0:50, ylim = 5:15) +
  theme_bw() +
  theme(panel.grid = element_blank())

d <-
  d %>%
  mutate(MedianAgeMarriage_z = my_scale(MedianAgeMarriage),
         Marriage_z = my_scale(Marriage))

b5.1 <-
  brm(Divorce ~ 1 + MedianAgeMarriage_z,
      data = d,
      family = gaussian,
      prior = c(prior(normal(10, 10), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)
                ),
      iter = 2000, warmup = 500, chains = 4, cores = 4
      )


nd <- tibble(MedianAgeMarriage_z = seq(-3, 3, length.out = 30))

f <-
  fitted(b5.1, newdata = nd) %>%
  as_tibble() %>%
  bind_cols(nd)

d %>%
  ggplot(aes(MedianAgeMarriage_z)) +
  geom_point(aes(y = Divorce), color = "firebrick", alpha = 1/2) +
  geom_line(data = f, aes(y = Estimate), color = "firebrick") +
  geom_ribbon(data = f, aes(x = MedianAgeMarriage_z,
                            ymin = `Q2.5`, ymax = `Q97.5`),
              alpha = .1,
              fill = "firebrick") +
  theme_bw() +
  theme(panel.grid = element_blank())


b5.2 <-
  brm(Divorce ~ 1 + Marriage_z,
      data = d,
      family = gaussian,
      prior = c(prior(normal(10, 10), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)
                ),
      iter = 2000, warmup = 500, chains = 4, cores = 4
      )

nd <- tibble(Marriage_z = seq(-2.5, 3.5, length.out = 30))

f <-
  fitted(b5.2, newdata = nd) %>%
  as_tibble() %>%
  bind_cols(nd)

d %>%
  ggplot(aes(Marriage_z)) +
  geom_point(aes(y = Divorce), color = "firebrick", alpha = 1/2) +
  geom_line(data = f, aes(y = Estimate), color = "firebrick") +
  geom_ribbon(data = f, aes(x = Marriage_z,
                            ymin = `Q2.5`, ymax = `Q97.5`),
              alpha = .1,
              fill = "firebrick") +
  theme_bw() +
  theme(panel.grid = element_blank())


b5.3 <-
  brm(Divorce ~ 1 + MedianAgeMarriage_z + Marriage_z,
      data = d,
      family = gaussian,
      prior = c(prior(normal(10, 10), class = Intercept),
                prior(normal(0, .5), class = b),
                prior(exponential(1), class = sigma)
                ),
      iter = 2000, warmup = 500, chains = 4, cores = 4
      )

post <- posterior_samples(b5.3)

mcmc_intervals(post[, 0:4],
               prob = .5,
               point_est = "median")

post %>%
  select(-lp__) %>%
  as_tibble() %>%
  gather() %>%
  ggplot()

f <-
  residuals(b5.3) %>%
  as_tibble() %>%
  bind_cols(d)


f %>%
  ggplot(aes(Estimate, Divorce)) +
  geom_point() +
  geom_smooth(method = "lm")

counterfactual <-
  tibble(
    Marriage_z = seq(-3, 3, length.out = 30),
    MedianAgeMarriage_z = mean(d$MedianAgeMarriage_z)
  )

fitted(b5.3, counterfactual) %>%
  as_tibble() %>%
  rename(f_ll = `Q2.5`,
         f_ul = `Q97.5`) %>%
  bind_cols(
    predict(b5.3, newdata = counterfactual) %>% as_tibble(),
    counterfactual
  ) %>%
  ggplot(aes(Marriage_z, Estimate)) +
  geom_smooth(color = "firebrick",
              method = "lm") +
  geom_ribbon(aes(ymin = `Q2.5`, ymax = `Q97.5`),
              fill = "firebrick",
              alpha = .2) +
  lims(y = range(d$Divorce))


library(rethinking)
data(milk)
d <- milk
rm(milk)
detach(package:rethinking, unload = TRUE)

# `kcal.per.g` regressed on `perc.fat`
b5.10 <- 
  brm(data = d, family = gaussian,
      kcal.per.g ~ 1 + perc.fat,
      prior = c(prior(normal(.6, 10), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5)

b5.11 <- 
  update(b5.10,
         newdata = d,
         formula = kcal.per.g ~ 1 + perc.lactose)

tidy(b5.11)

posterior_summary(b5.10) %>% round(digits = 3)

posterior_summary(b5.11) %>% round(digits = 3)

n <- 100

set.seed(5)
d <- 
  tibble(h0        = rnorm(n, mean = 10, sd = 2), 
         treatment = rep(0:1, each = n / 2),
         fungus    = rbinom(n, size = 1, prob = .5 - treatment * 0.4),
         h1        = h0 + rnorm(n, mean = 5 - 3 * fungus, sd = 1))


b5.13 <- 
  brm(data = d, family = gaussian,
      h1 ~ 1 + h0 + treatment + fungus,
      prior = c(prior(normal(0, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.99),
      seed = 5)

b5.14 <-
  update(b5.13,
         h1 ~ 1 + h0 + treatment)

