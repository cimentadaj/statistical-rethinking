library(rethinking)

pr_pos_vampire <- 0.95
pr_pos_mortal <- 0.01
pr_vampire <- 0.001

posterior <- pr_pos_vampire * pr_vampire /
  (pr_pos_vampire * pr_vampire + pr_pos_mortal * (1 - pr_vampire))

posterior




pr_vampire <- 100 / 1e5
pr_pos_vampire <- 95 / 100
pr_pos_mortal <- 999 / 99900

pr_pos_vampire * pr_vampire /
  (pr_pos_vampire * pr_vampire + (pr_pos_mortal * (1 - pr_vampire)))

# You could also do

95 / (95 + 999)


## Hypothesis probability
pr_hyp_positive <- 0.95
pr_hyp_false <- 1 - pr_hyp_positive
pr_true <- 0.01



pr_hyp_positive * pr_true /
  (pr_hyp_positive * pr_true + pr_hyp_false * (1 - pr_true))



n <- 1000
p_grid <- seq(0, 1, length.out = n)
prior <- rep(1, times = n)
prob_data <- dbinom(6, 9, prob = p_grid)
posterior <- prob_data * prior
posterior <- posterior / sum(posterior)

samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
plot(samples)

library(rethinking)
dens(samples)

sum(posterior[p_grid < 0.5])

sum(samples < 0.5) / 1e4

sum(samples > 0.5 & samples < 0.75) / length(samples)

####################################################
n <- 100
p_grid <- seq(0, 1, length.out = n)
prior <- rep(1, times = n)
prob_data <- dbinom(6, 9, p_grid)
posterior <- prob_data * prior
posterior <- posterior / sum(posterior)

samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

PI(samples, prob = 0.5)
HPDI(samples, prob = 0.5)


##################################################
w <- rbinom(1e4, size = 9, p = samples)

simplehist(w)
