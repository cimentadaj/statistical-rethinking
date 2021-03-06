library(ggplot2)
# homework

## 2E1
## 2
## Option 2 is the most straight forward but (4) if also the answer
## as it is just adjusting for the probability that it is monday


## 2E2
## 3

## 2E3
## 1
## Also (5), see answer to 2E1

## 2E4
## Given our limited knowledge and the results of the experiment, we believe that the true proportion of water in the globe is 0.7.
## Taken from http://rstudio-pubs-static.s3.amazonaws.com/326789_e280ba45916a400f9556212409406da2.html


## 2M1

compute_posterior <- function(w, n, N = 20) {
  p_grid <- seq(0, 1, length.out = N)
  prior <- rep(1, times = N)

  likelihood <- dbinom(x = w, size = n, prob = p_grid)
  unstd.posterior <- likelihood * prior
  std.posterior <- unstd.posterior / sum(unstd.posterior)
  comb_result <- list(p_grid = p_grid,
                      posterior = std.posterior)
  comb_result
}

plot_posterior <- function(p_grid, posterior) {
  qplot(x = p_grid, y =  posterior, geom = "point",
        xlab = "Probability there is water",
        ylab = "Posterior probability") +
    geom_line()
}


## 1)
posterior <- compute_posterior(3, 3)
plot_posterior(posterior$p_grid, posterior$posterior)

## 2)
posterior <- compute_posterior(3, 4)
plot_posterior(posterior$p_grid, posterior$posterior)

## 3)
posterior <- compute_posterior(5, 7)
plot_posterior(posterior$p_grid, posterior$posterior)


## 2M2

compute_posterior <- function(w, n, N = 20) {
  p_grid <- seq(0, 1, length.out = N)
  prior <- ifelse(p_grid < 0.5, 0, .6)
  likelihood <- dbinom(x = w, size = n, prob = p_grid)
  unstd.posterior <- likelihood * prior
  std.posterior <- unstd.posterior / sum(unstd.posterior)
  comb_result <- list(p_grid = p_grid,
                      posterior = std.posterior)
  comb_result
}


## 1)
posterior <- compute_posterior(3, 3)
plot_posterior(posterior$p_grid, posterior$posterior)

## 2)
posterior <- compute_posterior(3, 4)
plot_posterior(posterior$p_grid, posterior$posterior)

## 3)
posterior <- compute_posterior(5, 7)
plot_posterior(posterior$p_grid, posterior$posterior)

# 2M3

likelihood <- c('earth' = .5, 'mars' = .5)
prior <- c('earth' = .3, 'mars' = 1)

unstd.posterior <- likelihood * prior
std.posterior <- unstd.posterior / sum(unstd.posterior)
round(std.posterior, 2)


tst

# 2M4

# I counted this manually!
# For the B/B card
#    B     B
#     B - B
# The first B's mean each card's face and the other tree
# is the opposite side. Here there are two ways to get B/B
# Given that we do not introduce the card back to the bag,
# we do not have two outcomes as McElreath's example with
# the

# For the B/W card
#  W     B
#   B - W
# Here there's only one way to get the first side as black

# The question asked what is probability of getting a black on the other side.
# For that we only have to take proportion of B/B combinations from what we
# counted above. 

black_cards <- c('B/B' = 2, 'B/W' = 1, 'W/W' = 0)
# No need to multiply by the prior because they are all equally likely
# so it would be the same as multiplying by 1

res <- black_cards / sum(black_cards)

# This is probability that it will be B/B
res['B/B']


# 2M5
# Here B/B is 4 because there are now 2 cards with B/B
black_cards <- c('B/B' = 4, 'B/W' = 1, 'W/W' = 0)
# No need to multiply by the prior because they are all equally likely
# so it would be the same as multiplying by 1

res <- black_cards / sum(black_cards)

# This is probability that it will be B/B
res['B/B']

# 2M6
black_cards <- c('B/B' = 2, 'B/W' = 1, 'W/W' = 0)
prior <- c('B/B' = 1, 'B/W' = 2, 'W/W' = 3)

# This is likelihood * prior
unstd.posterior <- black_cards * prior

std.posterior <- unstd.posterior / sum(unstd.posterior)

# This is probability that it will be B/B
std.posterior['B/B']

# 2M7
# I did not understood this one. I'm copying the answer from https://github.com/jffist/statistical-rethinking-solutions/blob/master/ch02_hw.R
# but I'm adding an explanation to my logic behind it.

# For the black card, there are 2 possibility of getting black in the first side.
# We multiply it by the number of possible whites if thiis is the first card
card.bb.likelihood <- 2*3

# For the W/B card, we only have one black but there is the possibility
# of having 2 whites from card W/W
card.wb.likelihood <- 1*2

# From card W/W there is no blacks so anything multipled by 0 will be 0
card.ww.likelihood <- 0

# No need to multiply by the prior (1's) because it's the same in this case
posterior <- c(card.bb.likelihood, card.wb.likelihood, card.ww.likelihood)

posterior <- posterior/sum(posterior)

# The probability that the first card is B/B is now 0.75
posterior[1]

# You can think of as the the computations above as the # of blacks
# adjusted by the possible number of whites


# 2H1
pandas <- c("species_a" = 0.5, "species_b" = 0.5)
prob_twins <- c("species_a" = 0.1, "species_b" = 0.2)

posterior <- pandas * prob_twins
# This is the probability of having twins in the FIRST birth for each species.
# Because we're multiply the likelihood of being any of the two pandas
# with their probability of having twins.
std.posterior <- posterior / sum(posterior)

# With the standardized posterior of having first birth as twins, we multiply
# it again with the probability of having twins to have the probabilities
# of a second birth.
sum(std.posterior * prob_twins)


# 2H2
# This is the probability that the first birh is twins for pandas A.
posterior[1]

# 2H3
pandas <- c("species_a" = 0.5, "species_b" = 0.5)
prob_twins <- c("species_a" = 0.1, "species_b" = 0.2)
posterior <- pandas * (prob_twins * (1 - prob_twins))
posterior <- posterior / sum(posterior)

# Get the probability that it is a singleton
posterior[1]

# 2H4

## 1)
pandas <- c("species_a" = 0.8, "species_b" = 1 - 0.65)
## prob_twins <- c("species_a" = 0.1, "species_b" = 0.2)
posterior <- pandas / sum(pandas)
posterior[1]

## 2)
prob_twins <- c("species_a" = 0.1, "species_b" = 0.2)
posterior <- pandas * prob_twins
posterior <- posterior / sum(posterior)
posterior
