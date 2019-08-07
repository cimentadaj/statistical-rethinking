# homework

library(rethinking)
library(ggplot2)
library(dplyr)
library(brms)
## 5E1

## 4 for sure. 2 I'm not entirely sure because it doesn't have
## an intercept and the intercept reflects the beginning of a slope.
## Having said that, it has two parameters to be estimated and two slopes.
## I think it's also a a multiple linear regression.


## 5E2

anim_div ~
  alpha +
  b1 * lat +
  b2 * plant_div


## 5E3
n <- 1e4
constant <- 0.8
size_lab <- rnorm(n, 10)
# The minus sign makes the corr negative between the two
funding <- rnorm(n, -size_lab * constant, sd = sqrt(1-constant^2))
# Time is the addition of the previous two
time <- rnorm(n, size_lab + funding, sd = 0.5)
sim_df <- data.frame(time, size_lab, funding)

summary(lm(time ~ size_lab, data = sim_df))
summary(lm(time ~ funding, data = sim_df))
summary(lm(time ~ funding + size_lab, data = sim_df))

mod1 <-
  quap(
    alist(
      time ~ dnorm(mu, sigma),
      mu <- a + b1 * funding,
      a ~ dnorm(0, 0.2),
      b1 ~ dnorm(0, 0.5),
      sigma ~ dexp(1)
    ),
    data = sim_df
  )

precis(mod1)


mod2 <-
  quap(
    alist(
      time ~ dnorm(mu, sigma),
      mu <- a + b1 * size_lab,
      a ~ dnorm(0, 0.2),
      b1 ~ dnorm(0, 0.5),
      sigma ~ dexp(1)
    ),
    data = sim_df
  )

precis(mod2)

mod3 <-
  quap(
    alist(
      time ~ dnorm(mu, sigma),
      mu <- a + b1 * size_lab + b2 * funding,
      a ~ dnorm(0, 0.2),
      b1 ~ dnorm(0, 0.5),
      b2 ~ dnorm(0, 0.5),      
      sigma ~ dexp(1)
    ),
    data = sim_df
  )

precis(mod3)


## 5E4
data(milk) 
d <- milk
d$catvar <- as.factor(d$clade)
levels(d$catvar) <- c('A','B','C','D')

(d$catvar.A <- as.integer(d$catvar=='A'))
(d$catvar.B <- as.integer(d$catvar=='B'))
(d$catvar.C <- as.integer(d$catvar=='C'))
(d$catvar.D <- as.integer(d$catvar=='D'))

ggplot(d, aes(catvar, kcal.per.g, fill=catvar)) +
  geom_boxplot()


mod1 <-
  quap(
    alist(
      kcal.per.g ~ dnorm(mu, sigma),
      mu <- a + bA * catvar.A + bB * catvar.B + bD * catvar.D,
      a ~ dnorm(0.6, 0.3),
      bA ~ dnorm(0, 1),
      bB ~ dnorm(0, 1),
      bD ~ dnorm(0, 1),
      sigma ~ dexp(1)
    ),
    data = d
  )

sam_mod1 <- extract.samples(mod1)

mod3 <-
  quap(
    alist(
      kcal.per.g ~ dnorm(mu, sigma),
      mu <- a + bB * catvar.B + bC * catvar.C + bD * catvar.D,
      a ~ dnorm(0.6, 0.3),
      bB ~ dnorm(0, 1),
      bC ~ dnorm(0, 1),
      bD ~ dnorm(0, 1),
      sigma ~ dexp(1)
    ),
    data = d
  )

sam_mod3 <- extract.samples(mod3)

## Turn 1st model into 3rd model

## mod1
bC = a
bA = a + bA
bB = a + bB
bD = a + bD

## mod3
a = bA
bB = a + bB
bC = a + bC
bD = a + bD

## mod1 to mod3
a = a + bA
bB = bB - bA
bC = a - (a + bA)
bD = bD - bA

mod1_to_mod3 <-
  with(sam_mod1,
       data.frame(
         a = a + bA,
         bB = bB - bA,
         bC = a - (a + bA),
         bD = bD - bA,
         sigma = sigma,
         mod = "mod1to3",
         stringsAsFactors = FALSE
       )
   )

sim_post <-
  mod1_to_mod3 %>%
  bind_rows(sam_mod3)

  
sim_post %>%
  ggplot(aes(bB, fill = mod)) +
  geom_density()

sim_post %>%
  ggplot(aes(bC, fill = mod)) +
  geom_density()

sim_post %>%
  ggplot(aes(bD, fill = mod)) +
  geom_density()


mod2 <-
  quap(
    alist(
      kcal.per.g ~ dnorm(mu, sigma),
      mu <- a + bA * catvar.A + bB * catvar.B + bC * catvar.C + bD * catvar.D,
      a ~ dnorm(0.6, 0.3),
      bA ~ dnorm(0, 1),
      bB ~ dnorm(0, 1),
      bC ~ dnorm(0, 1),
      bD ~ dnorm(0, 1),
      sigma ~ dexp(1)
    ),
    data = d
  )

precis(mod2)

sam_mod2 <- extract.samples(mod2)

## Turn 2nd model into 3rd model
## Whever the 0 categoy is, because there's an intercept, it's alway relative
## so the operations are all intercept + deviation

## mod2
bA = a + bA
bB = a + bB
bC = a + bC
bD = a + bD

## mod3
a = bA
bB = a + bB
bC = a + bC
bD = a + bD

## mod1 to mod3
a = a + bA
bB = (a + bB) - (a + bA)
bC = a - (a + bA)
bD = bD - bA


mod2_to_mod3 <-
  with(sam_mod2,
       data.frame(
         a = a + bA,
         bB = (a + bB) - (a + bA),
         bC = (a + bC) - (a + bA),
         bD = (a + bD) - (a + bA),
         sigma = sigma,
         mod = "mod2to3",
         stringsAsFactors = FALSE
       )
   )

sim_post <-
  mod2_to_mod3 %>%
  bind_rows(sam_mod3)
  
sim_post %>%
  ggplot(aes(bB, fill = mod)) +
  geom_density()

sim_post %>%
  ggplot(aes(bC, fill = mod)) +
  geom_density()

sim_post %>%
  ggplot(aes(bD, fill = mod)) +
  geom_density()

## Turn 4th model into 3rd model
## All coefficients are means for each category
mod4 <-
  quap(
    alist(
      kcal.per.g ~ dnorm(mu, sigma),
      mu <- bA * catvar.A + bB * catvar.B + bC * catvar.C + bD * catvar.D,
      bA ~ dnorm(0, 1),
      bB ~ dnorm(0, 1),
      bC ~ dnorm(0, 1),
      bD ~ dnorm(0, 1),
      sigma ~ dexp(1)
    ),
    data = d
  )

precis(mod4)

sam_mod4 <- extract.samples(mod4)

## mod4
bA = bA
bB = bB
bC = bC
bD = bD

## mod3
a = bA
bB = a + bB
bC = a + bC
bD = a + bD

## mod1 to mod3
a = bA
bB = bB - bA
bC = bC - bA
bD = bD - bA


mod4_to_mod3 <-
  with(sam_mod4,
       data.frame(
         a = bA,
         bB = bB - bA,
         bC = bC - bA,
         bD = bD - bA,
         sigma = sigma,
         mod = "mod4to3",
         stringsAsFactors = FALSE
       )
   )

sim_post <-
  mod4_to_mod3 %>%
  bind_rows(sam_mod3)
  
sim_post %>%
  ggplot(aes(bB, fill = mod)) +
  geom_density()

sim_post %>%
  ggplot(aes(bC, fill = mod)) +
  geom_density()

sim_post %>%
  ggplot(aes(bD, fill = mod)) +
  geom_density()


## Turn 5th model into 3rd model
## This model is essentially the same model as before. The
## expression in bA is just saying that catvar.A is the
## extra category whenever all the rows are 0 in any of columns.
## For example, for a row which belongs to catvar.B the operation
## would yield a 0: (1 - 1 - 0 -0) but for an operation where
## all catvar are 0, it would yield a 1, suggesting it's an A category
## (1 - 0 - 0 -0)
## You need to think of it literally as new variable which is 1 - (all other
## categories)

mod5 <-
  quap(
    alist(
      kcal.per.g ~ dnorm(mu, sigma),
      mu <-
        bA * (1 - catvar.B - catvar.C - catvar.D) +
        bB * catvar.B +
        bC * catvar.C +
        bD * catvar.D,
      bA ~ dnorm(0, 1),
      bB ~ dnorm(0, 1),
      bC ~ dnorm(0, 1),
      bD ~ dnorm(0, 1),
      sigma ~ dexp(1)
    ),
    data = d
  )

precis(mod5)

sam_mod5 <- extract.samples(mod5)

## mod5
bA = bA
bB = bB
bC = bC
bD = bD

## mod3
a = bA
bB = a + bB
bC = a + bC
bD = a + bD

## mod5 to mod3
a = bA
bB = bB - bA
bC = bC - bA
bD = bD - bA

mod5_to_mod3 <-
  with(sam_mod5,
       data.frame(
         a = bA,
         bB = bB - bA,
         bC = bC - bA,
         bD = bD - bA,
         sigma = sigma,
         mod = "mod4to3",
         stringsAsFactors = FALSE
       )
   )

sim_post <-
  mod5_to_mod3 %>%
  bind_rows(sam_mod3)
  
sim_post %>%
  ggplot(aes(bB, fill = mod)) +
  geom_density()

sim_post %>%
  ggplot(aes(bC, fill = mod)) +
  geom_density()

sim_post %>%
  ggplot(aes(bD, fill = mod)) +
  geom_density()


## 5M1
## By multiplying by rho we influence the specific correlation
## coefficient.

n <- 1e4
rho <- .7
x <- rnorm(n)
y <- rnorm(n, x * rho, sqrt(1 - rho^2))
cor(x, y)

outcome <- rnorm(n, x * sqrt(1 - rho^2))

summary(lm(outcome ~ y))
summary(lm(outcome ~ x))
summary(lm(outcome ~ x + y))


## 5M2
n <- 1e4
rho <- .7
x <- rnorm(n)
y <- rnorm(n, x * rho, sqrt(1 - rho^2))
outcome <- rnorm(n, x - y)
cor_df <- data.frame(outcome, x, y)
pairs(cor_df)

precis(lm(outcome ~ y))
precis(lm(outcome ~ x))
precis(lm(outcome ~ x + y))

## 5M3
## One possible scenario is changes over times. The marriage rate is done
## as proportion of couples getting married over the total population.

## If in t_1 the divorce rate decreases then in t_2 marriage rate could
## increase simply because in t_1 the stock of people that can get married
## increased.

## We could evaluate it using an indicator of time and marriage and divorce
## rates

## 5M4

data(WaffleDivorce)
d <- WaffleDivorce

library(htmltab)
q <- htmltab('https://www.worldatlas.com/articles/mormon-population-by-state.html',
             which="//div[@id='artReg-table']/table")

stopifnot("Percentage of Mormon Residents" %in% names(q))
stopifnot(nrow(q) == 51)
names(q)[2] <- 'Location'

lcd <-
  q %>%
  select(Location, 'Percentage of Mormon Residents') %>% 
  rename(ratio_str = 'Percentage of Mormon Residents') %>%
  mutate(lcd_ratio = as.double(gsub("[%]", "", ratio_str)))

d2 <- left_join(d, select(lcd, Location, lcd_ratio), by = 'Location')


my_scale <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

d2 <-
  d2 %>%
  mutate(
    MedianAgeMarriage_z = my_scale(MedianAgeMarriage),
    Marriage_z = my_scale(Marriage),
    lcd_ratio_z = my_scale(lcd_ratio),
    Divorce_z = my_scale(Divorce)
  )


mod1 <-
  brm(
    Divorce_z ~ MedianAgeMarriage_z + Marriage_z,
    data = d2,
    family = gaussian,
    prior = c(prior(normal(0, 0.2), class = Intercept),
              prior(normal(0, 0.5), class = b),
              prior(exponential(1), class = sigma)),
    iter = 2000, warmup = 1000, chains = 4, cores = 4
  )

mod2 <-
  update(mod1,
         Divorce_z ~ MedianAgeMarriage_z + Marriage_z + lcd_ratio_z,
         newdata = d2)

sam_mod1 <- posterior_samples(mod1)
sam_mod2 <- posterior_samples(mod2)

range_vals <- range(d2$MedianAgeMarriage_z)
newdata <-
  data.frame(
    MedianAgeMarriage_z = seq(range_vals[1], range_vals[2], length.out = 30),
    Marriage_z = 0
  )

newdata2 <- newdata
newdata2$lcd_ratio_z <- 0

first_predict <- as.data.frame(fitted(mod1, newdata = newdata))
second_predict <- as.data.frame(fitted(mod2, newdata = newdata2))

first_intervals_df <-
  first_predict %>%
  rename(lower = `Q2.5`,
         upper = `Q97.5`) %>%
  mutate(key = newdata$MedianAgeMarriage_z)

second_intervals_df <-
  second_predict %>%
  rename(lower = `Q2.5`,
         upper = `Q97.5`) %>%
  mutate(key = newdata$MedianAgeMarriage_z)

d2 %>%
  ggplot() +
  geom_point(aes(MedianAgeMarriage_z, Divorce_z)) +
  geom_abline(
    data = summarize_all(sam_mod1, mean),
    aes(intercept = b_Intercept, slope = b_MedianAgeMarriage_z),
    color = "red"
  ) +
  geom_ribbon(
    data = first_intervals_df,
    aes(x = key, ymin = lower, ymax = upper),
    alpha = .1,
    fill = "red"
  ) +
  geom_abline(
    data = summarize_all(sam_mod2, mean),
    aes(intercept = b_Intercept, slope = b_MedianAgeMarriage_z),
    color = "blue"
  ) +
  geom_ribbon(
    data = second_intervals_df,
    aes(x = key, ymin = lower, ymax = upper),
    alpha = .1,
    fill = "blue"
  )

## 5H1
data(foxes)
d <- foxes

mod1 <-
  brm(
    weight ~ area,
    data = d,
    family = gaussian,
    prior = c(prior(normal(0, 10), class = Intercept),
              prior(normal(0, 10), class = b),
              prior(exponential(1), class = sigma)),
    iter = 2000, warmup = 1000, chains = 4, cores = 4
  )
pred_df <- data.frame(area = seq(1, 5.30, length.out = 100))

pred_line <- posterior_samples(mod1,
                               newdata = pred_df
                               )
d %>%
  ggplot(aes(area, weight)) +
  geom_abline(
    data = pred_line,
    aes(intercept = b_Intercept, slope = b_area),
    alpha = .01
  ) +
  geom_point()


mod2 <-
  brm(
    weight ~ groupsize,
    data = d,
    family = gaussian,
    prior = c(prior(normal(0, 10), class = Intercept),
              prior(normal(0, 10), class = b),
              prior(exponential(1), class = sigma)),
    iter = 2000, warmup = 1000, chains = 4, cores = 4
  )

pred_df <- data.frame(groupsize = seq(2, 8, length.out = 100))

pred_line <- posterior_samples(mod2,
                               newdata = pred_df
                               )

d %>%
  ggplot(aes(groupsize, weight)) +
  geom_point() +
  geom_abline(
    data = pred_line,
    aes(intercept = b_Intercept, slope = b_groupsize),
    alpha = .01
  )

## Group size seems to be important

## 5H2

mod3 <-
  update(
    mod2,
    weight ~ area + groupsize,
    newdata = d
  )

pred_df <-
  data.frame(
    groupsize = seq(2, 8),
    area = mean(d$area)
  )

pred_line_previous <-
  fitted(
    mod2,
    newdata = data.frame(groupsize = seq(2, 8))
  ) %>%
  as.data.frame()

pred_line_previous <-
  setNames(pred_line_previous, paste0(names(pred_line_previous), "_mod2"))

pred_line <-
  cbind(
    pred_df,
    fitted(mod3, newdata = pred_df) %>% as.data.frame(),
    pred_line_previous
  )

select <- dplyr::select

first <-
  pred_line %>%
  dplyr::select(Q2.5, Q2.5_mod2) %>%
  gather() %>%
  rename(lower = value) %>%
  select(lower)

second <-
  pred_line %>%
  dplyr::select(Q97.5, Q97.5_mod2) %>%
  gather() %>% 
  rename(upper = value) %>%
  select(upper)  

pred_line <-
  pred_line %>%
  dplyr::select(-matches("Est.Error"), -starts_with("Q")) %>%
  gather(... = matches("Estimate")) %>%
  bind_cols(
    first,
    second
  )

d %>%
  ggplot() +
  geom_point(aes(groupsize, weight)) +
  geom_line(
    data = pred_line,
    aes(x = groupsize, y = value, color = key),
  ) +
  geom_ribbon(
    data = pred_line,
    aes(x = groupsize, ymin = lower, ymax = upper, fill = key),
    alpha = .1
  )



pred_df <-
  data.frame(
    area = seq(1, 5.30, length.out = 100),
    groupsize = mean(d$groupsize)
  )

pred_line_previous <-
  fitted(
    mod1,
    newdata = data.frame(area = pred_df$area)
  ) %>%
  as.data.frame()

pred_line_previous <-
  setNames(pred_line_previous, paste0(names(pred_line_previous), "_mod1"))

pred_line <-
  cbind(
    pred_df,
    fitted(mod3, newdata = pred_df) %>% as.data.frame(),
    pred_line_previous
  )

first <-
  pred_line %>%
  dplyr::select(Q2.5, Q2.5_mod1) %>%
  gather() %>%
  rename(lower = value) %>%
  select(lower)

second <-
  pred_line %>%
  dplyr::select(Q97.5, Q97.5_mod1) %>%
  gather() %>% 
  rename(upper = value) %>%
  select(upper)  

pred_line <-
  pred_line %>%
  dplyr::select(-matches("Est.Error"), -starts_with("Q")) %>%
  gather(... = matches("Estimate")) %>%
  bind_cols(
    first,
    second
  )

d %>%
  ggplot() +
  geom_point(aes(area, weight)) +
  geom_line(
    data = pred_line,
    aes(x = area, y = value, color = key),
  ) +
  geom_ribbon(
    data = pred_line,
    aes(x = area, ymin = lower, ymax = upper, fill = key),
    alpha = .1
  )

## 5H3

mod4 <-
  brm(
    weight ~ avgfood + groupsize,
    data = d,
    family = gaussian,
    prior = c(prior(normal(0, 10), class = Intercept),
              prior(normal(0, 10), class = b),
              prior(exponential(1), class = sigma)),
    iter = 2000, warmup = 1000, chains = 4, cores = 4
  )



mod5 <-
  update(
    mod4,
    weight ~ avgfood + groupsize + area,
    newdata = d
  )


## A model with area and groupsize bears only a slope of 0.62 and -0.43 for
## each respectively.
round(posterior_summary(mod3), 2)

## A model with avgfood and groupsize has slopws of 3.75 and -0.56 respectively,
## stronger than the previous model and probably significantly different between
## the two.
round(posterior_summary(mod4), 2)


mod_df <-
  rbind(
    tidy(mod3) %>% mutate(mode = "mod3"),
    tidy(mod4) %>% mutate(mode = "mod4")
  )

## For example, we can see that in the 4th model, avgfood is very uncertain
## but it's still much stronger than area alone.
mod_df %>%
  filter(grepl("b_", term)) %>% 
  ggplot(aes(term, estimate)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  facet_wrap(~ mode)

## As per point (b), I think it could be because they are very correlated and
## there's multicollinearity. For example, in the simulation below, we see the
## same thing:
n <- 1e4
x <- rnorm(n)
y <- rnorm(n, x, sd = 0.1)
outcome <- rnorm(n, x^2 + y, sd = 25) + runif(n)

precis(lm(outcome ~ x))
precis(lm(outcome ~ y))
precis(lm(outcome ~ x + y))
