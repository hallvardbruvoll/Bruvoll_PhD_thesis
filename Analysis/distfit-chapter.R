# Analyses for Part II - Size distributions
library(tidyverse)
library(poweRlaw)


# Wrapper and loop functions ----------------------------------------------

# Fill in and modify from poweRlaw_script.R
  # Don't do distfitting for every single row - very redundant and slow
  # Use group_by etc. and do it once per group.

# 1 Synthetic distributions -----------------------------------------------

# 1.1 Multiplicative process: from normal to power-law and back again -----

# OBS, the poweRlaw package doesn't include normal distributions. To start with
# that is not a problem, since the distribution is constructed as normal.
# When going back, use shapiro.test() to test for normality.

# Generate data set
set.seed(100)

# Multiplying all values with a single value results in wider normal
# distributions:

return <- rnorm(1000, mean = 1, sd = 0.4)
return <- return-min(return)
min(return)
hist(return)
constant <- 5
dists <- tibble(gen_1 = seq(40,40, length.out = 1000),
                gen_2 = gen_1*sample(x = return, size = length(gen_1))+constant,
                gen_3 = gen_2*sample(return, length(gen_1))+constant,
                gen_4 = gen_3*sample(return, length(gen_1))+constant,
                gen_5 = gen_4*sample(return, length(gen_1))+constant,
                gen_6 = gen_5*sample(return, length(gen_1))+constant,
                gen_7 = gen_6*sample(return, length(gen_1))+constant,
                gen_8 = gen_7*sample(return, length(gen_1))+constant,
                gen_9 = gen_8*sample(return, length(gen_1))+constant)
min(dists)

ggplot(dists)+
  aes(x = gen_1)+
  #geom_density()+
  geom_density(aes(x = gen_2), colour = 2)+
  geom_density(aes(x = gen_3), colour = 3)+
  geom_density(aes(x = gen_4), colour = 4)+
  geom_density(aes(x = gen_5), colour = 5)+
  geom_density(aes(x = gen_6), colour = 6)+
  geom_density(aes(x = gen_7), colour = 7)+
  geom_density(aes(x = gen_8), colour = 8)+
  geom_density(aes(x = gen_9), colour = 9)

ggplot(dists)+
  aes(x = normal,
      y = gen_4)+
  geom_point()

dists$normal/dists$gen_4

# Do this tomorrow:
# To generate skewed distributions from a normal, I need to multiply the
# normal with an exponential.
# Order both, so that each value is multiplied with the one of the same rank.
# Higher rates give shorter range in exp-dist.
# Add 1 to the exp-dist, to avoid multiplying values with <1.
# Iterate by raising powers incrementally.
# This is a bit harder to explain socially than what I had hoped for.
# Can't I just make a feedback loop???
# Try a linear multiplier as well maybe. Stop for today. Pensez-y.

# Fit models to each subset and select/mark power-laws

# Make plots/illustrations


# 1.2 Additive process: effect of temporal resolution--------------------

# Generate data set

# Fit models and select/mark power-laws

# Make plots/illustrations




