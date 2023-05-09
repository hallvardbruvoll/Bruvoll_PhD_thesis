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

# Reproducible random number sequence
set.seed(100)

# Make a random variable for rates of change.
# A rate of 1 generates no change. Below 1 is decrease, above is increase
# Mean and standard deviation can be played around with
change <- rnorm(1000, mean = 1, sd = 0.4)
min(change)
# if min(change) < 0
change <- change-min(change)
min(change)
hist(change)

# Looping function generating synthetic lognormal distributions
iterate.dist <- function(n_points, init_size, iterations, change){
  # Make a set of starting values and the first iteration of change
  output <- tibble(start = seq(init_size, init_size, length.out = n_points),
                   iter_1 = start*sample(change, size = n_points))
  # Loop over the given number of iterations
  for (i in 2:iterations-1) {
  single_iter <- tibble("iter_{i+1}" := pull(output, i)*sample(change, n_points))
  output <- bind_cols(output, single_iter)
  }
  return(output)
}

# Generate distributions
test <- iterate.dist(n_points = 1000, init_size = 50, iterations = 25,
                     change = change)

# Pivot data table, group by iteration and calculate cCDFs
test_long <- pivot_longer(data = test, cols = everything(),
                        names_to = "iteration", names_prefix = "iter_",
                        names_transform = list(iteration = as.numeric),
                        values_to = "house_size",
                        cols_vary = "slowest")
test_long <- test_long %>%
  group_by(iteration) %>%
  mutate(rank = min_rank(house_size),
         ccdf = round((length(rank)-rank+1)/length(rank), 3))

# Plot cCDF, log-log scales
ggplot(filter(test_long, !is.na(iteration)))+
  aes(x = house_size,
      y = ccdf,
      colour = as.factor(iteration))+
  geom_line()+
  theme_minimal()+
  scale_x_log10()+
  scale_y_log10()+
  scale_colour_grey(start = 0.8, end = 0.2)+
  theme(legend.position = "bottom")

# Plot density, log(x)
ggplot(filter(test_long, !is.na(iteration)))+
  aes(x = house_size,
      colour = as.factor(iteration))+
  geom_density()+
  scale_colour_grey(start = 0.8, end = 0.2)+
  scale_x_log10()+
  theme_minimal()

# Fit models to each subset and select/mark power-laws

# Make plots/illustrations


# 1.2 Additive process: effect of temporal resolution--------------------

# Generate data set

# Fit models and select/mark power-laws

# Make plots/illustrations




