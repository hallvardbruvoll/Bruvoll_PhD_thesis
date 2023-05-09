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

change <- rnorm(1000, mean = 1, sd = 0.4)
min(change)
# if min(change) < 0
change <- change-min(change)
hist(change)
min(change)

init_size <- 10
n_points <- 100


iterate.dist <- function(n_points, init_size, iterations, change){
  output <- tibble(start = seq(init_size, init_size, length.out = n_points),
                   iter_1 = start*sample(change, size = n_points))

  for (i in 2:iterations-1) {
  single_iter <- tibble("iter_{i+1}" := pull(output, i)*sample(change, n_points))
  output <- bind_cols(output, single_iter)
  }
  return(output)
}

test <- iterate.dist(n_points = 100, init_size = 10, iterations = 10,
                     change = change)

test_long <- pivot_longer(data = test, cols = everything(),
                        names_to = "iteration", values_to = "house_size",
                        cols_vary = "slowest")
test_long <- test_long %>%
  group_by(iteration) %>%
  mutate(iteration = as_factor(iteration),
         rank = min_rank(house_size),
         ccdf = round((length(rank)-rank+1)/length(rank), 3),
         group = as.numeric(iteration))

ggplot(test_long)+
  aes(x = house_size,
      y = ccdf,
      colour = iteration)+
  geom_line()+
  theme_minimal()+
  scale_x_log10()+
  scale_y_log10()+
  scale_colour_grey(start = 0.8, end = 0.2)+
  theme(legend.position = "none")



# Fit models to each subset and select/mark power-laws

# Make plots/illustrations


# 1.2 Additive process: effect of temporal resolution--------------------

# Generate data set

# Fit models and select/mark power-laws

# Make plots/illustrations




