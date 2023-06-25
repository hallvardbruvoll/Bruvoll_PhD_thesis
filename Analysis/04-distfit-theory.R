# Code for generating the plots in chapter 04: House sizes and social meaning
library(tidyverse)


# Log-normal distributions: financial investment example ---------------------

# generate 100 random numbers (uniform distribution) around 1,
  # to use as randomly fluctuating rate in exponential dist.
set.seed(100)
lambda <- function(n = 100, min = 0.75, max = 1.4){
  output <- runif(n = n, min = min, max = max)
  return(output)
}

# make table of 100 individual runs
test5 <- tibble()
for (i in 1:100) {
  one_lambda <- lambda()
  one_run <- tibble(x = 0:100,
                    y = accumulate(one_lambda, prod, .init = 1),
                    # product of previous y and next lambda
                    run = i)
  test5 <- bind_rows(test5, one_run)
}

  # plot all runs in linear and log scales
multi_exp <- ggplot(test5)+
  aes(x, y, colour = as.factor(run))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none")

multi_exp_log <- ggplot(test5)+
  aes(x, y, colour = as.factor(run))+
  geom_line()+
  scale_y_log10()+
  theme_bw()+
  theme(legend.position = "none")

  # plot density of all values at fixed x
lnorm_from_exp <- ggplot(filter(test5, x == 100))+
  aes(x = y)+
  geom_density()+
  theme_bw()+
  labs(x = "y (x = 100)", y = "p(y)")

lnorm_from_exp_log <- ggplot(filter(test5, x == 100))+
  aes(x = y)+
  geom_density()+
  scale_x_log10()+
  theme_bw()+
  labs(x = "y (x = 100)", y = "p(y)")

library(cowplot)
fig04_multi_exp <- plot_grid(multi_exp, multi_exp_log, labels = "auto")
save(fig04_multi_exp, file = "Results/fig04_multi_exp.RData")

fig04_lnorm_exp <- plot_grid(lnorm_from_exp, lnorm_from_exp_log, labels = "auto")
save(fig04_lnorm_exp, file = "Results/fig04_lnorm_exp.RData")

