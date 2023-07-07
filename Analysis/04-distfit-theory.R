# Code for generating the plots in chapter 04: House sizes and social meaning
library(tidyverse)
library(poweRlaw) # for power-law random number generation
library(cowplot) # for combining multiple plots per figure


# Intro: Examples of heavy-tailed distributions ---------------------------

# Generate examples of normal, exponential, log-normal and power-law
# distributions, with convenient parameter values (just for visibility)

x <- 0:1000
normal <- tibble(x, PDF = dnorm(x, mean = 50, sd = 8),
                 cCDF = pnorm(x, mean = 50, sd = 8, lower.tail = FALSE))
exponential <- tibble(x, PDF = dexp(x, rate = 0.1),
                      cCDF = pexp(x, rate = 0.1, lower.tail = FALSE))
lognormal <- tibble(x, PDF = dlnorm(x, meanlog = 3, sdlog = 0.5),
                    cCDF = plnorm(x, meanlog = 3,
                                  sdlog = 0.5, lower.tail = FALSE))
powerlaw <- tibble(x, PDF = dplcon(x, xmin = 1, alpha = 3),
                   cCDF = pplcon(x, xmin = 1, alpha = 3, lower.tail = FALSE))

distributions <- bind_rows(normal = normal, exponential = exponential,
                           "log-normal" = lognormal,
                          "power-law" = powerlaw, .id = "Type")

# PDFs, linear and logarithmic scales
PDF_lin <- ggplot(filter(distributions, x<100 &x>2))+
  aes(x = x, y = PDF, colour = Type)+
  geom_line()+
  theme_bw()+
  theme(legend.position = "bottom")

PDF_log <- ggplot(filter(distributions, x<100 &x>2))+
  aes(x = x, y = PDF, colour = Type)+
  geom_line()+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()+
  theme(legend.position = "none")

# cCDFs, linear and logarithmic scales
cCDF_lin <- ggplot(filter(distributions, x< 80 & x>0))+
  aes(x = x, y = cCDF, colour = Type)+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none")

cCDF_log <- ggplot(filter(distributions, x< 80 & x>0))+
  aes(x = x, y = cCDF, colour = Type)+
  geom_line()+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()+
  theme(legend.position = "none")

legend <- get_legend(PDF_lin)

# Put together and store
fig04_PDF <- plot_grid(plot_grid(PDF_lin+theme(legend.position = "none"),
                                 PDF_log, labels = "auto", nrow = 1),
                       legend, ncol = 1, rel_heights = c(2,0.1))
fig04_cCDF <- plot_grid(plot_grid(cCDF_lin, cCDF_log,
                                  labels = "auto", nrow = 1),
                        legend, ncol = 1, rel_heights = c(2,0.1))
save(fig04_PDF, file = "Results/fig04_PDF.RData")
save(fig04_cCDF, file = "Results/fig04_cCDF.RData")

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
multi_exp <- ggplot(filter(test5, x < 40))+
  aes(x, y, colour = as.factor(run))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none")

multi_exp_log <- ggplot(filter(test5, x < 40))+
  aes(x, y, colour = as.factor(run))+
  geom_line()+
  scale_y_log10()+
  theme_bw()+
  theme(legend.position = "none")

  # plot density of all values at fixed x
lnorm_from_exp <- ggplot(filter(test5, x == 40))+
  aes(x = y)+
  geom_density()+
  theme_bw()+
  labs(x = "y (x = 40)", y = "p(y)")

lnorm_from_exp_log <- ggplot(filter(test5, x == 40))+
  aes(x = y)+
  geom_density()+
  scale_x_log10()+
  theme_bw()+
  labs(x = "y (x = 40)", y = "p(y)")

fig04_multi_exp <- plot_grid(multi_exp, multi_exp_log, labels = "auto")
save(fig04_multi_exp, file = "Results/fig04_multi_exp.RData")

fig04_lnorm_exp <- plot_grid(lnorm_from_exp, lnorm_from_exp_log, labels = "auto")
save(fig04_lnorm_exp, file = "Results/fig04_lnorm_exp.RData")


# not log-normal without spread in rate -----------------------------------

# generate 100 random numbers (uniform distribution) around 1,
# to use as randomly fluctuating rate in exponential dist.
set.seed(100)
lambda <- runif(n = 100, min = 0.75, max = 1.4)

# make table of 100 individual runs
test6 <- tibble()
init <- rnorm(n = 100, mean = 10, sd = 2) # initial values are normally distr.
for (i in 1:100) {
  one_run <- tibble(x = 0:100,
                    y = accumulate(lambda, prod, .init = sample(init, 1)),
                    # product of previous y and next lambda
                    run = i)
  test6 <- bind_rows(test6, one_run)
}

# plot all runs
still_normal <- ggplot(filter(test6, x < 40))+
  aes(x, y, colour = as.factor(run))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none")

# plot density of all values at fixed x
still_normal_dens <- ggplot(filter(test6, x == 40))+
  aes(x = y)+
  geom_density()+
  theme_bw()+
  labs(x = "y (x = 40)", y = "p(y)")

library(cowplot)
fig04_still_normal <- plot_grid(still_normal, still_normal_dens, labels = "auto")
save(fig04_still_normal, file = "Results/fig04_still_normal.RData")

