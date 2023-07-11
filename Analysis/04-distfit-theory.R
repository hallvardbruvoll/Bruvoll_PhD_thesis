# Code for generating the plots in chapter 04: House sizes and social meaning
library(tidyverse)
library(poweRlaw) # for power-law random number generation
library(cowplot) # for combining multiple plots per figure
library(latex2exp) # for tex expressions inside ggplot code

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

fig04_still_normal <- plot_grid(still_normal, still_normal_dens, labels = "auto")
save(fig04_still_normal, file = "Results/fig04_still_normal.RData")


# Power-law distributions: combinations of exponentials and hierarchy --------
my_pl_1 <- tibble(x = 0:1000, y = dplcon(x, xmin = 1, alpha = 1.5))
my_pl_2 <- tibble(x = 0:1000, y = dplcon(x, xmin = 1, alpha = 2))
my_pl_3 <- tibble(x = 0:1000, y = dplcon(x, xmin = 1, alpha = 3))
my_pl <- bind_rows("1.5" = my_pl_1,"2" = my_pl_2,"3" = my_pl_3, .id = "model")

coord_1 <- my_pl_1 %>% #coordinates for dashed lines
  filter(x %in% c(10, 1000))
coord_1 <- coord_1 %>%
  add_row(x = 1000, y = coord_1$y[1]) %>%
  arrange(desc(y))

coord_3 <- my_pl_3 %>%
  filter(x %in% c(100, 1000))
coord_3 <- coord_3 %>%
  add_row(x = 100, y = coord_3$y[2]) %>%
  arrange(x)

my_pl_fig <- ggplot(filter(my_pl, x>0))+
  aes(x, y, colour = model)+
  geom_line()+
  geom_path(data = coord_1, aes(x, y), linetype = 2, colour = "red")+
  geom_path(data = coord_3, aes(x, y), linetype = 2, colour = "blue")+
  geom_text(aes(x = 100, y = 0.1),
            label = "3/2 = 1.5", colour = "red")+
  geom_text(aes(x = 50, y = 10^-7),
            label = "3/1 = 3", colour = "blue")+
  scale_x_log10(minor_breaks = rep(1:9, 3)*(10^rep(0:2, each = 9)))+
  scale_y_log10(minor_breaks = rep(1:9, 10)*10^rep(-9:0, each = 9))+
                #labels = scales::comma)+
  labs(x = "x", y = "p(x)", colour = TeX("$\\alpha$"))+
  theme_bw()
# save it below

# Power law as hierarchy --------------------------------------------------
my_hierarchy <- tibble(NULL)
iterations <- 4
sizes <- pexp(0:iterations, rate = 0.8, lower.tail = FALSE)
frequency <- 3^(0:iterations)

for (i in 1:length(frequency)) {
  if (i == 1) { # first point
  units <- tibble(size = sizes[i], level = i, y = 0.5)
  }
  else  {
    if (i > 3) { # for levels 4 and up (recursive)
      # assign full and empty spots for the entire row
      n_y_slots <- frequency[i]+sum(frequency[i]/3^(1:(i-2))*2)
      # with coordinates
      y_pos <- tibble(y = seq(0,1, length.out = n_y_slots), n = 1:n_y_slots)
      for (j in 1:(i-2)) { # loop for hierarchical grouping
        # vector of empty slots only
        dump <- as.integer(c(seq(1, n_y_slots, by = n_y_slots/3^j),
          seq(n_y_slots/3^j, n_y_slots, by = n_y_slots/3^j)))
        # keep full spots and update number of cells
        y_pos <- filter(y_pos, !n %in% dump) %>%
          mutate(n = 1:n())
        n_y_slots <- nrow(y_pos)
        }
      }
    else { # for levels 2 and 3 only
      n_y_slots <- frequency[i]+(frequency[i]/3*2)
      # coordinates
      y_pos <- tibble(y = seq(0,1, length.out = n_y_slots), n = 1:n_y_slots)
      # vector of empty slots only
      dump <- c(seq(1,n_y_slots, by = 5), seq(5,n_y_slots, by = 5))
      # filter out filled slots only
      y_pos <- filter(y_pos, !n %in% dump)
      }
    # add size
    units <- tibble(size = rep(sizes[i], frequency[i]),
                  level = i, y = y_pos$y)
    }
  my_hierarchy <- bind_rows(my_hierarchy, units)
}
# adjust coordinates
my_hierarchy <- my_hierarchy %>%
  mutate(y_wide = y*30) %>%
  rownames_to_column(var = "n")

my_hierarchy_fig <-  ggplot(my_hierarchy)+
  aes(x = y*40, y = size*20,
      height = sqrt(size), width = sqrt(size),
      #size = size,
      label = n)+
  geom_tile()+
  #geom_label()+
  #geom_point(shape = 15)+
  #scale_size_area()+
  #scale_y_log10()+
  coord_fixed()+
  theme_void()+
  theme(legend.position = "none")

fig04_pl <- plot_grid(my_pl_fig, my_hierarchy_fig, labels = "auto")
save(fig04_pl, file = "Results/fig04_pl.RData")
