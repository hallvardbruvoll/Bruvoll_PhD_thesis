# Code for generating the plots in Chapter 05: Methods - Distribution fitting

# Load functions and libraries
source("Analysis/Distfit-functions.R")

# Pre-analysis 1: testing for false positive power laws ---------------------

# Set parameter values
mu <- 0.3
sigma <- 2
lambda <- 0.125
alpha <- 2.5
scale <- 3
shape <- 0.5
xmin <- 15
n <- c(10, 100, 1000, 10000)
dists <- c("ln", "exp", "pl", "str exp")

# Generate data sets
pretest_data <- tibble()
set.seed(100) # Reproducible random numbers

for (j in 1:length(dists)) { # Loop for distribution types
  for (i in 1:length(n)) { # Loop for n (distribution sizes)
    if (j == 1) { # Log-normal distributions, generate a large enough dist.
                    # to filter out only values above xmin, and sample to
                      # needed size (same for exponential and stretched exp.)
      single <- tibble(x = rlnorm(n = 100000, mu, sigma)) %>%
        filter(x >= xmin) %>%
        slice_sample(n = n[i])
    }
    if (j == 2) { # Exponential distributions
      single <- tibble(x = rexp(n = 100000, lambda)) %>%
        filter(x >= xmin) %>%
        slice_sample(n = n[i])
    }
    if (j == 3) { # Power-law distributions
      single <- tibble(x = rplcon(n = n[i], xmin = 15, alpha = alpha))
    }
    if (j == 4) { # Stretched exponential
      single <- tibble(x = rweibull(n = 100000, shape, scale)) %>%
        filter(x >= xmin) %>%
        slice_sample(n = n[i])
      }
    single <- single %>%
      mutate(set = paste0(dists[j], n[i]),
             set_type = dists[j],
             n = n[i])
    pretest_data <- bind_rows(pretest_data, single)
  }
} # pretest_data should end with 44440 obs.
rm(single)

# Fit tail models to all distributions (this takes a while!)
pretest_results <- dist.fit.all(x = pretest_data$x,
                                set = pretest_data$set)

# Remove models x and y here, not needed
pretest_results <- filter(pretest_results, model == FALSE)

pretest_results <- pretest_results %>% # set type and n fell out, add again
  mutate(set_type = str_remove_all(set, "[10]"),
         n = parse_number(set))

# Summarise results for all (16) distributions
pretest_summary <- pretest_results %>%
  group_by(set, set_type, tail, n, ntail) %>%
  summarise()

# Plot distributions of synthetic data (same as Clauset et al. 2009, fig. 5a)
fig05_synthdist <- ggplot(filter(pretest_results, n == 100))+
  aes(x = value, y = ccdf, colour = set_type, shape = set_type)+
  geom_point()+
  scale_x_log10(labels = label_log(), breaks = c(10, 100, 1000))+
  scale_y_log10(labels = label_log())+
  scale_shape_manual(values = c(16, 17, 15, 18))+
  geom_vline(xintercept = 15, linetype = 2)+
  theme_bw()+
  labs(x = "x", y = "P(x)", colour = "", shape = "")

# Plot distribution type and best fit tail
fig05_type_tail <- ggplot(pretest_summary)+
  aes(x = set_type,
      y = n,
      size = ntail/n,
      shape = tail,
      colour = tail)+
  geom_point()+
  scale_y_log10()+
  scale_shape_manual(values = c(16, 17, 15, 18))+
  theme_bw()+
  labs(x = "Distribution type", shape = "Tail model", colour = "Tail model")

# Plot range of power-law fits over synthetic distributions
fig05_synth_pl <- ggplot(pretest_results)+
  aes(x = value, y = set)+
  geom_boxplot()+
  geom_point(data = filter(pretest_results, value >= xmin & tail == "pl"),
               colour = "red", shape = 1)+
  scale_x_log10(labels = scales::comma)+
  labs(x = "x", y = "Distribution")+
  theme_bw()

# Store output
save(pretest_results, file = "Results/pretest_results.RData")
save(pretest_summary, file = "Results/pretest_summary.RData")
save(fig05_synthdist, file = "Results/fig05_synthdist.RData")
save(fig05_type_tail, file = "Results/fig05_type_tail.RData")
save(fig05_synth_pl, file = "Results/fig05_synth_pl.RData")

# Pre-analysis 2: parameter scan for log-normal distributions ---------------

set.seed(100) # Reset random number generation
pretest2_results <- tibble()

# Set parameter values (natural logarithms)
# 6^2 parameter combinations gives 36 different distributions
logmean_range <- 1:6
exp(logmean_range)
logsd_range <- seq(0.5, 3, by = 0.5)
exp(logsd_range)
n <- 1000 # 36.000 data points

for (i in 1:length(logmean_range)) {
  for (j in 1:length(logsd_range)) {
    one_dist <- tibble(x = rlnorm(n = n,
                                  meanlog = logmean_range[i],
                                  sdlog = logsd_range[j]),
                       dist = paste0(format(logsd_range[j], nsmall = 1),
                                     "_",
                                     format(logmean_range[i], nsmall = 1)))
    pretest2_results <- bind_rows(pretest2_results, one_dist)
  }
}
rm(one_dist)

# Analyse the data (take a coffee break)
pretest2_results <- dist.fit.all(x = pretest2_results$x,
                                 set = pretest2_results$dist)

pretest2_results <- filter(pretest2_results, model == FALSE)

# Add back values for mean and sd from character string
pretest2_results <- pretest2_results %>%
  mutate(logmean = as.numeric(str_sub(set, -1)),
         logsd = as.numeric(str_sub(set, 1, 3)))

pretest2_summary <- pretest2_results %>%
  group_by(set, logmean, logsd, tail, xmin, ntail, par1) %>%
  summarise(n = n())

# Plot all distributions
fig05_synth_ln <- ggplot(pretest2_results)+
  aes(x = value, y = ccdf, colour = logsd, group = set)+
  geom_line()+
  scale_x_log10(labels = scales::comma)+
  scale_y_log10()+
  labs(x = "x", y = "P(x)", colour = TeX("$\\sigma$"))+
  theme_bw()

fig05_ln_tail <- ggplot(pretest2_summary)+
  aes(x = logmean, y = logsd,
      colour = tail, size = ntail/n, shape = tail)+
  geom_point()+
  scale_shape_manual(values = c(16, 17, 15, 18))+
  labs(x = TeX("$\\mu$"), y = TeX("$\\sigma$"),
       colour = "Tail model", shape = "Tail model")+
  theme_bw()

# Plot distributions and overlay with pl tails
fig05_ln_pl <- ggplot(pretest2_results)+
  aes(x = value, y = set)+
  geom_boxplot()+
  geom_point(data = filter(pretest2_results, tail == "pl" & value >= xmin),
             colour = "red")+
  scale_x_log10(labels = scales::comma)+
  labs(x = "x", y = TeX("$\\sigma\\_\\mu$"))+
  theme_bw()

# Store output
save(pretest2_results, file = "Results/pretest2_results.RData")
save(pretest2_summary, file = "Results/pretest2_summary.RData")
save(fig05_synth_ln, file = "Results/fig05_synth_ln.RData")
save(fig05_ln_tail, file = "Results/fig05_ln_tail.RData")
save(fig05_ln_pl, file = "Results/fig05_ln_pl.RData")

# Pre-analysis 3: Power laws from data aggregation --------------------------

# Generate data set
set.seed(100)
n <- 100
iter <- 20

# Set fixed param values based on my results from settlements
pretest3_results <- tibble(x = rlnorm(n = n,
                                      meanlog = 4.5,
                                      sdlog = 0.4),
                           iter = 1)
previous <- pretest3_results

for (i in 2:iter) {
  one_dist  <- tibble(x = c(rlnorm(n = n,
                                 meanlog = 4.5,
                                 sdlog = 0.4),
                        previous$x),
                      iter = i)
  previous <- one_dist
  pretest3_results <- bind_rows(pretest3_results, one_dist)
}
rm(one_dist)
rm(previous)

# Fit models and select/mark power-laws
pretest3_results <- dist.fit.all(x = pretest3_results$x,
                                 set = pretest3_results$iter)
pretest3_results <- filter(pretest3_results, model == FALSE)

pretest3_results <- pretest3_results %>%
  group_by(set) %>%
  mutate(rank = as.numeric(rank),
         set = as.numeric(set),
         rev_rank = max(rank)-rank+1)

pretest3_summary <- pretest3_results %>%
  group_by(set, tail, xmin, ntail, par1) %>%
  summarise(n = n()) %>%
  mutate(tail_P = ntail/n)

# Make plots/illustrations
fig05_multi_ln <- ggplot(pretest3_results)+
  aes(x = value, y = rev_rank, group = set, colour = set)+
  geom_line()+
  #geom_point()+
  geom_line(data = filter(pretest3_results, tail == "pl" &
                             value >= xmin), colour = "red", size = 1)+
  scale_x_log10()+
  scale_y_log10()+
  labs(x = "x", y = "Rank", colour = "Iteration")+
  theme_bw()+
  theme(legend.position = "bottom")

fig05_multi_ln_box <- ggplot(pretest3_results)+
  aes(x = value, y = set, group = set)+
  geom_boxplot()+
  geom_point(data = filter(pretest3_results, tail == "pl" &
                             value >= xmin), colour = "red")+
  scale_x_log10()+
  labs(x = "x", y = "Iteration")+
  theme_bw()

save(pretest3_results, file = "Results/pretest3_results.RData")
save(pretest3_summary, file = "Results/pretest3_summary.RData")
# combine plots and save below

# Pre-analysis 4: Data aggregation, mixed distributions -------------------
set.seed(100)

n <- 100
iter <- 20 # 21.000 data points
logsd_range <- round(runif(n = 20, min = 0.3, max = 0.5), 2)
meanlog_range <- round(runif(n = 20, min = 4, max = 5), 1)


pretest4_results <- tibble(x = rlnorm(n = n,
                                      meanlog = 4.5,
                                      sdlog = 0.4),
                           iter = 1)
previous <- pretest4_results

for (i in 2:iter) {
  one_dist  <- tibble(x = c(rlnorm(n = n,
                                   meanlog = sample(meanlog_range, 1),
                                   sdlog = sample(logsd_range, 1)),
                            previous$x),
                      iter = i)
  previous <- one_dist
  pretest4_results <- bind_rows(pretest4_results, one_dist)
}
rm(one_dist)
rm(previous)

# Fit models and select/mark power-laws
pretest4_results <- dist.fit.all(x = pretest4_results$x,
                                 set = pretest4_results$iter)
pretest4_results <- filter(pretest4_results, model == FALSE)

pretest4_results <- pretest4_results %>%
  group_by(set) %>%
  mutate(rank = as.numeric(rank),
         set = as.numeric(set),
         rev_rank = max(rank)-rank+1)

pretest4_summary <- pretest4_results %>%
  group_by(set, tail, xmin, ntail, par1) %>%
  summarise(n = n()) %>%
  mutate(tail_P = ntail/n)

# Make plots/illustrations
fig05_multi_mix <- ggplot(pretest4_results)+
  aes(x = value, y = rev_rank, group = set, colour = set)+
  geom_line()+
  geom_line(data = filter(pretest4_results, tail == "pl" &
                            value >= xmin), colour = "red", size = 1)+
  scale_x_log10()+
  scale_y_log10(labels = NULL)+
  labs(x = "x", y = "", colour = "Iteration")+
  theme_bw()+
  theme(legend.position = "none")

fig05_multi_mix_box <- ggplot(pretest4_results)+
  aes(x = value, y = set, group = set)+
  geom_boxplot()+
  geom_point(data = filter(pretest4_results, tail == "pl" &
                             value >= xmin), colour = "red")+
  scale_x_log10()+
  theme_bw()+
  theme(axis.text.y = element_blank())+
  labs(x = "x", y = "")



save(pretest4_results, file = "Results/pretest4_results.RData")
save(pretest4_summary, file = "Results/pretest4_summary.RData")

tail_legend <- get_legend(fig05_multi_ln)
fig05_multi_ln <- plot_grid(plot_grid(fig05_multi_ln+
                                        theme(legend.position = "none"),
                                      fig05_multi_mix, labels = "auto",
                                      nrow = 1),
                            tail_legend, ncol = 1, rel_heights = c(1, 0.1))

fig05_multi_ln_box <- plot_grid(fig05_multi_ln_box,
                                fig05_multi_mix_box,
                                labels = "auto", nrow = 1)

save(fig05_multi_ln, file = "Results/fig05_multi_ln.RData")
save(fig05_multi_ln_box, file = "Results/fig05_multi_ln_box.RData")

# END CHAPTER
