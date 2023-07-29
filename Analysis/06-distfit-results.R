# Analyse house-size data, and export results

# Load custom functions from 05-distfit-methods.R
source("Analysis/Distfit-functions.R")

# CHECK SECTIONS 1 AND 2 HERE
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

# 1.2 Additive process: effect of temporal resolution--------------------

# Generate data set

# Fit models and select/mark power-laws

# Make plots/illustrations


# 2 Settlements -----------------------------------------------

# 2.1 Data import and preparation ----------------------------------

lbk_house_sizes <- read.csv2("Data/lbk_house_sizes.csv",
                             encoding = "UTF-8") %>%
  mutate_if(is.character, as.factor)

tryp_house_sizes <- read.csv2("Data/trypillia_house_sizes.csv",
                              encoding = "UTF-8") %>%
  mutate_if(is.character, as.factor)

# Modify high orientation values to negative ones
lbk_house_sizes$mean_orien <- if_else(lbk_house_sizes$mean_orien > 180,
                                      lbk_house_sizes$mean_orien - 360,
                                      lbk_house_sizes$mean_orien)

# Add columns for construction year, abandonment after median duration,
# and abandonment after long duration estimate.
# All dates are calculated as positive numbers, though they are BC
# (i.e. negative). Time axis in plots will need to be reversed!
lbk_house_sizes <- lbk_house_sizes %>%
  mutate(model_constr_BC = (mean_orien+651.016)/0.129486,
         model_end_median = model_constr_BC-27.5,
         model_end_50 = model_constr_BC-50,
         model_end_100 = model_constr_BC-100)

# Zitava dataset with Vráble as a single settlement,
# and binary column with Vráble and the rest
zitava_sites_only <- filter(lbk_house_sizes,
                            neighbourhood_logic == "FALSE") %>%
  mutate(vrable_or_zitava = as.factor(
    if_else(site_name == "vrable_velke_lehemby_total",
            "Vr\u00e1ble",
            "\u017ditava without Vr\u00e1ble")))

# Vráble dataset with neighbourhood affiliation
vrable_neighbourhoods <- filter(lbk_house_sizes, neighbourhood_logic == "TRUE")

# I tested a bit back and forth on time samples.
# These ones seem reasonable, i.e. no voids, all houses (except early
# outliers) are included, correspond roughly to one human generation.
# The abs_degrees and abs_degrees_max columns provide the orientation
# interval of houses that were in use (i.e. constructed but not yet
# abandoned) at the given sample year.
time_samples <- tibble(dates = sort(seq(4990, 5290, by = 20),
                                    decreasing = TRUE),
                       sample = seq(1, length(dates), by = 1)) %>%
  mutate(degrees = (0.129486*dates)-651.016,
         abs_degrees = if_else(degrees < 0, degrees+360, degrees),
         abs_degrees_max = abs_degrees+(0.129486*27.5))

time_samples
save(time_samples, file = "Data/time_samples.RData") #For shortcut

# Assign houses to each time sample and combine (some houses are assigned to
# two or three samples, so table gets longer than the total number of houses).
vrable_samples <- tibble()

for (i in 1:nrow(time_samples)) {
  single_sample <- filter(vrable_neighbourhoods,
                          model_constr_BC > time_samples$dates[i] &
                            model_end_median < time_samples$dates[i]) %>%
    mutate(sample = time_samples$sample[i])
  vrable_samples <- bind_rows(vrable_samples, single_sample)
}

vrable_samples_neigh <- vrable_samples %>%
  group_by(sample, Settlement) %>%
  summarise(n = n(),
            gini = Gini(house_size))

vrable_samples_count <- vrable_samples %>%
  group_by(sample) %>%
  summarise(n = n(),
            gini = Gini(house_size))

# These sample orientations are used to single out corresponding houses
# in QGIS, and extract images for analysis of temporally coeval settlement
# plans of Vráble (see "Time samples" section below).

# 2.2 Analysis --------------------------------------------------

# Make object with settlements and house sizes
my_settlements <- bind_rows(filter(zitava_sites_only, region == "zitava_valley") %>%
  dplyr::select(house_size, Settlement, site_name_ill_short) %>%
  mutate(Culture = "Linear Pottery") %>%
  rename(site_name_ill = site_name_ill_short),
  tryp_house_sizes %>%
  dplyr::select(house_size, Settlement, site_name_ill, culture) %>%
    rename(Culture = culture))

# Filter out settlements with n < 10 (too small for dist.fit); add Gini index
my_settlements <- my_settlements %>%
  group_by(Settlement) %>%
  filter(n() > 10) %>%
  mutate(Gini = round(Gini(house_size), 3))

# Remove unused factor levels (should be 13 settlements left)
my_settlements <- droplevels.data.frame(my_settlements)
levels(my_settlements$Settlement)

# Analyse (takes a few moments)
settlements_results <- dist.fit.all(x = my_settlements$house_size,
                                    set = my_settlements$site_name_ill)
# Add back culture and gini
culture_gini <- my_settlements %>%
  group_by(site_name_ill, Culture, Gini) %>%
  summarise()
settlements_results <- left_join(settlements_results, culture_gini,
          by = c("set" = "site_name_ill"))

# Filter out houses and model values following power law
settlements_pl <- settlements_results %>%
  group_by(set) %>%
  filter(tail == "pl" & value >= xmin)
# Make label vector
settlement_labs <- filter(settlements_pl, model == FALSE) %>%
  group_by(set, Culture) %>%
  summarise(x = max(value), y = min(ccdf))
settlement_labs <- settlement_labs %>%
  ungroup() %>% # Setting label coordinates manually
  mutate(x = c(250, 270, 300, 800, 120),
         y = c(0.15, 0.0004, 0.02, 0.0007, 0.0025))

# Plot results (start with plot b)
# Pick out labels for the steepest distributions (check in list below)
steep_set_labs <- filter(settlements_results, tail == "exp" & par1 > 0.03) %>%
  group_by(set, Culture) %>%
  summarise(x = max(value), y = min(ccdf))
steep_set_labs <- steep_set_labs %>%
  ungroup() %>%
  mutate(x = c(65, 75), y = c(0.06, 0.035))

fig06_settle_other <- ggplot(filter(settlements_results, tail != "pl" & model == FALSE))+
  aes(x = value, y = ccdf, colour = Culture, shape = Culture, group = set)+
  geom_point(show.legend = FALSE)+
  scale_shape_manual(values = c(1,2))+
  geom_line(show.legend = FALSE)+
  geom_text(data = steep_set_labs, aes(x, y,
                                       colour = Culture, label = set),
            show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  labs(x = "House size", y = "cCDF")

# Extract extent of plot b for frame within plot a
plot_build <- ggplot_build(fig06_settle_other)
small_frame <- tibble(x = c(10^(plot_build$layout$panel_params[[1]]$x.range)),
                      y = c(10^(plot_build$layout$panel_params[[1]]$y.range)))

# And power-law tails (plot a)
fig06_settle_pl <- ggplot(filter(settlements_pl, model == FALSE))+
  aes(x = value, y = ccdf, colour = Culture, shape = Culture, group = set)+
  geom_point(data = filter(settlements_results, model == FALSE & tail == "pl"),
             colour = "grey")+
  geom_point()+
  geom_line()+
  geom_rect(data = small_frame, aes(xmin = x[1], ymin = y[1],
                                    xmax = x[2], ymax = y[2],
                                    group = NULL, x = NULL, y = NULL,
                                    shape = NULL),
            fill = alpha("grey", 0), colour = "grey",
            show.legend = FALSE)+
  scale_shape_manual(values = c(1,2))+
  #geom_line(data = filter(settlements_pl, model == TRUE), aes(group = set),
  #           size = 1, colour = "darkgrey", linetype = 2)+
  geom_text(data = settlement_labs, aes(x = x, y = y,
                                        colour = Culture, label = set),
            show.legend = FALSE)+
  scale_y_log10(labels = scales::comma)+
  scale_x_log10()+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x = "House size", y = "cCDF")

# Collect plots a and b
tail_legend <- get_legend(fig06_settle_pl)
fig06_settle_tails <- plot_grid(plot_grid(fig06_settle_pl+
                                            theme(legend.position = "none"),
                                          fig06_settle_other,
                                          labels = "auto", nrow = 1),
                                tail_legend, ncol = 1, rel_heights = c(2, 0.1))

fig06_settle_box <- ggplot(filter(settlements_results, model == FALSE))+
  aes(x = value, y = reorder(set, -value, FUN = median))+
  geom_boxplot(aes(fill = culture), alpha = 0.5)+
  geom_point(data = filter(settlements_pl, model == FALSE), colour = "red")+
  scale_x_log10()+
  theme_bw()+
  labs(x ="House size", y = "", fill = "Culture")

save(fig06_settle_box, file = "Results/fig06_settle_box.RData")
save(fig06_settle_tails, file = "Results/fig06_settle_tails.RData")

# Test the entire distributions and make table of results
tab06_wh_settle <- whole.dist(x = my_settlements$house_size,
                            set = my_settlements$site_name_ill,
                            culture = my_settlements$Culture)

tab06_settle <- filter(settlements_results, model == FALSE) %>%
  group_by(set, tail, par1, xmin, ntail, Gini, Culture) %>%
  summarise(N = n()) %>%
  arrange(tail, desc(par1), Gini) %>%
  ungroup() %>%
  mutate(Settlement = set,
         Tail = tail, T_Par1 = round(par1, 3), xmin = round(xmin, 1),
         Gini = Gini, N = N, N_tail = ntail,
         Tail_P = round(ntail/N, 2),
         Culture = Culture, .keep = "none")

tab06_settle <- left_join(tab06_settle, tab06_wh_settle,
          by = c("Settlement", "Gini", "N", "Culture")) %>%
  relocate(c(Model, Par1, Par2), .after = Settlement) %>%
  relocate(xmin, .after = T_Par1) %>%
  relocate(N, .after = xmin) %>%
  relocate(c(Gini, Culture), .after = Tail_P)

save(tab06_settle, file = "Results/tab06_settle.RData")

# Check normality specifically for Talne 3, see text
filter(my_settlements, Settlement == "Talne 3") %>%
  pull(house_size) %>%
  shapiro.test()

# 3 Quarters/Neighbourhoods -----------------------------------------------

# Prepare data object
my_quarters <- bind_rows(vrable_neighbourhoods %>%
                           dplyr::select(house_size, site_name_ill_short) %>%
                           mutate(house_size = house_size,
                                  Culture = "Linear Pottery",
                                  Quarter = site_name_ill_short,
                                  site_name_ill = "Vr\u00e1ble",
                                  .keep = "none") %>%
                           group_by(Quarter) %>%
                           mutate(Gini = round(Gini(house_size), 3), N = n()),
                         filter(tryp_house_sizes,
                                Settlement == "Nebelivka" &
                                  !quarter %in% c("", NA)) %>%
                           dplyr::select(house_size, site_name_ill,
                                         culture, quarter) %>%
                           rename(Culture = culture,
                                  Quarter = quarter) %>%
                           group_by(Quarter) %>%
                           mutate(Gini = round(Gini(house_size), 3),
                                  N = n()))

# Remove unused factor levels (should be 17 quarters)
my_quarters <- droplevels.data.frame(my_quarters)
levels(my_quarters$Quarter)

# Analyse (takes a few moments)
quarters_results <- dist.fit.all(x = my_quarters$house_size,
                                    set = my_quarters$Quarter)
# Add back Culture, Gini, N and Settlement
quarter_columns <- my_quarters %>%
  group_by(site_name_ill, Culture, Gini, N, Quarter) %>%
  summarise()
quarters_results <- left_join(quarters_results, quarter_columns,
                                 by = c("set" = "Quarter"))

# Filter out houses and model values following power law
quarters_pl <- quarters_results %>%
  group_by(set) %>%
  filter(tail == "pl" & value >= xmin)
# Make label vector (this time for non-pl for clarity)
quarter_labs <- filter(quarters_results, model == FALSE & tail != "pl") %>%
  group_by(set, site_name_ill, Culture) %>%
  summarise(x = max(value), y = min(ccdf)) %>%
  mutate(label = if_else(site_name_ill == "Nebelivka", paste0("Neb. ", set),
                         set))
quarter_labs <- quarter_labs %>%
  ungroup() %>% # Setting label coordinates manually
  mutate(x = c(123, 65, 183),
         y = c(0.013, 0.075, 0.1))

# Plot tails
fig06_quart_tails <- ggplot(filter(quarters_pl, model == FALSE))+
  aes(x = value, y = ccdf, colour = Culture, shape = Culture, group = set)+
  geom_point(data = filter(quarters_results, model == FALSE), colour = "grey")+
  geom_line(data = filter(quarters_results, model == FALSE), colour = "grey")+
  geom_point()+
  scale_shape_manual(values = c(1,2))+
  geom_line()+
  geom_text(data = quarter_labs, aes(x = x, y = y, label = label),
            colour = "darkgrey", show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  labs(x = "House size", y = "cCDF")+
  theme(legend.position = "bottom")

fig06_quart_box <- ggplot(filter(quarters_results, model == FALSE))+
  aes(x = value, y = reorder(set, -value, FUN = median))+
  geom_boxplot(aes(fill = Culture), alpha = 0.5)+
  geom_point(data = filter(quarters_pl, model == FALSE), colour = "red")+
  scale_x_log10()+
  theme_bw()+
  labs(x ="House size", y = "")

save(fig06_quart_box, file = "Results/fig06_quart_box.RData")
save(fig06_quart_tails, file = "Results/fig06_quart_tails.RData")


# Test the entire distributions and make table of results
tab06_wh_quart <- whole.dist(x = my_quarters$house_size,
                             set = my_quarters$Quarter,
                             culture = my_quarters$Culture)
tab06_quart <- filter(quarters_results, model == FALSE) %>%
  group_by(set, tail, par1, xmin, ntail, Culture) %>%
  summarise() %>%
  ungroup() %>%
  mutate(Tail = tail, T_Par1 = round(par1, 3),
         xmin = round(xmin, 1),
         N_tail = ntail, Settlement = set, .keep = "none")

tab06_quart <- left_join(tab06_quart, tab06_wh_quart, by = "Settlement") %>%
  rename(Quarter = Settlement) %>%
  mutate(Tail_P = round(N_tail/N, 2),
         Quarter = if_else(Culture == "Trypillia",
                           paste0("Neb. ", Quarter), Quarter)) %>%
  relocate(Tail_P, .before = Gini) %>%
  relocate(xmin, .after = T_Par1) %>%
  relocate(N, .after = xmin) %>%
  relocate(c(Tail, T_Par1, xmin, N, N_tail), .after = Par2) %>%
  arrange(Tail, desc(T_Par1), Gini)

save(tab06_quart, file = "Results/tab06_quart.RData")


# 4 Time samples --------------------------------------------------------

# Prepare data objects (Vráble total and SW only)
vrable_samples <- vrable_samples %>%
  group_by(sample) %>%
  filter(n() > 10)
vrable_SW_samples <- filter(vrable_samples, Settlement == "Vrable SW") %>%
  group_by(sample) %>%
  filter(n() > 10) %>%
  filter(house_size < max(house_size))

# Analyse
time_results <- dist.fit.all(x = vrable_samples$house_size,
                             set = vrable_samples$sample)
vrable_SW_results <- dist.fit.all(x = vrable_SW_samples$house_size,
                                  set = vrable_SW_samples$sample)

# Add date estimates
sample_dates <- time_samples %>% dplyr::select(dates, sample) %>%
  mutate(sample = as.character(sample),
         dates = as.factor(dates))
# Set levels in descending order
levels(sample_dates$dates)
sample_dates$dates <- factor(sample_dates$dates,
                             levels = rev(levels(sample_dates$dates)))
levels(sample_dates$dates)


time_results <- left_join(time_results, sample_dates,
                          by = c("set" = "sample"))
vrable_SW_results <- left_join(vrable_SW_results, sample_dates,
                               by = c("set" = "sample"))
# Single out power-law houses
time_pl <- time_results %>%
  group_by(set) %>%
  filter(tail == "pl" & value >= xmin)
time_SW_pl <- vrable_SW_results %>%
  group_by(set) %>%
  filter(tail == "pl" & value >= xmin)

# Plot cCDF
fig06_time_tails <- ggplot(filter(time_pl, model == FALSE))+
  aes(x = value, y = ccdf, group = set)+
  geom_point(data = filter(time_results, model == FALSE),
             colour = "grey", shape = 1)+
  geom_line(data = filter(time_results, model == FALSE),
            colour = "grey")+
  geom_point(colour = "red", shape = 1)+
  geom_line(colour = "red")+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  labs(x = "House size", y = "cCDF")

fig06_time_tails_b <- ggplot(filter(time_SW_pl, model == FALSE))+
  aes(x = value, y = ccdf, group = set)+
  geom_point(data = filter(vrable_SW_results, model == FALSE),
             colour = "grey", shape = 1)+
  geom_line(data = filter(vrable_SW_results, model == FALSE),
            colour = "grey")+
  geom_point(colour = "red", shape = 1)+
  geom_line(colour = "red")+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  labs(x = "House size", y = "")

fig06_time_tails <- plot_grid(fig06_time_tails, fig06_time_tails_b,
          labels = "auto", nrow = 1)

# Make boxplots
fig06_time_box <- ggplot(filter(time_results, model == FALSE))+
  aes(x = value,
      y = dates)+
  geom_boxplot()+
  geom_point(data = filter(time_pl, model == FALSE), colour = "red")+
  scale_x_log10()+
  theme_bw()+
  labs(x = "House size", y = "Time sample (cal. BCE)")

fig06_time_box_b <- ggplot(filter(vrable_SW_results, model == FALSE))+
  aes(x = value,
      y = dates)+
  geom_boxplot()+
  geom_point(data = filter(time_SW_pl, model == FALSE), colour = "red")+
  scale_x_log10()+
  scale_y_discrete(limits = factor(rev(seq(5050, 5230, by = 20))),
                   labels = NULL)+
  theme_bw()+
  labs(x = "House size", y = "")

fig06_time_box <- plot_grid(fig06_time_box, fig06_time_box_b,
                            labels = "auto", nrow = 1)

save(fig06_time_box, file = "Results/fig06_time_box.RData")
save(fig06_time_tails, file = "Results/fig06_time_tails.RData")

# Analyse whole distribution and make table(s)
tab06_time <- whole.dist(x = vrable_samples$house_size,
                         set = vrable_samples$sample,
                         culture = "Vr\u00e1ble")
tab06_time_SW <- whole.dist(x = vrable_SW_samples$house_size,
                            set = vrable_SW_samples$sample,
                            culture = "Vr\u00e1ble SW*")

tab06_time_tails <- filter(time_results, model == FALSE) %>%
  group_by(set, dates, tail, par1, xmin, ntail) %>%
  summarise() %>%
  ungroup() %>%
  mutate(Tail = tail, T_Par1 = round(par1, 3),
         xmin = round(xmin, 1), BCE = dates,
         N_tail = ntail, Sample = as.factor(set), .keep = "none")
tab06_time_SW_tails <- filter(vrable_SW_results, model == FALSE) %>%
  group_by(set, dates, tail, par1, xmin, ntail) %>%
  summarise() %>%
  ungroup() %>%
  mutate(Tail = tail, T_Par1 = round(par1, 3),
         xmin = round(xmin, 1), BCE = dates,
         N_tail = ntail, Sample = as.factor(set), .keep = "none")
tab06_time <- left_join(tab06_time, tab06_time_tails, by = c("Settlement" = "Sample")) %>%
  select(2:12) %>%
  mutate(Tail_P = round(N_tail/N, 2)) %>%
  relocate(BCE, .before = Model) %>%
  relocate(c(Tail, T_Par1), .after = Par2) %>%
  relocate(c(Gini, Culture), .after = Tail_P) %>%
  relocate(xmin, .before = N) %>%
  rename(Set = Culture) %>%
  arrange(BCE)

tab06_time_SW <- left_join(tab06_time_SW, tab06_time_SW_tails,
                           by = c("Settlement" = "Sample")) %>%
  select(2:12) %>%
  mutate(Tail_P = round(N_tail/N, 2)) %>%
  relocate(BCE, .before = Model) %>%
  relocate(c(Tail, T_Par1), .after = Par2) %>%
  relocate(c(Gini, Culture), .after = Tail_P) %>%
  relocate(xmin, .before = N) %>%
  rename(Set = Culture) %>%
  arrange(BCE)

tab06_time <- bind_rows(tab06_time, tab06_time_SW)

save(tab06_time, file = "Results/tab06_time.RData")

# END chapter
