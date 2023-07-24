# Analyse house-size data, and export results

# Load custom functions from 05-distfit-methods.R
source("Analysis/05-distfit-methods.R")

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


# Data import and preparation -----------------------------------------------

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

# 2 Settlements -----------------------------------------------------------

# Make object with settlements and house sizes
my_settlements <- bind_rows(filter(zitava_sites_only, region == "zitava_valley") %>%
  select(house_size, Settlement, site_name_ill_short) %>%
  mutate(culture = "Linear Pottery") %>%
  rename(site_name_ill = site_name_ill_short),
  tryp_house_sizes %>%
  select(house_size, Settlement, site_name_ill, culture))

# Filter out settlements with n < 10 (too small for dist.fit)
my_settlements <- my_settlements %>%
  group_by(Settlement) %>%
  filter(n() > 10)

# Remove unused factor levels (should be 13 settlements left)
my_settlements <- droplevels.data.frame(my_settlements)
levels(my_settlements$Settlement)

# Analyse
settlements_results <- dist.fit.all(x = my_settlements$house_size,
                                    set = my_settlements$site_name_ill)
settlements_results <- settlements_results %>%
  mutate(culture = if_else(set %in% tryp_house_sizes$site_name_ill,
                           "Trypillia", "Linear Pottery"))
# Filter out houses and model values following power law
settlements_pl <- settlements_results %>%
  group_by(set) %>%
  filter(tail == "pl" & value >= xmin)
# Make label vector
settlement_labs <- filter(settlements_pl, model == FALSE) %>%
  group_by(set, culture) %>%
  summarise(x = max(value), y = min(ccdf))
settlement_labs <- settlement_labs %>%
  ungroup() %>% # Setting label coordinates manually
  mutate(x = c(210, 480, 230, 280, 170),
         y = c(0.15, 0.0004, 0.02, 0.0007, 0.0025))

# And plot tails
ggplot(filter(settlements_pl, model == FALSE))+
  aes(x = value, y = ccdf, colour = culture, shape = culture, group = set)+
  geom_point(data = filter(settlements_results, model == FALSE),
             colour = "grey")+
  geom_point()+
  geom_line()+
  scale_shape_manual(values = c(1,2))+
  geom_line(data = filter(settlements_pl, model == TRUE), aes(group = set),
             size = 1, colour = "grey", linetype = 2)+
  geom_text(data = settlement_labs, aes(x = x, y = y,
                                        colour = culture, label = set),
            show.legend = FALSE)+
  scale_y_log10(labels = scales::comma)+
  scale_x_log10()+
  theme_bw()+
  labs(x = "House size", y = "cCDF",
       colour = "Culture", shape = "Culture")

