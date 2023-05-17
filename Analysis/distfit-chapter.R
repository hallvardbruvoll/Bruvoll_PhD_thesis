# Analyses for Part II - Size distributions
library(tidyverse)
library(poweRlaw)
library(AICcmodavg)
library(ineq)
library(roxygen2)

# Wrapper and loop functions ----------------------------------------------

#Fit a power-law, lognormal, exponential and stretched exponential
#(Weibull cCDF) models to the data by MLE, and estimating the best
#power-law xmin through KS-testing (cf. @clauset2009; @gillespie2015).
tail.models <- function(data) { #"data" must be a continuous numeric vector
  pl.model <- conpl$new(data) # power-law model
  xmin <- estimate_xmin(pl.model)
  pl.model$setXmin(xmin)
  ln.model <- conlnorm$new(data) # lognormal model
  ln.model$setXmin(xmin) # xmin for all models are set at the best pl fit
  ln.pars <- estimate_pars(ln.model)
  ln.model$setPars(ln.pars)
  exp.model <- conexp$new(data) # exponential model
  exp.model$setXmin(xmin)
  exp.pars <- estimate_pars(exp.model)
  exp.model$setPars(exp.pars)
  strexp.model <- conweibull$new(data) # stretched exponential/Weibull model
  strexp.model$setXmin(xmin)
  strexp.pars <- estimate_pars(strexp.model)
  strexp.model$setPars(strexp.pars)
  return(list("ln" = ln.model, # mind the model order here (for AIC below)
              "str exp" = strexp.model,
              "exp" = exp.model,
              "pl" = pl.model))
}
#Early experiments included a model function for power law with exponential
#cutoff, discussed in the Clauset et al. (2009) paper, not implemented in
#the poweRlaw package (May 2023). The function was borrowed from
#"https://github.com/jeffalstott/powerlaw/tree/master/testing/
#pli-R-v0.0.3-2007-07-25", written by C. Shalizi, co-author of the paper.
#Because of copyright issues, this model is not included here. However, when
#included it never passed as the best fit for the data used in this thesis
#(note that the code does not include any function for setting xmin, unlike
#all the models included in the poweRlaw package).

#Extract coordinates for cCDF (i.e. survival function) plot of models.
#Input "models" must be a list of the type produced by the tail.models
#function above.
extract.xy <- function(models) { #This function goes into the next one below
  plot.new()
  model_xy <- lines(models)
  model_xy <- tibble(model_xy) %>%
    # Filter out rows where y = 0 (a bug in the poweRlaw package)
    # This doesn't affect analyses, only graphical representation
    filter(y != 0) %>%
    rename(value = x,
           ccdf = y)
  return(model_xy)
}

models.xy <- function(models) { #Loop the above function for all input models
  output <- tibble()
  for (i in 1:length(models)) {
    modxy <- extract.xy(models = models[[i]]) %>%
      mutate(model = names(models[i]))
    output <- bind_rows(output, modxy)
  }
  return(output)
}

# Table with input and columns for storing results
# Can I include this inside one of the functions above?
dist.fit.object <- function(data.vector, set) {
  object <- tibble(value = data.vector,
                   rank = min_rank(value),
                   ccdf = round((length(rank)-rank+1)/length(rank), 3),
                   tail = NA, xmin = NA, ntail = NA, par1 = NA, par2 = NA,
                   set = set)
  return(object)
}

# Model selection with AICc (second order Akaike's Information Criterion)
aic.selection <- function(tail_models){ # For a single data series
  # Find log-likelihood for each model
  log_lik <- map_dbl(tail_models, dist_ll)
  # Number of parameters for each model (I gave up vectorising this properly)
  no_pars <- c(tail_models[[1]]$no_pars, tail_models[[2]]$no_pars,
               tail_models[[3]]$no_pars, tail_models[[4]]$no_pars)
  modnames <- names(tail_models)
  # Sample size (number of observations) above xmin
  nobs <- tail_models[[1]]$internal$n
  sample_AIC <- aictabCustom(logL = log_lik, K = no_pars,
                             modnames = modnames,
                             second.ord = TRUE, # better for small samples
                             nobs = nobs)
  return(sample_AIC)
}

# Add results to dist.fit.object
add.results <- function(data, tail_models, AIC.results) {
  # data must be a dist.fit.object (see above)
  data$tail <- AIC.results[1,1]
  data$xmin <- tail_models[[1]]$xmin
  data$ntail <- tail_models[[1]]$internal$n
  if (data$tail[1] == "ln") { # the poweRlaw objects are hard to vectorise
                                # so this gets a bit repetitive
    data$par1 <- tail_models$ln$pars[1]
    data$par2 <- tail_models$ln$pars[2]
  }
  if (data$tail[1] == "str exp") {
    data$par1 <- tail_models$`str exp`$pars[1]
    data$par2 <- tail_models$`str exp`$pars[2]
  }
  if (data$tail[1] == "exp") {
    data$par1 <- tail_models$exp$pars
  }
  if (data$tail[1] == "pl") {
    data$par1 <- tail_models$pl$pars
  }
  return(data)
}

data <- bind_rows(tibble(x = rlnorm(1000, 0.3, 2), set = "ln"),
                  tibble(x = rexp(1000, 0.125), set = "exp"))
dist.fit.all(data = data)

# Loop for multiple data sets
#' Distribution fitting loop
#'
#' Fit four heavy-tailed distribution models (log-normal, exponential, stretched exponential and power law) to multiple data series, and select best model for each series based on AICc. Xmin for all models is the one that gives the best power-law fit (KS-test). Dependencies: poweRlaw, tidyverse, AICcmodavg.
#'
#' @param data A data.table or tibble, with a numeric vector named "x" and a factor or character vector named "set".
#'
#' @return The input table with additional columns reporting the best fit with parameter values.
#' @export
#'
#' @examples
#' data <- bind_rows(tibble(x = rlnorm(1000, 0.3, 2), set = "ln"),
#' tibble(x = rexp(1000, 0.125), set = "exp"))
#' dist.fit.all(data = data)
dist.fit.all <- function(data){
  # "x" and "set" must be columns in "data"
  data <- data %>%
    group_by(set) # Group to analyse each set separately
  sets <- levels(as.factor(data$set))
  output <- tibble()
  for (i in 1:length(sets)) {
    # Filter out one set and create object with necessary columns
    one_set <- dist.fit.object(data.vector = filter(data, set == sets[i]) %>%
                                 pull(x),
                               set = sets[i])
    one_set_models <- tail.models(one_set$value) # Fit models to data
    one_set_AIC <- aic.selection(tail_models = one_set_models) # Compare models
    one_set <- add.results(data = one_set, # Add param values from best fit
                           tail_models = one_set_models,
                           AIC.results = one_set_AIC)
    output <- bind_rows(output, one_set) # Add results for each set to output
 }
  return(output)
}

# Pre-analysis: testing for false positive power laws ---------------------

mu <- 0.3
sigma <- 2
lambda <- 0.125
alpha <- 2.5
n <- c(10, 100, 1000, 10000)
dists <- c("ln", "exp", "pl")

# Generate data sets
pretest_data <- tibble()

for (j in 1:length(dists)) {
  for (i in 1:length(n)) {
    if (j == 1) {
      single <- tibble(x = rlnorm(n = n[i], mu, sigma))
    }
    if (j == 2) {
      single <- tibble(x = rexp(n = n[i], lambda))
    }
    if (j == 3) {
      single <- tibble(x = rplcon(n = n[i], xmin = 15, alpha = alpha))
    }
    single <- single %>%
      mutate(set = paste0(dists[j], n[i]))
    pretest_data <- bind_rows(pretest_data, single)
  }
}
pretest_data <- filter(pretest_data, x > 15)
# To only have positive values of log(x). Not sure how Clauset et al. (2009)
# set xmin on log-normal and exponential distributions

pretest_results <- dist.fit.all(data = pretest_data,
                                x = pretest_data$x,
                                set = pretest_data$set)

# Test these functions here:
test_model_data <- dist.fit.object(data.vector = rlnorm(n = 2000,
                                                      meanlog = 2, sdlog = 1),
                                   set = "test_set")
hist(test_model_data$value)

test_models <- tail.models(test_model_data$value)
test_models_xy <- models.xy(test_models)

ggplot(test_models_xy)+
  aes(x = value, y = ccdf, colour = model)+
  geom_line(data = test_model_data, colour  = "black")+
  geom_line(size = 0.8)+
  scale_x_log10()+
  scale_y_log10()+
  theme_minimal()

test_AIC <- aic.selection(tail_models = test_models)
test_AIC

test_model_data <- add.results(data = test_model_data,
                               tail_models = test_models,
                               AIC.results = test_AIC)

# Plot highlighting data that fits pl model
# Only do this if the pl model is actually selected of course
# Comparing multiple series, consider boxplot
ggplot(test_model_data)+
  aes(x = value, y = ccdf)+
  geom_point(shape = 1)+
  geom_point(data = filter(test_model_data, value >= xmin),
             colour = "red", shape = 1)+
  scale_x_log10()+
  scale_y_log10()+
  theme_minimal()

# Then: looping for multiple series. Or go to text again first?
# Then: the actual analyses...



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




