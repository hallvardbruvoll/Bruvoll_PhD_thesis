# This file is just personal notes.

my_village <- tibble(house_size = rep_len(1, length.out = 1000))

growth_rate <- 1

scission_rate <- 0.5
reps <- 0

while (reps < 100) {
  my_village$house_size <- my_village$house_size*exp(growth_rate)
  village_n <- nrow(my_village)
  my_village <- slice_sample(my_village, prop = exp(-scission_rate))
  village_n2 <- nrow(my_village)
  new_houses <- tibble(house_size = rep_len(1, length.out = village_n-village_n2))
  my_village <- bind_rows(my_village, new_houses)
  reps <- reps+1
}

hist(my_village$house_size)
# oups, that's a power law. Combination of exponentials.

# exponential 1:
# houses are added linearly, and grow exponentially
my_village <- tibble(house_size = 1)
growth_rate <- 0.05

reps <- 0

while (reps < 100) {
  my_village$house_size <- my_village$house_size*exp(growth_rate)
  my_village <- my_village %>%
    add_row(house_size = 1)
  reps <- reps+1
}

hist(my_village$house_size)
# that's more exponential, but also a more unlikely scenario (new houses are
# added linearly in time, and they all grow exponentially without limit)

# exponential 2:
# houses are added and grow linearly, but are abandoned exponentially
my_village <- tibble(house_size = rep(1, 100), iter = rep(1, 100))
scission_rate <- 0.1
reps <- 0

while (reps < 1000) {
  my_village$house_size <- my_village$house_size+1
  my_village$iter <- my_village$iter+1
  my_village <- slice_sample(my_village, prop = 1-scission_rate)
  my_village <- my_village %>%
    add_row(house_size = rep(1, 10), iter = rep(1, 10))
  reps <- reps+1
}
my_village <- my_village %>%
  mutate(rank = min_rank(house_size),
         ccdf = round(((1-rank)/length(rank))+1, 3))

ggplot(my_village)+
  aes(x = house_size, y = ccdf)+
  geom_point()+
#  scale_x_log10()+
  scale_y_log10()

# log-normal: product of random numbers
z <- runif(1000, min = 0.95, max = 1.2)
prod(sample(z, size = 50, replace = TRUE))

test <- tibble(x = map_dbl(1:1000, function(x) prod(sample(z, size = 100, replace = TRUE))))
ggplot(test)+
  aes(x = x)+
  geom_density()
  scale_x_log10()

# log-normal: sum of random exponents
test2 <- tibble(log_x = map_dbl(1:1000, function(x) sum(sample(log(z), size = 100, replace = TRUE))))
ggplot(test2)+
  aes(x = exp(log_x))+
  geom_density()

# log-normal: exponential of normal
test3 <- tibble(a = rnorm(1000, mean = 20, sd = 3),
                b = exp(-0.2)^a)
ggplot(test3)+
  aes(x = b)+
  geom_density()

# log-normal: normal of exponential
test4 <- tibble(a = rexp(1000, rate = 0.1),
                b = 1/(sqrt(2*pi)*1)*exp((-(a-10)^2)/(2*1^2)))
ggplot(test4)+
  aes(x = b)+
  geom_density()+
  scale_x_log10() # Tried some different values, does work but doesn't look nice

test1 <- tibble(z = dnorm(x = seq(0,10, length.out = 100), mean = 5, sd = 1),
                x = seq(0,10, length.out = 100),
                xlnorm = exp(x))

ggplot(test1)+
  aes(x = log(xlnorm), y = x)+
  geom_point()
