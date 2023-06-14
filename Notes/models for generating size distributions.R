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

#houses are added linearly, and grow exponentially
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

#houses are added and grow linearly, but are abandoned exponentially
my_village <- tibble(house_size = rep(1, 100))
scission_rate <- 0.1
reps <- 0

while (reps < 1000) {
  my_village$house_size <- my_village$house_size+1
  my_village <- slice_sample(my_village, prop = 1-scission_rate)
  my_village <- my_village %>%
    add_row(house_size = rep(1, 10))
  reps <- reps+1
}
my_village <- my_village %>%
  mutate(rank = min_rank(house_size),
         ccdf = round(((1-rank)/length(rank))+1, 3))

ggplot(my_village)+
  aes(x = house_size, y = ccdf)+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()
