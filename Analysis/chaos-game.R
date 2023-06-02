# Chaos game scripts for plotting fractals
library(tidyverse)

# Sierpinski triangle -----------------------------------------------------

# Width is 1. Define height (b) with Pythagoras to multiply with y values
a <- 1/2
b <- sqrt((2*a)^2-a^2)

# Define transformation functions
my_fractal <- tibble(x = 0, y = 0, iter = 0)
iterations <- 40000

for (i in 1:iterations) {  # Set loop
  random <- sample(1:3, 1) # Choose function randomly each round
  last_x <- last(my_fractal$x) # Use last round as input for the next always
  last_y <- last(my_fractal$y)
  if (random == 1) { # Transform to bottom left triangle
    one_point <- tibble(x = 1/2*last_x, y = 1/2*last_y, iter = i)
  }
  if (random == 2) { # Bottom right triangle
    one_point <- tibble(x = 1/2*last_x+1/2, y = 1/2*last_y, iter = i)
  }
  if (random == 3) { # Top triangle
    one_point <- tibble(x = 1/2*last_x+1/4, y = 1/2*last_y+1/2, iter = i)
  }
  my_fractal <- bind_rows(my_fractal, one_point) # Update object for each round
}
my_fractal <- filter(my_fractal, iter > 100) %>% # Cut the first iterations
  mutate(y = y*b) # Scale the y axis for a isocele triangle

sierpinski <- ggplot(my_fractal)+
  aes(x = x, y = y)+
  geom_tile(width = 0.0025, height = 0.0025)+
  coord_fixed()+
  theme_void()

# It f***ing worked!!!!

save(sierpinski, file = "Results/sierpinski_triangle.RData")

# with rotation -----------------------------------------------------------
spiral <- tibble(x = 0, y = 0, iter = 0)
iterations <- 40000

for (i in 1:iterations) {
  last_x <- last(spiral$x)
  last_y <- last(spiral$y)
  random <- sample(1:8, 1)
  if (random > 1) {
    one_point <- tibble(x = last_x*0.95, y = last_y*0.95)
    one_point <- plothelper::rotatexy(one_point, angle = pi/7,
                                      xmiddle = 0.5, ymiddle = 0.5) %>%
      mutate(iter = i)
   }
   if (random == 1) {
     one_point <- tibble(x = last_x*0.25+0.7, y = last_y*0.25+0.5, iter = i)
   }
  spiral <- bind_rows(spiral, one_point)
}
spiral <- filter(spiral, iter > 100)

#spiral_fractal
romanesco <- ggplot(spiral)+
  aes(x, y)+
  geom_tile(height = 0.0025, width = 0.0025)+
  coord_fixed()+
  theme_void()

save(romanesco, file = "Results/romanesco.RData")

# Another one -------------------------------------------------------------

tree <- tibble(x = 0, y = 0, iter = 0)
iterations <- 40000

for (i in 1:iterations) {
  last_x <- last(tree$x)
  last_y <- last(tree$y)
  random <- sample(1:3, 1)
  if (random == 1) {
    one_point <- tibble(x = last_x*0.7+0.15, y = last_y*0.3+0.2)
    one_point <- plothelper::rotatexy(one_point, angle = pi/3,
                                      xmiddle = 0.5, ymiddle = 0.2) %>%
      mutate(iter = i)
  }
  if (random == 2) {
    one_point <- tibble(x = last_x*0.7+0.15, y = last_y*0.3+0.2)
    one_point <- plothelper::rotatexy(one_point, angle = 300*(pi/180),
                                      xmiddle = 0.5, ymiddle = 0.2) %>%
      mutate(iter = i)
  }
  if (random == 3) {
     one_point <- tibble(x = last_x*0.8+0.1, y = last_y*0.9+0.5, iter = i)
  }
  tree <- bind_rows(tree, one_point)
}
tree <- filter(tree, iter > 100)

spruce <- ggplot(tree)+
  aes(x, y)+
  geom_tile(height = 0.015, width = 0.015)+
  coord_fixed()+
  theme_void()

save(spruce, file = "Results/spruce.RData")

# von Koch curve -------------------------------------------

von_koch <- tibble(x = 0, y = 0, iter = 0)
iterations <- 40000

for (i in 1:iterations) {
  random <- sample(1:4, 1)
  last_x <- last(von_koch$x)
  last_y <- last(von_koch$y)
  if (random == 1) {
    one_point <- tibble(x = last_x*1/3, y = last_y*1/3, iter = i)
  }
  if (random == 2) {
    one_point <- tibble(x = last_x*1/3+2/3, y = last_y*1/3, iter = i)
  }
  if (random == 3) {
    one_point <- tibble(x = last_x*1/3+1/3, y = last_y*1/3)
    one_point <- plothelper::rotatexy(one_point, angle = pi/3,
                                      xmiddle = 1/3, ymiddle = 0) %>%
      mutate(iter = i)
  }
  if (random == 4) {
    one_point <- tibble(x = last_x*1/3+1/3, y = last_y*1/3)
    one_point <- plothelper::rotatexy(one_point, angle = 300*(pi/180),
                                      xmiddle = 2/3, ymiddle = 0) %>%
      mutate(iter = i)
  }
  von_koch <- bind_rows(von_koch, one_point)
}
von_koch <- filter(von_koch, iter > 100)

von_koch_curve <- ggplot(von_koch)+
  aes(x, y)+
  geom_tile(height = 0.0025, width = 0.0025)+
  coord_fixed()+
  theme_void()

save(von_koch_curve, file = "Results/von_koch.RData")

# galaxy ------------------------------------------------------------------
# this is so much fun
galaxy <- tibble(x = 0, y = 0, iter = 0)
iterations <- 40000

for (i in 1:iterations) {
  last_x <- last(galaxy$x)
  last_y <- last(galaxy$y)
  random <- sample(1:9, 1)
  if (random > 2) {
    one_point <- tibble(x = last_x*0.85, y = last_y*0.85)
    one_point <- plothelper::rotatexy(one_point, angle = pi/8,
                                      xmiddle = 0.5, ymiddle = 0.5) %>%
      mutate(iter = i)
  }
  if (random == 1) {
    one_point <- tibble(x = last_x*0.4, y = last_y*0.4, iter = i)
  }
  if (random == 2) {
    one_point <- tibble(x = last_x*0.4+0.6, y = last_y*0.4+0.6, iter = i)
  }
  galaxy <- bind_rows(galaxy, one_point)
}
galaxy <- filter(galaxy, iter > 100)

fuzzy_wave <- ggplot(galaxy)+
  aes(x, y)+
  geom_tile(height = 0.002, width = 0.002)+
  coord_fixed()+
  theme_void()

save(fuzzy_wave, file = "Results/fuzzy_wave.RData")

# something complex -------------------------------------------------------
