# Code for generating the plots in Chapter 08: Methods - Image analysis

# Load functions and libraries
source("Analysis/Image-functions.R")
library(imager)

# Function for retrieving plot width and height for saving
# Output is in px (pixels) of 0.5m size
# (hence the *20 - sizes and distances in the plot are in 10 meter units,
# so a "house" of size 0.5^2 is 25m^2)
# This is to ensure that these images are comparable to the empirical ones
# in the next chapter, in terms of image resolution
extract.plot.size <- function(plot.input) {
  plot_build <- ggplot_build(plot.input)
  min.x <- plot_build$layout$panel_params[[1]]$x.range[1]
  max.x <- plot_build$layout$panel_params[[1]]$x.range[2]
  w <- round((max.x-min.x)*20, 0)
  min.y <- plot_build$layout$panel_params[[1]]$y.range[1]
  max.y <- plot_build$layout$panel_params[[1]]$y.range[2]
  h <- round((max.y-min.y)*20, 0)
  output <- tibble(width = w, height = h)
  return(output)
}

# Test plots --------------------------------------------------------------


iterations <- 20
N_plots <- list()

# Variable element count (N), constant density, size distribution and layout
  # First by varying the size of the image
  # i is number of boxes in a row, starting with 2
for (i in 2:(iterations+1)) {
  one_plot <- tibble(x = rep(1:i, i),
                     y = rep(1:i, each = i))
  plot_name <- paste0("N_plot_", i-1)
  N_plots[[plot_name]] <- ggplot(one_plot)+
    aes(x, y, width = 0.5, height = 0.5)+
    geom_tile(fill = "black", colour = NA)+ # fill must be explicitly set
                                            # for the fract2D to work!
    theme_void()+
    scale_x_continuous(limits = c(0.75, i+0.75), expand = c(0,0))+
    scale_y_continuous(limits = c(0.75, i+0.75), expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white", colour = NA))+
    coord_fixed()
}

  # Second by varying the size of houses (single image size)
for (i in 2:(iterations+1)) {
  unit_length <- (10/i)/2
  one_plot <- tibble(x = rep(seq(0, 9.99999, by = 10/i)+(unit_length/2), i),
                     y = rep(seq(0, 9.99999, by = 10/i)+(unit_length/2), each = i),
                     height = unit_length, width = unit_length)
  plot_name <- paste0("N_fix_plot_", i-1)
  limits <- c(0,10)

  N_plots[[plot_name]] <- ggplot(one_plot)+
    aes(x, y, width = width, height = height)+
    geom_tile(fill = "black", colour = NA)+
    theme_void()+
    scale_x_continuous(limits = limits, expand = c(0,0))+
    scale_y_continuous(limits = limits, expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white", colour = NA))+
    coord_fixed()
}

# Variable density, constant N, size distribution and layout
  # Keep sequence to steps of 0.05 (pixel size or 0.5m)
unit_length <- seq(0.05, 1, length.out = iterations)
limits <- c(0.5,9.5)
for (i in 1:iterations) {
  one_plot <- tibble(x = rep(1:9, 9),
                     y = rep(1:9, each = 9),
                     height = unit_length[i],
                     width = unit_length[i])
  plot_name <- paste0("dens_plot_", i)

  N_plots[[plot_name]] <- ggplot(one_plot)+
    aes(x, y, width = width, height = height)+
    geom_tile(fill = "black", colour = NA)+
    theme_void()+
    scale_x_continuous(limits = limits, expand = c(0,0))+
    scale_y_continuous(limits = limits, expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white", colour = NA))+
    coord_fixed()
}

# Variable size distribution, constant N, density and layout
  # Keep in mind to round to nearest 0.05 for image resolution
set.seed(100)
sigma <- seq(0.1, 1.1, length.out = 20)
limits <- c(0.5,9.5)
for (i in 1:iterations) {
  sizes <- rlnorm(n = 81, meanlog = 3.5, sdlog = sigma[i])
  # normalise sum of sizes to 1
  sizes <- sizes/sum(sizes)
  # multiply to wanted total area
    # and keep to nearest 0.05 for image resolution
  sizes <- round(sqrt(sizes*(9.5^2/6))*2, 1)/2
  one_plot <- tibble(x = rep(1:9, 9),
                     y = rep(1:9, each = 9),
                     height = sizes,
                     width = sizes)
  plot_name  <- paste0("distr_plot_", i)

  N_plots[[plot_name]] <- ggplot(one_plot)+
    aes(x, y, width = width, height = height)+
    geom_tile(fill = "black", colour = NA)+
    theme_void()+
    scale_x_continuous(limits = limits, expand = c(0,0))+
    scale_y_continuous(limits = limits, expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white", colour = NA))+
    coord_fixed()
}

# Variable layout, constant N, density and size distribution
limits <- c(0.5, 8.5)
one_plot <- tibble(x = rep(1:8, 8),
                   y = rep(1:8, each = 8),
                   height = 0.5, width = 0.5)
ggplot(one_plot)+
  aes(x, y, width = width, height = height)+
  geom_tile(fill = "black", colour = NA)+
  scale_x_continuous(limits = limits, expand = c(0,0))+
  scale_y_continuous(limits = limits, expand = c(0,0))+
  theme_bw()+
  coord_fixed()

N_plots$distr_plot_20+theme_bw()
# DELETE

# Sierpinski carpet -------------------------------------------------------

init1 <- 1/2
init2 <- 1/3
number_of_iterations <- 2
iter <- tibble(n = 1:number_of_iterations,
               gen = 3^n)


Sierp <- tibble(x = init1, y = x, w = init2, h = w)

for (j in 1:nrow(iter)) {
  new_x <- seq(init1/iter$gen[j], 1, by = init1/iter$gen[j]*2)
  for (i in 1:length(new_x)) {
    one_row <- tibble(x = new_x, y = new_x[i], w = init2/iter$gen[j], h = w)
    Sierp <- bind_rows(Sierp, one_row)
  }
}


ggplot(Sierp)+
  aes(x = x, y = y, width = w, height = h)+
  geom_tile(aes(width = 1), fill = "white")+
  geom_tile(aes(height = 1), fill = "white")+
  scale_x_continuous(limits = c(0,1), expand=c(0,0)) +
  scale_y_continuous(limits = c(0,1), expand=c(0,0)) +
  theme_void()+
  coord_fixed()+
  theme(plot.background = element_rect(fill = "black", color = NA))




# STOP DELETE

  #save plots
for (i in 1:length(N_plots)) {
  # Tiff format for lacunarity
  plot <- N_plots[[i]]
  plot_size <- extract.plot.size(plot.input = plot)
  ggsave(filename = paste0(names(N_plots[i]), ".tiff"),
         plot = plot,
         path = "Data/Lac_test/",
         width = plot_size$width, height = plot_size$height,
         units = "px")
  # And jpeg for fractal dimension
  ggsave(filename = paste0(names(N_plots[i]), ".jpg"),
         plot = plot,
         path = "Data/Frac_test/",
         width = plot_size$width, height = plot_size$height,
         units = "px")
}


test <- frac.lac(frac_path = "Data/Frac_test", lac_path = "Data/Lac_test")
test <- test$D_L_plot %>%
  mutate(label = gsub("N_plot_", "", id))
ggplot(test)+
  aes(D, L, label = label)+
  geom_point()+
  ggrepel::geom_text_repel()

test

#convert jpeg to greyscale when all images are made
filenames <- list.files(path = "Data/Frac_test/")
length(filenames)
for (i in 1:length(filenames)) {
  path <- paste0("Data/Frac_test/", filenames[i])
  one_image <- load.image(path)
  one_image <- grayscale(one_image, method = "Luma")
  save.image(one_image, file = path, quality = 1)
}
