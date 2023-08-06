# Code for generating the plots in Chapter 08: Methods - Image analysis

# Load functions and libraries
source("Analysis/Image-functions.R")

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
  # this one is very home made..
n <- 8
space_fac <- seq(0.01,0.25, length.out = 20)
limits <- c(-0.5, 8.5)
#loop for each plot
for (i in 1:length(space_fac)) {
  plot_name <- paste0("clust_", i)
  space1 <- n*space_fac[i]
  rest1 <- (n-space1)/2
  space2 <- rest1*space_fac[i]
  rest2 <- (rest1-space2)/2
  point <- rest2/4

  one_pair <- c(point, point*3)
  two_pairs <- c(one_pair, (rest1-one_pair))
  one_row <- c(two_pairs, n-two_pairs)
  one_plot <- tibble(x = rep(one_row, n),
                 y = rep(one_row, each = n)) %>%
    mutate(x = round(x*2, 1)/2,
           y = round(y*2, 1)/2)

  N_plots[[plot_name]] <- ggplot(one_plot)+
    aes(x, y, height = 0.5, width = 0.5)+
    geom_tile(fill = "black", colour = NA)+
    theme_void()+
    scale_x_continuous(limits = limits, expand = c(0,0))+
    scale_y_continuous(limits = limits, expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white", colour = NA))+
    coord_fixed()
}

N_plots$clust_20+theme_bw()

# Grid with random noise
grid <- tibble(x = rep(1:9, 9)*2,
               y = rep(1:9, each = 9)*2,
               plot = 1)
previous <- grid
step_length <- 0.13
for (i in 2:iterations) {
  one_step <- tibble()
  for (j in 1:nrow(previous)) {
    rw_dir <- randomDir(center = c(previous$x[j], previous$y[j]),
                        step.length = step_length)
    one_step <- bind_rows(one_step, rw_dir)
  }
  one_step <- one_step %>%
    mutate(plot = i)
  grid <- bind_rows(grid, one_step)
  previous <- one_step
}
  # Again round to nearest 0.05 to keep the images binary when saving
grid$x <- round(grid$x*2, 1)/2
grid$y <- round(grid$y*2, 1)/2

  # Store each one
limits <- c(0,20)
for (i in 1:iterations) {
  plot_name <- paste0("noise_", i)
  N_plots[[plot_name]] <- ggplot(filter(grid, plot == i))+
    aes(x, y, width = 0.5, height = 0.5)+
    geom_tile(fill = "black", colour = NA)+
    theme_void()+
    scale_x_continuous(limits = limits, expand = c(0,0))+
    scale_y_continuous(limits = limits, expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white", colour = NA))+
    coord_fixed()
}

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

#convert jpeg to greyscale when all images are made
filenames <- list.files(path = "Data/Frac_test/")
length(filenames)
for (i in 1:length(filenames)) {
  path <- paste0("Data/Frac_test/", filenames[i])
  one_image <- load.image(path)
  one_image <- grayscale(one_image, method = "Luma")
  save.image(one_image, file = path, quality = 1)
}

D_L_tests <- frac.lac(frac_path = "Data/Frac_test",
                               lac_path = "Data/Lac_test")

save(D_L_tests, file = "Results/D_L_tests.RData")

# test <- test$D_L_plot %>%
#   mutate(label = gsub("N_plot_", "", id))
# ggplot(test)+
#   aes(D, L, label = label)+
#   geom_point()+
#   ggrepel::geom_text_repel()
#
# test
