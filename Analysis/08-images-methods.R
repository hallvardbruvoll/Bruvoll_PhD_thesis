# Code for generating the plots in Chapter 08: Methods - Image analysis

# Load functions and libraries
source("Analysis/Image-functions.R")
library(ggimage) #for geom_image

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
                     y = rep(seq(0, 9.99999, by = 10/i)+(unit_length/2),
                             each = i),
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
  # Images were not rendered entirely binary here no matter what
  # Also, smallest length (0.05) gave no output
unit_length <- seq(0.1, 1, length.out = iterations)
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
save(N_plots, file = "Data/N_plots.RData")

# test <- D_L_tests$D_L_plot
#   mutate(label = gsub("N_plot_", "", id))
# ggplot(test)+
#   aes(D, L_mean)+
#   geom_point()
#   ggrepel::geom_text_repel()
#
# test

# Plots for each series ---------------------------------------------------

  # Tidy and arrange by groups
D_L_tests$D_L_plot
D_L_test_plots <- D_L_tests$D_L_plot

D_L_test_plots <- D_L_test_plots %>%
  mutate(Series = str_remove_all(id, pattern = "[:digit:]"),
         Series = str_replace(Series, pattern = "N_fix_plot_", "N objects"),
         Series = str_replace(Series, "N_plot_", "Image size"),
         Series = str_replace(Series, "clust_", "Clustering"),
         Series = str_replace(Series, "dens_plot_", "Density"),
         Series = str_replace(Series, "distr_plot_", "Size distr."),
         Series = str_replace(Series, "noise_", "Noise"),
         Series = as.factor(Series),
         filename = str_replace(filename, ".tiff", ".jpg"),
         path = str_c("Data/Frac_test/", filename),
         Iter = as.numeric(str_remove_all(
           id, pattern = "[[:punct:][:alpha:]]"))) %>%
  arrange(Series, Iter)

levels(D_L_test_plots$Series)

# Variable N
N1 <- ggplot(filter(D_L_test_plots, Series == "N objects"))+
  aes(D, L_mean, label = Iter)+
  geom_point()+
  geom_path()+
  ggrepel::geom_text_repel()+
  theme_bw()

N2 <- ggplot(filter(D_L_test_plots, Series == "N objects"))+
  aes(D, L, label = Iter)+
  geom_point()+
  geom_path()+
  ggrepel::geom_text_repel()+
  theme_bw()

fig08_N <- plot_grid(N1, N2, nrow = 1, labels = "auto")

  # N images
N_im_data <- filter(D_L_test_plots, Series == "N objects"
                    & Iter %in% c(1,4,8,15,20))
fig08_N_im <- ggplot(N_im_data)+
  aes(D, L_mean, label = Iter, image = path)+
  geom_image(size = 0.2)+
  geom_text(data = slice(N_im_data, c(1,2,4)), nudge_x = 0.035)+
  geom_text(data = slice(N_im_data, c(3,5)), nudge_x = -0.035)+
  scale_x_continuous(expand = c(0.1, 0.1))+
  scale_y_continuous(expand = c(0.2, 0.2))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "lightgrey"),
        panel.grid = element_blank())


# Variable image size
im_size1 <- ggplot(filter(D_L_test_plots, Series == "Image size"))+
  aes(D, L_mean, label = Iter)+
  geom_point()+
  geom_path()+
  ggrepel::geom_text_repel()+
  theme_bw()

im_size2 <- ggplot(filter(D_L_test_plots, Series == "Image size"))+
  aes(D, L, label = Iter)+
  geom_point()+
  geom_path()+
  ggrepel::geom_text_repel()+
  theme_bw()

fig08_im_size <- plot_grid(im_size1, im_size2, nrow = 1, labels = "auto")

  # Image size im
size_im_data <- filter(D_L_test_plots, Series == "Image size" &
                         Iter %in% c(1,2,3,6,20))
fig08_size_im <- ggplot(size_im_data)+
  aes(D, L_mean, label = Iter, image = path)+
  geom_text(data = slice(size_im_data, 1:3), nudge_x = -0.06)+
  geom_text(data = slice(size_im_data, 4), nudge_x = -0.1)+
  geom_text(data = slice(size_im_data, 5), nudge_x = -0.3, nudge_y = -0.1)+
  geom_image(aes(size = I(Iter/40)))+
  scale_x_continuous(expand = c(0.1, 0.2))+
  scale_y_continuous(expand = c(0.6, 0))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "lightgrey"),
        panel.grid = element_blank())

# Variable image density
dens1 <- ggplot(filter(D_L_test_plots, Series == "Density"))+
  aes(D, L_mean, label = Iter)+
  geom_point()+
  geom_path()+
  ggrepel::geom_text_repel()+
  theme_bw()

dens2 <- ggplot(filter(D_L_test_plots, Series == "Density"))+
  aes(D, L, label = Iter)+
  geom_point()+
  geom_path()+
  ggrepel::geom_text_repel()+
  theme_bw()

fig08_dens <- plot_grid(dens1, dens2, nrow = 1, labels = "auto")

  # Density images
fig08_dens_im <- ggplot(filter(D_L_test_plots, Series == "Density"
                                & Iter %in% c(2,4,6,10,16,20)))+
  aes(D, L_mean, label = Iter, image = path)+
  geom_image(size = 0.2)+
  geom_text(nudge_y = 4.5)+
  scale_x_continuous(expand = c(0.1, 0.1))+
  scale_y_continuous(expand = c(0.2, 0))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "lightgrey"),
        panel.grid = element_blank())

# Variable size distribution
dist1 <- ggplot(filter(D_L_test_plots, Series == "Size distr."))+
  aes(D, L_mean, label = Iter)+
  geom_point()+
  geom_path()+
  ggrepel::geom_text_repel()+
  theme_bw()

dist2 <- ggplot(filter(D_L_test_plots, Series == "Size distr."))+
  aes(D, L, label = Iter)+
  geom_point()+
  geom_path()+
  ggrepel::geom_text_repel()+
  theme_bw()

fig08_distr <- plot_grid(dist1, dist2, nrow = 1, labels = "auto")

  # Size distribution images
fig08_distr_im <- ggplot(filter(D_L_test_plots, Series == "Size distr."
                                     & Iter %in% c(1,4,8,14,18,19)))+
  aes(D, L_mean, label = Iter, image = path)+
  geom_image(size = 0.2)+
  geom_text(nudge_y = 0.1)+
  scale_x_continuous(expand = c(0.005, 0.005))+
  scale_y_continuous(expand = c(0.05, 0.05))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "lightgrey"),
        panel.grid = element_blank())


# Variable clustering
cluster1 <- ggplot(filter(D_L_test_plots, Series == "Clustering"))+
  aes(D, L_mean, label = Iter)+
  geom_point()+
  geom_path()+
  ggrepel::geom_text_repel()+
  theme_bw()

cluster2 <- ggplot(filter(D_L_test_plots, Series == "Clustering"))+
  aes(D, L, label = Iter)+
  geom_point()+
  geom_path()+
  ggrepel::geom_text_repel()+
  theme_bw()

fig08_clustering <- plot_grid(cluster1, cluster2, nrow = 1, labels = "auto")

  # Clustering images
fig08_clustering_im <- ggplot(filter(D_L_test_plots, Series == "Clustering"
                                & Iter %in% c(1,7,13,18,20)))+
  aes(D, L_mean, label = Iter, image = path)+
  geom_image(size = 0.2)+
  geom_text(nudge_y = 0.075)+
  scale_x_continuous(expand = c(0.005, 0.005))+
  scale_y_continuous(expand = c(0.05, 0.05))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "lightgrey"),
        panel.grid = element_blank())


# Variable noise
noise1 <- ggplot(filter(D_L_test_plots, Series == "Noise"))+
  aes(D, L_mean, label = Iter)+
  geom_point()+
  geom_path()+
  ggrepel::geom_text_repel()+
  theme_bw()

noise2 <- ggplot(filter(D_L_test_plots, Series == "Noise"))+
  aes(D, L, label = Iter)+
  geom_point()+
  geom_path()+
  ggrepel::geom_text_repel()+
  theme_bw()

fig08_noise <- plot_grid(noise1, noise2, nrow = 1, labels = "auto")

  # Noise images
noise_im_data <- filter(D_L_test_plots, Series == "Noise" &
                          Iter %in% c(1,11,15,20))
fig08_noise_im <- ggplot(noise_im_data)+
  aes(D, L_mean, label = Iter, image = path)+
  geom_image(size = 0.2)+
  geom_text(data = slice(noise_im_data, c(1,3)), nudge_x = -0.0013)+
  geom_text(data = slice(noise_im_data, c(2,4)), nudge_x = 0.0013)+
  scale_x_continuous(expand = c(0.002, 0.002))+
  scale_y_continuous(expand = c(0.01, 0.01))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "lightgrey"),
        panel.grid = element_blank())

# Maybe not use figure of all images? Or add it in the end of the chapter?
all1 <- ggplot(D_L_test_plots)+
  aes(D, L_mean, size = Iter, colour = Series)+
  geom_point(alpha = 0.6)+
  scale_size_area(max_size = 4)+
  theme_bw()+
  theme(legend.position = "bottom")

all2 <- ggplot(D_L_test_plots)+
  aes(D, L, size = Iter, colour = Series)+
  geom_point(alpha = 0.6)+
  scale_size_area(max_size = 4)+
  theme_bw()+
  theme(legend.position = "none")

all_legend <- get_legend(all1)
fig08_all <- plot_grid(plot_grid(all1+theme(legend.position = "none"),
                                 all2, nrow = 1, labels = "auto"),
                       all_legend, ncol = 1, rel_heights = c(1, 0.1))
# Note to self: From this it seems density is the only factor that
# has significant impact on both D and L
# Image size (N_plot) has important impact on D
# The other factors (clustering, noise and size distribution) have much more
# limited impact.
# However, the applied range of each of these factors is arbitrary and
# not straightforward to compare between them.

# Save plots
save(fig08_all, file = "Results/fig08_all.RData")
save(fig08_noise, file = "Results/fig08_noise.RData")
save(fig08_clustering, file = "Results/fig08_clustering.RData")
save(fig08_distr, file = "Results/fig08_distr.RData")
save(fig08_dens, file = "Results/fig08_dens.RData")
save(fig08_im_size, file = "Results/fig08_im_size.RData")
save(fig08_N, file = "Results/fig08_N.RData")

ggsave("Results/fig08_noise_im.pdf", plot = fig08_noise_im)
ggsave("Results/fig08_clustering_im.pdf", plot = fig08_clustering_im)
ggsave("Results/fig08_distr_im.pdf", plot = fig08_distr_im)
ggsave("Results/fig08_dens_im.pdf", plot = fig08_dens_im)
ggsave("Results/fig08_size_im.pdf", plot = fig08_size_im)
ggsave("Results/fig08_N_im.pdf", plot = fig08_N_im)

# END CHAPTER
