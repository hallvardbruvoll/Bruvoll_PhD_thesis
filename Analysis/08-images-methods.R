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

# Variable element count (N), constant density, size distribution and layout

iterations <- 15
N_plots <- list()
for (i in 2:(iterations+1)) {
  one_plot <- tibble(x = rep(1:i, i),
                     y = rep(1:i, each = i))
  plot_name <- paste0("N_plot_", i-1)
  N_plots[[plot_name]] <- ggplot(one_plot)+
    aes(x, y, width = 0.5, height = 0.5)+
    geom_tile(fill = "black", linetype = 0)+ # these aesthetics are needed
                                            # for the fract2D to work!
    theme_void()+
    scale_x_continuous(limits = c(0.75, i+0.75), expand = c(0,0))+
    scale_y_continuous(limits = c(0.75, i+0.75), expand = c(0,0))+
    theme(plot.background = element_rect(fill = "white", color = NA))+
    coord_fixed()
}
N_plots$N_plot_10+theme(panel.border = element_rect(colour = "black",
                                                   size = 1, fill = NA))
N_plots[[2]]

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

  #convert jpeg to greyscale
filenames <- list.files(path = "Data/Frac_test/")
length(filenames)
for (i in 1:length(filenames)) {
  path <- paste0("Data/Frac_test/", filenames[i])
  one_image <- load.image(path)
  one_image <- grayscale(one_image, method = "Luma")
  save.image(one_image, file = path, quality = 1)
}


test <- frac.lac(frac_path = "Data/Frac_test", lac_path = "Data/Lac_test")
test <- test$D_L_plot %>%
  mutate(label = gsub("N_plot_", "", id))
ggplot(test)+
  aes(D, L, label = label)+
  geom_point()+
  ggrepel::geom_text_repel()

test
