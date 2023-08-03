# Functions for calculating fractal dimension (D) and lacunarity (L)
# on binary images

# Load libraries
library(fractD)
library(terra)
library(envi)
library(lacunaritycovariance)
library(tidyverse)

# Loop function for lacunarity of images
# Fractal dimension function has inbuilt loop
# Set boxwidths default to that of fract2D (fractD package)
# Images must be in jpg/jpeg greyscale format for D and tif/tiff for L
# since I didn't find a workaround for that
frac.lac <- function(frac_path, lac_path,
                     boxwidths = c(1,2,4,8,16,32,64,128,256,512)) {
  # Lacunarity image filename vector
  filenames <- list.files(path = lac_path)
  # Object for raw data with estimates according to box sizes
  gbl_graph <- tibble()
  # Object for the estimated Lacunarity (power-law approximation) per site
  D_L_plot <- tibble(filename = filenames, L = as.numeric(NA),
                     L_prefactor = as.numeric(NA),
                     L_mean = as.numeric(NA), density = as.numeric(NA)) %>%
    mutate(id = tools::file_path_sans_ext(filename))
  #Loop lacunarity analysis for each site
  for (i in 1:length(filenames)) {
    #Load settlement plan
    one_lac_path <- if_else(endsWith(lac_path, suffix = "/") == TRUE,
                        paste0(lac_path, filenames[i]),
                        paste0(lac_path, "/", filenames[i]))
    lac_raster <- rast(one_lac_path, lyrs = 1)
    #Convert to binary if needed (concerns the synthetic plans only)
    if (max(lac_raster[]) > 1) {
      lac_raster[] <- if_else(lac_raster[] > max(lac_raster[])/2, 0, 1)
      }
    lac_im <- envi:::as.im.SpatRaster(lac_raster)
    #Analyse
    lac_gbl <- gbl(lac_im, boxwidths = boxwidths, estimators = "GBLcc.pickaH")
    # Fit linear model to log-log transformed values.
    # NAs are omitted by default
    image_fit <- lm(log(gblcc.pickaH) ~ log(s), data = lac_gbl$gbl.est, )
    #Extract values and tidy
    one_gbl <- tibble(lac_gbl$gbl.est) %>%
      mutate(id = D_L_plot$id[i]) %>%
      rename(box.size = s) %>%
      relocate(id, .before = box.size)
    #Bind results for all sites
    gbl_graph <- bind_rows(gbl_graph, one_gbl)
    #L here defined as the scaling exponent
    D_L_plot$L[i] <- image_fit$coefficients[2]*(-1)
    #Prefactor is the exponential of the y-intercept (in log-log lm)
    D_L_plot$L_prefactor[i] <- exp(image_fit$coefficients[1])
    #mean of lacunarity index across all box-sizes
    D_L_plot$L_mean[i] <- base::mean(lac_gbl$gbl.est$gblcc.pickaH,
                                     na.rm = TRUE)
    #image density is fraction of foreground pixels to total pixel count
    D_L_plot$density[i] <- round(sum(lac_im$v)/length(lac_im$v), 3)
    #Print progress status
    print(paste("progress (L):", round(i/length(filenames)*100, 1), "%"))
  }
  # Calculate fractal dimension
  boxcounting <- fract2D(dir = frac_path, box.size = boxwidths)
  # Compile results for output
  D_L_plot <- left_join(D_L_plot, boxcounting$D, by = "id") %>%
    relocate(c(id, D), .before = filename) %>%
    relocate(filename, .after = density)
  return(list("gbl_graph" = gbl_graph,
              "box_graph" = boxcounting$raw.dat,
              "D_L_plot" = D_L_plot))
}

#For calculation of summary stats in FracLac,
#see https://imagej.nih.gov/ij/plugins/fraclac/FLHelp/lactutorial.htm#barlambda
