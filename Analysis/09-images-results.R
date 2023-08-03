# Fractal dimension and lacunarity of archaeological settlement plans

# Load custom functions
source("Analysis/Image-functions.R")


images_results <-  frac.lac(frac_path = "Data/Fractal_dimension",
                            lac_path = "Data/Lacunarity")

save(images_results, file = "images_results.RData")
