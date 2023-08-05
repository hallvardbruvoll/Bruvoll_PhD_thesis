# Fractal dimension and lacunarity of archaeological settlement plans

# Load custom functions
source("Analysis/Image-functions.R")
library(tidyverse)
library(ggimage)

# Analyse the images¨and store results
# This takes a few hours
# images_results <-  frac.lac(frac_path = "Data/Fractal_dimension",
#                             lac_path = "Data/Lacunarity")
#
# save(images_results, file = "Results/images_results.RData")

# Load other variables and bind tables
image_variables <- read.csv2("Data/image_variables.csv")

D_L_plot <- left_join(images_results$D_L_plot, image_variables,
                      by = c("id", "filename")) %>%
  relocate(c(Gini, N), .after = density) %>%
  relocate(Set, .after = id) %>%
  relocate(filename, .after = Series) %>%
  mutate(plan_image = paste0("Data/Fractal_dimension/", id, ".jpg"))

save(D_L_plot, file = "Results/D_L_plot.RData")

D_L_plot <- D_L_plot %>%
  mutate(img_size = round((N^(1/1.65)/250)+0.05, 3))


# Now we're talking!
load("Results/D_L_plot.RData")

# Make plots
# All images
all1 <- ggplot(D_L_plot)+
  aes(x = D, y = L_mean, size = N, colour = Series)+
  geom_point()+
  theme_bw()+
  scale_size_area()+
  theme(legend.position = "bottom")

all2 <- ggplot(D_L_plot)+
  aes(x = D, y = L, size = N, colour = Series, label = Set)+
  geom_point()+
  theme_bw()+
  scale_size_area()+
  geom_text(data = filter(D_L_plot, id == "Vrable_01"),
            aes(size = NULL), nudge_x = 0.05, nudge_y = 0.04)+
  geom_text(data = filter(D_L_plot, id == "Maidanetske"),
            aes(size = NULL), nudge_x = -0.07, nudge_y = -0.04)+
  theme(legend.position = "none")

all_legend <- get_legend(all1)
fig09_all <- plot_grid(plot_grid(all1+theme(legend.position = "none"),
                                 all2, nrow = 1, labels = "auto"),
                       all_legend, ncol = 1, rel_heights = c(2, 0.1))

# Settlements only (with plan images and as points)
fig09_settlements <- ggplot(filter(D_L_plot, Series == "Settlements"))+
  aes(x = D, y = L_mean, label = Set, colour = Category, label = Set)+
  geom_point()+
  geom_image(aes(image = plan_image, size = I(img_size), colour = NULL))+
  geom_text(data = filter(D_L_plot, id %in% c("Nebelivka", "Maidanetske")),
             nudge_x = 0.07, nudge_y = 3, show.legend = FALSE)+
  geom_text(data = filter(D_L_plot, id %in% c("Cifare", "Vlkas",
                                              "Moshuriv", "Horny")),
              nudge_x = 0.02, nudge_y = -1, show.legend = FALSE)+
  geom_text(data = filter(D_L_plot, id %in% c("Nevidzany", "Cierne")),
            nudge_x = 0.03, nudge_y = 1, show.legend = FALSE)+
  geom_text(data = filter(D_L_plot, id == "Talne 3"),
            nudge_x = 0.04, show.legend = FALSE)+
  geom_text(data = filter(D_L_plot, id %in% c("Mana", "Ulany", "Vrable")),
                          nudge_x = -0.02, nudge_y = 1, show.legend = FALSE)+
  geom_text(data = filter(D_L_plot, id == "Telince"),
            nudge_x = -0.03, show.legend = FALSE)+
  #ggrepel::geom_text_repel(aes(colour = Category), show.legend = FALSE)+
  scale_x_continuous(expand = c(0, 0.05))+
  scale_y_continuous(expand = c(0.2, 0.1))+
  #guides(size = "none")+
  theme_bw()+
  theme(panel.background = element_rect(fill = "lightgrey"),
        panel.grid = element_blank(), legend.position = "bottom")

settle1 <- ggplot(filter(D_L_plot, Series == "Settlements"))+
  aes(x = D, y = L_mean, colour = Category, label = Set)+
  geom_point(aes(size = N))+
  ggrepel::geom_text_repel(show.legend = FALSE)+
  scale_x_continuous(expand = c(0, 0.05))+
  scale_size_area()+
  theme_bw()+
  theme(legend.position = "bottom")

settle2 <- ggplot(filter(D_L_plot, Series == "Settlements"))+
  aes(x = N, y = density, colour = Category, label = Set)+
  geom_point(aes(size = N))+
  ggrepel::geom_text_repel(show.legend = FALSE)+
  theme_bw()+
  scale_x_log10()+
  scale_y_continuous(trans = c("log10", "reverse"))+
  scale_size_area()+
  labs(y = "Density")+
  theme(legend.position = "none")

settle_legend <- get_legend(settle1)
fig09_settle_points <- plot_grid(
  plot_grid(settle1+theme(legend.position = "none"),
            settle2, nrow = 1, labels = "auto"),
  settle_legend, ncol = 1, rel_heights = c(2, 0.1))

D_L_plot %>%
  filter(Series == "Quarters") %>%
  mutate(label = if_else(Category == "Nebelivka",
        gsub("Nebelivka", "Neb.", Set), Set)) %>%
  select(label)

# Quarters/neighbourhoods, same, with images first
quarters_data <- filter(D_L_plot, Series == "Quarters") %>%
  mutate(label = if_else(Category == "Nebelivka",
                         gsub("Nebelivka", "Neb.", Set), Set))
fig09_quarters <- ggplot(quarters_data)+
  aes(x = D, y = L_mean, label = label, colour = Category)+
  geom_point()+
  geom_image(aes(image = plan_image, size = I(img_size), colour = NULL))+
  geom_text(data = filter(quarters_data, str_starts(label, "N")),
           nudge_x = 0.014, nudge_y = 0, show.legend = FALSE)+
  geom_text(data = filter(quarters_data, str_starts(label, "V")),
            nudge_x = 0, nudge_y = -0.4, show.legend = FALSE)+
  scale_x_continuous(expand = c(0.01, 0.02))+
  scale_y_continuous(expand = c(0.1, 0.1))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "lightgrey"),
        panel.grid = element_blank(), legend.position = "none")

  # And with points
quart1 <- ggplot(filter(D_L_plot, Series == "Quarters"))+
  aes(x = D, y = L_mean, colour = Category,
      label = gsub("Nebelivka", "Neb.", Set))+
  geom_point(aes(size = N))+
  ggrepel::geom_text_repel(show.legend = FALSE)+
  scale_size_area(max_size = 4)+
  theme_bw()+
  guides(colour = "none")+
  theme(legend.position = "bottom")

quart2 <- ggplot(filter(D_L_plot, Series == "Quarters"))+
  aes(x = N, y = density, colour = Category,
      label = gsub("Nebelivka", "Neb.", Set))+
  geom_point(aes(size = N))+
  ggrepel::geom_text_repel(show.legend = FALSE)+
  scale_size_area(max_size = 4)+
  theme_bw()+
  scale_x_log10()+
  scale_y_continuous(trans = c("log10", "reverse"))+
  labs(y = "Density")+
  theme(legend.position = "none")

quart_legend <- get_legend(quart1)

fig09_quart_points <- plot_grid(
  plot_grid(quart1+theme(legend.position = "none"),
            quart2, nrow = 1, labels = "auto"),
  quart_legend, ncol = 1, rel_heights = c(2, 0.1))

# Vráble time samples
load("Data/time_samples.RData")
time_data <- filter(D_L_plot, Series == "Time") %>%
  mutate(sample = as.numeric(gsub("Vrable_", "", id)),
         img_size = img_size*3.5) %>%
  left_join(., time_samples, by = "sample")
time_selection <- time_data %>%
  slice(c(3,5,9,14))

fig09_time <- ggplot(time_selection)+
  aes(x = D, y = L_mean, label = dates)+
  geom_image(aes(image = plan_image, size = I(img_size)))+
  geom_text(data = slice(time_selection, 1),
            nudge_x = 0.04, show.legend = FALSE)+
  geom_text(data = slice(time_selection, 2),
            nudge_x = 0.02, nudge_y = 50, show.legend = FALSE)+
  geom_text(data = slice(time_selection, 3),
            nudge_x = 0.02, nudge_y = 70, show.legend = FALSE)+
  geom_text(data = slice(time_selection, 4),
            nudge_x = -0.03, show.legend = FALSE)+
  scale_x_continuous(expand = c(0.15, 0))+
  scale_y_continuous(expand = c(0.4, 0.3))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "lightgrey"),
        panel.grid = element_blank())

time1 <- ggplot(filter(time_data, id != "Vrable_01"))+
  aes(x = D, y = L_mean, label = dates)+
  geom_point(aes(size = N))+
  geom_path()+
  ggrepel::geom_text_repel(show.legend = FALSE)+
  scale_size_area(max_size = 4)+
  theme_bw()+
  theme(legend.position = "bottom")

time2 <- ggplot(filter(time_data, id != "Vrable_01"))+
  aes(x = N, y = density, label = dates)+
  geom_point(aes(size = N))+
  geom_path()+
  ggrepel::geom_text_repel(show.legend = FALSE)+
  scale_size_area(max_size = 4)+
  theme_bw()+
  scale_x_log10()+
  scale_y_continuous(trans = c("log10", "reverse"))+
  labs(y = "Density")+
  theme(legend.position = "none")

time_legend <- get_legend(time1)

fig09_time_points <- plot_grid(
  plot_grid(time1+theme(legend.position = "none"),
            time2, nrow = 1, labels = "auto"),
  time_legend, ncol = 1, rel_heights = c(2, 0.1))

# Save plots
save(fig09_all, file = "Results/fig09_all.RData")
ggsave("Results/fig09_settlements.pdf", plot = fig09_settlements) #pdf
save(fig09_settle_points, file = "Results/fig09_settle_points.RData")
ggsave("Results/fig09_quarters.pdf", plot = fig09_quarters) #pdf
save(fig09_quart_points, file = "Results/fig09_quart_points.RData")
ggsave("Results/fig09_time.pdf", plot = fig09_time) #pdf
save(fig09_time_points, file = "Results/fig09_time_points.RData")
