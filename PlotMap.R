library(rnaturalearth)
library(sf)
library(ggplot2)

subplotFolder <- "subplots"


table <- readr::read_csv("EhdenData.csv")
maxSize <- table |>
  group_by(Country) |>
  summarise(maxSize = max(`Person Count`))

world <- ne_countries(scale = 50, returnclass = 'sf')
world <- world |>
  left_join(maxSize, by = join_by(iso_a2_eh == Country)) |>
  mutate(density = pmin(maxSize / pop_est, 1)) |>
  select(name_en, iso_a2_eh, maxSize, pop_est, density, geometry)

ggplot(world) + 
  geom_sf(aes(fill = density), color = 'black') +
  coord_sf(crs = st_crs(3035),
           xlim = c(2000000, 7000000), 
           ylim = c(1350000, 5400000)) +
  scale_fill_gradientn("Density",
    limits = c(0, 1),
                       colors = c("#336B91", "#11A08A", "#FBC511"),
                       values = c(0, 0.5, 1)) + 
  theme_void() +
  theme(panel.background = element_rect(fill = '#92c5f0'),
        panel.grid.major = element_line(linewidth = 0.1, 
                                        color = '#80808080')) 
ggsave(filename = file.path(subplotFolder, "Map.png"), width = 8.2, height = 8)


ggplot(world) + 
  geom_sf(aes(fill = density), color = 'black') +
  coord_sf(crs = st_crs(3035),
           xlim = c(0, 9000000), 
           ylim = c(350000, 6400000)) +
  scale_fill_gradientn("Density",
                       limits = c(0, 1),
                       colors = c("#336B91", "#11A08A", "#FBC511"),
                       values = c(0, 0.5, 1)) + 
  theme_void() +
  theme(panel.background = element_rect(fill = '#92c5f0'),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_line(linewidth = 0.1, 
                                        color = '#80808080')) 
ggsave(filename = file.path(subplotFolder, "Map.png"), width = 15, height = 8, dpi = 300)
