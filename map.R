#load packages
library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)

# Load world map shapefile and transform projection
world <- ne_countries(scale = "medium", returnclass = "sf")
world_proj <- st_transform(world, crs = "+proj=moll")

# Define countries to highlight
highlighted_countries <- c("Ecuador", "Kenya", "Madagascar", 
                           "Burundi", "Democratic Republic of the Congo", "Brazil")

# Filter highlighted countries before transforming
highlighted_map <- world %>%
  filter(admin %in% highlighted_countries)

# Reproject to Mollweide projection
highlighted_map_proj <- st_transform(highlighted_map, crs = "+proj=moll")

# Define colors for each country
country_colors <- c(
  "Brazil" = "blue",
  "Burundi" = "#D95F02",
  "Democratic Republic of the Congo" = "purple",
  "Ecuador" = "#E7298A",
  "Kenya" = "black",
  "Madagascar" = "#00AB66"
)

# Create the map plot
Figure4b <- ggplot() +
  geom_sf(data = world_proj, fill = "gray90", color = "black") +  # Land in gray
  geom_sf(data = highlighted_map_proj, aes(fill = admin), color = "black") +  # Highlighted countries
  scale_fill_manual(values = country_colors) +  # Apply color mapping
  theme_minimal() +
  coord_sf(crs = "+proj=moll") 

# Save the plot
ggsave("Figure 4b.tiff", plot = Figure4b, width = 13, height = 8, dpi = 300, units = "in", device = "tiff")
