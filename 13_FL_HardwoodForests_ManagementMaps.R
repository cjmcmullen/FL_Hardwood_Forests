## FL Hardwood Forests Species Complete Mapping
## 2-18-2025 by Sydney Barfus

library(sf)
library(ggplot2)
library(geodata)
library(raster)
library(dplyr)

setwd("/blue/soltis/share/FL_HardwoodForests/") 

## Get states for Florida shape, and convert to spatial
states <- gadm(country="USA", level=1, path = tempdir())
states_sf <- st_as_sf(states)

## Set up hardwood shape file
hardwood.shape <- st_read("/blue/soltis/share/FL_HardwoodForests/02_rasters/FL_HardwoodForest_Shapefile/vcom67_Hardwood.shp")
st_geometry_type(hardwood.shape)
hardwood.shape <- st_zm(hardwood.shape)
raster_img <- raster(hardwood.shape)
proj4string(raster_img) <- CRS("+proj=longlat +datum=WGS84")


## Set up management zones
#unzip("./02_rasters/SeaLevel_ManagementZones/Florida_Conservation_Lands_Dec_2024.kmz", exdir = "./02_rasters/SeaLevel_ManagementZones/Florida_Conservation_Lands.kml")
management_zones <- st_layers("./02_rasters/SeaLevel_ManagementZones/doc.kml")
selected_layers <- c("Federal Managing Agency", 
                     "State Managing Agency", 
                     "Local Managing Agency", 
                     "Private Managing Agency",
                     "Conservation Easements")
filtered_layer_names <- management_zones$name %in% selected_layers
# Read the filtered layers and keep both geometry and layer_name columns
filtered_layers <- lapply(management_zones$name[filtered_layer_names], function(layer_name) {
  layer_data <- st_read("./02_rasters/SeaLevel_ManagementZones/doc.kml", layer = layer_name)
  layer_data$layer_name <- layer_name  # Add the layer_name column to the data
  return(layer_data)})
# Combine the filtered layers into a single sf object
combined_filtered_layers <- do.call(rbind, filtered_layers)


## Find overlap of Hardwood Forests and management zones
hardwood.shape <- st_make_valid(hardwood.shape)
combined_filtered_layers <- st_make_valid(combined_filtered_layers)

overlap_percentages <- list()

# Loop through each unique value in layer_name
for(layer in unique(combined_filtered_layers$layer_name)) {
  layer_shape <- combined_filtered_layers %>% filter(layer_name == layer)
  # Calculate the intersection between hardwood.shape and the current layer
  intersection <- st_intersection(hardwood.shape, layer_shape)
  # Calculate the area of the intersection and the hardwood.shape
  intersection_area <- st_area(intersection)
  hardwood_area <- st_area(hardwood.shape)
  # Calculate the percentage overlap
  overlap_percentage <- sum(intersection_area) / sum(hardwood_area) * 100
  overlap_percentages[[layer]] <- overlap_percentage
}

overlap_percentages <- lapply(overlap_percentages, function(x) round(x, digits=2))
combined_filtered_layers <- combined_filtered_layers %>%
  mutate(layer_percentage = paste0(combined_filtered_layers$layer_name, " (", overlap_percentages[combined_filtered_layers$layer_name], "%)"))



## create plot
ggplot() +
  # Plot for states
  geom_sf(data = states_sf, fill = "grey90", color = "white") +
  # Plot for hardwood shape
  geom_sf(data = hardwood.shape, fill = "gray40", color = "gray40") +
  
  # Plot for management zones
  geom_sf(data = combined_filtered_layers, aes(fill = layer_percentage), color = NA, alpha = 0.8) +
  # Customize the fill colors for the management zones
  scale_fill_manual(values = c("Federal Managing Agency (0.47%)" = "red", 
                               "State Managing Agency (7.6%)" = "blue", 
                               "Local Managing Agency (0.95%)" = "green", 
                               "Private Managing Agency (0.14%)" = "yellow",
                               "Conservation Easements (2.03%)" = "purple"),
                    name = "Management Zones") +  # Set legend title for management zones
  
  # Set coordinate limits and labels
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  xlab("Longitude") + 
  ylab("Latitude") +
  # Add title
  ggtitle("Overlap of Management Zones and Florida Hardwood Forests") +
  # Theme customization
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 10)
)

## save plot 
#dir.create(file.path("10_Maps", "01_Management_Map"))
ggsave("Management_Zones.png", path = "./10_Maps/01_Management_Map/", 
       units= "in", height = 7.5, width = 11)

