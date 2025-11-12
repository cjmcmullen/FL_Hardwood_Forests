### Species Richness 
## script by ME Mabry, Sydney Barfus
library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(ggnewscale)
library(ggpubr)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

# Set working directory
setwd("/blue/soltis/share/FL_HardwoodForests/")

# Load SLR polygon (simplified, aggregated)
slr_poly <- st_read("/blue/soltis/share/FL_Scrub/share/08_shapefile/NOAA_SLR_2FT_INUNDATED_AGG100.shp") |> 
  st_make_valid()


# CURRENT -----------------------------------------------------------------

# Load the species richness raster
richness_cur <- rast("11_SpeciesRichness/current_binary_SpeciesRichness.asc")

# Convert species raster to data frame
richness_df_cur <- as.data.frame(richness_cur, xy = TRUE, na.rm = TRUE)
colnames(richness_df_cur) <- c("x", "y", "richness")

# Load Florida basemap
states <- ne_states(country = "United States of America", returnclass = "sf")
florida <- states %>% dplyr::filter(name == "Florida")

# create current plot
cur_plot <- ggplot() +
  geom_sf(data = states, fill = "grey90", color = "white") +
  geom_raster(data = richness_df_cur, aes(x = x, y = y, fill = richness)) +
  scale_fill_viridis_c(name = "Species\nRichness", na.value = "transparent",
                       guide = guide_colorbar(order = 1, , title.hjust = 0.5)) +
  
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  labs(title = "\n\nCurrent Species Richness",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 6),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.key.height = unit(0.5, "cm"))

ggsave("11_SpeciesRichness/02_Species_Richness_Plots/Current_Richness_Plot.png", cur_plot, width = 8, height = 6, dpi = 300)



# FUTURE ------------------------------------------------------------------

# Load the species richness raster
richness_fut <- rast("11_SpeciesRichness/ACCESS-CM2_2081-2100_ssp370_binary_SpeciesRichness.asc")

# Reproject polygon to match raster CRS (if needed)
# slr_poly <- project(slr_poly, crs(richness_fut))
slr_poly <- st_transform(slr_poly, crs = crs(richness_fut, proj=TRUE))

# Mask species raster with SLR polygon (to get overlap)
in_slr_fut <- mask(richness_fut, slr_poly)

# Calculate overlap statistics
total_pixels_fut <- global(richness_fut, fun = "sum", na.rm = TRUE)[1,1]
overlap_pixels_fut <- global(in_slr_fut, fun = "sum", na.rm = TRUE)[1,1]
overlap_percent_fut <- (overlap_pixels_fut / total_pixels_fut) * 100

# Convert species raster to data frame
richness_df_fut <- as.data.frame(richness_fut, xy = TRUE, na.rm = TRUE)
colnames(richness_df_fut) <- c("x", "y", "richness")

# df to plot overlapping pixels
overlap_df_fut <- as.data.frame(in_slr_fut, xy = TRUE, na.rm = TRUE)
colnames(overlap_df_fut) <- c("x", "y", "richness")

# add types so that it can be included in legend
slr_poly$type <- "Sea Level Rise"
overlap_df_fut$type <- "Overlap"

# Load Florida basemap
states <- ne_states(country = "United States of America", returnclass = "sf")
florida <- states %>% dplyr::filter(name == "Florida")

# create future plot
fut_plot <- ggplot() +
  geom_sf(data = states, fill = "grey90", color = "white") +
  geom_raster(data = richness_df_fut, aes(x = x, y = y, fill = richness)) +
  scale_fill_viridis_c(name = "Species\nRichness", na.value = "transparent",
                       guide = guide_colorbar(order = 1, , title.hjust = 0.5)) +
  new_scale_fill() +
  
  geom_raster(data = overlap_df_fut, aes(x = x, y = y, fill = type), alpha = 0.6) +
  geom_sf(data = slr_poly, aes(fill = type), alpha = 0.3, color = NA) +
  scale_fill_manual(
    name = "Overlay",
    values = c("Sea Level Rise" = "blue", "Overlap" = "red"),
    breaks = c("Sea Level Rise", "Overlap")) +
  
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  labs(title = "\nFuture Species Richness and 2ft Sea Level Rise",
       subtitle = paste0("Overlay shows projected inundated area (", round(overlap_percent_fut, 2), " %)" ),
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 6),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.size = unit(0.5, "cm"))

ggsave("11_SpeciesRichness/02_Species_Richness_Plots/Future_SeaLevel_Richness_Plot.png", fut_plot, width = 8, height = 6, dpi = 300)



# COMPARISON --------------------------------------------------------------

# Calculate change in species richness (future - current)
richness_change <- richness_fut - richness_cur

# Classify the change values into categories
richness_change_classified <- app(richness_change, function(x) {
  ifelse(is.na(x), NA,  # Keep NA values as NA
         ifelse(x < 0, -1,  # Species loss
                ifelse(x == 0, 0,  # No change
                       ifelse(x > 0, 1, NA))))  # Species gain
})

# Count the number of species lost, no change, and gained
species_loss_count <- global(richness_change_classified == -1, "sum", na.rm = TRUE)[1, 1]
species_no_change_count <- global(richness_change_classified == 0, "sum", na.rm = TRUE)[1, 1]
species_gain_count      <- global(richness_change_classified == 1, "sum", na.rm = TRUE)[1, 1]

# Convert the classified raster to a data frame for plotting
change_df <- as.data.frame(richness_change_classified, xy = TRUE) %>%
  rename(change = lyr.1) %>%
  filter(!is.na(change))  # Remove NA values for plotting

# Create a plot with classified species richness change
comp_plot <- ggplot() +
  geom_sf(data = states, fill = "grey90", color = "white") +
  geom_raster(data = change_df, aes(x = x, y = y, fill = factor(change))) +
  scale_fill_manual(values = c("-1" = "#FDE725FF", "0" = "#31688EFF", "1" = "#440154FF"),
                    labels = c(
                      paste("Loss (", species_loss_count, " pixels)", sep = ""),
                      paste("No Change (", species_no_change_count, " pixels)", sep = ""),
                      paste("Gain (", species_gain_count, " pixels)", sep = "")
                    ),
                    name = "Change") +
  guides(fill = guide_legend(order = 1, title.hjust = 0.5)) +

  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  theme_minimal() +
  labs(title = "Change in Species Richness",
       subtitle = "Current vs ACCESS-CM2 2081-2100 ssp370",
       x = "Longitude", y = "Latitude") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 6),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.height = unit(0.5, "cm"))

ggsave("11_SpeciesRichness/02_Species_Richness_Plots/Comparison_Richness_Plot.png", comp_plot, width = 8, height = 6, dpi = 300)



# MANAGEMENT PLOT ---------------------------------------------------------

# Load in management zones and filter to federal and state levels
management_zones <- st_layers("./02_rasters/SeaLevel_ManagementZones/doc.kml")
selected_layers <- c("Federal Managing Agency" , 
                     "State Managing Agency")
filtered_layer_names <- management_zones$name %in% selected_layers

# Read the filtered layers and keep both geometry and layer_name columns
filtered_layers <- lapply(management_zones$name[filtered_layer_names], function(layer_name) {
  layer_data <- st_read("./02_rasters/SeaLevel_ManagementZones/doc.kml", layer = layer_name)
  layer_data$layer_name <- layer_name  # Add the layer_name column to the data
  return(layer_data)})

# Combine the filtered layers into a single sf object
combined_filtered_layers <- do.call(rbind, filtered_layers)
combined_filtered_layers <- st_make_valid(combined_filtered_layers)

# Load in hardwood forest shape file
hardwood.shape <- st_read("/blue/soltis/share/FL_HardwoodForests/02_rasters/FL_HardwoodForest_Shapefile/vcom67_Hardwood.shp")

# Transform management zones and hardwood to match SLR CRS (same as richness raster)
hardwood.shape <- st_transform(hardwood.shape, crs = st_crs(slr_poly))
combined_filtered_layers <- st_transform(combined_filtered_layers, crs = st_crs(slr_poly))

# Create plot
mang_plot <- ggplot() +
  geom_sf(data = states, fill = "grey90", color = "white") +
  geom_sf(data = hardwood.shape, fill = "gray30", color = NA) +
  
  geom_sf(data = slr_poly, aes(fill = type), alpha = 0.3, color = NA) +
  scale_fill_manual(
    name = "Overlay",
    values = c("Sea Level Rise" = "blue"),
    breaks = c("Sea Level Rise")) +
  guides(fill = guide_legend(order = 2, , title.hjust = 0.5)) +
  new_scale_fill() +

  geom_sf(data = combined_filtered_layers, aes(fill = "Management Zones (8.07%)"), color = NA, alpha = 0.5) +
  scale_fill_manual(values = c("Management Zones (8.07%)" = "red"),
                    name = "Management\n(% of Hardwoods)") + 
  guides(fill = guide_legend(order = 1, , title.hjust = 0.8)) +
  new_scale_fill() +
  
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  labs(title = "Management Zones and 2ft Sea Level Rise",
       subtitle = paste0("State and Federal Management" ),
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 6),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.height = unit(0.5, "cm"))

ggsave("11_SpeciesRichness/02_Species_Richness_Plots/Mangement_SeaLevel_Richness_Plot.png", mang_plot, width = 8, height = 6, dpi = 300)



# CREATE FIGURE -----------------------------------------------------------

all_plots <- list(cur_plot, fut_plot, comp_plot, mang_plot)

final_plot <- ggarrange(plotlist = all_plots, 
                        ncol = 4, 
                        nrow = 1,
                        labels = LETTERS[1:4])

ggsave("14_Figures/Fig5_SpeciesRichness-REDONE.png",  bg = "white", plot = final_plot, width = 20, height = 5, dpi = 300)

