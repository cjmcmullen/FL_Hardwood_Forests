## Fig3
library(ggplot2)
library(sf)
library(raster)
library(dplyr)
library(tidyr)
library(viridis)
library(ggpubr)
library(grid)
library(biomod2)


setwd("/blue/soltis/share/FL_HardwoodForests")

time_period <- "2081-2100"
model <- "ACCESS-CM2"
ssp <- "ssp370"

# Load hardwood shapefile and state outlines
hardwood.shape <- st_read("02_rasters/FL_HardwoodForest_Shapefile/vcom67_Hardwood.shp")
states <- map_data("state")
world <- map_data("world")

# Load alldf for occurrence points
alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldf_V3.csv")

# Specify species to plot
species_list <- c("Hdiptera", "Tamericana")

# Set global theme
theme_set(theme_minimal() + 
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
              legend.position = "bottom"))

# Set up list for plots
all_plots <- list()


# create plots to add to figure
for (species in species_list) {
  
  # get percentages for each species' titles
  if (species == "Hdiptera") {
    percentages <- c("54.92%", "54.71%")
  } else if (species == "Tamericana"){
    percentages <- c("70.69%", "13.66%")
  }
  
  # Get species subset and full name
  species_df <- dplyr::filter(alldf, accepted_name == species)
  fullname <- unique(species_df$full_name)
  
  # Load current projection
  load(paste0("05_CurrentProjections/", species, "_CurrentHardwood_Projection.RData"))
  p_df <- as.data.frame(rasterToPoints(p))
  colnames(p_df)[3] <- "habitat_suitability"
  
  current_proj <- ggplot(data = world) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
    geom_tile(data = p_df, aes(x = x, y = y, fill = habitat_suitability)) +
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
    labs(
      title = bquote(italic(.(fullname)) ~ "Florida Hardwood Forests"),
      subtitle = paste0("Current Habitat Suitability (", percentages[1], ")"), 
      x = "Longitude", y = "Latitude",
      fill = "Habitat Suitability") +
    scale_fill_gradientn(colours = viridis::mako(99, direction = -1),
                         na.value = "white", 
                         breaks=c(0, 0.25, 0.5, 0.75, 1),
                         labels=c("Low",0.25, 0.5, 0.75, "High"), 
                         limits=c(0,1)) +
    guides(fill = guide_colorbar(title.hjust = 1))
  
  all_plots[[length(all_plots) + 1]] <- current_proj
  
  # Load future projection
  load(paste0("06_FutureProjections/", species, "/", species, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.RData"))
  p_df_future <- as.data.frame(rasterToPoints(p))
  colnames(p_df_future)[3] <- "habitat_suitability"
  
  future_proj <- ggplot(data = world) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
    geom_tile(data = p_df_future, aes(x = x, y = y, fill = habitat_suitability)) +
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
    labs(
      title = bquote(italic(.(fullname)) ~ "Florida Hardwood Forests"),
      subtitle = paste0("Future Habitat Suitability (", percentages[2], ")"), 
      x = "Longitude", y = "Latitude",
      fill = "Habitat Suitability"
    ) +
    scale_fill_gradientn(colours = viridis::mako(99, direction = -1),
                         na.value = "white", 
                         breaks=c(0, 0.25, 0.5, 0.75, 1),
                         labels=c("Low",0.25, 0.5, 0.75, "High"), 
                         limits=c(0,1)) +
    guides(fill = guide_colorbar(title.hjust = 1))
  
  
  all_plots[[length(all_plots) + 1]] <- future_proj
  
  # Load binary change raster and plot
  current_bin <- raster(paste0("07C_ProjectionCompairsons/", species, "/", species, "_current_binary.asc"))
  future_bin <- raster(paste0("07C_ProjectionCompairsons/", species, "/", species, "_", model, "_", time_period, "_", ssp, "_binary.asc"))
  change <- BIOMOD_RangeSize(current_bin, future_bin)
  results <- change$Compt.By.Models
  df_change <- as.data.frame(change$Diff.By.Pixel, xy = TRUE)
  
  # Identify the name of the raster column (it's the third column)
  raster_col <- colnames(df_change)[3]
  
  # Rename it to "layer" for clarity
  colnames(df_change)[3] <- "layer"
  
  # Convert to factor with meaningful labels
  df_change$layer <- factor(df_change$layer,
                            levels = c(-2, -1, 0, 1))
  
  # Extract pixel change counts
  loss_count     <- results[1]
  stable0_count  <- results[2]
  stable1_count  <- results[3]
  gain_count     <- results[4]
  
  comp_plot <- ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "white") +
    geom_tile(data = df_change, aes(x = x, y = y, fill = layer)) +
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
    labs(
      title = bquote(italic(.(fullname)) ~ "Florida Hardwood Forests"),
      subtitle = expression("Change in Habitat Suitability (1 pixel " %~% "1 km"^2*")"),
      x = "Longitude", y = "Latitude") +
    scale_fill_manual(
      ## manually set viridis color scale
      values = c("1" = "#440154", "0" = "#31688E", "-1" = "#35b779", "-2" = "#FDE725" ),
      na.value = "white",    
      name = "Pixel Change",
      breaks = c("-2", "-1", "0", "1"),  
      labels = c(
        paste0("Lost \n(", loss_count, " pixels)"),
        paste0("Maintained \n(", stable1_count, " pixels)"),
        paste0("Never Occupied \n(", stable0_count, " pixels)"),
        paste0("Gained \n(", gain_count, " pixels)"))) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.hjust = 0.8)) 
  
  
  all_plots[[length(all_plots) + 1]] <- comp_plot
  
  # Load ETF projection
  load(paste0("08B_FutureETFProjections/", species, "/", species, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.RData"))
  p_df_etf <- as.data.frame(rasterToPoints(p))
  colnames(p_df_etf)[3] <- "habitat_suitability"
  
  etf_proj <- ggplot(data = world) +
    geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
    geom_tile(data = p_df_etf, aes(x = x, y = y, fill = habitat_suitability)) +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "transparent", color = "white") +
    geom_point(data = species_df, mapping =aes(x = long, y = lat), pch = 19, col='red', cex=0.2, alpha = 0.5) +
    coord_sf(xlim = c(-98.85417, -61.42083), ylim = c(26.1375, 48.29583)) +
    labs(
      title = bquote(italic(.(fullname)) ~ "Eastern Temperate Forests"),
      subtitle = "Future Habitat Suitability", 
      x = "Longitude", y = "Latitude"
    ) +
    scale_fill_gradientn(colours = viridis::mako(99, direction = -1),
                         na.value = "white",
                         name = "Habitat Suitability",
                         breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = c("Low", 0.25, 0.5, 0.75, "High"),
                         limits = c(0, 1)) +
    guides(fill = guide_colorbar(title.hjust = 1))
  
  
  all_plots[[length(all_plots) + 1]] <- etf_proj
}
  

# Final Figure ------------------------------------------------------------

final_plot <- ggarrange(plotlist = all_plots, 
                        ncol = 4, 
                        nrow = 2,
                        labels = LETTERS[1:8])

ggsave("14_Figures/Fig3_TwoSpecies_ENM.png",  bg = "white", plot = final_plot, width = 18, height = 8, dpi = 300)
