## FL Hardwood Forests Species Species Richness
## 4-1-2025 by ME Mabry, Sydney Barfus


# Load required libraries
library(raster)  
library(terra)   
library(ggplot2)
library(dplyr)
library(sf)

setwd("/blue/soltis/share/FL_HardwoodForests/")

# Plot Species Richness Only ----------------------------------------------


## Load all df to get species names
alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldfBOR_V2.csv")
species_names <- unique(alldf$accepted_name)

# Load your raster stack (change here for each model)
# Specify the directory containing your files - CHANGE HERE FOR FUTURE COMPARISON
directory_path <- "07C_ProjectionCompairsons"

# Define the file patterns for all the models and scenarios
file_patterns <- c(
  # "current_binary.asc"#, # for the current projection, run by itself
  "ACCESS-CM2_2041-2060_ssp245_binary.asc",
  "ACCESS-CM2_2041-2060_ssp370_binary.asc",
  "ACCESS-CM2_2041-2060_ssp585_binary.asc",
  "ACCESS-CM2_2081-2100_ssp245_binary.asc",
  "ACCESS-CM2_2081-2100_ssp370_binary.asc",
  "ACCESS-CM2_2081-2100_ssp585_binary.asc",
  "GISS-E2-1-G_2041-2060_ssp245_binary.asc",
  "GISS-E2-1-G_2041-2060_ssp370_binary.asc",
  "GISS-E2-1-G_2041-2060_ssp585_binary.asc",
  "GISS-E2-1-G_2081-2100_ssp245_binary.asc",
  "GISS-E2-1-G_2081-2100_ssp370_binary.asc",
  "GISS-E2-1-G_2081-2100_ssp585_binary.asc"
)

# Set basemap data for plotting
states <- map_data("state")

# create list to add files
file_list <- list()


# Loop through each file pattern
for (pattern in file_patterns) {
  
  file_list <- NULL
  
  # iterate through each species' directory
  for (species in species_names) {
  # Get list of files that match the current pattern
  species_files <- list.files(path = paste0(directory_path, "/", species), pattern = pattern, full.names = TRUE)
  file_list <- c(file_list, species_files)
  }

  # Print progress message
  cat("Processing files for pattern:", pattern, "\n")
  
  # Load the raster stack
  habitat_stack <- stack(file_list)
  
  # Sum the raster stack layers to calculate the number of species with suitable habitat per pixel
  species_richness_raster <- calc(habitat_stack, sum)
  
  # Create an output filename for the raster
  raster_output_file <- paste0("11_SpeciesRichness/", sub("\\.asc$", "_SpeciesRichness.asc", pattern))
  
  # Save the raster as an ASCII file
  writeRaster(species_richness_raster, filename = raster_output_file, format = "ascii", overwrite = TRUE)
  
  # Convert the raster to a data frame for ggplot
  species_df <- as.data.frame(species_richness_raster, xy = TRUE) %>%
    rename(fill = layer)  # Rename 'layer' to 'fill' for consistency
  
  # Create a plot filename
  plot_filename <- paste0("11_SpeciesRichness/", sub("_binary\\.asc$", ".pdf", pattern))
  
  # Open a PDF device to save the plot
  pdf(file = plot_filename, width = 18, height = 10)
  
  # Generate the plot
  plot <- ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "white") +
    geom_raster(data = species_df, aes(x = x, y = y, fill = fill)) +
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
    scale_fill_viridis_c(option = "viridis", name = "Species Count", na.value = "transparent") +
    theme_minimal() +
    labs(title = paste0("Species Richness (", sub("\\.asc$", "", pattern), ")"),
         x = "Longitude", y = "Latitude") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA), # Set white background
          plot.background = element_rect(fill = "white", color = NA),  # Ensure entire plot stays white
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 8),
          plot.title = element_text(size = 10))
  
  # Print the plot to the PDF
  print(plot)
  
  # Close the PDF device
  dev.off()
  
  # Print message indicating the plot was saved
  cat("Saved plot and raster for pattern:", pattern, "\n")
}




# Plot with Management Zones ----------------------------------------------

## Load all df to get species names
alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldfBOR_V2.csv")
species_names <- unique(alldf$accepted_name)

# Load your raster stack (change here for each model)
# Specify the directory containing your files ## CHANGE HERE FOR FUTURE COMPARISON
directory_path <- "07C_ProjectionCompairsons"

# Define the file patterns for all the models and scenarios
file_patterns <- c(
  "current_binary.asc"#, # for the current projection, run by itself
  # "ACCESS-CM2_2041-2060_ssp245_binary.asc",
  # "ACCESS-CM2_2041-2060_ssp370_binary.asc",
  # "ACCESS-CM2_2041-2060_ssp585_binary.asc",
  # "ACCESS-CM2_2081-2100_ssp245_binary.asc",
  # "ACCESS-CM2_2081-2100_ssp370_binary.asc",
  # "ACCESS-CM2_2081-2100_ssp585_binary.asc",
  # "GISS-E2-1-G_2041-2060_ssp245_binary.asc",
  # "GISS-E2-1-G_2041-2060_ssp370_binary.asc",
  # "GISS-E2-1-G_2041-2060_ssp585_binary.asc",
  # "GISS-E2-1-G_2081-2100_ssp245_binary.asc",
  # "GISS-E2-1-G_2081-2100_ssp370_binary.asc",
  # "GISS-E2-1-G_2081-2100_ssp585_binary.asc"
)

# Set basemap data for plotting
states <- map_data("state")

# create list to add files
file_list <- list()


## set up management zones
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


# merged_layer <- combined_filtered_layers %>%
#   st_combine() %>%  # Combine all geometries into one
#   st_union()

# Loop through each file pattern to create plots
for (pattern in file_patterns) {
  
  file_list <- NULL
  
  # iterate through each species' directory
  for (species in species_names) {
    # Get list of files that match the current pattern
    species_files <- list.files(path = paste0(directory_path, "/", species), pattern = pattern, full.names = TRUE)
    file_list <- c(file_list, species_files)
  }
  
  # Print progress message
  cat("Processing files for pattern:", pattern, "\n")
  
  # Load the raster stack
  habitat_stack <- stack(file_list)
  
  # Sum the raster stack layers to calculate the number of species with suitable habitat per pixel
  species_richness_raster <- calc(habitat_stack, sum)
  
  # Create an output filename for the raster
  raster_output_file <- paste0("11_SpeciesRichness/01_Management_Zones/", sub("\\.asc$", "_SpeciesRichness.asc", pattern))
  
  # Save the raster as an ASCII file
  writeRaster(species_richness_raster, filename = raster_output_file, format = "ascii", overwrite = TRUE)
  
  # Convert the raster to a data frame for ggplot
  species_df <- as.data.frame(species_richness_raster, xy = TRUE) %>%
    rename(fill = layer)  # Rename 'layer' to 'fill' for consistency
  
  # Create a plot filename
  plot_filename <- paste0("11_SpeciesRichness/01_Management_Zones/", sub("_binary\\.asc$", ".pdf", pattern))
  
  # Open a PDF device to save the plot
  pdf(file = plot_filename, width = 18, height = 10)
  
  # Generate the plot
  combined_plot <- ggplot() +
    ## plot states
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "white") + 
    ## plot richness
    geom_raster(data = species_df, aes(x = x, y = y, fill = fill)) +
    scale_fill_viridis_c(option = "viridis", name = "Species Count", na.value = "transparent") +
    
    ## plot federal management zones (separate fill scale)
    new_scale_fill() + 
    geom_sf(data = combined_filtered_layers, aes(fill = "Management Zones (8.07%)"), color = NA, alpha = 0.3) +
    
    # geom_sf(data = subset(combined_filtered_layers, layer_name == "Federal Managing Agency"), 
    #         aes(fill = "Management Zones (8.07%)"), color = NA, alpha = 0.3) + 
    # ## plot state management zones
    # geom_sf(data = subset(combined_filtered_layers, layer_name == "State Managing Agency"), 
    #         aes(fill = "Management Zones (8.07%)"), color = NA, alpha = 0.3) + 
    #  

    ## Define manual colors for management zones
    scale_fill_manual(values = c("Management Zones (8.07%)" = "red"),
                      name = "Management Zones\n(% of Hardwoods)") +
  
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
    labs(title = paste0("Species Richness & Management Zones (", sub("\\.asc$", "", pattern), ")"),
         x = "Longitude", y = "Latitude") +
    theme_minimal() +
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
  
  # Print the plot to the PDF
  print(combined_plot)
  
  # Close the PDF device
  dev.off()
  
  # Print message indicating the plot was saved
  cat("Saved plot and raster for pattern:", pattern, "\n")
}



















# ### Part 2, calculate species changes
# 
# 
# # Load the current species richness raster
# current_richness <- raster("10_SpeciesRichness/current_binary_SpeciesRichness.asc")
# 
# # Load a future species richness raster (e.g., one model, one time period, one SSP)
# future_richness <- raster("10_SpeciesRichness/GISS-E2-1-G_2041-2060_ssp245_binary_SpeciesRichness.asc")
# 
# # Calculate change in species richness (future - current)
# richness_change <- future_richness - current_richness
# 
# # Classify the change values into categories
# richness_change_classified <- calc(richness_change, function(x) {
#   ifelse(is.na(x), NA,  # Keep NA values as NA
#          ifelse(x < 0, -1,  # Species loss
#                 ifelse(x == 0, 0,  # No change
#                        ifelse(x > 0, 1, NA))))  # Species gain
# })
# 
# # Count the number of species lost, no change, and gained
# species_loss_count <- cellStats(richness_change_classified == -1, sum)
# species_no_change_count <- cellStats(richness_change_classified == 0, sum)
# species_gain_count <- cellStats(richness_change_classified == 1, sum)
# 
# # Convert the classified raster to a data frame for plotting
# change_df <- as.data.frame(richness_change_classified, xy = TRUE) %>%
#   rename(change = layer) %>%
#   filter(!is.na(change))  # Remove NA values for plotting
# 
# # Set basemap with states and counties if needed
# states <- map_data("state")
# fl_counties <- map_data("county", region = "florida")
# 
# # Create a plot filename
# plot_filename <- "10_SpeciesRichness/Current_v_GISS-E2-1-G_2041-2060_ssp245_SpeciesRichnessChanage.pdf"
# 
# # Open a PDF device to save the plot
# pdf(file = plot_filename, width = 18, height = 10)
# 
# 
# # Create a plot with classified species richness change
# plot <- ggplot() +
#   geom_polygon(data = states, aes(x = long, y = lat, group = group),
#                fill = "grey90", color = "white") +
#   
#   geom_raster(data = change_df, aes(x = x, y = y, fill = factor(change))) +
#   
#   scale_fill_manual(values = c("-1" = "#FDE725FF", "0" = "#31688EFF", "1" = "#440154FF"),
#                     labels = c(
#                       paste("Species Loss (", species_loss_count, " pixel changes)", sep = ""),
#                       paste("No Change (", species_no_change_count, " pixel changes)", sep = ""),
#                       paste("Species Gain (", species_gain_count, " pixel changes)", sep = "")
#                     ),
#                     name = "Change in Species Richness") +
#   
#   geom_polygon(data = fl_counties, aes(x = long, y = lat, group = group),
#                fill = NA, color = "grey70", size = 0.3) +
#   
#   coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
#   theme_minimal() +
#   labs(title = "Change in Species Richness (Future : GISS-E2-1-G_2041-2060_ssp245 vs Current)",
#        x = "Longitude", y = "Latitude") +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         legend.title = element_text(size = 12),
#         plot.title = element_text(size = 10))
# 
# # Print the plot to the PDF
# print(plot)
# 
# # Close the PDF device
# dev.off()