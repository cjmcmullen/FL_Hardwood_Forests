### Future ETF projections
## script by ME Mabry

# Load Packages
library(tidyverse)
library(raster)
library(gtools)
library(dplyr)
library(dismo)
library(ENMeval)
library(devtools)
#library(kuenm)
library(usdm)

### setwd 
#setwd("/blue/soltis/share/FL_HardwoodForests/") 

args <- commandArgs(trailingOnly = TRUE)
species <- args[1]

# Print the species being processed
print(paste("Processing species:", species))

############################################################################
#################### 1. load maxent species file ###########################
############################################################################
# Load data file just to get species names

alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldf_FL_hardwood_Cleaned_maxent.csv")

# get species df
spec_subset <- dplyr::filter(alldf, accepted_name == species)

############################################################################
########################## 2. set up variables #############################
############################################################################
# Define base directories
input_base_dir <- "02_rasters/Future_ETFLayers"  # Base directory for rasters
model_base_dir <- "04_ENMs"  # Base directory for models
output_base_dir <- "08B_FutureETFProjections"  # Base directory for output projections

# Load VIF layers that are common to all species
vif_path <- "02_rasters/CroppedLayers"

#make species dir
dir.create(paste0(output_base_dir, "/", species))

# Get the list of models available
model_dirs <- list.dirs(input_base_dir, full.names = TRUE, recursive = FALSE)

############################################################################
############################ 3. Run models #################################
############################################################################

# Load the optimal models for the species if they exist
optimal_model_file <- file.path(model_base_dir, paste0(species, "/", species, "_optimalSeq_ENM.RData"))

if (file.exists(optimal_model_file)) {
  load(optimal_model_file)
} else {
  print(paste("Optimal model for species", species, "not found. Skipping to next species."))
  next
}

# Loop through each specific model directory
for (model_dir in model_dirs) {
  
  # Get the model and other parts using basename() and more flexible splitting
  model_name_part <- basename(model_dir)  # Extracts the last part of the directory path
  
  # Split by underscores and manually assign
  model_split <- strsplit(model_name_part, "_")[[1]]
  model <- model_split[1] 
  
  # Extract time period and SSP as before
  time_period <- model_split[2]  # e.g., "2041_2060"
  ssp <- model_split[3]  # e.g., "ssp126" or "ssp585"
  
  
  # Load climate and species data
  Bio <- list.files(model_dir, pattern = "\\.asc$", full.names = TRUE)
  Soil <- list.files("02_rasters/Current_ETFLayers", pattern = "*v2.asc", full.names = TRUE)
  Elev <- list.files("02_rasters/Current_ETFLayers", pattern = "*elev.asc", full.names = TRUE)
  
  climlist <- c(Bio, Soil, Elev)
  climstack <- stack(climlist)
  
  # Load species VIF data and match climate layers to VIF stack
  specstack <- stack(mixedsort(sort(list.files(path = paste0(vif_path, "/", species, "/VIF"), full.names = TRUE))))
  layerNames <- names(specstack)
  Future_subset <- subset(climstack, layerNames)
  
  # Prediction output filename
  projection_filename <- file.path(output_base_dir, paste0(species, "/", species, "_", model, "_", time_period, "_", ssp, "_ENM_ProjectionPoints.asc"))
  
  # Predict using the optimal model
  p <- tryCatch({
    dismo::predict(mod.seq, Future_subset, filename = projection_filename, overwrite = TRUE)
  }, error = function(e) {
    print(paste("Error predicting for species:", species, "model:", model_dir, "time period:", time_period, "SSP:", ssp))
    return(NULL)
  })
  
  # Save the RData file for future reference
  save(p, file = file.path(output_base_dir, paste0(species, "/", species, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.RData")))
  
  # Convert prediction to a plottable dataframe
  p_df <- as.data.frame(rasterToPoints(p))
  colnames(p_df)[3] <- "Habitat_Suitability"
  
  # Set up basemap and plot
  states <- map_data("state")
  world <- map_data("world")
  
  # Plot and save
  plot_filename <- file.path(output_base_dir, paste0(species, "/", species, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.pdf"))
  
  pdf(file = plot_filename, width = 10, height = 7)
  
  plot <- ggplot() +
    #geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
    #geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
    geom_raster(data = p_df, aes(x = x, y = y, fill = Habitat_Suitability)) +
    geom_point(data = spec_subset, mapping =aes(x = long, y = lat), pch = 19, col='red', cex=0.4, ) +
    #coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
    xlab("Longitude") + ylab("Latitude") +
    scale_fill_gradientn(colours = viridis::mako(99, direction = -1),
                         na.value = "white",
                         breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = c("Low", 0.25, 0.5, 0.75, "High"),
                         limits = c(0, 1)) +
    ggtitle(paste0(species, " ", model, " ", time_period, " (", ssp, ") Habitat Suitability")) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))+
    theme(axis.text = element_text(size = 8), 
          axis.title = element_text(size = 8)) + 
    theme(plot.title = element_text(size = 10))
  
  print(plot)
  dev.off()
  
  print(paste("Finished processing:", species, "model:", model_dir, "time period:", time_period, "SSP:", ssp))
  
}  # End model directory loop

