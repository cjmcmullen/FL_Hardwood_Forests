### Projection Comparions
## script by ME Mabry

# Load necessary libraries
library(tidyverse)
library(terra)
library(biomod2)
library(raster)
library(dplyr)
library(ggplot2)
library(maps)
library(viridis)

### setwd 
#setwd("/blue/soltis/share/FL_HardwoodForests/") 

args <- commandArgs(trailingOnly = TRUE)
species <- args[1]

alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldf_FL_hardwood_Cleaned_maxent.csv")
spec_subset <- dplyr::filter(alldf, accepted_name == species)

# Print the species being processed
print(paste("Processing species:", species))

# Define scenarios and time periods
time_periods <- c("2041-2060", "2081-2100")
ssps <- c("ssp245","ssp370", "ssp585")
models <- c("ACCESS-CM2", "GISS-E2-1-G")

dir.create(paste0("07C_ProjectionCompairsons/", species))

# Define function to perform comparison and plot for a given species, scenario, and time period
compare_scenarios <- function(species, ssp, year, model) {
  
  print(paste0("processing:", species, " ", model, " ", year, " ", ssp))
  
  # Read in current and future raster files
  current <- raster(paste0("05_CurrentProjections/", species, "_CurrentHardwood_Projection.asc"))
  future <- raster(paste0("06_FutureProjections/", species, "/", species, "_", model, "_", year, "_", ssp, "_ENM_Projection.asc"))
  
  SuitabilityScores <- raster::extract(current, spec_subset[,6:5])
  SuitabilityScores <- SuitabilityScores[complete.cases(SuitabilityScores)]

  # Calculate the min suitability score for current and future suitability
  threshold_min_current <- min(SuitabilityScores)

  # Create binary rasters using the min suitability threshold
  current_binary <- calc(current, fun = function(x) ifelse(x >= threshold_min_current, 1, 0))
  future_binary <- calc(future, fun = function(x) ifelse(x >= threshold_min_current, 1, 0))

  # # Create a binary raster using a cutoff value of 0.34 (average min suitablity for all species)
  # # Used for species with no points in hardwood forests
  # current_binary <- calc(current, fun = function(x) ifelse(x >= 0.34, 1, 0))
  # future_binary <- calc(future, fun = function(x) ifelse(x >= 0.34, 1, 0))

  #Define output file names for binary rasters
  current_output_file <- paste0("07C_ProjectionCompairsons/", species, "/", species, "_current_binary.asc")
  future_output_file <- paste0("07C_ProjectionCompairsons/", species, "/", species, "_", model, "_", year, "_", ssp, "_binary.asc")

  # Save the current binary raster as an ASCII file
  writeRaster(current_binary, filename = current_output_file, format = "ascii", overwrite = TRUE)

  # Save the future binary raster as an ASCII file
  writeRaster(future_binary, filename = future_output_file, format = "ascii", overwrite = TRUE)

  # Calculate range size differences
  RangeSizeDiff <- BIOMOD_RangeSize(current_binary, future_binary)
  results <- RangeSizeDiff$Compt.By.Models
  
  # Save results to CSV
  write.csv(results, file = paste0("07C_ProjectionCompairsons/", species, "/", species, "_current_v_", model, "_", year, "_", ssp, "_Comparison.csv"))
  
  # Create a dataframe for plotting
  df <- as.data.frame(RangeSizeDiff$Diff.By.Pixel, xy = TRUE)
  fill <- factor(df[, 3])
  
  # Set basemap
  states <- map_data("state")
  
  # Extract counts dynamically from the results variable
  loss_count <- results[1]
  stable0_count <- results[2]
  stable1_count <- results[3]
  gain_count <- results[4]
  
  # Generate plot
  plot <- ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
    geom_raster(data = df, aes(x = x, y = y, fill = fill)) +
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) + 
    theme_bw() +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(paste0(species, " Current versus ", model, " ", year, "(", ssp, ") Habitat Suitability")) +
    guides(colour = guide_legend(override.aes = list(size=2))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    scale_fill_viridis_d(na.value = "white", 
                         name="Pixel Change",
                         breaks=c(-2, -1, 0, 1),
                         labels=c(
                           paste0("lost (", loss_count, " pixels)"),
                           paste0("unchanged (", stable1_count, " pixels)"),
                           paste0("not occupied (", stable0_count, " pixels)"),
                           paste0("occupied in the future (", gain_count, " pixels)")
                         )) +
    theme(legend.title = element_blank(), 
          plot.title = element_text(size = 10))
  
  # Save plot as PNG
  ggsave(paste0("07C_ProjectionCompairsons/", species, "/", species, "_current_v_", model, "_", year, "_", ssp, "_Comparison.png"))
}


# Loop through all combinations of unique species, models, scenarios, and years
for (model in models) {
  for (ssp in ssps) {
    for (time_period in time_periods) {
      #print(time_period)
      # Run the comparison function for each combination
      compare_scenarios(species, ssp, time_period, model)
    }
  }
}

