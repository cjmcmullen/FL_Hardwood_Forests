## Record Cleaning and spatial thinning
## Modified from Botany ENM workshop Cite: https://github.com/soltislab/BotanyENMWorkshops

### Load Packages
#library(dplyr)
#library(tidyr)
library(sp)
library(raster)
#library(spatstat)
#library(spThin)
library(fields)
#library(lubridate)
#library(CoordinateCleaner) old version 
#library(gatoRs)

#installing gethub version of CoordinateCleaner
#devtools::install_github("ropensci/CoordinateCleaner")
#library(CoordinateCleaner)

##set working directory
setwd("/blue/soltis/share/FL_HardwoodForests/01_data/FL_HardwoodForests_Clean/")

# speciesfile <- "Tamericana_Endemic.csv"
# species6_df <- read.csv(speciesfile)
# accepted_name <- strsplit(speciesfile, '[_]')[[1]][1] #splitting up the file path of species name to take out everything until species name
# 
# 
# ### remove basis of record ###
# ##check basis column
# unique_values <- unique(species6_df$basisOfRecord)
# 
# # Display the unique values and ask for user input to decide which ones to keep
# selected_values <- select.list(unique_values, multiple = TRUE, title = "Select unique basisOfRecord to keep:")
# 
# # Filter the data frame based on the selected values
# species7_df <- species6_df[species6_df$basisOfRecord %in% selected_values, ]
# 
# 
# 
# # Write out file after filtering by BoR for large species (Fgrandifolia and Pquinquefolia)
# write.csv(species7_df, file = paste0(accepted_name, "_BoRFiltered.csv" ),row.names = FALSE)

# ONLY FOR SUBMITTED SCRIPTS
# Comment out lines 23-41
speciesfile <- "Tamericana_BoRFiltered.csv"
species7_df <- read.csv(speciesfile)
accepted_name <- strsplit(speciesfile, '[_]')[[1]][1]


### spatial thinning based on nearest neighbor

nnDm_species <- rdist.earth(as.matrix(data.frame(lon = species7_df$long, lat = species7_df$lat)), miles = FALSE, R = NULL) #using matrix with curve of earth to remove nearest neighbor
diag(nnDm_species) <- NA #making the diagonal of the matrix into NAs
thinpar <- min(nnDm_species[nnDm_species >0], na.rm = TRUE) #thinning to take out nearest neighbor
print(thinpar) #smallest distance between two individuals 
thin_data <- spThin::thin(loc.data =  species7_df,
                               verbose = FALSE,
                               long.col = "long",
                               lat.col = "lat",
                               spec.col = "accepted_name",
                               thin.par = thinpar,
                               reps = 100,
                               locs.thinned.list.return = TRUE,
                               write.files = FALSE)
  
thin_data <- thin_data[[100]] #list of one of the 100 with lat and long
species8_df <- species7_df[species7_df$lat %in% thin_data$Latitude & species7_df$long %in% thin_data$Longitude, ]
#write.csv(species8_df, file = paste0("FL_HardwoodForests_Clean/",accepted_name, "_filtered4.csv" ),row.names = FALSE)


### function for Spatial Correction, we will retain only one pt per pixel. 

bio1 <- raster("/blue/soltis/share/CWR_Proj/02_rasters/BioClim/wc2.1_30s_bio_1.tif") ## Read in raster file, using bio1 raster file
rasterResolution <- max(res(bio1)) # Set resolution

  
#Remove a point which nearest neighbor distance is smaller than the resolution size
while(min(spatstat.geom::nndist(species8_df[,4:5])) < rasterResolution){
  nnD_species <- spatstat.geom::nndist(species8_df[,4:5])
  species8_df <- species8_df[-(which(min(nnD_species) == nnD_species) [1]), ] ##weird never ending thing unless you overwrite species8
}
#while loop to run until it has one point per pixel and when the distance is less than the resolution

write.csv(species8_df, file = paste0(accepted_name, "_cleaned.csv" ),row.names = FALSE)
#creating cleaned csv file
