## FL Scrub Species Environmental Variables
## Modified from ML Gaynor's scripts, modified from Jonathan Barz's scripts
## 10-27-2023 by Charisse Sproha 

library(raster)
library(gtools)
library(dplyr)
library(sp)
library(rangeBuilder) 
library(sf)
library(caret)
library(usdm)  
library(dismo)
library(stringr)
library(rJava)
library(gatoRs)
library(ggpubr)
library(rgdal)

### setwd 
#setwd("/blue/soltis/share/FL_HardwoodForests/") 

args <- commandArgs(trailingOnly = TRUE)
species <- args[1]

## Make new directory for keeping rasters/environmental data
#dir.create("02_rasters/CroppedLayers/")
dir <- "02_rasters/CroppedLayers/"


############################################################################
####### 1. Make your species specific environmental variables ##############
############################################################################
# Load bioclim layers
biolist <- list.files("/blue/soltis/share/FL_Scrub/share/06_rasters/BioClim/", pattern = "*.tif", full.names = TRUE)
soillist <- list.files("/blue/soltis/share/FL_Scrub/share/06_rasters/SoilGrid/", pattern = "*_v2.tif", full.names = TRUE)


# add these together
climlist <- c(biolist, soillist)


### Load rasters and stack them
climstack <- raster::stack(climlist)

# Make maxent file
# Read in all cleaned files
alldf <- list.files(("01_data/FL_HardwoodForests_Clean/"), full.names = TRUE,
                   recursive = FALSE, include.dirs = FALSE, pattern = "cleaned_FINAL.csv")
alldf <- lapply(alldf, read.csv)
alldf <- do.call(rbind, alldf)

#write.csv(alldf, "01_data/FL_HardwoodForests_Clean/alldf_FL_hardwood_Cleaned_maxent.csv")

#alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldf_FL_hardwood_Cleaned_maxent.csv")

#for(i in 1:length(unique(alldf$accepted_name))){
  #species <- unique(alldf$accepted_name)[1]
  print(species)

  #subset species from dataframe
  spp_df <- alldf %>%
    dplyr::filter(accepted_name == species)

  ## Make into a spatial point data frame
  sppdfsp <- st_as_sf(spp_df, coords = c("long", "lat"), crs = 4326)

  #spatial dataframe
  #coordinates(spp_df) <- ~ long+lat
  #proj4string(spp_df) <- CRS("+proj=longlat +datum=WGS84")

  ## Create alpha hull
  sphull <- rangeBuilder::getDynamicAlphaHull(x = spp_df,
                                              coordHeaders = c("long", "lat"),
                                              fraction = 1, # min. fraction of records we want included
                                              partCount = 1, # number of polygons allowed
                                              initialAlpha = 20, # initial alpha size, 20m
                                              clipToCoast = "terrestrial",
                                              verbose = TRUE)


  ### Visualize
  plot(sphull[[1]], col=transparentColor('gray50', 0.5), border = NA)
  points(x = spp_df$long, y = spp_df$lat, cex = 0.5, pch = 3)

  ### Transform into CRS related to meters
  #sphullTrans <- spTransform(sphull[[1]], "+proj=cea +lat_ts=0 +lon_0=0")
  sphullTrans <- st_transform(sphull[[1]], crs="+proj=cea +lat_ts=0 +lon_0")
  spp_dfTrans <- st_transform(sppdfsp, crs="+proj=cea +lat_ts=0 +lon_0")

  ### Calculate buffer size
  #### Here we take the 80th quantile of the max distance between points
  spbuffDist <- quantile(x = (apply(sf::st_distance(spp_dfTrans), 2, FUN = function(x) sort(x)[2])),
                         probs = 0.80, na.rm = TRUE) ## 7397 m

  ### Buffer the hull
  spbuffer_m <- sf::st_buffer(sphullTrans, spbuffDist, dissolve = TRUE)
  spbuffer <- sf::st_transform(spbuffer_m, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  spbuffer <- st_transform(spbuffer, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  ### Visualize
  plot(spbuffer, col=transparentColor('gray50', 0.5), border = NA)
  points(x = spp_df$long, y = spp_df$lat, cex = 0.5, pch = 3)

  spbuffer_df <- st_sf(var=1, spbuffer)
  ggplot(spbuffer_df)+
    geom_sf()

  #Crop and Mask
  #dir.create(paste0(dir, "/", species))
  path <- paste0(dir, species, "/")
  end <- ".asc"

  for(j in 1:length(names(climstack))){

    # Subset raster layer
    rast <- climstack[[j]]

    # Setup file names
    name <- names(rast)
    out <- paste0(path, name)
    outfile <- paste0(out, end)


    # Crop and mask
    c <- crop(rast, sf::as_Spatial(spbuffer))
    c <- mask(c, sf::as_Spatial(spbuffer))

    # Write raster
    writeRaster(c, outfile, format = "ascii", overwrite = TRUE)


  }
#}



############################################################################
####################### 5. Variable selection ##############################
############################################################################
# Select layers for MaxEnt
## We only want to include layers that are not highly correlated.
## To assess which layers we will include, we will use Variable inflation factors (VIFs) 
## VIF can detect for multicollinearity in a set of multiple regression variables. 
### tutorial https://rstudio-pubs-static.s3.amazonaws.com/300995_e6e0edf09915431480ed089c3a3e0ff3.html

### tutorial which was followed https://rstudio-pubs-static.s3.amazonaws.com/300995_e6e0edf09915431480ed089c3a3e0ff3.html

#for(i in  1:length(unique(alldf$accepted_name))){ #can run this with all species as normal for loop
  #species <- unique(alldf$accepted_name)[i]
  print(species)

  ##create dir
  #dir.create(paste0(dir, species, "/VIF"))
  
  # ### Stack layers for each species
  clippedlist <- list.files(paste0(dir, species), pattern = "\\.asc$", full.names = TRUE)
  clippedstack <- raster::stack(clippedlist)
  #clippedstack2 <- terra::rast(clippedstack) #only use if you have an error
  print("rasters stacked")

  #calculate VIFs using a threshold of 10
  stepstack <- usdm::vifstep(clippedstack, th=10)
  print("VIF calculated")

  # exclude the collinear variables that were identified in the previous step
  v2 <- exclude(clippedstack,stepstack)

  ## finally copy the layers we want to a new folder!
  print("Starting to copy the layers")

  ## transfer files into directory per species per method
  for(i in 1:length(names(v2))){
    name <- names(v2)[i]
    print(name)

    from <- paste0(dir, species, "/", name, ".asc")
    to <- paste0(dir, species, "/VIF/", name, ".asc")

    file.copy(from, to,
              overwrite = TRUE, recursive = FALSE,
              copy.mode = TRUE)
  }
#}

