# Projecting models to current FL_Hardwood
## Last modified on 4/3/2024 by MEM

# Load Packages
library(tidyverse)
library(raster)
library(gtools)
library(dplyr)
library(dismo)
library(devtools)
library(kuenm)
library(ggplot2)
library(ggspatial)
library(viridis)
library(ggpubr)


### setwd 
#setwd("/blue/soltis/share/FL_HardwoodForests/") 

args <- commandArgs(trailingOnly = TRUE)
species <- args[1]

# print species name
print(species)

############################################################################
############### 1. load maxent species file #######################
############################################################################
# Load data file just to get species names

alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldf_FL_hardwood_Cleaned_maxent.csv")

# get species df
spec_subset <- dplyr::filter(alldf, accepted_name == species)

############################################################################
##################### 2. load  optimal model  ##############################
############################################################################
# Load Rdata file of the models
load(paste0("04_ENMs/", species, "/", species, "_optimalSeq_ENM.RData"))

############################################################################
############## 3. Project ENMs to Hardwood current layers ##################
############################################################################
# Load FL scrub layers
climlist <- list.files("02_rasters/Current_HardwoodLayers/", pattern = "*.asc", full.names = TRUE)

### Load rasters and stack them
climstack <- raster::stack(climlist) 

# Get names of rasters that were used for species models (using vif)
specstack <- stack(mixedsort(sort(list.files(path=paste0("02_rasters/CroppedLayers/", species, "/VIF/"), full.names = TRUE))))

layerNames <- names(specstack)

# Get  rasters which match the layers used in your model by the names
Hardwood_Rasters <- subset(climstack, layerNames)

# Project model to FL Hardwood Forest rasters
p <- dismo::predict(mod.seq, Hardwood_Rasters, filename = paste0("05_CurrentProjections/", species, "_CurrentHardwood_Projection.asc"))

save(p, file = paste0("05_CurrentProjections/", species, "_CurrentHardwood_Projection.RData"))


############################################################################
############################# 4. plot projection ###########################
############################################################################
# Make p plottable 
p_df <- as.data.frame(rasterToPoints(p))

# Change the name of the third column
colnames(p_df)[3] <- "Habitat_Suitablity"

# Set basemap
states <- map_data("state")
world <- map_data("world")

# ggplot to make sure it is correct
ggplot(data = world) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white")+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_raster(data = p_df, aes(x = x, y = y, fill = Habitat_Suitablity)) +
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) + 
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradientn(colours = viridis::mako(99, direction = -1),
                       na.value = "white", breaks=c(0, 0.25, 0.5, 0.75, 1),labels=c("Low",0.25, 0.5, 0.75, "High"),
                       limits=c(0,1)) +
  ggtitle(paste0(species," Hardwood Forest Habitat Suitability")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8)) + #originally 5
  #theme(plot.title = element_text(face = "italic")) + 
  theme(plot.title = element_text(size = 10))

#dev.off()
ggsave(file = paste0("05_CurrentProjections/", species, "_CurrentHardwood_Projection.pdf"), width = 10, height = 7)

