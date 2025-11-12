library(raster)

setwd("/blue/soltis/share/FL_HardwoodForests/")

alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldf_FL_hardwood_Cleaned_maxent.csv")

for(i in 1:length(unique(alldf$accepted_name))){
  species <- unique(alldf$accepted_name)[i]
  print(species)

  ### AE Melton
  # Run this code to generate a function for making a binary map of predicted occurrences
  speciesfile <- paste0("05_CurrentProjections/", species, "_CurrentHardwood_Projection.asc")
  model <- raster(speciesfile)
  
  # get species df
  spec_subset <- dplyr::filter(alldf, accepted_name == species)


  #make.binary.map <- function(model, occ.dat){
  
  ###Extract suitability scores
  SuitabilityScores <- raster::extract(model, spec_subset[,6:5])
  #print(SuitabilityScores)
  ###Get rid of NAs 
  SuitabilityScores <- SuitabilityScores[complete.cases(SuitabilityScores)]
  
  print(length(SuitabilityScores))
  
  ###Reclassify the raster; set threshold to minimum suitability score at a known occurrence
  threshold_min <- min(SuitabilityScores)
  threshold_mean <- mean(SuitabilityScores)
  threshold_max <- max(SuitabilityScores)
  
  print(threshold_min)
  print(threshold_mean)
  print(threshold_max)
  
  # Plot histogram
  #hist(SuitabilityScores, main = "Histogram habitat suitablity", xlab = "Values", ylab = "Frequency")
  
  #M <- c(0, threshold_mean, 0,  threshold_mean, 1, 1); 
  
  #rclmat <- matrix(M, ncol=3, byrow=TRUE); 
  
  #Dist <- raster::reclassify(model, rcl = rclmat);
#}
}

