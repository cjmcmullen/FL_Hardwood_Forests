### Resolving taxonomic names
## 09/27/21
##note, make sure you have made the proper file and folder structure as needed below.

### Load Packages
library(dplyr)
library(tidyr)

##set working directory
dir <- "/blue/soltis/share/FL_HardwoodForests/"
setwd(dir)


############################################################################
########################### 1. Read in CSV files ##########################
############################################################################
FL_HardwoodForests_Dir <- list.files(path="01_data", pattern = "*.csv", full.names = TRUE)
print(FL_HardwoodForests_Dir)

# Create logfile folder for files to print to
#dir.create(paste0(dir, "/", crop, "/logfiles"))

############################################################################
########################### 2. Make species lists ##########################
############################################################################

i <- "Tamericana_Nov_6_24.csv"

# make csv file of all unique names. Use this files to match to known synonyms and remove those which should not be included
for(i in FL_HardwoodForests_Dir){
  print(i)
  species_df <- read.csv(i)
  speciesUnique <- data.frame(unique(species_df$scientificName))
  names(speciesUnique) <- NULL
  names(speciesUnique)[i] <- "scientificName"
  species_name <- strsplit(i, '[/]')[[1]][2]
  species_name2 <- strsplit(species_name, '[_]')[[1]][1]
  write.csv(speciesUnique, file = paste0(dir, "01_data/Unique_Names/", species_name2, "_uniqueNames.csv"),row.names = FALSE)
}
