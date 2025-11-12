## Record Cleaning and spatial thinning
## Modified from Botany ENM workshop Cite: https://github.com/soltislab/BotanyENMWorkshops

### Load Packages
library(dplyr)
library(tidyr)
library(raster)
library(sp)
library(spatstat)
library(spThin)
library(fields)
library(lubridate)
#library(CoordinateCleaner) old version 
library(gatoRs)

#installing gethub version of CoordinateCleaner
#devtools::install_github("ropensci/CoordinateCleaner")
library(CoordinateCleaner)

##set working directory
setwd("/blue/soltis/share/FL_HardwoodForests/01_data/")

#clean environment for each new species 

############
#Reducing the data frame 
speciesfile <- "/blue/soltis/share/FL_HardwoodForests/01_data/Asaccharum_Sept_4_24.csv" #reading in each new species file ,can tab for each new species
species_df <- read.csv(speciesfile) #reading in the csv file

species_name <- strsplit(speciesfile, '[/]')[[1]][7] #splitting up the file path of species name to take out everything until species name 
accepted_name <- strsplit(species_name, '[_]')[[1]][1] #taking out date and .csv from end
species2_df <- species_df %>% #using pipe to create species2 data frame to reduce columns into only the ten important columns
  dplyr::select(ID = ID, 
                scientificName = scientificName, 
                basisOfRecord = basisOfRecord, 
                occurrenceID = occurrenceID,
                institutionCode = institutionCode,
                latitude = latitude, 
                longitude = longitude, 
                month = month, 
                day = day, 
                year = year)
write.table(species2_df, file = paste0("FL_HardwoodForests_Clean/", accepted_name, "_observations.csv"),col.names = FALSE, append = FALSE, sep = ",")
#writing the file name for the observations csv for the species

##########################
##########################
 
### filtering raw data based on accepted synonyms
unique_df <- read.csv(file = paste0("Unique_Names/", accepted_name, "_uniqueNames.csv"), header = FALSE) # data frame of the unique names 
species_search <- as.list(unique_df[,1]) #list of unique names in the species 
species3_df <- gatoRs::taxa_clean(species2_df, synonyms.list = species_search, taxa.filter = "exact", accepted.name = accepted_name) #adding column for accepted name in data frame 
write.csv(species3_df, file = paste0("FL_HardwoodForests_Clean/",accepted_name, "_filtered1.csv" ),row.names = FALSE) #writing csv file for species 3



#Modify column names
species3_df <- species3_df %>% #creating pipe to shorted species3 data frame to only 8 columns
  dplyr::select(ID = ID, 
                accepted_name = accepted_name, 
                basisOfRecord = basisOfRecord, 
                lat = latitude, 
                long = longitude, 
                year = year, 
                month = month, 
                day = day)



## filter NA's, Precision, Remove 00s,Remove Cultivated and Outlier coordinates.
# https://www.rdocumentation.org/packages/CoordinateCleaner/versions/2.0-20
species4_df <- species3_df %>% #filtering out NAs
  filter(!is.na(long)) %>%
  filter(!is.na(lat))

species4_df$lat <- round(species4_df$lat, digits = 2) #rounding lat and long to only 2 digits
species4_df$long <- round(species4_df$long, digits = 2)

species5_df <- species4_df %>% #filtering out lat and longs that are 0 
    filter(long != 0.00) %>%
    filter(lat != 0.00)

species5_df <- filter(species5_df, lat < 90 & lat > -90 & long < 180 & long > -180)

species5_df <- cc_inst(species5_df, 
                       lon = "long", 
                       lat = "lat", 
                       species = "accepted_name", 
                       #value = "clean", 
                       #buffer = 1,
                       geod = FALSE) ## testing observations against institutions 

species5_df <- cc_outl(species5_df, 
                        lon = "long", 
                        lat = "lat", 
                        species = "accepted_name") ## removing outliers 



#writing csv file for filtered 2
write.csv(species5_df, file = paste0("FL_HardwoodForests_Clean/",accepted_name, "_filtered2.csv" ),row.names = FALSE)



### Removing Duplicates, Fix dates, separate into Year/Month/Day,Remove Identical Rows
species6_df <- distinct(species5_df, lat, long, year, month, day, .keep_all = TRUE) 
write.csv(species6_df, file = paste0("FL_HardwoodForests_Clean/",accepted_name, "_filtered3.csv" ),row.names = FALSE) #writing csv file for filtered 3



