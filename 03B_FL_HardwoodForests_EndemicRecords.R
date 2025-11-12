## Keep Only Endemic records
## Modified from and cite: https://github.com/mbelitz/Odo_SDM_Rproj and https://github.com/soltislab/BotanyENMWorkshops
## 09-23-24

library(raster)
library(gtools)
library(ggplot2)
library(plyr)
library(dplyr)
library(rgdal)
library(sp)
library(sf)
library(tmap)
library(maptools)
library(tidyr)
library(geodata)


### setwd 
setwd("/blue/soltis/share/FL_HardwoodForests/01_data/FL_HardwoodForests_Clean/")




############################################################################
########################### 1. Read in CSV files ##########################
############################################################################

speciesfile <- "/blue/soltis/share/FL_HardwoodForests/01_data/FL_HardwoodForests_Clean/Asaccharum_filtered3.csv"

species_df <- read.csv(speciesfile)

## set up variable names
species_name <- strsplit(speciesfile, '[/]')[[1]][8] #splitting up the file path of species name to take out everything until species name 
accepted_name <- strsplit(species_name, '[_]')[[1]][1] #taking out date and .csv from end

#make species dataframe spatial 
point_data <- st_as_sf(species_df, coords = c("long", "lat"), crs = 4326)

###############################################################################
############################### 2. Make basemap ###############################
###############################################################################
# Find your extent for bounding the map
my_extent <- extent(point_data)
my_extent
extent_x_min <- my_extent[1]
extent_x_max <- my_extent[2]
extent_y_min <- my_extent[3]
extent_y_max <- my_extent[4]

# get datasets
states <- gadm(country="USA", level=1, path = tempdir())
world <- world(path = tempdir())
provinces <- gadm(country="Canada", level=1, path = tempdir())

## make spatial and rename columns
states_sf <- st_as_sf(states)
colnames(states_sf)[colnames(states_sf) == "NAME_1"] <- "NAME"
states_sf_subset <- states_sf[c("NAME", "geometry")]

world_sf <- st_as_sf(world)
colnames(world_sf)[colnames(world_sf) == "NAME_0"] <- "NAME"
world_sf_subset <- world_sf[c("NAME", "geometry")]

# Remove the USA from the world_sf so it doesn't plot on top of the states or Providences
world_sf_subset <- world_sf_subset %>% filter(!NAME %in% c("United States", "Canada"))

provinces_sf <- st_as_sf(provinces)
colnames(provinces_sf)[colnames(provinces_sf) == "NAME_1"] <- "NAME"
provinces_sf_subset <- provinces_sf[c("NAME", "geometry")]

## combine them together
areas_sf <- rbind(states_sf_subset, world_sf_subset, provinces_sf_subset)

###############################################################################
############################ 3. set up Endemic areas ##########################
###############################################################################
# Set up list names
stateList <- as.data.frame(unique(states$NAME_1))
countryList <- as.data.frame(unique(world$NAME_0))
canadaList <- as.data.frame(unique(provinces$NAME_1))
areaList <- unlist(c(stateList, countryList, canadaList))

# Specify the endemic areas
#Asaccharum 
endemic <- c("Alabama", "Arkansas", "Florida", "Georgia", "Illinois", "Kentucky", "Louisiana", "Mexico", "Mississippi", "Missouri", "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia")
#Aspinosa
endemic <- c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri", "New Jersey", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia") 
#Aplatyneuron
endemic <- c("Alabama", "Arizona", "Arkansas", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Lesotho", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "South Africa", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin")
#Acanadensis
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Maine", "Manitoba", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "Saskatchewan", "South Carolina", "South Dakota", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin")
#Cfloridus
endemic <- c("Alabama", "Florida", "Georgia", "Kentucky", "Maryland", "Mississippi", "North Carolina", "Ohio", "Pennsylvania", "South Carolina", "Tennessee", "Virginia", "West Virginia")
#Cdasycarpa 
endemic <- c("Alabama", "Florida", "Georgia", "Mississippi", "South Carolina")
#Ccaroliniana
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin")
#Cglabra
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maryland", "Massachusetts", "Michigan", "Mississippi", "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Ontario", "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin")
#Claevigata
endemic <- c("Alabama", "Arkansas", "Florida", "Georgia", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Maryland", "Mexico", "Mississippi", "Missouri", "New Mexico", "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia")
#Coccidentalis 
endemic <- c("Alabama", "Arkansas", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Maine", "Manitoba", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin", "Wyoming")
#Ccanadensis
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maryland", "Massachusetts", "Mexico", "Michigan", "Mississippi", "Missouri", "Nebraska", "New Jersey", "New Mexico", "New York", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia", "Wisconsin")
#Claxum
endemic <- c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri", "New York", "North Carolina", "Oklahoma", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia")
#Cflorida
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Mississippi", "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia")
#Erepens
endemic <- c("Alabama", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kentucky", "Labrador", "Maine", "Manitoba", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "New Brunswick", "New Hampshire", "New Jersey", "New York", "Newfoundland", "North Carolina", "Nova Scotia", "Ohio", "Ontario", "Pennsylvania", "Prince Edward Island", "Rhode Island", "South Carolina", "Tennessee", "Vermont", "Virginia", "West Virginia", "Wisconsin")
#Eumbilicatum
endemic <- c("Alabama", "Florida", "Georgia", "Kentucky", "North Carolina", "South Carolina", "Tennessee", "Virginia", "West Virginia")
#Eamericanus
endemic <- c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia")
#Fgrandifolia 
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Mississippi", "Missouri", "New Brunswick", "New Hampshire", "New Jersey", "New York", "North Carolina", "Nova Scotia", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Prince Edward Island", "Québec", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin")
#Famericana
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin")
#Hdiptera
endemic <- c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "South Carolina", "Texas")
#Harifolia
endemic <- c("Alabama", "Florida", "Georgia", "Kentucky", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Tennessee", "Virginia")
#Iopaca
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kentucky", "Louisiana", "Maryland", "Massachusetts", "Mississippi", "Missouri", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia")
#Lstyraciflua
endemic <- c("Alabama", "Arkansas", "Belize", "Connecticut", "El Salvador", "Florida", "Georgia", "Guatemala", "Honduras", "Illinois", "Indiana", "Kentucky", "Louisiana", "Maryland", "Mexico", "Mississippi", "Missouri", "New Jersey", "New York", "Nicaragua", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia")
#Mgrandiflora
endemic <- c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Texas")
#Mfloridana
endemic <- c("Florida", "Georgia")
#Malabamensis
endemic <- c("Alabama", "Florida", "Georgia")
#Mflavidula
endemic <- c("Alabama", "Florida", "Georgia", "Mississippi", "North Carolina", "South Carolina", "Tennessee")
#Mrepens
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "Florida", "Georgia", "Guatemala", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "New Brunswick", "New Hampshire", "New Jersey", "New York", "Newfoundland and Labrador", "North Carolina", "Nova Scotia", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin")
#Mreynoldsiae
endemic <- c("Alabama", "Delaware", "Florida", "Georgia", "Kentucky", "Maryland", "North Carolina", "South Carolina", "Tennessee", "Virginia")
#Ohirtellus
endemic <- c("Alabama", "Angola", "Antigua and Barbuda", "Argentina", "Arkansas", "Australia", "Bahamas", "Belize", "Benin", "Bolivia", "Bonaire, Saint Eustatius and Saba", "Botswana", "Brazil", "Burkina Faso", "Burundi", "Cameroon", "Cabo Verde", "Cayman Islands", "Central African Republic", "China", "Colombia", "Comoros", "Congo", "Cook Islands", "Costa Rica", "Côte d'Ivoire", "Cuba", "Democratic Republic of the Congo", "Dominica", "Dominican Republic", "Ecuador", "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Fiji", "Florida", "French Guiana", "French Polynesia", "Gabon", "Gambia", "Georgia", "Ghana", "Guadeloupe", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Indonesia", "Jamaica", "Japan", "Kenya", "Liberia", "Louisiana", "Madagascar", "Malawi", "Mali", "Martinique", "Maryland", "Mauritius", "Mexico", "Micronesia", "Mississippi", "Missouri", "Montserrat", "New Caledonia", "New Zealand", "Nicaragua", "Nigeria", "Niue", "North Carolina", "North Korea", "Northern Mariana Islands", "Oklahoma", "Panama", "Paraguay", "Papua New Guinea", "Peru", "Philippines", "Puerto Rico", "Rwanda", "Réunion", "Saint Kitts and Nevis", "Saint Lucia", "Saint-Martin", "Saint Vincent and the Grenadines", "Samoa", "São Tomé and Príncipe", "Senegal", "Sierra Leone", "Solomon Islands", "South Africa", "South Carolina", "South Korea", "South Sudan", "Sudan", "Suriname", "Taiwan", "Tanzania", "Texas", "Thailand", "Togo", "Tonga", "Trinidad and Tobago", "Uganda", "Uruguay", "Vanuatu", "Venezuela", "Vietnam", "Virgin Islands, U.S.", "Virginia", "Yemen", "Zambia", "Zimbabwe")
#Ovirginiana
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "El Salvador", "Florida", "Georgia", "Guatemala", "Honduras", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Manitoba", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Brunswick", "New Hampshire", "New Jersey", "New York", "North Carolina", "North Dakota", "Nova Scotia", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Prince Edward Island", "Québec", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin", "Wyoming")
#Pquinquefolia
endemic <- c("Alabama", "Arkansas", "Bahamas", "Colorado", "Connecticut", "Cuba", "Delaware", "District of Columbia", "El Salvador", "Florida", "Georgia", "Guatemala", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "New Brunswick", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "Saskatchewan", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "West Virginia", "Wisconsin")
#Pborbonia
endemic <- c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Texas")
#Pglabra
endemic <- c("Alabama", "Florida", "Georgia", "Louisiana", "Mississippi", "South Carolina")
#Ptaeda
endemic <- c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi", "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia")
#Pcaroliniana
endemic <- c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Texas")
#Pserotina
endemic <- c("Alabama", "Arizona", "Arkansas", "British Columbia", "Colorado", "Connecticut", "Delaware", "District of Columbia", "El Salvador", "Florida", "Georgia", "Guatemala", "Honduras", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Brunswick", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "Nova Scotia", "Ohio", "Oklahoma", "Ontario", "Panama", "Pennsylvania", "Québec", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin")
#Qalba
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin")
#Qhemisphaerica
endemic <- c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Texas", "Virginia")
#Qmichauxii
endemic <- c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri", "New Jersey", "North Carolina", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia")
#Qvirginiana
endemic <- c("Alabama", "Florida", "Georgia", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Texas", "Virginia")
#Slanuginosum
endemic <- c("Alabama", "Arizona", "Arkansas", "Florida", "Georgia", "Illinois", "Kansas", "Kentucky", "Louisiana", "Mexico", "Mississippi", "Missouri", "New Mexico", "Oklahoma", "South Carolina", "Texas")
#Spumila
endemic <- c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "South Carolina", "Texas")
#Stinctoria
endemic <- c("Alabama", "Arkansas", "Delaware", "Florida", "Georgia", "Louisiana", "Maryland", "Mississippi", "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia")
#Tamericana
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Manitoba", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Brunswick", "New Hampshire", "New Jersey", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "Saskatchewan", "South Carolina", "South Dakota", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin")
#Ualata
endemic <- c("Alabama", "Arkansas", "Florida", "Georgia", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Mississippi", "Missouri", "North Carolina", "Ohio", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia")
#Vsororia
endemic <-c("Alabama", "Arkansas", "British Columbia", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Manitoba", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "New Brunswick", "New Hampshire", "New Jersey", "New Mexico", "New York", "Newfoundland and Labrador", "North Carolina", "North Dakota", "Nova Scotia", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Prince Edward Island", "Québec", "Rhode Island", "Saskatchewan", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "West Virginia", "Wisconsin")


## Check to see if all you areas can be found in the dataset
endemic[!(endemic %in% areaList)]

## add column to areas_sf with endemic or not 
endemic_df <- areas_sf %>%
  mutate(is_endemic = ifelse(NAME %in% endemic, "Endemic", "Non-Endemic"))

## remove country of Georgia if included and should not be
# endemic_df$is_endemic[125] <- "Non-Endemic" 

## plot to make sure the area looks correct
ggplot() + 
  geom_sf(data = endemic_df, aes(fill = is_endemic), color = "black") +  # Use fill aesthetic for areas
  
  # Fill in endemic areas with grey
  scale_fill_manual(values = c("Endemic" = "grey", "Non-Endemic" = "white")) +  # Fill colors
  
  # Add points
  geom_sf(data = point_data, mapping = aes(color = basisOfRecord), size = 0.5) +
  
  #set coords
  coord_sf(xlim = c(extent_x_min, extent_x_max), ylim = c(extent_y_min, extent_y_max)) + 
  
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


###############################################################################
######################### 4. Remove introduced records ########################
###############################################################################
# Filter endemic areas from endemic_df
endemic_areas <- endemic_df %>% filter(is_endemic == "Endemic")

# Combine all endemic areas into a single MULTIPOLYGON
# used st_make_valid for Ohirtellus because Fiji was not reading in correctly
endemic_union <- st_union(endemic_areas)

# Use st_intersects() to find points within the union of endemic areas
points_in_endemic <- point_data[st_intersects(point_data, endemic_union, sparse = FALSE), ]

# Now you can plot only these points on the map
ggplot() +
  # Plot the endemic and non-endemic areas
  geom_sf(data = endemic_df, 
          aes(fill = is_endemic),  
          color = "black") +       
  scale_fill_manual(values = c("Endemic" = "grey", "Non-Endemic" = "white")) +  
  
  # Add points from points_in_endemic (only in endemic areas)
  geom_sf(data = points_in_endemic, 
          color = "red",         # Set the point color (e.g., red)
          size = 1.5) +  # Set the point size
  #set coords
  coord_sf(xlim = c(extent_x_min, extent_x_max), ylim = c(extent_y_min, extent_y_max)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))




# Get the row indices of the points in endemic areas (from point_data)
matching_indices <- st_drop_geometry(points_in_endemic) %>% row.names() %>% as.integer()

# Use these indices to subset species_df
species_in_endemic <- species_df[matching_indices, ]

# Check the new dataframe
head(species_in_endemic)


### write out endemic file,since maps are done per species, you should append in endemic data per species
write.csv(species_in_endemic,file = paste0(accepted_name, "_Endemic.csv"),row.names = FALSE)


## you will make final maps after filtering out basis of records types and spatial thinning
