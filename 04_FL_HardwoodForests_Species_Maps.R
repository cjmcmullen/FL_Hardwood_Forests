## FL Scrub Species Environmental Variables
## Modified from ML Gaynor's scripts, modified from Jonathan Barz's scripts
## 10-27-2023 by Charisse Sproha 

library(geodata)
library(sf)
library(dplyr)
library(ggplot2)
library(raster)

# setwd 
setwd("/blue/soltis/share/FL_HardwoodForests/01_data/FL_HardwoodForests_Clean/")

#read cleaned data
speciesfile <- "Aplatyneuron_cleaned.csv"
species_df <- read.csv(speciesfile)

## set up variable names
accepted_name <- strsplit(speciesfile, '[_]')[[1]][1] #taking out date and .csv from end

# change basis of records to match
unique(species_df$basisOfRecord)

species_df$basisOfRecord[species_df$basisOfRecord == "Human Observation"]  <- "HUMAN_OBSERVATION"
species_df$basisOfRecord[species_df$basisOfRecord == "HumanObservation"]   <- "HUMAN_OBSERVATION"
species_df$basisOfRecord[species_df$basisOfRecord == "OBSERVATION"]        <- "HUMAN_OBSERVATION"

species_df$basisOfRecord[species_df$basisOfRecord == "FossilSpecimen"]     <- "FOSSIL_SPECIMEN"

species_df$basisOfRecord[species_df$basisOfRecord == "MATERIAL_CITATION"]  <- "MATERIAL_SAMPLE"

species_df$basisOfRecord[species_df$basisOfRecord == "Occurence"]          <- "OCCURENCE"

species_df$basisOfRecord[species_df$basisOfRecord == "Preserved specimen"] <- "PRESERVED_SPECIMEN"
species_df$basisOfRecord[species_df$basisOfRecord == "Preserved Specimen"] <- "PRESERVED_SPECIMEN"
species_df$basisOfRecord[species_df$basisOfRecord == "preservedspecimen"]  <- "PRESERVED_SPECIMEN"
species_df$basisOfRecord[species_df$basisOfRecord == "Preserved"]          <- "PRESERVED_SPECIMEN"
species_df$basisOfRecord[species_df$basisOfRecord == "PreservedSpecimen"]  <- "PRESERVED_SPECIMEN"

#check that they all match
unique(species_df$basisOfRecord)

#make species dataframe spatial 
point_data <- st_as_sf(species_df, coords = c("long", "lat"), crs = 4326)

# Find your extent for bounding the map
my_extent <- extent(point_data)
my_extent
extent_x_min <- my_extent[1]
extent_x_max <- my_extent[2]
extent_y_min <- my_extent[3]
extent_y_max <- my_extent[4]


###############################################################################
############################# Set up Endemic areas ############################
###############################################################################

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

# Set up list names
stateList <- as.data.frame(unique(states$NAME_1))
countryList <- as.data.frame(unique(world$NAME_0))
canadaList <- as.data.frame(unique(provinces$NAME_1))
areaList <- unlist(c(stateList, countryList, canadaList))

# Specify the endemic areas
# Asaccharum 
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
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maryland", "Massachusetts", "Michigan", "Mississippi", "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin")
#Claevigata
endemic <- c("Alabama", "Arkansas", "Florida", "Georgia", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Maryland", "Mexico", "Mississippi", "Missouri", "New Mexico", "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia")
#Coccidentalis 
endemic <- c("Alabama", "Arkansas", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Maine", "Manitoba", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin", "Wyoming")
#Ccanadensis
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maryland", "Massachusetts", "Mexico", "Michigan", "Mississippi", "Missouri", "Nebraska", "New Jersey", "New Mexico", "New York", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia", "Wisconsin")
#Claxum
endemic <- c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri", "New Jersey", "New York", "North Carolina", "Oklahoma", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia")
#Cflorida
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Mississippi", "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia")
#Erepens
endemic <- c("Alabama", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kentucky", "Maine", "Manitoba", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "New Brunswick", "New Hampshire", "New Jersey", "New York", "Newfoundland and Labrador", "North Carolina", "Nova Scotia", "Ohio", "Ontario", "Pennsylvania", "Prince Edward Island", "Rhode Island", "South Carolina", "Tennessee", "Vermont", "Virginia", "West Virginia", "Wisconsin")
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
endemic <- c("Alabama", "Arkansas", "Bahamas", "Connecticut", "Cuba", "Delaware", "District of Columbia", "El Salvador", "Florida", "Georgia", "Guatemala", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "Saskatchewan", "South Carolina", "South Dakota", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin")
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


###############################################################################
################################### Mapping ###################################
###############################################################################

#get shapefile
hardwood.shape <- st_read("/blue/soltis/share/FL_HardwoodForests/02_rasters/FL_HardwoodForest_Shapefile/vcom67_Hardwood.shp")
st_geometry_type(hardwood.shape)
hardwood.shape <- st_zm(hardwood.shape)
raster_img <- raster(hardwood.shape)
proj4string(raster_img) <- CRS("+proj=longlat +datum=WGS84")








#### CHANGE SPECIES TITLE ####

# ggplot
ggplot() +
  ggtitle(expression(paste(italic("Asplenium platyneuron"), " Cleaned Data"))) +
    
  # Plot the endemic and non-endemic areas
  geom_sf(data = endemic_df, 
          aes(fill = is_endemic),  
          color = "black") +       
  scale_fill_manual(values = c("Endemic" = "gray90", "Non-Endemic" = "white")) +  
    
  geom_sf(data = hardwood.shape, fill="gray40", color= "gray40") +
    
  geom_point(species_df, mapping = aes(x=long, y=lat, color = basisOfRecord), alpha = 0.8, size= 0.3) +
    
  labs(fill = "Endemic Areas") +
    
  #custom_colors
  scale_color_manual(name = "Basis of Record", values = c("HUMAN_OBSERVATION" = "blue", "PRESERVED_SPECIMEN" = "red", "MATERIAL_SAMPLE" = "green", "FOSSIL_SPECIMEN" = "yellow", "OCCURRENCE" = "orange")) +
  
  #set coords
  coord_sf(xlim = c(extent_x_min, extent_x_max), ylim = c(extent_y_min, extent_y_max)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
  
## save plot 
ggsave(paste0(accepted_name, "_map_Oct_30.jpg"), plot = last_plot(), path = "./Maps", units= "in", height = 7.5, width = 11)

