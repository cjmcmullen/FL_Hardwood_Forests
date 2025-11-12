## FL Hardwood Forests Species Complete Mapping
## 2-18-2025 by Sydney Barfus

library(geodata)
library(sf)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringr)
library(raster)
library(dismo)
library(gtools)
library(biomod2)
library(ggpubr)

#setwd("/blue/soltis/share/FL_HardwoodForests/")

alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldf_V3.csv")
all_species <- unique(alldf$accepted_name)

# create lookup table for endemic areas
endemic_areas <- list(
  "Asaccharum" = c("Alabama", "Arkansas", "Florida", "Georgia", "Illinois", "Kentucky", "Louisiana", "Mexico", "Mississippi", "Missouri", "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia"),
  "Aspinosa" = c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri", "New Jersey", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia"),
  "Aplatyneuron" = c("Alabama", "Arizona", "Arkansas", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Lesotho", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "South Africa", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin"),
  "Acanadensis" = c("Alabama", "Arkansas", "Connecticut", "Delaware", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Maine", "Manitoba", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "Saskatchewan", "South Carolina", "South Dakota", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin"),
  "Cfloridus" = c("Alabama", "Florida", "Georgia", "Kentucky", "Maryland", "Mississippi", "North Carolina", "Ohio", "Pennsylvania", "South Carolina", "Tennessee", "Virginia", "West Virginia"),
  "Cdasycarpa" = c("Alabama", "Florida", "Georgia", "Mississippi", "South Carolina"),
  "Ccaroliniana" = c("Alabama", "Arkansas", "Connecticut", "Delaware", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin"),
  "Cglabra" = c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maryland", "Massachusetts", "Michigan", "Mississippi", "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Ontario", "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin"),
  "Claevigata" = c("Alabama", "Arkansas", "Florida", "Georgia", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Maryland", "Mexico", "Mississippi", "Missouri", "New Mexico", "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia"),
  "Coccidentalis" = c("Alabama", "Arkansas", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Maine", "Manitoba", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin", "Wyoming"),
  "Ccanadensis" = c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maryland", "Massachusetts", "Mexico", "Michigan", "Mississippi", "Missouri", "Nebraska", "New Jersey", "New Mexico", "New York", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia", "Wisconsin"),
  "Claxum" = c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri", "New York", "North Carolina", "Oklahoma", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia"),
  "Cflorida" = c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Mississippi", "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia"),
  "Erepens" = c("Alabama", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kentucky", "Labrador", "Maine", "Manitoba", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "New Brunswick", "New Hampshire", "New Jersey", "New York", "Newfoundland", "North Carolina", "Nova Scotia", "Ohio", "Ontario", "Pennsylvania", "Prince Edward Island", "Rhode Island", "South Carolina", "Tennessee", "Vermont", "Virginia", "West Virginia", "Wisconsin"),
  "Eumbilicatum" = c("Alabama", "Florida", "Georgia", "Kentucky", "North Carolina", "South Carolina", "Tennessee", "Virginia", "West Virginia"),
  "Eamericanus" = c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia"),
  "Fgrandifolia" = c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Mississippi", "Missouri", "New Brunswick", "New Hampshire", "New Jersey", "New York", "North Carolina", "Nova Scotia", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Prince Edward Island", "Québec", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin"),
  "Famericana" = c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin"),
  "Hdiptera" = c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "South Carolina", "Texas"),
  "Harifolia" = c("Alabama", "Florida", "Georgia", "Kentucky", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Tennessee", "Virginia"),
  "Iopaca" = c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kentucky", "Louisiana", "Maryland", "Massachusetts", "Mississippi", "Missouri", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia"),
  "Lstyraciflua" = c("Alabama", "Arkansas", "Belize", "Connecticut", "El Salvador", "Florida", "Georgia", "Guatemala", "Honduras", "Illinois", "Indiana", "Kentucky", "Louisiana", "Maryland", "Mexico", "Mississippi", "Missouri", "New Jersey", "New York", "Nicaragua", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia"),
  "Mgrandiflora" = c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Texas"),
  "Mfloridana" = c("Florida", "Georgia"),
  "Malabamensis" = c("Alabama", "Florida", "Georgia"),
  "Mflavidula" = c("Alabama", "Florida", "Georgia", "Mississippi", "North Carolina", "South Carolina", "Tennessee"),
  "Mrepens" = c("Alabama", "Arkansas", "Connecticut", "Delaware", "Florida", "Georgia", "Guatemala", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "New Brunswick", "New Hampshire", "New Jersey", "New York", "Newfoundland and Labrador", "North Carolina", "Nova Scotia", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin"),
  "Mreynoldsiae" = c("Alabama", "Delaware", "Florida", "Georgia", "Kentucky", "Maryland", "North Carolina", "South Carolina", "Tennessee", "Virginia"),
  "Ohirtellus" = c("Alabama", "Angola", "Antigua and Barbuda", "Argentina", "Arkansas", "Australia", "Bahamas", "Belize", "Benin", "Bolivia", "Bonaire, Saint Eustatius and Saba", "Botswana", "Brazil", "Burkina Faso", "Burundi", "Cameroon", "Cabo Verde", "Cayman Islands", "Central African Republic", "China", "Colombia", "Comoros", "Congo", "Cook Islands", "Costa Rica", "Côte d'Ivoire", "Cuba", "Democratic Republic of the Congo", "Dominica", "Dominican Republic", "Ecuador", "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Fiji", "Florida", "French Guiana", "French Polynesia", "Gabon", "Gambia", "Georgia", "Ghana", "Guadeloupe", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Indonesia", "Jamaica", "Japan", "Kenya", "Liberia", "Louisiana", "Madagascar", "Malawi", "Mali", "Martinique", "Maryland", "Mauritius", "Mexico", "Micronesia", "Mississippi", "Missouri", "Montserrat", "New Caledonia", "New Zealand", "Nicaragua", "Nigeria", "Niue", "North Carolina", "North Korea", "Northern Mariana Islands", "Oklahoma", "Panama", "Paraguay", "Papua New Guinea", "Peru", "Philippines", "Puerto Rico", "Rwanda", "Réunion", "Saint Kitts and Nevis", "Saint Lucia", "Saint-Martin", "Saint Vincent and the Grenadines", "Samoa", "São Tomé and Príncipe", "Senegal", "Sierra Leone", "Solomon Islands", "South Africa", "South Carolina", "South Korea", "South Sudan", "Sudan", "Suriname", "Taiwan", "Tanzania", "Texas", "Thailand", "Togo", "Tonga", "Trinidad and Tobago", "Uganda", "Uruguay", "Vanuatu", "Venezuela", "Vietnam", "Virgin Islands, U.S.", "Virginia", "Yemen", "Zambia", "Zimbabwe"),
  "Ovirginiana" = c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "El Salvador", "Florida", "Georgia", "Guatemala", "Honduras", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Manitoba", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Brunswick", "New Hampshire", "New Jersey", "New York", "North Carolina", "North Dakota", "Nova Scotia", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Prince Edward Island", "Québec", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin", "Wyoming"),
  "Pquinquefolia" = c("Alabama", "Arkansas", "Bahamas", "Colorado", "Connecticut", "Cuba", "Delaware", "District of Columbia", "El Salvador", "Florida", "Georgia", "Guatemala", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "New Brunswick", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "Saskatchewan", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "West Virginia", "Wisconsin"),
  "Pborbonia" = c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Texas"),
  "Pglabra" = c("Alabama", "Florida", "Georgia", "Louisiana", "Mississippi", "South Carolina"),
  "Ptaeda" = c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi", "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia"),
  "Pcaroliniana" = c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Texas"),
  "Pserotina" = c("Alabama", "Arizona", "Arkansas", "British Columbia", "Colorado", "Connecticut", "Delaware", "District of Columbia", "El Salvador", "Florida", "Georgia", "Guatemala", "Honduras", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Brunswick", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "Nova Scotia", "Ohio", "Oklahoma", "Ontario", "Panama", "Pennsylvania", "Québec", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin"),
  "Qalba" = c("Alabama", "Arkansas", "Connecticut", "Delaware", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin"),
  "Qhemisphaerica" = c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Texas", "Virginia"),
  "Qmichauxii" = c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri", "New Jersey", "North Carolina", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia"),
  "Qvirginiana" = c("Alabama", "Florida", "Georgia", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Texas", "Virginia"),
  "Slanuginosum" = c("Alabama", "Arizona", "Arkansas", "Florida", "Georgia", "Illinois", "Kansas", "Kentucky", "Louisiana", "Mexico", "Mississippi", "Missouri", "New Mexico", "Oklahoma", "South Carolina", "Texas"),
  "Spumila" = c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "South Carolina", "Texas"),
  "Stinctoria" = c("Alabama", "Arkansas", "Delaware", "Florida", "Georgia", "Louisiana", "Maryland", "Mississippi", "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia"),
  "Tamericana" = c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Manitoba", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Brunswick", "New Hampshire", "New Jersey", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "Saskatchewan", "South Carolina", "South Dakota", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin"),
  "Ualata" = c("Alabama", "Arkansas", "Florida", "Georgia", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Mississippi", "Missouri", "North Carolina", "Ohio", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia"),
  "Vsororia" = c("Alabama", "Arkansas", "British Columbia", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Manitoba", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "New Brunswick", "New Hampshire", "New Jersey", "New Mexico", "New York", "Newfoundland and Labrador", "North Carolina", "North Dakota", "Nova Scotia", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Prince Edward Island", "Québec", "Rhode Island", "Saskatchewan", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "West Virginia", "Wisconsin")
)

## set global theme for plots
theme_set(theme_minimal() + 
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white", color = NA), # Set white background
              plot.background = element_rect(fill = "white", color = NA),  # Ensure entire plot stays white
              axis.line = element_line(colour = "black"),
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 12),
              plot.title = element_text(size = 12),
              legend.text = element_text(size = 10)
            ))

# load plotting function
generate_custom_labels <- function(n) {
  # Start with A to F
  single_letters <- LETTERS[1:6]
  
  # Then G to R, each with a pair
  paired_letters <- LETTERS[7:18]  # G to R
  paired <- unlist(lapply(paired_letters, function(x) c(x, paste0(x, "'"), paste0(x, "''"))))
  
  # Combine both
  all_labels <- c(single_letters, paired)
  
  # Return only as many as needed
  if (n > length(all_labels)) {
    stop("Too many plots: exceeds available custom labels.")
  }
  
  all_labels[1:n]
}

## set up time period, models, ssps for for-loop
time_periods <- c("2041-2060", "2081-2100")
models <- c("ACCESS-CM2", "GISS-E2-1-G")
ssps <- c("ssp245", "ssp370", "ssp585")

# Grey Box Placeholder for A
grey_box <- ggplot() + theme_void() + theme(panel.background = element_rect(fill = "grey80"))

######################### 1. Set up Mapping Areas ##########################

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

# remove the USA from the world_sf so it doesn't plot on top of the states or Providences
world_sf_subset <- world_sf_subset %>% filter(!NAME %in% c("United States", "Canada"))

provinces_sf <- st_as_sf(provinces)
colnames(provinces_sf)[colnames(provinces_sf) == "NAME_1"] <- "NAME"
provinces_sf_subset <- provinces_sf[c("NAME", "geometry")]

## combine them together
areas_sf <- rbind(states_sf_subset, world_sf_subset, provinces_sf_subset)

# set up list names
stateList <- as.data.frame(unique(states$NAME_1))
countryList <- as.data.frame(unique(world$NAME_0))
canadaList <- as.data.frame(unique(provinces$NAME_1))
areaList <- unlist(c(stateList, countryList, canadaList))

# get shapefile
hardwood.shape <- st_read("/blue/soltis/share/FL_HardwoodForests/02_rasters/FL_HardwoodForest_Shapefile/vcom67_Hardwood.shp")
st_geometry_type(hardwood.shape)
hardwood.shape <- st_zm(hardwood.shape)
raster_img <- raster(hardwood.shape)
proj4string(raster_img) <- CRS("+proj=longlat +datum=WGS84")



species <- "Asaccharum"


########################## 2. Supplementals Loop ###########################

#for (species in all_species) {
  species_df <- filter(alldf, accepted_name == species)
  fullname <- unique(species_df$full_name) # get the full name
  
  # get endemic areas for species
  # print(species)
  endemic <- endemic_areas[[species]]
  endemic_df <- areas_sf %>%
    mutate(is_endemic = ifelse(NAME %in% endemic, "Endemic", "Non-Endemic"))
  
  my_extent <- extent(st_as_sf(species_df, coords = c("long", "lat"), crs = 4326))
  extent_x_min <- my_extent[1] - 2
  extent_x_max <- my_extent[2] + 2
  extent_y_min <- my_extent[3] - 2
  extent_y_max <- my_extent[4] + 2

  ########################### 2a. Occurrence Map #############################
  
  #### FIND COUNTS IN HARDWOOD FOR LEGEND
  # Convert the logical matrix to a vector of row indices
  spec_sf <- st_as_sf(species_df, coords = c("long", "lat"), crs = st_crs(hardwood.shape))
  inside_index <- st_intersects(spec_sf, hardwood.shape, sparse = FALSE)  # Returns logical matrix
  inside_rows <- apply(inside_index, 1, any)  # TRUE if the point is inside any polygon
  
  # Subset only the points that are inside
  records_inside <- spec_sf[inside_rows, ]
  
  # Count the number of records per basisOfRecord type within scrub.shape
  counts <- records_inside %>%
    group_by(basisOfRecord) %>%
    summarise(count = n())
  
  # Create a named vector for legend labels with counts
  legend_labels <- setNames(
    paste0(levels(factor(species_df$basisOfRecord)), " (",
      ifelse(
        is.na(counts$count[match(levels(factor(species_df$basisOfRecord)), counts$basisOfRecord)]),
         0,
        counts$count[match(levels(factor(species_df$basisOfRecord)), counts$basisOfRecord)]),
      ")"),
    levels(factor(species_df$basisOfRecord)))
  
  ## create plot
  occ_plot <- ggplot() +
    # plot the endemic and non-endemic areas
    geom_sf(data = endemic_df, 
            aes(fill = is_endemic),  
            color = "black") +        
    scale_fill_manual(values = c("Endemic" = "gray90", "Non-Endemic" = "white")) +  
    # plot hardwood forests
    geom_sf(data = hardwood.shape, fill="gray40", color= "gray40") +
    # plot data points
    geom_point(species_df, mapping = aes(x=long, y=lat, color = basisOfRecord), alpha = 0.8, size= 0.3) +
    labs(fill = "Endemic Areas") +
    # set custom colors
    scale_color_manual(name = "Basis of Record (# in Hardwoods)", 
                       values = c("HUMAN_OBSERVATION" = "blue",
                                  "PRESERVED_SPECIMEN" = "red", 
                                  "MATERIAL_SAMPLE" = "green", 
                                  "FOSSIL_SPECIMEN" = "yellow", 
                                  "OCCURRENCE" = "orange"),
                       labels = legend_labels) +
    # set coords and titles
    coord_sf(xlim = c(extent_x_min, extent_x_max), ylim = c(extent_y_min, extent_y_max)) +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(substitute(italic(x) ~ "Cleaned Data", list(x = fullname))) +
    # stack legends at bottom
    theme(legend.position = "bottom", 
          legend.box = "vertical",
          legend.spacing.y = unit(0.0005, "lines")) +  # Adjust spacing here
    guides(color = guide_legend(override.aes = list(size = 1), order = 1, nrow = 2), # Order BoR legend first
           fill = guide_legend(override.aes = list(size = 2), order = 2)) # Order endemic legend second
  
  
  print("Occurence plot loaded")
  
  
  
  ## save plot 
  # ggsave(paste0(species, "_CleanedData.png"), path = output_dir, 
  #        units= "in", height = 7.5, width = 11)
  

  ########################### 2b. Response Curves ############################  
  
  plot_response_curve <- function(mod, var) {
    response_data <- as.data.frame(dismo::response(mod, var = var))  # Extract response curve data
    colnames(response_data) <- c("value", "suitability")
    
    ggplot(response_data, aes(x = value, y = suitability)) +
      geom_line(color = "blue", size = 1) +
      labs(title = paste("Response Curve"), x = var, y = "Suitability") +
      ylim(0, 1) +
      theme_minimal()
  }
  
  
  print("response function loaded")
  
  # Load Model Results
  load(paste0("/blue/soltis/share/FL_HardwoodForests/04_ENMs/", species, "/", species, "_optimalSeq_ENM.RData"))
  
  print("response rdata loaded")
  
  var_contributions <- as.data.frame(mod.seq@results) %>% rownames_to_column(var = "variable")
  top_vars <- var_contributions %>%
    filter(str_detect(variable, "contribution")) %>%
    arrange(desc(V1)) %>%
    dplyr::slice(1:3) %>%
    mutate(variable = gsub("\\.contribution", "", variable)) %>%
    pull(variable)
  
  # Generate Response Curves for C, D, E
  response_curves <- lapply(top_vars, function(var) {
    plot_response_curve(mod.seq, var)
  })
  
  # Ensure all response curves are ggplot objects
  response_curves <- Filter(function(x) inherits(x, "ggplot"), response_curves)

  
  print("response curves finished")
  
    

  ###################### 2c. Current Habitat Suitability ######################
  
  ## load rData and make p plottable
  load(paste0("/blue/soltis/share/FL_HardwoodForests/05_CurrentProjections/", species, "_CurrentHardwood_Projection.RData"))
  
  print("Current rdata loaded")
  
  
  p_df1 <- as.data.frame(rasterToPoints(p))
  
  print("Current rdata assigned")
  
  
  
  colnames(p_df1)[3] <- "Habitat_Suitablity"
  
  ## create plot
  current_proj <- ggplot() + 
    geom_sf(data = states_sf_subset, fill = "grey90", color = "white") +
    geom_tile(data = p_df1, aes(x = x, y = y, fill = Habitat_Suitablity)) +
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) + 
    xlab("Longitude") +
    ylab("Latitude") +
    scale_fill_gradientn(colours = viridis::mako(99, direction = -1),
                         na.value = "white", 
                         breaks=c(0, 0.25, 0.5, 0.75, 1),
                         labels=c("Low",0.25, 0.5, 0.75, "High"), 
                         limits=c(0,1)) +
    ggtitle("Current Hardwood Forest Habitat Suitability")
    
  ## save plot
  # ggsave(paste0(species, "_CurrentSuitability.png"), path = output_dir, 
  #          units= "in", height = 7.5, width = 11)
  
  
  ################### 2d. Future, Comparison, and ETF Plots ###################
  
  plot_list <- list()
  
  for (model in models) {
    for (time_period in time_periods) {
      for (ssp in ssps) {
        
        ### FUTURE PROJECTIONS
        ## load rData and make p plottable
        load(paste0("/blue/soltis/share/FL_HardwoodForests/06_FutureProjections/", species, "/", species, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.RData"))
        
        print("future rdata loaded")
        
        
        p_df2 <- as.data.frame(rasterToPoints(p))
        
        
        print("Current rdata assigned")
        
        colnames(p_df2)[3] <- "Habitat_Suitability"
        
        ##create plot
        future_proj <- ggplot() +
          #geom_sf(data = world_sf_subset, fill = "grey90", color = "white") +  
          geom_sf(data = states_sf_subset, fill = "grey90", color = "white") +
          geom_tile(data = p_df2, aes(x = x, y = y, fill = Habitat_Suitability)) +
          coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
          xlab("Longitude") + 
          ylab("Latitude") +
          scale_fill_gradientn(colours = viridis::mako(99, direction = -1),
                               na.value = "white", breaks = c(0, 0.25, 0.5, 0.75, 1),
                               labels = c("Low", 0.25, 0.5, 0.75, "High"), limits = c(0, 1)) +
          ggtitle(paste0(time_period, " Future Hardwood Forest Suitability ", model, " (", ssp, ")"))
        
        
        ### COMPARISONS
        ## load in current and future rasters (generated in script 9)
        current_binary <- raster(paste0("07C_ProjectionCompairsons/", species, "/", species, "_current_binary.asc"))
        future_binary <- raster(paste0("07C_ProjectionCompairsons/", species, "/", species, "_", model, "_", time_period, "_", ssp, "_binary.asc"))
        
        # Calculate range size differences
        RangeSizeDiff <- BIOMOD_RangeSize(current_binary, future_binary)
        
        # Create a dataframe for plotting
        comparison_df <- as.data.frame(RangeSizeDiff$Diff.By.Pixel, xy = TRUE)
        fill <- factor(comparison_df[, 3])
        
        ## load in comparison results
        results <- read.csv(paste0("07C_ProjectionCompairsons/", species, "/", species, "_current_v_", model, "_", time_period, "_", ssp, "_Comparison.csv"))
        
        # extract counts dynamically from the results variable
        loss_count <- results[2]
        stable0_count <- results[3]
        stable1_count <- results[4]
        gain_count <- results[5]
        
        # Generate plot
        comp_plot <- ggplot() +
          geom_sf(data = states_sf_subset, fill = "grey90", color = "white") +
          geom_tile(data = comparison_df, aes(x = x, y = y, fill = as.factor(fill))) +  # Ensure `fill` is treated as a factor
          coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) + 
          xlab("Longitude") +
          ylab("Latitude") +
          ggtitle("Change in Habitat Suitability")
          scale_fill_manual(
            ## manually set viridis color scale
            values = c("1" = "#440154", "0" = "#31688E", "-1" = "#35b779", "-2" = "#FDE725" ),
            # Apply custom colors
            na.value = "white",    
            name = "Pixel Change",
            breaks = c("-2", "-1", "0", "1"),  
            labels = c(
              paste0("Lost (", loss_count, " pixels)"),
              paste0("Maintained (", stable1_count, " pixels)"),
              paste0("Never Occupied (", stable0_count, " pixels)"),
              paste0("Gained (", gain_count, " pixels)")))
        
        
        ### FUTURE ETF PROJECTIONS
        ## load rData and make p plottable
        load(paste0("/blue/soltis/share/FL_HardwoodForests/08B_FutureETFProjections/", species, "/", species, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.RData"))
        
        
        print("etf rdata loaded")
        
        p_df3 <- as.data.frame(rasterToPoints(p))
        
        
        print("Current rdata assigned")
        
        colnames(p_df3)[3] <- "Habitat_Suitability"
        
        ## get extents for mapping
        my_extent_etf <- extent(st_as_sf(p_df3, coords = c("x", "y"), crs = 4326))
        extent_x_min_etf <- my_extent_etf[1]
        extent_x_max_etf <- my_extent_etf[2]
        extent_y_min_etf <- my_extent_etf[3]
        extent_y_max_etf <- my_extent_etf[4]
        
        etf_plot <- ggplot() +
          geom_sf(data = states_sf_subset, fill = "grey90", color = "white") +
          geom_tile(data = p_df3, aes(x = x, y = y, fill = Habitat_Suitability)) +
          xlab("Longitude") + ylab("Latitude") +
          coord_sf(xlim = c(extent_x_min_etf, extent_x_max_etf), ylim = c(extent_y_min_etf, extent_y_max_etf)) +
          scale_fill_gradientn(colours = viridis::mako(99, direction = -1),
                               na.value = "white",
                               breaks = c(0, 0.25, 0.5, 0.75, 1),
                               labels = c("Low", 0.25, 0.5, 0.75, "High"),
                               limits = c(0, 1)) +
          ggtitle("Future Eastern Temperate Forest Suitability")
        
        # Store plots in order
        plot_list[[length(plot_list) + 1]] <- future_proj
        plot_list[[length(plot_list) + 1]] <- comp_plot
        plot_list[[length(plot_list) + 1]] <- etf_plot
        
        print(paste("Finished processing:", species, model, time_period, ssp))
      }}}
  
  
  ######################### 3. Create Supplementals ##########################
  
  # Combine all plots
  all_plots <- c(list(grey_box, occ_plot), response_curves, list(current_proj), plot_list)
  all_plots <- Filter(function(x) inherits(x, "ggplot"), all_plots)
  
  # Arrange plots in a 7-row x 6-column grid and save as a full-page PDF
  pdf(paste0("12_SupplementalFigures/", species, "_SupplementalFigure_test2.pdf"), height = 20, width = 28)
  # arranged_plot <-ggarrange(plotlist = all_plots, 
  #                           ncol = 6, 
  #                           nrow = 7, 
  #                           labels = generate_custom_labels(length(all_plots)))
  
  arranged_plot <- ggarrange(
    plotlist = all_plots,
    ncol = 6,
    nrow = 7,
    labels = generate_custom_labels(length(all_plots)),
    widths = rep(1, 6), # Example: all columns have equal width
    heights = rep(1, 7) # Example: all rows have equal height
  )
  print(arranged_plot)
  #grid::grid.draw(arranged_plot)  # This explicitly draws the arranged plot
  dev.off()
  
  print(paste0("Finished and Saved: ", species))
#}


