library(ggplot2)
library(sf)
library(raster)
library(dplyr)
library(dismo)
library(tidyr)
library(stringr)
library(tibble)
library(biomod2)
library(patchwork)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(jpeg)
library(grid)

#setwd("/blue/soltis/share/FL_HardwoodForests/")

# loading plotting function
generate_custom_labels <- function(n) {
  # Start with A to F
  single_letters <- LETTERS[1:6]
  
  # G-R as triplets
  triplet_letters <- LETTERS[7:18]  # G to R
  triplets <- unlist(lapply(triplet_letters, function(x) c(x, paste0(x, "'"), paste0(x, '"'))))
  
  # Combine both
  all_labels <- c(single_letters, triplets)
  
  # Return only as many as needed
  if (n > length(all_labels)) {
    stop("Too many plots: exceeds available custom labels.")
  }
  
  all_labels[1:n]
}

  
# Load data
alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldf_V4.csv")
unique_species <- unique(alldf$accepted_name)

# Define triplet labels (G to R)
triplet_letters <- LETTERS[7:18]  # G to R

# Define model assignment (ACCESS and GISS alternating)
triplet_models <- rep(c("ACCESS-CM2", "GISS-E2-1-G"), times = 6)

# Optional: time periods and SSPs for matching
triplet_time_periods <- rep(c("2041-2060", "2081-2100"), each = 6)
triplet_ssps <- rep(c("ssp245", "ssp370", "ssp585"), times = 2, each =2)

endemic_areas <- list(
  "Asaccharum" = c("Alabama", "Arkansas", "Florida", "Georgia", "Illinois", "Kentucky", "Louisiana", "Mexico", "Mississippi", "Missouri", "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas", "Virginia"),
  "Aspinosa" = c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri", "New Jersey", "North Carolina", "Ohio", "Oklahoma", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia"),
  "Aplatyneuron" = c("Alabama", "Arizona", "Arkansas", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Lesotho", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "South Africa", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin"),
  "Acanadensis" = c("Alabama", "Arkansas", "Connecticut", "Delaware", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Maine", "Manitoba", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "Saskatchewan", "South Carolina", "South Dakota", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin"),
  "Aarifolium" = c("Alabama", "Florida", "Georgia", "Kentucky", "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Tennessee", "Virginia"),
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

# Set global theme
theme_set(theme_minimal() + 
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white", color = NA), # Set white background
              plot.background = element_rect(fill = "white", color = NA),  # Ensure entire plot stays white
              axis.line = element_line(colour = "black"),
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 10),
              plot.title = element_text(size = 10),
              legend.title = element_text(size = 8, face = "bold"),
              legend.text = element_text(size = 6)
            ))

## to run one species:
species <- "Aarifolium"


## to run all species:
#for (species in unique_species) {
  
  ## skip O.hirtellus until ENMs finish
  # if (species == "Ohirtellus") next
  
  spec <- gsub(" ", "_", species)
  
  spec_subset <- dplyr::filter(alldf, accepted_name == species)
  fullname <- unique(spec_subset$full_name)
  
  # Set up endemic areas
  endemic <- endemic_areas[[species]]
  
  ##### Photo for A
  # Construct path to species-specific image
  img_path <- file.path("13_Species_Images", paste0(spec, ".jpg"))
  
  if (file.exists(img_path)) {
    img <- readJPEG(img_path)
    g <- rasterGrob(img, interpolate = TRUE)
    image_plot <- ggplot() +
      annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme_void()
  } else {
    # fallback grey box if image not found
    image_plot <- ggplot() +
      theme_void() +
      theme(panel.background = element_rect(fill = "grey80"))
  }
  
  # Basemap
  states <- map_data("state")
  states_data <- states %>%
    mutate(is_endemic = ifelse(tolower(region) %in% tolower(endemic), "Endemic", "Non-Endemic"))
  world <- map_data("world")
  world_data <- world %>%
    mutate(is_endemic = ifelse(tolower(region) %in% tolower(endemic), "Endemic", "Non-Endemic"))
  lat_range <- range(spec_subset$lat, na.rm = TRUE) + c(-1, 1)
  lon_range <- range(spec_subset$long, na.rm = TRUE) + c(-3, 3)
  
  # Hardwood forest shapefile
  hardwood.shape <- st_read("02_rasters/FL_HardwoodForest_Shapefile/vcom67_Hardwood.shp")
  spec_sf <- st_as_sf(spec_subset, coords = c("long", "lat"), crs = st_crs(hardwood.shape))

  # Function to plot response curves
  plot_response_curve <- function(mod, var) {
    response_data <- as.data.frame(dismo::response(mod, var = var))  # Extract response curve data
    colnames(response_data) <- c("value", "suitability")  # Ensure proper column names
    
    ggplot(response_data, aes(x = value, y = suitability)) +
      geom_line(color = "blue", size = 1) +
      labs(title = paste("Response Curve"), x = var, y = "Suitability") +
      ylim(0, 1) +  # Force Y-axis limits to 0-1
      theme_minimal()
  }
  
  ##### Occurrence Data Plot for B
  # Convert the logical matrix to a vector of row indices
  inside_index <- st_intersects(spec_sf, hardwood.shape, sparse = FALSE)  # Returns logical matrix
  inside_rows <- apply(inside_index, 1, any)  # TRUE if the point is inside any polygon
  
  # Subset only the points that are inside
  records_inside <- spec_sf[inside_rows, ]
  
  # Count the number of records per basisOfRecord type within hardwood.shape
  counts <- records_inside %>%
    group_by(basisOfRecord) %>%
    summarise(count = n())
  
  # Create a named vector for legend labels with counts
  legend_labels <- setNames(
    paste0(levels(factor(spec_subset$basisOfRecord)), " (", 
           counts$count[match(levels(factor(spec_subset$basisOfRecord)), counts$basisOfRecord)], ")"),
    levels(factor(spec_subset$basisOfRecord))
  )
  
  occ_plot <- ggplot(data = world_data) + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = is_endemic), color = "black", linewidth = 0.2) +
    geom_polygon(data = states_data, aes(x = long, y = lat, group = group, fill = is_endemic), color = "black", linewidth = 0.2) +
    #geom_polygon(data = endemic_df, aes(x = long, y = lat, group = group, fill = is_endemic), color = "black") #+        
    scale_fill_manual(values = c("Endemic" = "gray90", "Non-Endemic" = "white")) +
    geom_sf(data = hardwood.shape, fill = "grey30", color = "grey30") +
    geom_point(data = spec_subset, aes(x = long, y = lat, color = basisOfRecord), 
               alpha = 0.6, size= 0.3) +
    scale_color_manual(values = c("HUMAN_OBSERVATION" = "blue", 
                                  "PRESERVED_SPECIMEN" = "red", 
                                  "MATERIAL_SAMPLE" = "green4", 
                                  "FOSSIL_SPECIMEN" = "yellow2", 
                                  "OCCURRENCE" = "orange"),
                       labels = legend_labels) +
    labs(fill = "Endemic Areas") +
    ggtitle(substitute(italic(x) ~ "Cleaned Data", list(x = fullname))) +
    coord_sf(xlim = lon_range, ylim = lat_range) + # dynamically set limits based on spec_subset data
    xlab("Longitude") +
    ylab("Latitude") +
    theme(legend.key.size = unit(0.4, "cm"),
          legend.position = "right")

  # Load Model Results
  load(paste0("04_ENMs/", spec, "/", spec, "_optimalSeq_ENM.RData"))
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
  
  # Load Current Projection
  load(paste0("05_CurrentProjections/", species, "_CurrentHardwood_Projection.RData"))
  p_df <- as.data.frame(rasterToPoints(p))
  colnames(p_df)[3] <- "habitat_suitability"
  
  current_proj <- ggplot(data = world) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white")+
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
    geom_tile(data = p_df, aes(x = x, y = y, fill = habitat_suitability)) +
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) + 
    xlab("Longitude") +
    ylab("Latitude") +
    labs(fill = "Habitat\nSuitability") +
    scale_fill_gradientn(colours = viridis::mako(99, direction = -1),
                         na.value = "white", 
                         breaks=c(0, 0.25, 0.5, 0.75, 1),
                         labels=c("Low",0.25, 0.5, 0.75, "High"), 
                         limits=c(0,1)) +
    ggtitle("Current Habitat Suitability")
  
  
  # Future Projections & Comparisons
  time_periods <- c("2041-2060", "2081-2100")
  models <- c("ACCESS-CM2", "GISS-E2-1-G")
  ssps <- c("ssp245", "ssp370", "ssp585")
  
  plot_list <- list()
  
  for (i in seq_along(triplet_letters)) {
    model <- triplet_models[i]
    triplet_label <- triplet_letters[i]
    time_period <- triplet_time_periods[i]
    ssp <- triplet_ssps[i]
    
    # Load future projection
    future_file <- paste0("06_FutureProjections/", species, "/", species, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.RData")
    load(future_file)
    
    p_df <- as.data.frame(rasterToPoints(p))
    colnames(p_df)[3] <- "habitat_suitability"
    
    future_proj <- ggplot(data = world) +
      geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
      geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
      geom_tile(data = p_df, aes(x = x, y = y, fill = habitat_suitability)) +
      coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
      xlab("Longitude") + 
      ylab("Latitude") +
      scale_fill_gradientn(colours = viridis::mako(99, direction = -1),
                           na.value = "white", 
                           name = "Habitat\nSuitability",
                           breaks = c(0, 0.25, 0.5, 0.75, 1),
                           labels = c("Low", 0.25, 0.5, 0.75, "High"), 
                           limits = c(0, 1)) +
      ggtitle(paste("Future Suitability", time_period, "(", model, ssp, ")"))
    
    # Load binary raster for comparison
    current_output_file <- paste0("07C_ProjectionCompairsons/", species, "/", species, "_current_binary.asc")
    future_output_file <- paste0("07C_ProjectionCompairsons/", species, "/", species, "_", model, "_", time_period, "_", ssp, "_binary.asc")
    
    current_binary <- raster(current_output_file)
    future_binary <- raster(future_output_file)
    
    RangeSizeDiff <- BIOMOD_RangeSize(current_binary, future_binary)
    results <- RangeSizeDiff$Compt.By.Models
    df <- as.data.frame(RangeSizeDiff$Diff.By.Pixel, xy = TRUE)
    df$layer <- factor(df[, 3],
                       levels = c(-2, -1, 0, 1))#,
                       #labels = c("lost", "unchanged", "not occupied", "occupied in the future"))
    
    # Extract counts dynamically from the results variable
    loss_count <- results[1]
    stable0_count <- results[2]
    stable1_count <- results[3]
    gain_count <- results[4]
    
    comp_plot <- ggplot() +
      geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
      geom_tile(data = df, aes(x = x, y = y, fill = layer)) +
      coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) + 
      xlab("Longitude") +
      ylab("Latitude") +
      ggtitle("Change in Suitability") +
      scale_fill_manual(
        ## manually set viridis color scale
        values = c("1" = "#440154", "0" = "#31688E", "-1" = "#35b779", "-2" = "#FDE725" ),
        na.value = "white",    
        name = "Pixel Change",
        breaks = c("-2", "-1", "0", "1"),  
        labels = c(
          paste0("Lost (", loss_count, " pixels)"),
          paste0("Maintained (", stable1_count, " pixels)"),
          paste0("Never Occupied (", stable0_count, " pixels)"),
          paste0("Gained (", gain_count, " pixels)"))) 
      # theme(legend.key.size = unit(0.4, "cm"),
      #       legend.position = "bottom")
      
      # Load the ETF projection for the same model/time/ssp
      etf_file <- file.path("08B_FutureETFProjections", species, paste0(species, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.RData"))
    
      load(etf_file)  # loads object 'p'
      
      # Convert to data frame for plotting
      p_df_etf <- as.data.frame(rasterToPoints(p))
      colnames(p_df_etf)[3] <- "habitat_suitability"
      
      my_extent_etf <- extent(st_as_sf(p_df_etf, coords = c("x", "y"), crs = 4326))
      extent_x_min_etf <- my_extent_etf[1]
      extent_x_max_etf <- my_extent_etf[2]
      extent_y_min_etf <- my_extent_etf[3]
      extent_y_max_etf <- my_extent_etf[4]
      
      
      etf_proj <- ggplot(data = world) +
        geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
        #geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
        geom_raster(data = p_df_etf, aes(x = x, y = y, fill = habitat_suitability)) +
        geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "transparent", color = "white") +
        coord_sf(xlim = c(extent_x_min_etf, extent_x_max_etf), ylim = c(extent_y_min_etf, extent_y_max_etf)) +
        xlab("Longitude") + ylab("Latitude") +
        scale_fill_gradientn(colours = viridis::mako(99, direction = -1),
                             na.value = "white",
                             name = "Habitat\nSuitability",
                             breaks = c(0, 0.25, 0.5, 0.75, 1),
                             labels = c("Low", 0.25, 0.5, 0.75, "High"),
                             limits = c(0, 1)) +
        ggtitle("Eastern Temperate Forest Suitability")
        # theme(legend.key.size = unit(0.4, "cm"),
        #       legend.position = "bottom")

      # Only include if all three plots are available
      if (exists("etf_proj")) {
        plot_triplet <- list(future_proj, comp_plot, etf_proj)
      } else {
        plot_triplet <- list(future_proj, comp_plot)
      }
      
      plot_list <- append(plot_list, plot_triplet)
  }
  
  
  # Combine all plots
  all_plots <- c(list(image_plot, occ_plot), response_curves, list(current_proj), plot_list)
  all_plots <- Filter(function(x) inherits(x, "ggplot"), all_plots)
  
  # Arrange plots in a 5-row x 6-column grid and save as a full-page PDF
  pdf(paste0("12_SupplementalFigures/", spec, "_SupplementalFigure.pdf"), height = 20, width = 28)
  arranged_plot <-ggarrange(plotlist = all_plots, 
                            ncol = 6, 
                            nrow = 7, 
                            labels = generate_custom_labels(length(all_plots)))
  grid::grid.draw(arranged_plot)  # This explicitly draws the arranged plot
  dev.off()
  
  print(paste0("Completed: ", species)) ## track progress

#}
