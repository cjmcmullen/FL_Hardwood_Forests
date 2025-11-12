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


## VALUES TO CHANGE:
## specify species
species <- "Acanadensis"
fullname <- "Aquilegia canadensis"
## get endemic areas for species (script 3B)
endemic <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Maine", "Manitoba", "Maryland", "Massachusetts", "Mexico", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", "New Jersey", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Ontario", "Pennsylvania", "Québec", "Rhode Island", "Saskatchewan", "South Carolina", "South Dakota", "Tennessee", "Texas", "Vermont", "Virginia", "West Virginia", "Wisconsin")

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

## create species subset
alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldfBOR_V2.csv")
species_df <- dplyr::filter(alldf, accepted_name == species)


# Define triplet labels (G to R)
triplet_letters <- LETTERS[7:18]  # G to R

# Define model assignment (ACCESS and GISS alternating)
triplet_models <- rep(c("ACCESS-CM2", "GISS-E2-1-G"), times = 6)

# Optional: time periods and SSPs for matching
triplet_time_periods <- rep(c("2041-2060", "2081-2100"), each = 6)
triplet_ssps <- rep(c("ssp245", "ssp370", "ssp585"), times = 2, each =2)


## set global theme for plots
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
              legend.text = element_text(size = 8),
              legend.key.size = unit(0.4, "cm"),
              legend.position = "bottom")
            )


##### Grey Box Placeholder for A
grey_box <- ggplot() + theme_void() + theme(panel.background = element_rect(fill = "grey80"))

# Map cleaned records (B)
my_extent <- extent(st_as_sf(species_df, coords = c("long", "lat"), crs = 4326))
extent_x_min <- my_extent[1]-6
extent_x_max <- my_extent[2]+6
extent_y_min <- my_extent[3]-2
extent_y_max <- my_extent[4]+2

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

## add column to areas_sf with endemic or not 
endemic[!(endemic %in% areaList)]
endemic_df <- areas_sf %>%
  mutate(is_endemic = ifelse(NAME %in% endemic, "Endemic", "Non-Endemic"))

# get shapefile
hardwood.shape <- st_read("/blue/soltis/share/FL_HardwoodForests/02_rasters/FL_HardwoodForest_Shapefile/vcom67_Hardwood.shp")
st_geometry_type(hardwood.shape)
hardwood.shape <- st_zm(hardwood.shape)
raster_img <- raster(hardwood.shape)
proj4string(raster_img) <- CRS("+proj=longlat +datum=WGS84")

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
  levels(factor(species_df$basisOfRecord))
)


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




############################################################################
########################### 1b. Response Curves ############################
############################################################################

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

# Load Model Results
load(paste0("04_ENMs/", species, "/", species, "_optimalSeq_ENM.RData"))
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

############################################################################
###################### 2. Current Habitat Suitability ######################
############################################################################
## script 7

## load rData and make p plottable
load(paste0("05_CurrentProjections/", species, "_CurrentHardwood_Projection.RData"))
p_df1 <- as.data.frame(rasterToPoints(p))
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
                       breaks=c(0, 0.5, 1),
                       labels=c("Low", 0.5, "High"), 
                       limits=c(0,1)) +
  ggtitle("Current Hardwood Forest Habitat Suitability")
  

############################################################################
###################### 3. Future Habitat Suitability #######################
############################################################################

time_periods <- c("2041-2060", "2081-2100")
models <- c("ACCESS-CM2", "GISS-E2-1-G")
ssps <- c("ssp245", "ssp370", "ssp585")

plot_list <- list()

for (i in seq_along(triplet_letters)) {
  model <- triplet_models[i]
  triplet_label <- triplet_letters[i]
  time_period <- triplet_time_periods[i]
  ssp <- triplet_ssps[i]
  
  
  ### FUTURE PROJECTIONS
  ## load rData and make p plottable
  load(paste0("06_FutureProjections/", species, "/", species, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.RData"))
  p_df2 <- as.data.frame(rasterToPoints(p))
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
                         na.value = "white", breaks = c(0, 0.5, 1),
                         labels = c("Low", 0.5, "High"), limits = c(0, 1)) +
    ggtitle(paste0(time_period, " Future Hardwood Forest Suitability ", model, " (", ssp, ")"))
  

  ############################################################################
  ######################## 4. Projection Comparisons #########################
  ############################################################################
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
    ggtitle("Change in Habitat Suitability") +
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
  

  ############################################################################
  ######################## 5. Future ETF Projections #########################
  ############################################################################
  ## load rData and make p plottable
  load(paste0("08B_FutureETFProjections/", species, "/", species, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.RData"))
  p_df3 <- as.data.frame(rasterToPoints(p))
  colnames(p_df3)[3] <- "Habitat_Suitability"
  
  ## get extents for mapping
  my_extent <- extent(st_as_sf(p_df3, coords = c("x", "y"), crs = 4326))
  extent_x_min <- my_extent[1]
  extent_x_max <- my_extent[2]
  extent_y_min <- my_extent[3]
  extent_y_max <- my_extent[4]
  
  etf_plot <- ggplot() +
    geom_sf(data = states_sf_subset, fill = "grey90", color = "white") +
    geom_tile(data = p_df3, aes(x = x, y = y, fill = Habitat_Suitability)) +
    xlab("Longitude") + ylab("Latitude") +
    coord_sf(xlim = c(extent_x_min, extent_x_max), ylim = c(extent_y_min, extent_y_max)) +
    scale_fill_gradientn(colours = viridis::mako(99, direction = -1),
                         na.value = "white",
                         breaks = c(0, 0.5, 1),
                         labels = c("Low", 0.5, "High"),
                         limits = c(0, 1)) +
    ggtitle("Future Eastern Temperate Forest Suitability")
  
  # Store plots in order
  # Only include if all three plots are available
  if (exists("etf_plot")) {
    plot_triplet <- list(future_proj, comp_plot, etf_plot)
  } else {
    plot_triplet <- list(future_proj, comp_plot)
  }
  
  plot_list <- append(plot_list, plot_triplet)
  
  print(paste("Finished processing:", species, model, time_period, ssp))
}  

############################################################################
######################### 6. Create Supplementals ##########################
############################################################################
# Combine all plots
#all_plots <- c(list(grey_box, occ_plot), response_curves, list(current_proj), plot_list)
all_plots2 <- c(list(grey_box), response_curves, list(current_proj), plot_list)
all_plots2 <- Filter(function(x) inherits(x, "ggplot"), all_plots2)


pdf("SupplementalFigureTEST5.pdf", height = 30, width = 38)
grid::grid.draw(ggarrange(
  plotlist = all_plots2, 
  ncol = 6, 
  nrow = 7, 
  labels = generate_custom_labels(length(all_plots2))
))
dev.off()

