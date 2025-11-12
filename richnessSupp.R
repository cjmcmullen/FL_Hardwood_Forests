### Species Richness 
## script by ME Mabry, Sydney Barfus
library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(ggnewscale)
library(ggpubr)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

# Set working directory
setwd("/blue/soltis/share/FL_HardwoodForests/")

# loading plotting function
generate_custom_labels <- function(n) {
  
  # G-R as triplets
  letters <- LETTERS[1:12]  # A to L
  labels <- unlist(lapply(letters, function(x) c(x, paste0(x, "'"))))
  
# Return only as many as needed
  if (n > length(labels)) {
    stop("Too many plots: exceeds available custom labels.")
  }
  
  labels[1:n]
}

current_rast <- rast("11_SpeciesRichness/current_binary_SpeciesRichness.asc")

# Define model assignment (ACCESS and GISS alternating)
models <- c("ACCESS-CM2", "GISS-E2-1-G")
time_periods <- c("2041-2060", "2081-2100")
ssps <- c("ssp245", "ssp370", "ssp585")

#create blank list for plots
all_plots <- list()

for (period in time_periods) {
  for (ssp in ssps) {
    for (model in models) {
      
      # FUTURE ------------------------------------------------------------------
      
      # Load the species richness raster
      richness_rast <- rast(paste0("11_SpeciesRichness/", model, "_", period, "_", ssp, "_binary_SpeciesRichness.asc"))
      
      # Convert species raster to data frame
      richness_df <- as.data.frame(richness_rast, xy = TRUE, na.rm = TRUE)
      colnames(richness_df) <- c("x", "y", "richness")
      
      # Load Florida basemap
      states <- ne_states(country = "United States of America", returnclass = "sf")
      florida <- states %>% dplyr::filter(name == "Florida")
      
      # create future plot
      fut_plot <- ggplot() +
        geom_sf(data = states, fill = "grey90", color = "white") +
        geom_raster(data = richness_df, aes(x = x, y = y, fill = richness)) +
        scale_fill_viridis_c(name = "Species\nRichness", na.value = "transparent") +
        
        coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
        labs(title = "Future Species Richness", 
             subtitle = paste(model, period, ssp),
             x = "Longitude", y = "Latitude") +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          axis.line = element_line(color = "black", size = 0.3),
          axis.ticks = element_line(color = "black", size = 0.3),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 10),
          legend.title = element_text(size = 8, face = "bold"),
          legend.text = element_text(size = 6))
      
      ggsave(paste0("11_SpeciesRichness/02_Species_Richness_Plots/", model, "_",
                    period, "_", ssp, "_Richness_Comparison_Plot.png"),
             fut_plot, width = 8, height = 6, dpi = 300)
      
      all_plots[[length(all_plots) + 1]] <- fut_plot
      
      
      # COMPARISON --------------------------------------------------------------
      
      # Calculate change in species richness (future - current)
      richness_change <- richness_rast - current_rast
      
      # Classify the change values into categories
      richness_change_classified <- app(richness_change, function(x) {
        ifelse(is.na(x), NA,  # Keep NA values as NA
               ifelse(x < 0, -1,  # Species loss
                      ifelse(x == 0, 0,  # No change
                             ifelse(x > 0, 1, NA))))  # Species gain
      })
      
      # Count the number of species lost, no change, and gained
      species_loss_count <- global(richness_change_classified == -1, "sum", na.rm = TRUE)[1, 1]
      species_no_change_count <- global(richness_change_classified == 0, "sum", na.rm = TRUE)[1, 1]
      species_gain_count      <- global(richness_change_classified == 1, "sum", na.rm = TRUE)[1, 1]
      
      # Convert the classified raster to a data frame for plotting
      change_df <- as.data.frame(richness_change_classified, xy = TRUE) %>%
        rename(change = lyr.1) %>%
        filter(!is.na(change))  # Remove NA values for plotting
      
      # Create a plot with classified species richness change
      comp_plot <- ggplot() +
        geom_sf(data = states, fill = "grey90", color = "white") +
        geom_raster(data = change_df, aes(x = x, y = y, fill = factor(change))) +
        scale_fill_manual(values = c("-1" = "#FDE725FF", "0" = "#31688EFF", "1" = "#440154FF"),
                          labels = c(
                            paste("Loss (", species_loss_count, " pixels)", sep = ""),
                            paste("No Change (", species_no_change_count, " pixels)", sep = ""),
                            paste("Gain (", species_gain_count, " pixels)", sep = "")
                          ),
                          name = "Change") +
        
        coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
        theme_minimal() +
        labs(title = "Change in Species Richness",
             subtitle = paste("Current vs", model, period, ssp), 
             x = "Longitude", y = "Latitude") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          axis.line = element_line(color = "black", size = 0.3),
          axis.ticks = element_line(color = "black", size = 0.3),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 10),
          legend.title = element_text(size = 8, face = "bold"),
          legend.text = element_text(size = 6))
          # legend.position = "bottom",
          # legend.box = "vertical")
      
      ggsave(paste0("11_SpeciesRichness/02_Species_Richness_Plots/", model, "_",
                    period, "_", ssp, "_Richness_Comparison_Plot.png"),
             comp_plot, width = 8, height = 6, dpi = 300)
      
      all_plots[[length(all_plots) + 1]] <- comp_plot
      
      print(paste(model, period, ssp, "Completed."))

}}}


# CREATE FIGURE -----------------------------------------------------------

final_plot <- ggarrange(plotlist = all_plots, 
                        ncol = 4, 
                        nrow = 6,
                        labels = generate_custom_labels(length(all_plots)))

ggsave("12_SupplementalFigures/042_SpeciesRichness_Supp.png",  bg = "white", plot = final_plot, width = 28, height = 30, dpi = 300)

