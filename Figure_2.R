## table 1
# Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Set working directory
setwd("/blue/soltis/share/FL_HardwoodForests/07C_ProjectionCompairsons/") 

# Total number of pixels in Hardwood forests (used as denominator)
total_hardwood_pixels <- 9158

# List all relevant files
files <- list.files(pattern = "_Comparison.csv$", recursive = TRUE, full.names = TRUE)

## Lookup table for full names
fullname_lookup <- c(
  Acanadensis = "Aquilegia canadensis",
  Aplatyneuron = "Asplenium platyneuron",
  Asaccharum = "Acer saccharum ssp. floridanum",
  Aspinosa = "Aralia spinosa",
  Ccanadensis = "Cercis canadensis",
  Ccaroliniana = "Carpinus caroliniana",
  Cdasycarpa = "Carex dasycarpa",
  Cflorida = "Cornus florida",
  Cfloridus = "Calycanthus floridus",
  Cglabra = "Carya glabra",
  Claevigata = "Celtis laevigata",
  Claxum = "Chasmanthium laxum",
  Eamericanus = "Euonymus americanus",
  Famericana = "Fraxinus americana",
  Fgrandifolia = "Fagus grandifolia",
  Harifolia = "Asarum arifolium",
  Hdiptera = "Halesia diptera",
  Iopaca = "Ilex opaca",
  Lstyraciflua = "Liquidambar styraciflua",
  Mfloridana = "Matelea floridana",
  Mgrandiflora = "Magnolia grandiflora",
  Mrepens = "Mitchella repens",
  Mreynoldsiae = "Monotropsis reynoldsiae",
  Ovirginiana = "Ostrya virginiana",
  Pborbonia = "Persea borbonia",
  Pcaroliniana = "Prunus caroliniana",
  Pglabra = "Pinus glabra",
  Pquinquefolia = "Parthenocissus quinquefolia",
  Pserotina = "Prunus serotina",
  Ptaeda = "Pinus taeda",
  Qalba = "Quercus alba",
  Qhemisphaerica = "Quercus hemisphaerica",
  Qmichauxii = "Quercus michauxii",
  Qvirginiana = "Quercus virginiana",
  Spumila = "Smilax pumila",
  Stinctoria = "Symplocos tinctoria",
  Tamericana = "Tilia americana",
  Ualata = "Ulmus alata",
  Vsororia = "Viola sororia"
)

# Extract data from each file
extract_values <- function(file) {
  filename <- basename(file)
  df <- read_csv(file, show_col_types = FALSE)
  
  # Extract metadata from filename
  parts <- str_split(filename, "_")[[1]]
  species <- parts[1]
  
  # Skip "Erepens" species (removed after comparisons made)
  if (species == "Erepens") {
    return(NULL)
  }
  
  model <- parts[4]
  time <- parts[5]
  ssp <- parts[6]
  
  scenario <- paste(model, time, ssp, sep = "_")
  
  # Calculate % of current suitable habitat
  current <- df$CurrentRangeSize[1]
  current_perc <- round((current / total_hardwood_pixels) * 100, 2)
  
  # Get species full name from lookup table
  full_name <- fullname_lookup[species]
  if (is.na(full_name)) {
    warning(paste("Species code not found in lookup table:", species))
  }
  
  tibble(
    Species = species,
    FullName = full_name,
    Scenario = scenario,
    SpeciesRangeChange = df$SpeciesRangeChange[1],
    Current_Habitat_Hardwood_Perc = current_perc
  )
}

# Apply across all files
all_data <- bind_rows(lapply(files, extract_values))

# Keep one value per species for current % suitability
current_vals <- all_data %>%
  dplyr::select(Species, FullName, Current_Habitat_Hardwood_Perc) %>%
  distinct()

# Pivot wider to have one column per scenario
wide_change <- all_data %>%
  dplyr::select(FullName, Scenario, SpeciesRangeChange) %>%
  pivot_wider(names_from = Scenario, values_from = SpeciesRangeChange)

# Join together
final_table <- left_join(current_vals, wide_change, by = "FullName")

# View result
print(final_table)

# Optional: write to CSV
write_csv(final_table, "01_Hardwood_Habitat_Summary_Perc.csv")

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Assuming `final_table` is already created

# Reshape to long format
heatmap_data <- final_table %>%
  pivot_longer(
    cols = -c(Species, FullName, Current_Habitat_Hardwood_Perc),
    names_to = "Scenario",
    values_to = "SpeciesRangeChange"
  )

# Modify species names to include current %
heatmap_data <- heatmap_data %>%
  mutate(FullName = paste0(FullName, " (", Current_Habitat_Hardwood_Perc, "%)"))


# Process species names: remove underscores, italicize, and order A-Z with A at top
heatmap_data <- heatmap_data %>%
  #mutate(Species = gsub("_", " ", Species)) %>%
  mutate(FullName = factor(FullName, levels = sort(unique(FullName), decreasing = TRUE)))  # A at top

# Plot
ggplot(heatmap_data, aes(x = Scenario, y = FullName, fill = SpeciesRangeChange)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(SpeciesRangeChange, 1)), size = 3) +
  scale_fill_gradient2(
    low = "#FDE725",
    mid = "white",
    high = "#6b2e8a",
    midpoint = 0,
    guide = "none"  # removes legend
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(face = "italic"),
    panel.grid = element_blank()
  ) +
  labs(
    x = "Climate Scenario",
    y = "Species (Current Suitability)",
    title = "Projected % Change in Habitat Suitability in Florida Hardwoods",
    subtitle = "Yellow = habitat loss; Purple = habitat gain"
  )

ggsave(
  filename = "/blue/soltis/share/FL_HardwoodForests/Fig2_Habitat_Suitability_HeatmapV2.png",
  plot = last_plot(),        # or replace with your plot object if named
  width = 12,                # in inches
  height = 10,
  dpi = 300                  # high resolution for publication
)
