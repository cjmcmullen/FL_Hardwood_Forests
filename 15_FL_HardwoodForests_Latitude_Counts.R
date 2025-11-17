## FL Hardwood Forests Species Unfilling Plots
## 3-27-2025 by Sydney Barfus

library(ggplot2)
library(dplyr)
library(RColorBrewer)

setwd("/blue/soltis/share/FL_HardwoodForests/")



# Faceted min and max latitude --------------------------------------------

## Load dataset
alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldf_V4.csv")

## Filter out invalid years
alldf2 <- alldf %>%
  filter(year != 0.00, !is.na(year), year < 2025, year > 1200) %>%
  filter(!(accepted_name == "Aplatyneuron" & lat < 0)) ## remove occurrences in South Africa

## find min/max latitude for each species
species_summary <- alldf2 %>%
  # mutate(year_group = floor(year / 5) * 5) %>%  # Group years into 5-year intervals)
  group_by(accepted_name, full_name, year) %>% 
  summarise(
    max_lat = max(lat, na.rm = TRUE),
    min_lat = min(lat, na.rm = TRUE),
    .groups = "drop"
  )

species_summary$full_name <- factor(species_summary$full_name)
levels(species_summary$full_name) <- paste0("italic('", levels(species_summary$full_name), "')")

## Create faceted plot
faceted_plot <- ggplot(species_summary, aes(x = year)) +
  geom_line(aes(y = max_lat, color = "Max Latitude"), size = 2) +
  geom_line(aes(y = min_lat, color = "Min Latitude"), size = 2) +
  geom_smooth(aes(y = max_lat), color = "darkred", method = "loess", se = FALSE) +
  geom_smooth(aes(y = min_lat), color = "darkblue", method = "loess", se = FALSE) +
  scale_color_manual(values = c("Max Latitude" = "red", "Min Latitude" = "blue")) +
  labs(title = "Minimum and Maximum Latitude Over Time",
       x = "Year",
       y = "Latitude",
       color = "Latitude") +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 10)) +
  scale_x_continuous(
    breaks = seq(
      from = floor(min(species_summary$year, na.rm = TRUE) / 25) * 25,
      to = ceiling(max(species_summary$year, na.rm = TRUE) / 25) * 25,
      by = 25)) +
  facet_wrap(~ full_name, ncol = 1, scales = "free_y", labeller = label_parsed)  # Facet by species

## Save the faceted plot
ggsave("041_Niche_Unfilling_Supp(ungrouped-final).png", plot = faceted_plot, 
       path = "./12_SupplementalFigures", units= "in", height = 40, width = 11)

print(faceted_plot)



# Figure Plot - 2 species -----------------------------------------------------

## specify species
species_list <- c("Hdiptera", "Tamericana")

species_df <- alldf2 %>%
  filter(accepted_name %in% species_list)

## find min/max latitude for each species
species_summary <- species_df %>%
  mutate(year_group = floor(year / 5) * 5) %>%  # Group years into 5-year intervals)
  group_by(accepted_name, full_name, year_group) %>% 
  summarise(
    max_lat = max(lat, na.rm = TRUE),
    min_lat = min(lat, na.rm = TRUE),
    .groups = "drop"
  )

species_summary$full_name <- factor(species_summary$full_name)
levels(species_summary$full_name) <- paste0("italic('", levels(species_summary$full_name), "')")

## Create plot
two_species_figure <- ggplot(species_summary, aes(x = year_group)) +
  geom_line(aes(y = max_lat, color = "Max Latitude"), size = 2) +
  geom_line(aes(y = min_lat, color = "Min Latitude"), size = 2) +
  geom_smooth(aes(y = max_lat), color = "darkred", method = "loess", se = FALSE) +
  geom_smooth(aes(y = min_lat), color = "darkblue", method = "loess", se = FALSE) +
  scale_color_manual(values = c("Max Latitude" = "red", "Min Latitude" = "blue")) +
  labs(title = "Minimum and Maximum Latitude Over Time",
       x = "Year (Grouped by 5)",
       y = "Latitude",
       color = "Latitude") +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 10)) +
  scale_x_continuous(
    breaks = seq(
      from = floor(min(species_summary$year_group, na.rm = TRUE) / 25) * 25,
      to = ceiling(max(species_summary$year_group, na.rm = TRUE) / 25) * 25,
      by = 25)) +
  facet_wrap(~ full_name, ncol = 1, scales = "free_y", labeller = label_parsed)  # Facet by species

## Save the faceted plot
ggsave("Fig4_MinMax_Latitude(grouped).png", plot = two_species_figure, 
       path = "./14_Figures", units= "in", height = 6, width = 11)


# # Latitude Counts per Year ------------------------------------------------
# 
# # find counts for each year and latitude
# species_summary <- species_df %>%
#   mutate(lat_degree = floor(lat)) %>%  # Round latitude down to the nearest whole number
#   group_by(year, lat_degree) %>%       # group by year and latitude
#   summarise(count = n(), .groups = 'drop') %>%  # count occurrences
#   arrange(year, lat_degree)  # arrange for readability
# 
# ## create red to blue color palette
# num_colors <- length(unique(species_summary$lat_degree))
# custom_palette <- colorRampPalette(c("darkred", "red", "purple", "blue", "darkblue"))(num_colors)
# 
# # plot data with line for each latitude degree
# original_plot <- ggplot(species_summary, aes(x = year, y = count, color = as.factor(lat_degree))) +
#   geom_line() +
#   scale_color_manual(values = custom_palette) +
#   labs(title = bquote(italic(.(fullname)) ~ "Species Occurences by Year and Latitude (1925-2024)"),
#        x = "Year",
#        y = "Count",
#        color = "Degrees Latitude") +
#   theme_minimal() + 
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "white", color = NA), # Set white background
#     plot.background = element_rect(fill = "white", color = NA),  # Ensure entire plot stays white
#     axis.line = element_line(colour = "black"),
#     axis.text = element_text(size = 8),
#     axis.title = element_text(size = 8),
#     plot.title = element_text(size = 10)) +
#   scale_x_continuous(breaks = c(seq(1925, 2024, by = 25), 2024), limits = c(1925, 2024)) +
#   scale_y_continuous(breaks = seq(0, max(species_summary$count, na.rm = TRUE), by = 50))
# 
# print(original_plot)
# 
# ## create plot using log of count
# log_plot <- ggplot(species_summary, aes(x = year, y = count, color = as.factor(lat_degree))) +
#   geom_line() +
#   scale_color_manual(values = custom_palette) +
#   labs(title = bquote(italic(.(fullname)) ~ "Logarithmic Species Occurences by Year and Latitude (1925-2024)"),
#        x = "Year",
#        y = "Log(Count)",
#        color = "Degrees Latitude") +
#   theme_minimal() + 
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "white", color = NA), # Set white background
#     plot.background = element_rect(fill = "white", color = NA),  # Ensure entire plot stays white
#     axis.line = element_line(colour = "black"),
#     axis.text = element_text(size = 8),
#     axis.title = element_text(size = 8),
#     plot.title = element_text(size = 10)) +
#   scale_x_continuous(breaks = c(seq(1925, 2024, by = 25), 2024), limits = c(1925, 2024)) +
#   scale_y_log10()  # Apply log transformation to the y-axis
# 
# print(log_plot)
# 
# ## create directory and save maps
# #dir.create("./10_Maps/02_Niche_Unfilling")
# ggsave(paste0(species, "_NicheUnfillingTEST.png"), plot = original_plot, 
#        path = "./10_Maps/02_Niche_Unfilling/", units= "in", height = 7.5, width = 11)
# ggsave(paste0(species, "_NicheUnfillingLOG_TEST.png"), plot = log_plot, 
#        path = "./10_Maps/02_Niche_Unfilling/", units= "in", height = 7.5, width = 11)
# 
# 
# 
# # PLOT ALLDF -------------------------------------------------------------
# 
# ## filter NA, 0, or out of range (input error) years
# alldf2 <- alldf %>%
#   filter(year != 0.00) %>% 
#   filter(!is.na(year)) %>% 
#   filter(year < 2025) %>%
#   filter(year > 1200)
# 
# # find counts for each year and latitude
# alldf_summary <- alldf2 %>%
#   filter(year >= 1925 & year <= 2024) %>%  # only years 1925 to 2024
#   mutate(lower_bound = floor(lat / 5) * 5 + 1,
#          upper_bound = floor(lat / 5) * 5 + 5,    
#          lat_range = paste(lower_bound, "-", upper_bound)) %>% 
#   group_by(year, lat_range, lower_bound) %>%      # group by year, range, and lower_bound
#   summarise(count = n(), .groups = 'drop') %>%     # count occurrences
#   mutate(lat_range = factor(lat_range,            # order the factor by the numeric lower_bound
#                             levels = unique(lat_range[order(lower_bound)]))) %>%
#   arrange(year, lower_bound)
# 
# 
# 
# ## create red to blue color palette
# num_colors <- length(unique(alldf_summary$lat_range))
# custom_palette <- colorRampPalette(c("darkred", "red", "purple", "blue", "darkblue"))(num_colors)
# 
# ## create plot of counts per year and latitude
# alldf_original_plot <- ggplot(alldf_summary, aes(x = year, y = count, color = as.factor(lat_range))) +
#   geom_line() +
#   scale_color_manual(values = custom_palette) +
#   labs(title = "Occurences by Year and Latitude for All Species (1925-2024)",
#        x = "Year",
#        y = "Count",
#        color = "Degrees Latitude") +
#   theme_minimal() + 
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "white", color = NA), # Set white background
#     plot.background = element_rect(fill = "white", color = NA),  # Ensure entire plot stays white
#     axis.line = element_line(colour = "black"),
#     axis.text = element_text(size = 8),
#     axis.title = element_text(size = 8),
#     plot.title = element_text(size = 10)) +
#   scale_x_continuous(breaks = c(seq(1925, 2024, by = 25), 2024), limits = c(1925, 2024)) +
#   scale_y_continuous(breaks = seq(0, max(alldf_summary$count, na.rm = TRUE), by = 2000))
# 
# print(alldf_original_plot)
# 
# ## create plot using log of count
# alldf_log_plot <- ggplot(alldf_summary, aes(x = year, y = count, color = as.factor(lat_range))) +
#   geom_line() +
#   scale_color_manual(values = custom_palette) +
#   labs(title = "Logarithmic Species Occurences by Year and Latitude for All Species (1925-2024)",
#        x = "Year",
#        y = "Log(Count)",
#        color = "Degrees Latitude") +
#   theme_minimal() + 
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "white", color = NA), # Set white background
#     plot.background = element_rect(fill = "white", color = NA),  # Ensure entire plot stays white
#     axis.line = element_line(colour = "black"),
#     axis.text = element_text(size = 8),
#     axis.title = element_text(size = 8),
#     plot.title = element_text(size = 10)) +
#   scale_x_continuous(breaks = c(seq(1925, 2024, by = 25), 2024), limits = c(1925, 2024)) +
#   scale_y_log10()  # Apply log transformation to the y-axis
# 
# print(alldf_log_plot)
# 
# ## save plots
# ggsave("alldf_NicheUnfillingTEST.png", plot = alldf_original_plot, 
#        path = "./10_Maps/02_Niche_Unfilling/", units= "in", height = 7.5, width = 11)
# ggsave("alldf_NicheUnfillingLOG_TEST.png", plot = alldf_log_plot, 
#        path = "./10_Maps/02_Niche_Unfilling/", units= "in", height = 7.5, width = 11)
# 
# 
# 
# # Max and Min Latitudes ---------------------------------------------------
# 
# group years by 5 and find minimum and maximum latitude of each
# species_summary <- species_df %>%
#   mutate(year_group = floor(year / 5) * 5) %>%  # Group years into 5-year intervals
#   group_by(year_group) %>%
#   summarise(
#     max_lat = max(lat, na.rm = TRUE),
#     min_lat = min(lat, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # create plot
# grouped_year_plot <- ggplot(species_summary, aes(x = year_group)) +
#   geom_line(aes(y = max_lat, color = "Max Latitude"), size = 2) +
#   geom_line(aes(y = min_lat, color = "Min Latitude"), size = 2) +
#   geom_smooth(aes(y = max_lat), color = "darkred", method = "loess", se = FALSE) +
#   geom_smooth(aes(y = min_lat), color = "darkblue", method = "loess", se = FALSE) +
#   scale_color_manual(values = c("Max Latitude" = "red", "Min Latitude" = "blue")) +
#   labs(title = bquote(italic(.(full_name)) ~ "Minimum and Maximum Latitude Over Time"),
#        x = "Year (Grouped by 5)",
#        y = "Latitude",
#        color = "Latitude") +
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "white", color = NA), # Set white background
#     plot.background = element_rect(fill = "white", color = NA),  # Ensure entire plot stays white
#     axis.line = element_line(colour = "black"),
#     axis.text = element_text(size = 8),
#     axis.title = element_text(size = 8),
#     plot.title = element_text(size = 10)) +
#   scale_x_continuous(breaks = seq(min(species_summary$year_group), max(species_summary$year_group), by = 25))
# 
# print(grouped_year_plot)
# 
# ggsave(paste0(species, "_min_max_smooth.png"), plot = grouped_year_plot,
#        path = "./", units= "in", height = 2.5, width = 11)



# # Minimum Latitude Counts -------------------------------------------------
# 
# ## Load dataset
# alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldfBOR_V2.csv")
# 
# ## Filter out invalid years
# alldf2 <- alldf %>%
#   filter(year != 0.00, !is.na(year), year < 2025, year > 1200) %>%
#   filter(!(accepted_name == "Aplatyneuron" & lat < 0))
# 
# ## group by 5 years and find min latitude for each species
# species_summary <- alldf2 %>%
#   mutate(year_group = floor(year / 5) * 5) %>%  # Group years into 5-year intervals
#   group_by(accepted_name, year_group) %>% 
#   summarise(
#     min_lat = min(lat, na.rm = TRUE),
#     .groups = "drop"
#   ) 
#   
# species_counts <- alldf2 %>%
#   mutate(year_group = floor(year / 5) * 5) %>%  # Ensure the same grouping
#   inner_join(species_summary, by = c("accepted_name", "year_group")) %>%  # Join to get min_lat
#   filter(#basisOfRecord=="PRESERVED_SPECIMEN", 
#     lat >= min_lat & lat <= min_lat + 2) %>%  # Filter within the range
#   group_by(accepted_name, year_group) %>% 
#   summarise(count = n(), .groups = "drop")
# 
# min_latitude_bar_chart <- ggplot(species_counts, aes(x = factor(year_group), y = count)) +
#   geom_bar(stat = "identity") +  # Bar chart
#   facet_wrap(~ accepted_name, ncol = 1, scales = "free_y") +  # Facet by species
#   labs(title = "Species Count by Year Group",
#        x = "Year Group",
#        y = "Count") +
#   theme_minimal() + 
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "white", color = NA),
#     plot.background = element_rect(fill = "white", color = NA),
#     axis.line = element_line(colour = "black"),
#     axis.text = element_text(size = 8),
#     axis.title = element_text(size = 8),
#     plot.title = element_text(size = 10),
#     axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for clarity
#   )
# 
# ggsave("min_latitude_bar_chart.png", plot = min_latitude_bar_chart, 
#        path = "./10_Maps/02_Niche_Unfilling/", units= "in", height = 40, width = 11)
