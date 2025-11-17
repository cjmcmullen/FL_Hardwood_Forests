## FL Hardwood Forests Species Unfilling Plots
## 2-6-2025 by Sydney Barfus

library(ggplot2)
library(dplyr)

setwd("/blue/soltis/share/FL_HardwoodForests/")


## get data, filter NA, 0, or out of range(input error) years
alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldf_V4.csv")

species <- "Vsororia"

species_df <- dplyr::filter(alldf, accepted_name == species)

species_df2 <- species_df %>%
  filter(year != 0.00) %>% 
  filter(!is.na(year)) %>% 
  filter(year < 2025) %>%
  filter(year > 1200)


## set basis of records to matching formats
species_df2$basisOfRecord[species_df2$basisOfRecord == "Human Observation"]  <- "HUMAN_OBSERVATION"
species_df2$basisOfRecord[species_df2$basisOfRecord == "HumanObservation"]   <- "HUMAN_OBSERVATION"
species_df2$basisOfRecord[species_df2$basisOfRecord == "OBSERVATION"]        <- "HUMAN_OBSERVATION"

species_df2$basisOfRecord[species_df2$basisOfRecord == "FossilSpecimen"]     <- "FOSSIL_SPECIMEN"

species_df2$basisOfRecord[species_df2$basisOfRecord == "MATERIAL_CITATION"]  <- "MATERIAL_SAMPLE"

species_df2$basisOfRecord[species_df2$basisOfRecord == "Occurence"]          <- "OCCURENCE"

species_df2$basisOfRecord[species_df2$basisOfRecord == "Preserved specimen"] <- "PRESERVED_SPECIMEN"
species_df2$basisOfRecord[species_df2$basisOfRecord == "Preserved Specimen"] <- "PRESERVED_SPECIMEN"
species_df2$basisOfRecord[species_df2$basisOfRecord == "preservedspecimen"]  <- "PRESERVED_SPECIMEN"
species_df2$basisOfRecord[species_df2$basisOfRecord == "Preserved"]          <- "PRESERVED_SPECIMEN"
species_df2$basisOfRecord[species_df2$basisOfRecord == "PreservedSpecimen"]  <- "PRESERVED_SPECIMEN"

## find global linear regression line statistics
lm_model <- lm(lat ~ year, data = species_df2)
model_summary <- summary(lm_model)

slope <- coef(lm_model)[2]
intercept <- coef(lm_model)[1]
r_squared <- model_summary$r.squared


# Create the plot
ggplot(species_df2, aes(x=year, y=lat, color = basisOfRecord)) +
  
  geom_point(alpha = 0.8, size = 0.3) +
  
  geom_smooth(method = "lm", aes(group = basisOfRecord), se = FALSE, size = 0.7,
              show.legend = FALSE) +
  geom_smooth(method = "lm", aes(group = 1, color = "Global Regression Line"),
              se = FALSE, size = 0.9, linetype = "solid", show.legend = TRUE) +
  
  scale_color_manual(name = "Basis of Record",
                     values = c("HUMAN_OBSERVATION" = "blue",
                                "PRESERVED_SPECIMEN" = "red",
                                "MATERIAL_SAMPLE" = "green",
                                "FOSSIL_SPECIMEN" = "yellow",
                                "OCCURRENCE" = "orange",
                                "Global Regression Line" = "black")) +
  
  ggtitle(expression(paste(italic("Viola sororia"), " Latitude Over Time"))) +
  xlab("Year") +
  ylab("Latitude") +
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  
  # Annotating the regression statistics in the bottom left corner
  annotate("text", x = min(species_df2$year), y = min(species_df2$lat) + 0.5,
           label = paste0("Slope: ", round(slope, 3), "\n",
                         "Intercept: ", round(intercept, 3), "\n",
                         "R-squared: ", round(r_squared, 3)),
           size = 4, color = "black", hjust = 0, vjust = 0)



## save plot 
ggsave(paste0(species, "_Lat_v_Year.png"), plot = last_plot(), path = "./09_Niche_Unfilling")

