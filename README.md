# FL_Hardwood_Forests
Scripts used for FL Hardwood Forests at the University of Florida (ECOLOGICAL NICHE MODELS REVEAL CLIMATE THREATS TO FLORIDA’S HARDWOOD FOREST BIODIVERSITY)

## Contents Overview
A suite of R and shell scripts, organized sequentially to support the complete workflow from raw data acquisition and cleaning through modeling and visualization:

- 01_FL_HardwoodForests_Download_Records.R
- 02_FL_HardwoodForests_taxonomic_names.R
- 03A_FL_HardwoodForests_Record_cleaning.R
- 03B_FL_HardwoodForests_EndemicRecords.R
- 03C_FL_HardwoodForests_Record_cleaning.R
- 04_FL_HardwoodForests_SpeciesMaps.R
- 05B_FL_HardwoodForests_Pearsons.R
- 05_FL_HardwoodForests_SpeciesEnvironmentalVariables.R
- 06_FL_HardwoodForests_Ecological_Niche_Modeling.R
- 07B_HardwoodForests_GetHardwoodSuitability.R
- 07_FL_HardwoodForests_Projections.R
- 08_FL_HardwoodForests_FutureProjections.R
- 09_FL_HardwoodForests_ProjectionComparisons.R
- 10_FL_HardwoodForests_ETF_Projections.R
- 10_FL_HardwoodForests_US_Projections.R
- 11_FL_HardwoodForests_UnfillingPlots.R
- 12_FL_HardwoodForests_Mapping.R
- 13_FL_HardwoodForests_ManagementMaps.R
- 14_FL_HardwoodForests_LatitudeCounts.R
- 15_FL_HardwoodForests_SpeciesRichness.R
- CopyOf12_FL_HardwoodForests_Mapping.R
- Fig2.R
- Fig3.R
- Fig5_SpeciesRichness_SeaLevel.R
- HardwoodMap.R
- alldf_Edits.R
- richnessSupp.R
- suppFigs_Plotting.R

## Purpose
This repository provides a structured, reproducible pipeline for:

- Downloading occurrence records for Florida hardwood forest flora,
- Standardizing taxonomic names,
- Cleaning and filtering georeferencing errors or duplicates,
- Mapping species distributions,
- Calculating environmental variables (including VIF and Pearson correlation analyses to guide variable selection),
- Modeling current and future species distributions via ecological niche models,
- Projecting changes under future climates,
- Mapping species richness and generating key figures for interpretation.
- This project was developed as part of a Course-Based Undergraduate Research Experience (CURE) at the University of Florida.

## Prerequisites
- R (≥ 4.x recommended)
- Required R packages, such as (but not limited to): dplyr, tidyr, sf, raster, sp, usdm, biomod2, ggplot2.
- Shell environment for Unix/Linux/macOS to run .sh scripts (Bash-compatible)

## Output
This pipeline produces:

- Cleaned occurrence datasets,
- Species distribution maps,
- Environmental variable correlation diagnostics,
- Ecological niche model outputs (current & future),
- Comparative projections (e.g., differences between current and future ranges),
- Species richness layers,
- Figures for publication-ready interpretation,
- Supplementary materials.

## Organization & Naming Conventions
- Numerical prefixes (e.g., 01_, 02_) ensure execution in intended sequence.
- Shell scripts (.sh) provide a convenient wrapper for running R scripts while tracking execution steps.
- R scripts are modular, each performing a distinct analytical task (e.g., data download, cleaning, mapping, modeling).
- FigX_.R scripts generate final visual outputs; supplementary scripts (supp) produce ancillary figures and tables.

## Contributions & Acknowledgments
Developed for the Spring 2024 Florida Hardwood Forests CURE at the University of Florida. Contributions are welcome following academic use and appropriate citation.

## Contact
For questions, suggestions, or clarifications, please contact Makenzie via GitHub or through institutional channels.
