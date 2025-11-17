library(dplyr)

## get data
setwd("/blue/soltis/share/FL_HardwoodForests/")
alldf <- read.csv("01_data/FL_HardwoodForests_Clean/alldf_FL_hardwood_Cleaned_maxent.csv")


## set basis of records to matching formats
alldf$basisOfRecord[alldf$basisOfRecord == "Human Observation"]  <- "HUMAN_OBSERVATION"
alldf$basisOfRecord[alldf$basisOfRecord == "HumanObservation"]   <- "HUMAN_OBSERVATION"
alldf$basisOfRecord[alldf$basisOfRecord == "OBSERVATION"]        <- "HUMAN_OBSERVATION"

alldf$basisOfRecord[alldf$basisOfRecord == "FossilSpecimen"]     <- "FOSSIL_SPECIMEN"

alldf$basisOfRecord[alldf$basisOfRecord == "MATERIAL_CITATION"]  <- "MATERIAL_SAMPLE"

alldf$basisOfRecord[alldf$basisOfRecord == "Occurence"]          <- "OCCURENCE"

alldf$basisOfRecord[alldf$basisOfRecord == "Preserved specimen"] <- "PRESERVED_SPECIMEN"
alldf$basisOfRecord[alldf$basisOfRecord == "Preserved Specimen"] <- "PRESERVED_SPECIMEN"
alldf$basisOfRecord[alldf$basisOfRecord == "preservedspecimen"]  <- "PRESERVED_SPECIMEN"
alldf$basisOfRecord[alldf$basisOfRecord == "Preserved"]          <- "PRESERVED_SPECIMEN"
alldf$basisOfRecord[alldf$basisOfRecord == "PreservedSpecimen"]  <- "PRESERVED_SPECIMEN"

unique(alldf$basisOfRecord)

# write out with correct BoR
# write.csv(alldf, "./01_data/FL_HardwoodForests_Clean/alldfBOR.csv")


## Remove Erepens from alldf
alldf <- alldf %>% filter(accepted_name != "Erepens")
# write.csv(filtered_df, file = "./01_data/FL_HardwoodForests_Clean/alldfBOR_V2.csv")

## Remove Ohirtellus from alldf
alldf <- alldf %>% filter(accepted_name != "Ohirtellus")
unique(alldf$accepted_name)

## taxonomic name used changed from H.arifolia to A.arifolium
alldf <- alldf %>%
  mutate(accepted_name = ifelse(accepted_name == "Harifolia", "Aarifolium", accepted_name))

## Add full name column to alldf
# Create the mapping
  name_mapping <- c(
    "Acanadensis" = "Aquilegia canadensis",
    "Aplatyneuron" = "Asplenium platyneuron",
    "Asaccharum" = "Acer saccharum ssp. floridanum",
    "Aspinosa" = "Aralia spinosa",
    "Aarifolium" = "Asarum arifolium",
    "Ccanadensis" = "Cercis canadensis",
    "Ccaroliniana" = "Carpinus caroliniana",
    "Cdasycarpa" = "Carex dasycarpa",
    "Cflorida" = "Cornus florida",
    "Cfloridus" = "Calycanthus floridus",
    "Cglabra" = "Carya glabra",
    "Claevigata" = "Celtis laevigata",
    "Claxum" = "Chasmanthium laxum",
    "Eamericanus" = "Euonymus americanus",
    "Famericana" = "Fraxinus americana",
    "Fgrandifolia" = "Fagus grandifolia",
    "Harifolia" = "Hexastylis arifolia",
    "Hdiptera" = "Halesia diptera",
    "Iopaca" = "Ilex opaca",
    "Lstyraciflua" = "Liquidambar styraciflua",
    "Mfloridana" = "Matelea floridana",
    "Mgrandiflora" = "Magnolia grandiflora",
    "Mrepens" = "Mitchella repens",
    "Mreynoldsiae" = "Monotropsis reynoldsiae",
    "Ohirtellus" = "Oplismenus hirtellus",
    "Ovirginiana" = "Ostrya virginiana",
    "Pborbonia" = "Persea borbonia",
    "Pcaroliniana" = "Prunus caroliniana",
    "Pglabra" = "Pinus glabra",
    "Pquinquefolia" = "Parthenocissus quinquefolia",
    "Pserotina" = "Prunus serotina",
    "Ptaeda" = "Pinus taeda",
    "Qalba" = "Quercus alba",
    "Qhemisphaerica" = "Quercus hemisphaerica",
    "Qmichauxii" = "Quercus michauxii",
    "Qvirginiana" = "Quercus virginiana",
    "Spumila" = "Smilax pumila",
    "Stinctoria" = "Symplocos tinctoria",
    "Tamericana" = "Tilia americana",
    "Ualata" = "Ulmus alata",
    "Vsororia" = "Viola sororia"
)

# Add the 'full_name' column using the mapping
alldf <- alldf %>%
  mutate(full_name = name_mapping[accepted_name]) %>%
  arrange(full_name)
unique(alldf$full_name)

write.csv(alldf, file = "./01_data/FL_HardwoodForests_Clean/alldf_V4.csv")
