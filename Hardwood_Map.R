library(leaflet)
library(sf)
library(htmlwidgets)
library(webshot)


# setwd 
setwd("/blue/soltis/share/FL_HardwoodForests/02_rasters/FL_HardwoodForest_Shapefile/")

# Load the shapefile
shape_data <- st_read("vcom67_Hardwood.shp")

# Ensure the shapefile is in the WGS 84 CRS (EPSG: 4326)
shape_data <- st_transform(shape_data, crs = 4326)

map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addProviderTiles("CartoDB.PositronOnlyLabels") %>%
        addPolygons(data = shape_data, color = "darkgreen", weight = 3, fill = "darkgreen") %>%
        setView(lng = -84, lat = 29, zoom = 7)  # Set the center of Florida with an appropriate zoom level

map

# Save the map as an HTML file
saveWidget(map, "HardwoodMap.html")

#save map
webshot("HardwoodMap.html", "Hardwood_Map.png")

