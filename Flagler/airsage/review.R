# load libraries
library(tidyr)
library(dplyr)
library(knitr)
library(leaflet)
library(rgdal)
library(htmltools)
library(readr)
library(ggplot2)
library(geosphere)

library(maptools)    # SpatialPolygons2PolySet
library(PBSmapping)  # calcCentroid


# list files
csvFiles <- dir(getwd(), pattern = ".csv")

# see contents of the files
for (f in 1:length(csvFiles)){
  print(head(read.csv(csvFiles[f])))
}

# Add project shape files
shape <- readOGR("airsarge_districts/TAZs.shp", layer = "TAZs", verbose = FALSE)

# Add  color pallets
pal_blue <- colorNumeric(
  palette = "Blues",
  domain = shape$HHDEN
)

pal_red <- colorNumeric(
  palette = "Reds",
  domain = shape$POPDEN
)

pal_fact <- colorFactor(topo.colors(6), shape$PlnArea)

leaflet() %>%
  # Base Map
  addProviderTiles("Stamen.Toner",group = "Stamen") %>%
  addTiles(group = "OSM") %>%
  setView(lng = -80.1918, lat = 25.7617, zoom = 10) %>%
  
  # add district shape files
  addTiles %>%
  addPolygons(
    data = shape, group = "Districts", smoothFactor = 0.5,
    stroke = TRUE,  weight = 3, color = "grey",opacity = 0.5,
    fill = TRUE, fillOpacity = 0.5,
    fillColor = ~pal_fact(PlnArea), 
    popup = ~paste(sep = "<br/>",
                   htmlEscape(paste("District = ",as.character(PlnArea),sep="")),
                   htmlEscape(paste("TAZ = ",as.character(TAZ),sep="")),
                   htmlEscape(paste("AREA = ",as.character(SQMILE), sep="")),
                   htmlEscape(paste("Population =", as.character(TOTPOP), sep="")),
                   htmlEscape(paste("Households =", as.character(TOTHH), sep="")),
                   htmlEscape(paste("Workers =", as.character(TOTWORK), sep="")),
                   htmlEscape(paste("Pop density =", as.character(POPDEN), sep="")),
                   htmlEscape(paste("HH density =", as.character(HHDEN), sep="")),
                   htmlEscape(paste("Work density =", as.character(WORKDEN), sep=""))
    )
  )  %>%
  addPolygons(
    data = shape, group = "Households", smoothFactor = 0.5,
    stroke = TRUE,  weight = 3, color = "grey",opacity = 0.5,
    fill = TRUE, fillOpacity = 0.5,
    fillColor = ~pal_blue(HHDEN)
  )  %>%  
  addPolygons(
    data = shape, group = "Pop Density",smoothFactor = 0.5,
    stroke = TRUE,  weight = 3, color = "grey",opacity = 0.5,
    fill = TRUE, fillOpacity = 0.5,
    fillColor = ~pal_red(POPDEN)
  )  %>%
    
  addLegend(
    "bottomright",pal = pal_blue, values = shape$HHDEN,
     title = "Households"
   ) %>%
  
  addLegend(
    "bottomleft",pal = pal_red, values = shape$POPDEN,
    title = "Population", layerId = 1
  ) %>% 
  
 addLayersControl(
  baseGroups = c("OSM","Stamen"),
  overlayGroups = c("Districts", "Households", "Pop Density"),
  options = layersControlOptions(collapsed = FALSE)
  #layerId = 1
 ) %>%

hideGroup(c( "Households","Pop Density"))
   

centroids <- getSpPPolygonsLabptSlots(shape)









