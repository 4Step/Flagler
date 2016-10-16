# Script to get corridor routes from GTFS network

# shapes.txt (contains stop location points)
# trips.txt (contains route_id and shape_id)
# routes.txt (contains route_ids)

# User Inputs
gtfsShapesFile <- "GTFS/miami-dade-transit_20150626_0146/shapes.txt"
gtfsTripsFile <- "GTFS/miami-dade-transit_20150626_0146/trips.txt"
gtfsRoutesFile <- "GTFS/miami-dade-transit_20150626_0146/routes.txt"
routeName <- "flagler"

gtfsShapes <- read.csv(gtfsShapesFile)
gtfsTrips <- read.csv(gtfsTripsFile)
gtfsRoutes <- read.csv(gtfsRoutesFile)

# Get Flagler shape point from above three files
flaglerShape <- gtfsShapes %>%   
                left_join(gtfsTrips, by = "shape_id") %>%
                left_join(gtfsRoutes, by = "route_id") %>%
                filter(grepl(routeName, route_long_name, ignore.case = TRUE)) %>%
                select (route_id, shape_id, route_long_name, route_color, 
                        shape_pt_lat, shape_pt_lon, shape_pt_sequence, 
                        shape_dist_traveled) %>%
                distinct()

# Generate from and to data points from shape points
flaglerShape <- flaglerShape %>% group_by(shape_id) %>% 
                rename(from_lat = shape_pt_lat, from_lon = shape_pt_lon) %>%
                mutate(to_lat = lead(from_lat, 1),
                       to_lon = lead(from_lon, 1)) %>%
                ungroup() %>%
                filter(!is.na(to_lat))

 total_shapes <- unique(flaglerShape$shape_id)
 factpal <- colorFactor(topo.colors(8), flaglerShape$shape_id)
 groupColors <- colorRampPalette(c("blue", "orange","darkgreen", "red", "green", "white"))(8)

 # Map data
 map <- leaflet() %>%
   
   # Base Map
   addProviderTiles("Stamen.Toner",group = "Stamen") %>%
   addTiles(group = "OSM") %>%
   setView(lng = -80.1918, lat = 25.7617, zoom = 10) %>%
   addCircles(data = flaglerShape, lng = ~from_lon, lat = ~from_lat, 
               weight = 1, radius =100, color = ~factpal(shape_id)) 
 
  shapes <- c(1:3,7)
  # Convert each shape into spatial object and add to map
   for (i in shapes) {
     flaglerShape1 <- flaglerShape %>% filter(shape_id == total_shapes[i])

     # Convert this into shape data
     flaglerRoute <- gcIntermediate(
       #p1 = select(flaglerShape1, from_lon:from_lat),
       #p2 = select(flaglerShape1, to_lon:to_lat),
       p1 = select(flaglerShape1, to_lon:to_lat),
       p2 = select(flaglerShape1, from_lon:from_lat),
       sp = TRUE,
       addStartEnd = TRUE
     )

     flaglerRoute <- SpatialLinesDataFrame(flaglerRoute, flaglerShape1)

     map <- map %>% addTiles %>%
       addPolylines(
         data = flaglerRoute,
         lng = ~from_lon,
         lat = ~from_lat,
         group = ~unique(shape_id),
         weight = 5,
         color = groupColors
       )
    }

 map




  



