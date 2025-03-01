# get cities from coordinates using shape file
#install.packages("sf")
library(sf)

# Step 1: Load the shapefile for California cities
city_shapes <- st_read("ca_cities/California_Incorporated_Cities.shp") # WGS84 => 3857
city_shapes <- st_transform(city_shapes, st_crs(4326))
city_shapes <- st_make_valid(city_shapes)
#st_crs(city_shapes)

county_shapes <- st_read("ca_counties/CA_Counties.shp")
county_shapes <- st_transform(county_shapes, st_crs(4326))
county_shapes <- st_make_valid(county_shapes)

places_shapes <- st_read("ca_places/CA_Places.shp")
places_shapes <- st_transform(places_shapes, st_crs(4326)) # transform to same coordinate system
places_shapes <- st_make_valid(places_shapes)

# Step 2: Get coordinates from collisions 
coordinates <- dbGetQuery(db, "SELECT latitude, longitude from collisions where latitude NOT NULL AND longitude NOT NULL;")

# Step 3: Convert the coordinates to an sf object
coordinates <- st_as_sf(coordinates, coords = c("longitude", "latitude"), crs = 4326)

# Step 4: Perform a spatial join to map collisions to places
places <- st_join(coordinates, places_shapes, join = st_intersects)
na_places <- places$geometry[is.na(places$NAME)]
places <- places %>% filter(!is.na(NAME))

cities <- st_join(coordinates, city_shapes, join = st_intersects)
na_cities <- cities$geometry[is.na(cities$CITY)]
cities <- cities %>% filter(!is.na(CITY))

counties <- st_join(coordinates, county_shapes, join = st_intersects)
na_counties <- counties$geometry[is.na(counties$NAME)]
counties <- counties %>% filter(!is.na(NAME))


# Example: Plotting points on a map
# Assume `map_data` is an `sf` object and `points` is a data frame with longitude and latitude

ggplot() +
  geom_sf(data = county_shapes, fill = "white", color = "black") +  # Plot the map
  geom_sf(data = st_as_sf(cities) %>% slice_sample(n = 1000), color = "blue", size = .5) +  # Plot cities
  geom_sf(data = st_as_sf(na_counties) %>% slice_sample(n = 1000), color = "red", size = .5) +  # Plot outside counties
  geom_sf(data = st_as_sf(places) %>% slice_sample(n = 1000), color = "green", size = .5) +  # plot places
  labs(title = "Crashes in California") +
  theme_minimal()


# Example: Plotting large datasets

install.packages('leaflet')
library(leaflet)

leaflet(data = places) %>%
  addTiles() %>%
  addCircleMarkers(clusterOptions = markerClusterOptions())
