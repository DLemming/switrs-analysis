install.packages("RSQLite")
install.packages("dplyr")
install.packages("tidyverse")
install.packages('reshape')
install.packages("MASS")  # Regression mit ordinalen Variablen


library(DBI)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(reshape)
library(MASS)

setwd("%working directory%")
db<-dbConnect(RSQLite::SQLite(), "./switrs.sqlite")

# Create indexes for optimal performance
dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_collisions_case_id ON collisions(case_id);")
dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_parties_case_id ON parties(case_id);")
dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_parties_case_id_party_number ON parties(case_id, party_number);")
dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_victims_case_id_party_number ON victims(case_id, party_number);")

# Import database into R-dataframes
collisions <- dbGetQuery(db, "SELECT * FROM collisions")
parties <- dbGetQuery(db, "SELECT * FROM parties")
victims <- dbGetQuery(db, "SELECT * FROM victims")


########################## Process Population Data ##########################


# ---------------------- Init load city_population data ----------------------
population_data <- read.csv("Califoria DOF Population Counts/population_by_city.csv", sep=";")
population_data$X <- NULL
population_data$X.1 <- NULL
population_data$X.2 <- NULL

long_city_population_data <- population_data %>%
  pivot_longer(
    cols = starts_with("X20"),   # Select columns starting with "X20" (years)
    names_to = "year",          # Create a new column "year"
    values_to = "population"    # Create a new column "population"
  )
long_city_population_data$year <- as.numeric(gsub("\\X", "", long_city_population_data$year))
long_city_population_data$population <- as.numeric(gsub("\\.", "", long_city_population_data$population))
long_city_population_data$city_name <- tolower(long_city_population_data$city_name)
long_city_population_data$city_name <- trimws(long_city_population_data$city_name) # strip leading and trailing white spaces
long_city_population_data <- na.omit(long_city_population_data)
dbWriteTable(db, "city_populations", long_city_population_data, overwrite = TRUE)
dbListTables(db)

# ---------------------- Init load county_population data ----------------------
population_data <- read.csv("Califoria DOF Population Counts/population_by_county.csv")
long_population_data <- population_data %>%
  pivot_longer(
    cols = starts_with("X20"),   # Select columns starting with "X20" (years)
    names_to = "year",          # Create a new column "year"
    values_to = "population"    # Create a new column "population"
    )
long_population_data$year <- as.numeric(gsub("\\X", "", long_population_data$year))
long_population_data$population <- as.numeric(gsub("\\.", "", long_population_data$population))
long_population_data$county_name <- tolower(long_population_data$county_name)
dbWriteTable(db, "county_populations", long_population_data, overwrite = TRUE)
dbListTables(db)


# ---------------------- Update collisions table with county_population ----------------------
dbExecute(db, "ALTER TABLE collisions ADD COLUMN county_population INT;")
dbExecute(db, "UPDATE collisions
               SET county_population = (
                  SELECT population
                  FROM county_populations
                  WHERE collisions.county_location = county_populations.county_name
                  AND collisions.year = county_populations.year
               );")

# ---------------------- Update collisions table with city_location & city_population ----------------------
# Step 1: Load the shapefile for California cities
city_shapes <- st_read("ca_cities/California_Incorporated_Cities.shp") # WGS84 => 3857
city_shapes <- st_transform(city_shapes, st_crs(4326)) # transform coordinate space
city_shapes <- st_make_valid(city_shapes) # validate shapes

# Step 2: Get coordinates from collisions 
coordinates <- dbGetQuery(db, "SELECT case_id, latitude, longitude FROM collisions WHERE latitude IS NOT NULL AND longitude IS NOT NULL;")

# Step 3: Convert the coordinates to an sf object
coordinates <- st_as_sf(coordinates, coords = c("longitude", "latitude"), crs = 4326)

# Step 4: Perform a spatial join to map collisions to cities
cities <- st_join(coordinates, city_shapes, join = st_intersects)
cities <- cities %>% filter(!is.na(CITY)) # filter out na cities (crash did not take place in a city)

# Step 5: Keep only case_id and CITY
collision_city_map <- cities %>%
  st_drop_geometry() %>% # Drop geometry to keep only attribute data
  select(case_id, CITY)

# Step 6: convert city names to lower case
collision_city_map$CITY <- tolower(collision_city_map$CITY)


# ---------------------- Update collisions table with city_location ----------------------
# Write collision_city_map to a temporary table in the database
dbWriteTable(db, "temp_city_map", collision_city_map, overwrite = TRUE)

# Create indices for faster join
dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_collisions_case_id ON collisions(case_id);")
dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_temp_city_map_case_id ON temp_city_map(case_id);")

# Update collisions table
dbExecute(db, "ALTER TABLE collisions ADD COLUMN city_location TEXT;")

dbExecute(db, "
  UPDATE collisions
  SET city_location = temp_city_map.CITY
  FROM temp_city_map
  WHERE collisions.case_id = temp_city_map.case_id;")

dbExecute(db, "DROP TABLE temp_city_map;")

# ---------------------- Update collisions table with city_population ---------------------- 
# Create indices to make joins faster
dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_collisions_city_year ON collisions(city_location, year);")
dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_city_populations_city_year ON city_populations(city_name, year);")

# check that all city_locations in collisions have a corresponding match in city_populations.CITY
dbGetQuery(db, "SELECT DISTINCT collisions.city_location
  FROM collisions
  LEFT JOIN city_populations
    ON collisions.city_location = city_populations.city_name
    AND collisions.year = city_populations.year
  WHERE city_populations.city_name IS NULL;")

dbExecute(db, "ALTER TABLE collisions ADD COLUMN city_population INT;")
dbExecute(db, "UPDATE collisions
               SET city_population = city_populations.population
               FROM city_populations
               WHERE collisions.city_location = city_populations.city_name
               AND collisions.year = city_populations.year;")


# ----------------------  Add years to collisions table ----------------------
dbExecute(db, "ALTER TABLE collisions ADD COLUMN year INT;")
dbExecute(db, "UPDATE collisions
               SET year = CAST(substr(collision_date, 1, 4) AS INTEGER);")




# ---------------------- Save Data Frames ----------------------
saveRDS(na_cities, "./Data Frames/na_cities.Rds")
saveRDS(na_counties, "./Data Frames/na_counties.Rds")
saveRDS(na_places, "./Data Frames/na_places.Rds")

saveRDS(cities, "./Data Frames/cities.Rds")
saveRDS(counties, "./Data Frames/counties.Rds")
saveRDS(places, "./Data Frames/places.Rds")

saveRDS(shape_cities, "./Data Frames/shape_cities.Rds")
saveRDS(shape_counties, "./Data Frames/shape_counties.Rds")
saveRDS(shape_places, "./Data Frames/shape_places.Rds")

saveRDS(coordinates, "./Data Frames/coordinates.Rds")
# -------------------- End Save Data Frames --------------------


# ---------------------- Load Data Frames ----------------------
na_cities <- readRDS("./Data Frames/na_cities.Rds")
na_counties <- readRDS("./Data Frames/na_counties.Rds")
na_places <- readRDS("./Data Frames/na_places.Rds")

cities <- readRDS("./Data Frames/cities.Rds")
counties <- readRDS("./Data Frames/counties.Rds")
places <- readRDS("./Data Frames/places.Rds")

shape_cities <- readRDS("./Data Frames/shape_cities.Rds")
shape_counties <-readRDS("./Data Frames/shape_counties.Rds")
shape_places <- readRDS("./Data Frames/shape_places.Rds")

coordinates <- readRDS("./Data Frames/coordinates.Rds")
# -------------------- End Load Data Frames --------------------