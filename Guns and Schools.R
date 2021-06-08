# Standardizing Community Indicators
# GIS for Public Health Using R Programming
# https://classroom.google.com/u/0/c/MzUwMTM3NjEzODk5
# Rene F. Najera, DrPH
# Summer 2021

# Libraries ----
library(tidyverse)
library(rgdal)
library(tigris)
library(sf)
library(tmap)
library(raster)

# Shapefiles ----

baltimore.shape <- readOGR("Total_Population",
                           "Total_Population")
water.shape <- readOGR("Water",
                       "water")
baltimore.schools <- readOGR("BCPSS_Schools",
                             "BCPSS_Schools")

# Homicides, but only the gun-related ones in 2017 ----

baltimore.homicides <- read.csv("Baltimore-2018-homicides-geocoded.csv") %>% 
  mutate(event_date = lubridate::mdy(factor(event_date))) %>% # Make event date into a date format
  filter(lubridate::year(event_date) == 2017) %>% # Only those ocurring on 2017
  filter(mode == "shooting") %>% # Only the shooting ones
  dplyr::select(id_code,lon,lat) # Keep only the shooting ID and the longitude and latitude

# Make the homicide list into a spatial file ----

baltimore.homicides <- st_as_sf(x = baltimore.homicides, # Transform dataframe to simple feature (sf)
                                coords = c("lon", "lat"), # Tell it where the latitude and longitude are
                                crs = "+proj=longlat +datum=WGS84") # Give it a CRS
baltimore.homicides <- as(baltimore.homicides, "Spatial") # Make the simple feature into a spatial feature
baltimore.shape <- spTransform(baltimore.shape, CRS("+proj=longlat +datum=WGS84")) # Give the baltimore shape the same CRS
baltimore.schools <- spTransform(baltimore.schools,proj4string(baltimore.homicides)) # Give the baltimore shape the same CRS

# Quick map of all the schools and homicides ----

tmap_mode("plot")

map.1 <- tm_shape(baltimore.shape) +
  tm_borders() +
  tm_shape(baltimore.schools) +
  tm_dots(col = "black") +
  tm_shape(baltimore.homicides) +
  tm_dots(col = "red")
map.1

# Make the buffers around the schools at 1,000 feet ----

buffer <- buffer(baltimore.schools, # Layer for buffers
              width = 304.8, # Buffer in meters
              dissolve = F)
plot(buffer) # Take a look at the result

# Map of schools with buffers and homicides ----

map.2 <- tm_shape(baltimore.shape) +
  tm_borders() +
  tm_shape(baltimore.homicides) +
  tm_dots(col = "black",
          size = 0.025,
          title = "Gun Homicide") +
  tm_shape(baltimore.schools) +
  tm_symbols(col = "orange",
             shape = 24,
             size = 0.05) +
  tm_shape(buffer) +
  tm_borders(col = "blue") +
  tm_layout(
    main.title = "Homicides and Schools with 1,000-ft Buffers\nin Baltimore City, 2017",
    main.title.size = 1,
    compass.type = "4star",
    legend.outside = T
  ) +
  tm_compass(position = c("left","bottom")
             ) +
  tm_scale_bar(
    text.size = 0.5,
    color.dark = "black",
    color.light = "yellow",
    lwd = 1,
    position = c("left","bottom")
  ) +
  tm_add_legend(
    "symbol",
    col = "orange",
    shape = 17,
    size = 0.5,
    labels = "School"
  ) +
  tm_add_legend("line",
                col = "blue",
                size = 0.5,
                labels = "1000-ft Buffer") +
  tm_add_legend(
    "symbol",
    col = "black",
    shape = 21,
    size = 0.5,
    labels = "Gun Homicide"
  ) +
  tmap_options(unit = "mi")
map.2

# Let's see which schools had homicides within 1,000 feet, and how many homicides ----

crimespoints <- as.data.frame(sp::over(baltimore.homicides, # Points
                                            buffer)) # Polygons
crimesbuffer <- crimespoints %>% 
  filter(name != "NA") %>% # Creating a dataframe of only the points inside the polygons ("name" is not NA)
  group_by(name) %>% # Group the school-homicide pairings by name
  summarise(homicides = n()) # Count how many homicides

# Map, but this time the schools with homicides are red triangles ----

school.homicide <- as.data.frame(geo_join(baltimore.schools,
                            crimesbuffer,
                            "name",
                            "name")) %>% 
  filter(homicides != "NA")
school.homicide <- st_as_sf(x = school.homicide, # Transform dataframe to simple feature (sf)
                                coords = c("coords.x1", "coords.x2"), # Tell it where the latitude and longitude are
                                crs = "+proj=longlat +datum=WGS84") # Give it a CRS
school.homicide <- as(school.homicide, "Spatial") # Make the simple feature into a spatial feature

map.3 <- tm_shape(baltimore.shape) +
  tm_borders() +
  tm_shape(baltimore.homicides) +
  tm_dots(col = "black",
          size = 0.025,
          title = "Gun Homicide") +
  tm_shape(baltimore.schools) +
  tm_symbols(col = "orange",
             shape = 24,
             size = 0.05) +
  tm_shape(school.homicide) +
  tm_symbols(col = "red",
             shape = 23,
             size = 0.25) +
  tm_shape(buffer) +
  tm_borders(col = "blue") +
  tm_layout(
    main.title = "Schools w/Homicides w/in 1,000-ft Buffers\nin Baltimore City, 2017",
    main.title.size = 1,
    compass.type = "4star",
    legend.outside = T
  ) +
  tm_compass(position = c("left","bottom")
  ) +
  tm_scale_bar(
    text.size = 0.5,
    color.dark = "black",
    color.light = "yellow",
    lwd = 1,
    position = c("left","bottom")
  ) +
  tm_add_legend(
    "symbol",
    col = "orange",
    shape = 17,
    size = 0.5,
    labels = "School"
  ) +
  tm_add_legend(
    "symbol",
    col = "red",
    shape = 23,
    size = 0.5,
    labels = "School w/ Homicides w/in 1,000-ft"
  ) +
  tm_add_legend("line",
                col = "blue",
                size = 0.5,
                labels = "1000-ft Buffer") +
  tm_add_legend(
    "symbol",
    col = "black",
    shape = 21,
    size = 0.5,
    labels = "Gun Homicide"
  ) +
  tmap_options(unit = "mi")
map.3

# But what if we want to see each specific location with a little more detail?

tmap_mode("view")
map.3
