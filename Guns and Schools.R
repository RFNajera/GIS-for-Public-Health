# Mapping guns within 1,000 feet from schools in Baltimore
# GIS for Public Health Using R Programming
# https://classroom.google.com/u/0/c/MzUwMTM3NjEzODk5
# Rene F. Najera, DrPH
# Updated July 2023

# Libraries ----
library(tidyverse)
library(rgdal)
library(tigris)
library(sf)
library(tmap)
library(raster)

# Shapefiles ----

baltimore.shape <- st_read("Total_Population",
                           "Total_Population")
water.shape <- st_read("Water",
                       "water")
baltimore.schools <- st_read("BCPSS_Schools",
                             "BCPSS_Schools")

# Homicides, but only the gun-related ones in 2017 ----
baltimore.homicides <- read.csv("Baltimore-2018-homicides-geocoded.csv") %>% 
  mutate(event_date = lubridate::mdy(factor(event_date))) %>% # Make event date into a date format
  filter(lubridate::year(event_date) == 2017) %>% # Only those ocurring on 2017
  filter(mode == "shooting") %>% # Only the shooting ones
  dplyr::select(id_code,lon,lat) %>% # Keep only the shooting ID and the longitude and latitude
  st_as_sf(coords = c("lon","lat"), # Convert to an sf object
           crs = "+proj=longlat +datum=WGS84")

# Check it
ggplot() +
  geom_sf(data = baltimore.homicides) +
  theme_minimal()


# Quick map of all the schools and homicides ----

tmap_mode("plot")

map.1 <- tm_shape(baltimore.shape) +
  tm_borders() +
  tm_shape(baltimore.schools) +
  tm_dots(col = "black") +
  tm_shape(baltimore.homicides) +
  tm_dots(col = "red")
map.1

# Make the buffers around the schools at 1,000 feet or 304.8 meters ----
buffer <- st_buffer(baltimore.schools, # Layer for buffers
              304.8 # Buffer in meters
              ) %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") # Assign the CRS

# Check it
ggplot() +
  geom_sf(data = buffer) +
  theme_minimal()


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

# Duplicating the data sets to manipulate them
points <- baltimore.homicides
polygons <- buffer

# Joining the layers
crime.points <- st_join(points,
                        polygons)


# Map, but this time the schools with homicides are red triangles ----

# Filter out the locations without a name, which is where a homicide was NOT inside a buffer
school.homicide <- st_join(points,
                            polygons) %>%
  filter(name != "NA")

# Create a map with the different data
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
  tm_shape(water.shape) +
  tm_polygons(col = "blue") +
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
  tm_add_legend(
    "fill",
    col = "blue",
    size = 0.5,
    labels = "Body of Water"
  ) +
  tmap_options(unit = "mi")
map.3

# But what if we want to see each specific location with a little more detail?

tmap_mode("view") # Set the map mode to interactive
map.3
