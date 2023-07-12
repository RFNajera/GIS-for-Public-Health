# Creating a very basic map in R
# GIS for Public Health Using R Programming
# https://classroom.google.com/u/0/c/MzUwMTM3NjEzODk5
# Rene F. Najera, DrPH
# Summer 2021

# First, the libraries we'll need
library(tmap)
library(sf)
library(leaflet)

# First, bring in the shapefile of the city of Baltimore

baltimore.shape <- read_sf("Percent_of_Population_65_Years_and_over", # Folder within your main folder where the shape file is
                           "Percent_of_Population_65_Years_and_over" # Name of the shapefile
                           )

# Now, the map!

plot(baltimore.shape) # Super basic map of Baltimore with boundaries of the Community Statistical Areas

# Now, a quick thematic map with no color

tmap_mode("plot")
map.1 <- qtm(baltimore.shape)
map.1

# Now, a leaflet map

content <- paste(sep = "<br/>",
                 "<b><a href='http://thewalters.org/'>The Walters Art Museum</a></b>",
                 "600 N Charles St, Baltimore, MD 21201"
)

map.2 <- leaflet() %>% 
  setView(lng = -76.6167377, lat = 39.2962229, zoom = 12) %>% 
  addTiles() %>% 
  addPopups(-76.6167377, 39.2962229, content,
            options = popupOptions(closeButton = FALSE)
  )
map.2
