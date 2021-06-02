# Creating an interactive map in R
# GIS for Public Health Using R Programming
# https://classroom.google.com/u/0/c/MzUwMTM3NjEzODk5
# Rene F. Najera, DrPH
# Summer 2021

# First, the libraries we'll need
library(tmap)
library(rgdal)
library(maps)
library(leaflet)
library(htmltools)

# Bring in the shapefile

baltimore.shape <- readOGR("Percent_of_Population_65_Years_and_over", # Folder within your main folder where the shape file is
                           "Percent_of_Population_65_Years_and_over" # Name of the shapefile
)

# Create the map with Leaflet

# Set value for the minZoom and maxZoom settings.
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 13)) # Limit how close or how far people can zoom into

bins <- c(0,5,10,15,20,25,30)
pal <- colorBin("YlOrRd", domain = baltimore.shape$age65_19, bins = bins)
labels <- sprintf(
  "<strong>%s</strong><br/>%g percent over 65",
  baltimore.shape$CSA2010,
  round(baltimore.shape$age65_19,1)
  ) %>% 
  lapply(htmltools::HTML)

map.1 <- leaflet(data = baltimore.shape) %>% 
  setView(lng = -76.6167377, lat = 39.2962229, zoom = 12) %>% 
  addTiles() %>% 
  addPolygons(
    fillColor = ~pal(age65_19),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
    ) %>% 
  addLegend(pal = pal,
            values = ~age65_19,
            opacity = 0.7,
            title = NULL,
            position = "bottomright")
map.1
