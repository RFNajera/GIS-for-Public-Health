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

# We can also make an interactive map with tmap

tmap_mode("view")

map.2 <- tm_shape(baltimore.shape) + # Tell tmap which shapefile to use
  tm_fill(id = "CSA2010",
          popup.format = list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")),
          alpha = 0.5,
          col = "age65_19", # Tell tmap what color to use for each Community Statistical Area (CSA)
          title = "Percent of People Age 65 and Older", # Title of the legend
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")) # Numbers as percents
  ) +
  tm_borders(col = "black" # Color of the borders
  ) +
  tm_shape(water.shape) + # Adds the water shapefile
  tm_fill(col = "blue") + # Colors the water blue
  tm_layout(legend.outside = T, # Legend is outside the map frame
            legend.title.size = 1, # Size of the legend title
            title.size = 1 # Size of the title
  ) +
  tm_scale_bar()
map.2
