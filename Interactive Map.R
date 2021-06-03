# Creating an interactive map in R
# GIS for Public Health Using R Programming
# https://classroom.google.com/u/0/c/MzUwMTM3NjEzODk5
# Rene F. Najera, DrPH
# Summer 2021

# First, the libraries we'll need
library(tmap)
library(rgdal)
library(leaflet)
library(htmltools)

# Bring in the shapefiles for the city and for the water features

baltimore.shape <- readOGR("Percent_of_Population_65_Years_and_over", # Folder within your main folder where the shape file is
                           "Percent_of_Population_65_Years_and_over" # Name of the shapefile
)

water.shape <- readOGR("water","water")

# Create the map with Leaflet


bins <- c(0,5,10,15,20,25,30) # How many bins the data will take up
pal <- colorBin("YlOrRd", domain = baltimore.shape$age65_19, bins = bins) # Color yellow-orange-red palette
labels <- sprintf( # Create the lables
  "<strong>%s</strong><br/>%g percent over 65", # Bold the name of the CSA
  baltimore.shape$CSA2010, # Name of the CSA
  round(baltimore.shape$age65_19,1) # Rounding the percent to the nearest decimal to add to the pop up labels
  ) %>% 
  lapply(htmltools::HTML) # Turn the labels into HTML widgets

map.1 <- leaflet(data = baltimore.shape) %>% # The shapefile we're mapping
  setView(lng = -76.6167377, lat = 39.2962229, zoom = 12) %>% # The latitude and longitude to center the map
  addTiles() %>% # Add tiles (here you can specify what kind of map you want to add)
  addPolygons(
    fillColor = ~pal(age65_19), # The variable used for the color
    weight = 2, # How heavy the boundary lines should be
    opacity = 1, # How transparent the boundary lines should be 
    color = "white", # Color of the boundary lines
    dashArray = "3", # Dashed boundary lines
    fillOpacity = 0.7, # Transparency of the fill color of the polygons
    highlight = highlightOptions(
      weight = 5, # When hovering over a CSA, how heavy the border should be
      color = "#666", # The color of the border when hovering over a CSA
      dashArray = "", # Border lines dashed? Not in this case
      fillOpacity = 0.7, # The transparency of the CSA color when hovered over
      bringToFront = TRUE), # Bring the polygon (CSA) to the front?
    label = labels, # Add the labels created above
    labelOptions = labelOptions( # Options for the labels
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
    ) %>% 
  addLegend(pal = pal, # Color palette of the legend to match the colors on the map
            values = ~age65_19, # The variables presented on the legend
            opacity = 0.7, # The transparency of the colors on the legend
            title = NULL,
            position = "bottomright") # Where to place the legend
map.1 # View your map

# We can also make an interactive map with tmap

tmap_mode("view") # Set the mode for tmap to view (other was "plot")

map.2 <- tm_shape(baltimore.shape) + # Tell tmap which shapefile to use
  tm_fill(id = "CSA2010", # What variable to use for the fill color
          popup.format = list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")), # Format as percent
          alpha = 0.5, # Transparency of the colors
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
  tm_scale_bar() # Add the scale bar, for distance
map.2 # Look at your map
