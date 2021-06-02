# Creating a choropleth map of people 65 and over in Baltimore
# GIS for Public Health Using R Programming
# https://classroom.google.com/u/0/c/MzUwMTM3NjEzODk5
# Rene F. Najera, DrPH
# Summer 2021

# First, the libraries we'll need
library(tmap)
library(rgdal)

# Now, the shapefile of the city of Baltimore

baltimore.shape <- readOGR("Percent_of_Population_65_Years_and_over", # Folder within your main folder where the shape file is
                           "Percent_of_Population_65_Years_and_over" # Name of the shapefile
                           )

# A quick choropleth map of people over age 65 in Baltimore in 2019

map.1 <- qtm(baltimore.shape, # Which shapefile to use
             fill = "age65_19" # What value to use to fill the polygons
             ) 
map.1

# Not so quick but pretty map of people over age 65 in Baltimore in 2019

water.shape <- readOGR("water","water")

hist(baltimore.shape$age65_19) # Look at the distribution of age in Baltimore

map.2 <- tm_shape(baltimore.shape) + # Tell tmap which shapefile to use
  tm_fill(col = "age65_19", # Tell tmap what color to use for each Community Statistical Area (CSA)
          title = "Percent of People Age 65 and Older", # Title of the legend
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")) # Numbers as percents
              ) +
  tm_borders(col = "black" # Color of the borders
             ) +
  tm_shape(water.shape) + # Adds the water shapefile
  tm_fill(col = "blue") + # Colors the water blue
  tm_layout(legend.outside = T, # Legend is outside the map frame
            legend.title.size = 1, # Size of the legend title
            legend.position = c("left","center"), # Position of the legend
            title = "Distribution of Residents Age 65 and Older in Baltimore, 2019", # Title of the map
            title.size = 1 # Size of the title
            ) +
  tm_compass(type = "8star", # Type of compass
             position = c("left","bottom"), # Position of the compass
             size = 8 # Size of the compass
             )
map.2
