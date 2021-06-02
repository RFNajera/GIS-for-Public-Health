# Creating an animated map of people 65 and over in Baltimore
# GIS for Public Health Using R Programming
# https://classroom.google.com/u/0/c/MzUwMTM3NjEzODk5
# Rene F. Najera, DrPH
# Summer 2021

# First, the libraries we'll need
library(tmap)
library(rgdal)
library(tigris)
library(gifski)

# Now, the shapefile of the city of Baltimore

baltimore.shape <- readOGR("Percent_of_Population_65_Years_and_over", # Folder within your main folder where the shape file is
                           "Percent_of_Population_65_Years_and_over" # Name of the shapefile
)

# First, the choropleth we created of the population 65 and over

water.shape <- readOGR("water","water")

map.1 <- tm_shape(baltimore.shape) + # Tell tmap which shapefile to use
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
  ) +
  tm_scale_bar()
map.1

# Now, we are going to facet the map

map.2 <- tm_shape(baltimore.shape) + 
  tm_fill(col = c("age65_10", # Each one of these will be a frame
                  "age65_15",
                  "age65_16",
                  "age65_17",
                  "age65_18",
                  "age65_19"), # Tell tmap what variable to fill in for each Community Statistical Area (CSA)
          title = "Percent of People Age 65 and Older", # Title of the legend
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")), # Numbers as percents
          style = "fixed", # Keep the legend fixed to a certain number of intervals in breaks below
          breaks = c(0,5,10,15,20,25,30) # The intervals for the legend
  ) +
  tm_borders(col = "black" # Color of the borders
  ) +
  tm_facets(sync = TRUE, ncol = 1, nrow = 1) +
  tm_shape(water.shape) + # Adds the water shapefile
  tm_fill(col = "blue") + # Colors the water blue
  tm_layout(legend.outside = T, # Legend is outside the map frame
            legend.title.size = 1, # Size of the legend title
            legend.position = c("left","center"), # Position of the legend
            title.size = 1, # Size of the title
            panel.labels = c("2010",
                            "2015",
                            "2016",
                            "2017",
                            "2018",
                            "2019") 
  ) +
  tm_compass(type = "8star", # Type of compass
             position = c("left","bottom"), # Position of the compass
             size = 8 # Size of the compass
  ) +
  tm_scale_bar()
map.2

# And then we animate it

map.3 <- tmap_animation(map.2,
                        loop = T)
