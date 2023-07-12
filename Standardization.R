# Standardizing Community Indicators
# GIS for Public Health Using R Programming
# https://classroom.google.com/u/0/c/MzUwMTM3NjEzODk5
# Rene F. Najera, DrPH
# Updaetd July 2023

# Libraries
library(tidyverse)
library(tmap)
library(rgdal)
library(tigris)
library(sf)

# Bring in the data

my.data <- read.csv("baltimore_indicators.csv")

# Now, we create a function "uhn.index" that will do several things
# noted in the code:

lower.better <- c("Percent_Over_65") # Choose the variable(s) whose value is "better" if it is lower

uhn.index <-
  function(x, y) {
    # x is the dataframe we're using, y are the "lower is better" variables
    chars <-
      x %>% select_if(negate(is.numeric)) # Extracts the names of the community areas
    x <- x[, sapply(x, is.numeric)] # Extracts the numeric variables
    x = scale(x) # Calculates the Z scores of the numeric variables
    myvars <- y # Incorporates the variables in y above as the ones
    x[, myvars] <-
      x[, myvars] * -1 # Multiplies the "lower is better" variables by negative one
    index <- rowSums(x) # Sums all the Z scores into the index score
    x <-
      data.frame(chars, x, index) # Creates the dataframe from the names, the Z scores and the index
    x <-
      x[order(index), ] # Sorts the index variable from smallest to largest
    return(x) # Returns the resulting dataframe
  }

# Now we apply that function to the Baltimore data

baltimore.index <- uhn.index(my.data, lower.better)

# Import the shapefile

baltimore.shape <- st_read("Percent_of_Population_65_Years_and_over",
                           "Percent_of_Population_65_Years_and_over")

water.shape <- st_read("Water",
                       "water")

# Join the data to the shapefile

baltimore.map.data <- left_join(baltimore.shape,
                               baltimore.index)

# Make your map!

map.1 <- tm_shape(baltimore.map.data) + # Tell tmap which shapefile to use
  tm_fill(col = "index", # Tell tmap what color to use for each Community Statistical Area (CSA)
          title = "Index of COVID vulnerability", # Title of the legend
          style = "quantile"
  ) +
  tm_borders(col = "black" # Color of the borders
  ) +
  tm_shape(water.shape) + # Adds the water shapefile
  tm_fill(col = "blue") + # Colors the water blue
  tm_layout(legend.outside = T, # Legend is outside the map frame
            legend.title.size = 1, # Size of the legend title
            legend.position = c("left","center"), # Position of the legend
            title = "Index of COVID vulnerability in Baltimore, 2019", # Title of the map
            title.size = 1 # Size of the title
  ) +
  tm_compass(type = "8star", # Type of compass
             position = c("left","bottom"), # Position of the compass
             size = 8 # Size of the compass
  ) +
  tm_scale_bar()
map.1

