# Creating a very basic map in R
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

# Now, the map!

plot(baltimore.shape) # Super basic map of Baltimore with boundaries of the Community Statistical Areas
