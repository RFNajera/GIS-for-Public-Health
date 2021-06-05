# Standardizing Community Indicators
# GIS for Public Health Using R Programming
# https://classroom.google.com/u/0/c/MzUwMTM3NjEzODk5
# Rene F. Najera, DrPH
# Summer 2021

# Libraries ----

library(tidyverse) # For data manipulation
library(tmap) # For mapping
library(rgdal) # For dealing with spatial data, like reading the shapefiles in
library(tigris) # For dealing with spatial data, like spatial joins
library(sf) # For dealing with spatial datal, like setting CRS
library(spdep) # For calculating spatial dependencies, like Moran's I or Getis-Ord


# Load shapefiles ----

baltimore.shape <- readOGR("Total_Population",
                           "Total_Population")
water.shape <- readOGR("Water",
                       "water")

# Load the homicide data ----

baltimore.homicides.data <- read.csv("Baltimore-2018-homicides-geocoded.csv") %>% 
  mutate(event_date = lubridate::mdy(factor(event_date))) %>% # Make event date into a date format
  mutate(month = factor(lubridate::month(event_date), # Extract month and label it
                        labels = c("Jan","Feb","Mar","Apr",
                                   "May","Jun","Jul","Aug",
                                   "Sep","Oct","Nov","Dec"))) %>% 
  mutate(year = lubridate::year(event_date)) %>% # Extract the year number
  mutate(week = lubridate::epiweek(event_date)) # Extract the epidemiological week

# Descriptive statistics of the victims and the date/time of homicides ----

table(baltimore.homicides.data$race) # Make a table of number of homicides by race/ethnicity
hist(baltimore.homicides.data$age) # Make a histogram of age variable
table(baltimore.homicides.data$sex) # Make a table of number of homicides by sex
table(baltimore.homicides.data$mode) # Make a table of the number of homicides by mode of death
date.graph <- baltimore.homicides.data %>% # Make a bar graph of homicides by week
  ggplot(aes(x = week)) +
  geom_bar() +
  theme_classic() +
  labs(x = "MMWR Week",
       y = "Number of Homicides",
       title = "Homicides per MMWR Week, Baltimore, 2005 to 2017")
date.graph # Look at the bar graph of homicides by week
month.graph <- baltimore.homicides.data %>% # Make a bar graph of homicides by month
  ggplot(aes(x = month)) +
  geom_bar()
month.graph # Look at the bar graph of homicides by month
year.graph <- baltimore.homicides.data %>% # Make a bar graph of homicides by year
  ggplot(aes(x = year)) +
  geom_bar() +
  theme_classic() +
  labs(x = "Year",
       y = "Number of Homicides",
       title = "Number of Homicides per Year in Baltimore, 2005 to 2017")
year.graph # Look at the bar graph of homicides by year

# Get the points to be a shapefile ----

baltimore.homicides <- st_as_sf(x = baltimore.homicides.data, # Transform dataframe to simple feature (sf)
                        coords = c("lon", "lat"), # Tell it where the latitude and longitude are
                        crs = "+proj=longlat +datum=WGS84") # Give it a CRS
baltimore.homicides <- as(baltimore.homicides, "Spatial") # Make the simple feature into a spatial feature
baltimore.shape <- spTransform(baltimore.shape, CRS("+proj=longlat +datum=WGS84")) # Give the baltimore shape the same CRS

# Map of where the homicides happened ----

tmap_mode("plot") # Set tmap to plot. To make interactive, use "view"

homicide.dots.map <- # Create the map of homicides with points
  tm_shape(baltimore.shape) +
  tm_borders(col = "black",
             lwd = 0.5) +
  tm_shape(baltimore.homicides) +
  tm_dots(col = "red",
          title = "2018 Homicides",
          size = 0.1, ) +
  tm_compass(position = c("left","bottom")) +
  tm_layout(
    main.title = "Map of Homicides in Baltimore City, 2005 to 2017",
    main.title.size = 0.8,
    legend.position = c("left", "center"),
    compass.type = "4star",
    legend.text.size = 1,
    legend.title.size = 1,
    legend.outside = T
  ) +
  tm_scale_bar(
    text.size = 0.5,
    color.dark = "black",
    color.light = "yellow",
    lwd = 1,
    position = c("left","bottom")
  ) +
  tm_add_legend(
    type = "symbol",
    col = "red",
    shape = 21,
    title = "Homicide",
    size = 0.5
  ) +
  tmap_options(unit = "mi")
homicide.dots.map # Look at the map

# Map of where and what month the homicides happened ----

homicide.dots.map <- # Create a map of dots, with each dot representing a month
  tm_shape(baltimore.shape) +
  tm_borders(col = "black",
             lwd = 0.5) +
  tm_shape(baltimore.homicides) +
  tm_symbols(col = "month",
          size = 0.1,
          shape = 21,
          title.col = "Month") +
  tm_compass(position = c("left","bottom")) +
  tm_layout(
    main.title = "Map of Homicides in Baltimore City, 2005 to 2017",
    main.title.size = 0.8,
    legend.position = c("left", "center"),
    compass.type = "4star",
    legend.text.size = 1,
    legend.title.size = 1,
    legend.outside = T
  ) +
  tm_scale_bar(
    text.size = 0.5,
    color.dark = "black",
    color.light = "yellow",
    lwd = 1,
    position = c("left","bottom")
  ) +
  tmap_options(unit = "mi")
homicide.dots.map # Look at your map

# Map of where and who was killed ----

homicide.dots.map <-
  tm_shape(baltimore.shape) +
  tm_borders(col = "black",
             lwd = 0.5) +
  tm_shape(baltimore.homicides) +
  tm_symbols(col = "race",
             size = 0.1,
             shape = "race",
             legend.shape.show = T,
             legend.col.show = F,
             title.shape = "Race/Ethnicity") +
  tm_compass(position = c("left","bottom")) +
  tm_layout(
    main.title = "Map of Homicides in Baltimore City, 2005 to 2017",
    main.title.size = 0.8,
    legend.position = c("left", "center"),
    compass.type = "4star",
    legend.text.size = 1,
    legend.title.size = 1,
    legend.outside = T
  ) +
  tm_scale_bar(
    text.size = 0.5,
    color.dark = "black",
    color.light = "yellow",
    lwd = 1,
    position = c("left","bottom")
  ) +
  tmap_options(unit = "mi")
homicide.dots.map

# Join the points to the polygons to calculate rates ----

homicide.dots.polys <- sp::over(baltimore.homicides, # The dots
                                baltimore.shape) %>% # The polygons
  group_by(CSA2010,tpop10) %>% # By which variables you're grouping
  summarise(homicides = n()) %>% # Number of homicides in each CSA
  mutate(rate = (homicides/tpop10)*10000) # Caluclate the rate per 10,000  residents
homicide.rates <- geo_join(baltimore.shape, # Join the baltimore shape to...
                           homicide.dots.polys, # The data with the rates in it, by...
                           "CSA2010", # These variables for the shape and...
                           "CSA2010") # These variables for the one with rate

# Make the map of the rates ----

homicide.dots.map <-
  tm_shape(homicide.rates) +
  tm_borders(col = "black") +
  tm_fill(col = "rate",
          textNA = "No Homicides",
          title = "Rate per 10k Residents") +
  tm_compass(position = c("left","bottom")) +
  tm_layout(
    main.title = "Map of Homicides in Baltimore City, 2005 to 2017",
    main.title.size = 0.8,
    legend.position = c("left", "center"),
    compass.type = "4star",
    legend.text.size = 1,
    legend.title.size = 1,
    legend.outside = T
  ) +
  tm_scale_bar(
    text.size = 0.5,
    color.dark = "black",
    color.light = "yellow",
    lwd = 1,
    position = c("left","bottom")
  ) +
  tmap_options(unit = "mi")
homicide.dots.map

# Spatial Autocorrelation with Moran's I ----

map_nbq <-
  poly2nb(homicide.rates) # Creates list of neighbors to each CSA
added <-
  as.integer(c(7, 50)) # Adds neighbors 7 and 50 to CSA 4 (it looks like an island)
map_nbq[[4]] <- added # Fixes the region (4) without neighbors
head(map_nbq) # View to make sure CSA 4 has neighbors 7 and 50.

map_nbq_w <-
  nb2listw(map_nbq, zero.policy = T) # Creates list of neighbors and weights. Weight = 1/number of neighbors.
head(map_nbq_w) # View the list

local.moran <-
  localmoran(homicide.rates$rate, # Which variable you'll use for the autocorrelation
             map_nbq_w, # The list of weighted neighbors
             zero.policy = T) # Calculate local Moran's I
local.moran <- as.data.frame(local.moran)
summary(local.moran) # Look at the summary of Moran's I

homicide.rates$srate <-
  scale(homicide.rates$rate) # save to a new column the standardized rate

homicide.rates$lag_srate <- lag.listw(map_nbq_w,
                                      homicide.rates$srate) # Spatially lagged standardized rate in new column

summary(homicide.rates$srate) # Summary of the srate variable
summary(homicide.rates$lag_srate) # Summary of the lag_srate variable

# Moran's I test & plot

moran.test(homicide.rates$rate,map_nbq_w) # Moran's I test
moran.plot(homicide.rates$rate,map_nbq_w) # Create a plot

# Moran's I Plot alone

x <- homicide.rates$srate %>% as.vector() # The X variables
y <- homicide.rates$lag_srate %>% as.vector() # The Y variables

moran.plot(x, map_nbq_w) # One way to make a Moran Plot

# Designate the quadrants to match what we see on the plot

homicide.rates$quad <-
  NA # Create variable for where the pair falls on the quadrants of the Moran plot
homicide.rates@data[(homicide.rates$srate >= 0 &
                       homicide.rates$lag_srate >= 0), "quad"] <- "High-High" # High-High
homicide.rates@data[(homicide.rates$srate <= 0 &
                       homicide.rates$lag_srate <= 0), "quad"] <- "Low-Low" # Low-Low
homicide.rates@data[(homicide.rates$srate >= 0 &
                       homicide.rates$lag_srate <= 0), "quad"] <- "High-Low" # High-Low
homicide.rates@data[(homicide.rates$srate <= 0 &
                       homicide.rates$lag_srate >= 0), "quad"] <- "Low-High" # Low-High

# Make the map

local.moran$OBJECTID <- 0 # Creating a new variable
local.moran$OBJECTID <- 1:nrow(local.moran) # Adding an object ID
local.moran$pvalue <-
  round(local.moran$`Pr(z > 0)`, 3) # Rounding the p value to three decimal places
local.moran$Ii <- round(local.moran$Ii,1) # Rounding the I variable to one decimal point

moran.i.map <- geo_join(homicide.rates, #Join the data from the rates dataframe to...
                       local.moran, # The local Moran shape by...
                       "OBJECTID", # These variables
                       "OBJECTID")

colors <-
  c("red", "lightpink", "skyblue2", "blue", "white") # Color Palette

local.moran.map <-
  tm_shape(moran.i.map) +
  tm_fill("quad",
          title = "Local Moran's I",
          palette = colors,
          colorNA = "white") +
  tm_borders(col = "black",
             lwd = 0.5) +
  tm_text("Ii",
          size = 0.5) +
  tm_compass() +
  tm_layout(
    main.title = "Map of Local Moran's I for Homicide Rates in Baltimore, 2005 to 2017",
    main.title.size = 0.8,
    legend.position = c("left", "bottom"),
    compass.type = "4star",
    legend.text.size = 1,
    legend.title.size = 1,
    legend.outside = T,
    bg.color = "#d0e2ec"
  ) +
  tm_scale_bar(
    text.size = 0.5,
    color.dark = "black",
    color.light = "yellow",
    lwd = 1,
    position = c("left","bottom")
  ) +
  tm_add_legend(
    type = "text",
    col = "black",
    title = "Moran's I Index",
    text = "0.0",
    size = 4
  ) +
  tmap_options(unit = "mi")
local.moran.map

# Keep only the CSAs with p-values less than 0.05 (though we should be using much smaller)

moran.i.map$quad_sig <-
  NA # Creates a variable for where the significant pairs fall on the Moran plot
moran.i.map@data[(moran.i.map$srate >= 0 &
                   moran.i.map$lag_srate >= 0) &
                  (local.moran[, 5] <= 0.05), "quad_sig"] <- "High-High, p <0.05" # High-High
moran.i.map@data[(moran.i.map$srate <= 0 &
                   moran.i.map$lag_srate <= 0) &
                  (local.moran[, 5] <= 0.05), "quad_sig"] <- "Low-Low, p <0.05" # Low-Low
moran.i.map@data[(moran.i.map$srate >= 0 &
                   moran.i.map$lag_srate <= 0) &
                  (local.moran[, 5] <= 0.05), "quad_sig"] <- "High-Low, p <0.05" # High-Low
moran.i.map@data[(moran.i.map$srate <= 0 &
                   moran.i.map$lag_srate >= 0) &
                  (local.moran[, 5] <= 0.05), "quad_sig"] <- "Low-High, p <0.05" # Low-High
moran.i.map@data[(moran.i.map$srate <= 0 &
                   moran.i.map$lag_srate >= 0) &
                  (local.moran[, 5] <= 0.05), "quad_sig"] <-"Not Significant. p>0.05" # Non-significant

colors2 <-
  c("red", "blue", "white") # Color Palette

local.moran.map.sig <- # Make the map of the CSAs that have significant observations
  tm_shape(moran.i.map) +
  tm_fill(
    "quad_sig",
    title = "Local Moran's I",
    palette = colors2,
    colorNA = "white",
    textNA = "Not Significant"
  ) +
  tm_borders(col = "black",
             lwd = 0.5) +
  tm_text("pvalue",
          size = 0.5) +
  tm_compass(position = c("left","bottom")) +
  tm_layout(
    main.title = "Map of Local Moran's I for Homicide Rates in Baltimore, 2005 to 2017",
    main.title.size = 0.8,
    legend.position = c("left", "bottom"),
    compass.type = "4star",
    legend.text.size = 1,
    legend.title.size = 1,
    legend.outside = T,
    bg.color = "#d0e2ec"
  ) +
  tm_scale_bar(
    text.size = 0.5,
    color.dark = "black",
    color.light = "yellow",
    lwd = 1,
    position = c("left", "bottom")
  ) +
  tm_add_legend(
    type = "text",
    col = "black",
    title = "Moran's I p-value",
    text = "0.000",
    size = 4
  ) +
  tmap_options(unit = "mi")
local.moran.map.sig # Look at the map

# Getis-Ord Hot Spots ----

#Calculate Getis-Ord "Hot Spots"
homicide.rates$Gstat<-round(localG(homicide.rates$rate,
                             map_nbq_w),2) # Calculating the G statistic and rounding it to two decimals
head(homicide.rates) # Quick peek to see it worked

#Set breaks at standard confidence levels
breaksg<-c(-Inf,-1.96,-1.645,1.645,1.96,Inf) # Remember the 68-95-99 rule? This is kind of like that.
colorsg<-c("blue","cyan","white","magenta","red") # Gives colors similar to what ArcGIS does

# Make the map of Gi* statistics by CSA

getis.map <- tm_shape(homicide.rates) +
  tm_fill(
    "Gstat",
    title = "Getis-Ord Gi* Statistic",
    palette = colorsg,
    breaks = breaksg,
    midpoint = 0,
    labels = c("Sig. Neg. (5%)","Sig. Neg. (10%)","Not Significant","Sig. Pos. (10%)","Sig. Pos. (5%)")
  ) +
  tm_borders(col = "black",
             lwd = 0.5) +
  tm_text("Gstat",
          size = 0.5) +
  tm_compass(position = c("left","bottom")) +
  tm_layout(
    main.title = "Map of Getis-Ord Gi* Statistic for Homicide Rates in Baltimore, 2005 to 2017",
    main.title.size = 0.8,
    legend.position = c("left", "bottom"),
    compass.type = "4star",
    legend.text.size = 1,
    legend.title.size = 1,
    legend.outside = T,
    bg.color = "#d0e2ec"
  ) +
  tm_scale_bar(
    text.size = 0.5,
    color.dark = "black",
    color.light = "yellow",
    lwd = 1,
    position = c("left","bottom")
  ) +
  tmap_options(unit = "mi")
getis.map # Look at the map

