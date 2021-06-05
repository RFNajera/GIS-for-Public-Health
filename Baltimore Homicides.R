# Standardizing Community Indicators
# GIS for Public Health Using R Programming
# https://classroom.google.com/u/0/c/MzUwMTM3NjEzODk5
# Rene F. Najera, DrPH
# Summer 2021

# Libraries ----

library(tidyverse)
library(raster)
library(tmap)
library(rgdal)
library(tigris)
library(sf)
library(spdep)
library(SpatialEpi)

# Load shapefiles ----

baltimore.shape <- readOGR("Total_Population",
                           "Total_Population")
water.shape <- readOGR("Water",
                       "water")

# Load the homicide data ----

baltimore.homicides.data <- read.csv("Baltimore-2018-homicides-geocoded.csv") %>% 
  mutate(event_date = lubridate::mdy(factor(event_date))) %>% 
  mutate(month = factor(lubridate::month(event_date),
                        labels = c("Jan","Feb","Mar","Apr",
                                   "May","Jun","Jul","Aug",
                                   "Sep","Oct","Nov","Dec"))) %>% 
  mutate(week = lubridate::epiweek(event_date))

# Descriptive statistics of the victims and the date/time of homicides ----

table(baltimore.homicides.data$race)
hist(baltimore.homicides.data$age)
table(baltimore.homicides.data$sex)
table(baltimore.homicides.data$mode)
date.graph <- baltimore.homicides.data %>% 
  ggplot(aes(x = week)) +
  geom_bar()
date.graph
month.graph <- baltimore.homicides.data %>% 
  ggplot(aes(x = month)) +
  geom_bar()
month.graph

# Get the points to be a shapefile ----

baltimore.homicides <- st_as_sf(x = baltimore.homicides.data, 
                        coords = c("lon", "lat"),
                        crs = "+proj=longlat +datum=WGS84")
baltimore.homicides <- as(baltimore.homicides, "Spatial")
baltimore.shape <- spTransform(baltimore.shape, CRS("+proj=longlat +datum=WGS84"))

# Map of where the homicides happened ----

tmap_mode("plot") # Set tmap to plot. To make interactive, use "view"

homicide.dots.map <-
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
homicide.dots.map

# Map of where and what month the homicides happened ----

homicide.dots.map <-
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
homicide.dots.map

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

homicide.dots.polys <- sp::over(baltimore.homicides,
                                baltimore.shape) %>% 
  group_by(CSA2010,tpop10) %>% 
  summarise(homicides = n()) %>% 
  mutate(rate = (homicides/tpop10)*10000)
homicide.rates <- geo_join(baltimore.shape,
                           homicide.dots.polys,
                           "CSA2010",
                           "CSA2010")

# Make the map of the rates ----

homicide.dots.map <-
  tm_shape(homicide.rates) +
  tm_borders(col = "black") +
  tm_fill(col = "rate",
          textNA = "No Homicides",
          title = "Rate per 10k Residents") +
  # tm_shape(baltimore.homicides) +
  # tm_symbols(col = "race",
  #            size = 0.1,
  #            shape = "race",
  #            legend.shape.show = T,
  #            legend.col.show = F,
  #            title.shape = "Race/Ethnicity") +
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

# Spatial Autocorrelation ----

map_nbq <-
  poly2nb(homicide.rates) # Creates list of neighbors to each CSA
added <-
  as.integer(c(7, 50)) # Adds neighbors 7 and 50 to CSA 4 (it looks like an island)
map_nbq[[4]] <- added # Fixes the region (4) without neighbors
View(map_nbq) # View to make sure CSA 4 has neighbors 7 and 50.

map_nbq_w <-
  nb2listw(map_nbq, zero.policy = T) # Creates list of neighbors and weights. Weight = 1/number of neighbors.
View(map_nbq_w) # View the list

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

# Moran's I test

moran.test(homicide.rates$rate,map_nbq_w)
moran.plot(homicide.rates$rate,map_nbq_w)

# Moran's I Plot

x <- homicide.rates$srate %>% as.vector()
y <- homicide.rates$lag_srate %>% as.vector()
xx <- data.frame(x, y) # Matrix of the variables we just created

moran.plot(x, map_nbq_w) # One way to make a Moran Plot

# Another way to make Moran's I Plot

plot(x = homicide.rates$srate,
     y = homicide.rates$lag_srate,
     main = "Moran Scatterplot Homicide Rate",
     xlab = "Homicide Rate (Scaled)",
     ylab = "Lagged Homicides Rate (Scaled)"
     )
abline(h = 0,
       v = 0) # Adds the crosshairs at (0,0)
abline(
  lm(homicide.rates$lag_srate ~ homicide.rates$srate),
  lty = 3,
  lwd = 4,
  col = "red"
) # Adds a red dotted line to show the line of best fit

# Designate the quadrants

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
local.moran$Ii <- round(local.moran$Ii,1)

map.counts <- geo_join(homicide.rates,
                       local.moran,
                       "OBJECTID",
                       "OBJECTID")

colors <-
  c("red", "lightpink", "skyblue2", "blue", "white") # Color Palette

local.moran.map <-
  tm_shape(map.counts) +
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

map.counts$quad_sig <-
  NA # Creates a variable for where the significant pairs fall on the Moran plot
map.counts@data[(map.counts$srate >= 0 &
                   map.counts$lag_srate >= 0) &
                  (local.moran[, 5] <= 0.05), "quad_sig"] <- "High-High, p <0.05" # High-High
map.counts@data[(map.counts$srate <= 0 &
                   map.counts$lag_srate <= 0) &
                  (local.moran[, 5] <= 0.05), "quad_sig"] <- "Low-Low, p <0.05" # Low-Low
map.counts@data[(map.counts$srate >= 0 &
                   map.counts$lag_srate <= 0) &
                  (local.moran[, 5] <= 0.05), "quad_sig"] <- "High-Low, p <0.05" # High-Low
map.counts@data[(map.counts$srate <= 0 &
                   map.counts$lag_srate >= 0) &
                  (local.moran[, 5] <= 0.05), "quad_sig"] <- "Low-High, p <0.05" # Low-High
map.counts@data[(map.counts$srate <= 0 &
                   map.counts$lag_srate >= 0) &
                  (local.moran[, 5] <= 0.05), "quad_sig"] <-"Not Significant. p>0.05" # Non-significant

colors2 <-
  c("red", "blue", "white") # Color Palette

local.moran.map.sig <-
  tm_shape(map.counts) +
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
    lwd = 1
  ) +
  tm_add_legend(
    type = "text",
    col = "black",
    title = "Moran's I p-value",
    text = "0.000",
    size = 4
  ) +
  tmap_options(unit = "mi")
local.moran.map.sig

# Getis-Ord Hot Spots ----

#Calculate Getis-Ord "Hot Spots"
homicide.rates$Gstat<-round(localG(homicide.rates$rate,
                             map_nbq_w),2)
head(homicide.rates)

#Set breaks at standard confidence levels
breaksg<-c(-Inf,-1.96,-1.645,1.645,1.96,Inf)
colorsg<-c("blue","cyan","white","magenta","red")

#make a choropleth map
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
getis.map
