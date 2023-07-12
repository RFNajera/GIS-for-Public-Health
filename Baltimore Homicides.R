# Baltimore Homicide Mapping
# GIS for Public Health Using R Programming
# https://classroom.google.com/u/0/c/MzUwMTM3NjEzODk5
# Rene F. Najera, DrPH
# Updated July 2023

# Libraries ----

library(tidyverse) # For data manipulation
library(tmap) # For mapping
library(tigris) # For dealing with spatial data, like spatial joins
library(sf) # For dealing with spatial datal, like setting CRS
library(spdep) # For calculating spatial dependencies, like Moran's I or Getis-Ord


# Load shapefiles ----

baltimore.shape <- read_sf("Total_Population",
                           "Total_Population")
water.shape <- read_sf("Water",
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

baltimore.homicides <- st_as_sf(x = baltimore.homicides.data, # Transform data frame to simple feature (sf)
                        coords = c("lon", "lat"), # Tell it where the latitude and longitude are
                        crs = "+proj=longlat +datum=WGS84") %>% # Give it a CRS
  mutate(CSA2010 = community)
baltimore.shape <- st_as_sf(baltimore.shape, CRS("+proj=longlat +datum=WGS84")) # Give the Baltimore shape the same CRS

# Map of where the homicides happened ----

tmap_mode("plot") # Set tmap to plot. To make interactive, use "view"

homicide.dots.map <- # Create the map of homicides with points
  tm_shape(baltimore.shape) +
  tm_borders(col = "black",
             lwd = 0.5) +
  tm_shape(baltimore.homicides) +
  tm_dots(col = "red",
          title = "2017 Homicides",
          size = 0.1, ) +
  tm_compass(position = c("left","bottom")) +
  tm_layout(
    main.title = "Homicides in Baltimore City, 2005 to 2017",
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
    main.title = "Homicides in Baltimore City, 2005 to 2017",
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
    main.title = "Homicides in Baltimore City, 2005 to 2017",
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

# set CRS for the points to be the same as shapefile
st_crs(baltimore.homicides) <- st_crs(baltimore.shape)

# Join the points to the polygons to calculate rates ----

homicide.rates.by.csa <- baltimore.homicides %>% 
  group_by(CSA2010) %>% 
  summarise(n = n()) %>% 
  st_drop_geometry() %>% 
  left_join(baltimore.shape) %>% 
  mutate(rate = (n/tpop10)*10000) %>% 
  st_as_sf()
summary(homicide.rates.by.csa$rate) # To see the legend range

# Make the map of the rates ----

homicide.rate.map <-
  tm_shape(homicide.rates.by.csa) +
  tm_borders(col = "black") +
  tm_fill(col = "rate",
          colorNA = "white",
          textNA = "No Homicides",
          title = "Rate per 10k Residents") +
  tm_compass(position = c("left","bottom")) +
  tm_layout(
    main.title = "Homicides in Baltimore City, 2005 to 2017",
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
homicide.rate.map

# Spatial Autocorrelation with Moran's I ----

homicide.rates <- homicide.rates.by.csa # Duplicate to process
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

homicide.rates <- homicide.rates %>% 
  as.data.frame() %>% 
  mutate(quad = as.character(case_when(homicide.rates$srate >= 0 &
                            homicide.rates$lag_srate >= 0 ~ "High-High",
                          homicide.rates$srate <= 0 &
                            homicide.rates$lag_srate <= 0 ~ "Low-Low",
                          homicide.rates$srate >= 0 &
                            homicide.rates$lag_srate <= 0 ~ "High-Low",
                          homicide.rates$srate <= 0 &
                            homicide.rates$lag_srate >= 0 ~ "Low-High",
                          TRUE ~ "Error")),
         OBJECTID = 1:nrow(homicide.rates)
         ) %>% 
  st_as_sf()

# Make the map

local.moran <- local.moran %>% # Creating some variables to clean up
  mutate(OBJECTID = 1:nrow(local.moran), # Object ID from 1 to 55
         pvalue = round(`Pr(z != E(Ii))`,3), # Rounding to three places
         Ii = round(Ii,1)) # Rounding to one place

moran.i.map <- st_join(baltimore.shape,
                       homicide.rates)

moran.i.map.2 <- left_join(moran.i.map,
                       local.moran,
                       by = c("OBJECTID.x"="OBJECTID"))


colors <-
  c("red", "lightpink", "skyblue2", "blue", "white") # Color Palette

local.moran.map <-
  tm_shape(moran.i.map.2) +
  tm_fill("quad",
          title = "Local Moran's I",
          palette = colors,
          colorNA = "white") +
  tm_borders(col = "black",
             lwd = 0.5) +
  tm_compass(position = c("left","bottom")) +
  tm_layout(
    main.title = "Local Moran's I for Homicide Rates in Baltimore, 2005 to 2017",
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
local.moran.map

# Keep only the CSAs with p-values less than 0.05 (though we should be using much smaller)

moran.i.map.2 <- moran.i.map.2 %>% 
  mutate(quad_sig = case_when(quad == "High-High" &
                                (`Pr(z != E(Ii))` <= 0.05) ~ "High-High, p <0.05",
                              quad == "Low-Low" &
                                (`Pr(z != E(Ii))` <= 0.05) ~ "Low-Low, p <0.05",
                              quad == "High-Low" &
                                (`Pr(z != E(Ii))` <= 0.05) ~ "High-Low, p <0.05",
                              quad == "Low-High" &
                                (`Pr(z != E(Ii))` <= 0.05) ~ "Low-High, p <0.05",
                              `Pr(z != E(Ii))` > 0.05 ~ "Not Significant, p>0.05",
                              TRUE ~ "Error"
                              )
         )

colors2 <-
  c("red", "blue", "white") # Color Palette

local.moran.map.sig <- # Make the map of the CSAs that have significant observations
  tm_shape(moran.i.map.2) +
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
    main.title = "Local Moran's I for Homicide Rates in Baltimore, 2005 to 2017",
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
    main.title = "Getis-Ord Gi* Statistic for Homicide Rates in Baltimore, 2005 to 2017",
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
