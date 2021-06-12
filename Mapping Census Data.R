# Mapping Census Data
# GIS for Public Health Using R Programming
# https://classroom.google.com/u/0/c/MzUwMTM3NjEzODk5
# Rene F. Najera, DrPH
# Summer 2021

# Libraries ----

library(tidycensus) # To load the ACS data
# census_api_key(key="YOUR_KEY", install = T, overwrite = T) # Don't forget to restart R
library(tidyverse) # To manipulate data
library(tigris) # To read/manipulate shape files
library(sf) # To create simple features for mapping
library(tmap) # To make the map

# Look at ACS Variables ----

# The following code allows you to look at all the variables in the ACS

v19 <- load_variables(2019, # Year you're interested
                      "acs5", # What part of the ACS you're pulling data from
                      cache = T # Save the data in cache for quicker use each time
                      )
View(v19)

# Mapping overcrowding ----

overcrowding.vars <- c(paste0("B25014", c("H", "I", "B", "D", "G"), "_001"), # Pull the total households by race
          paste0("B25014", c("H", "I", "B", "D", "G"), "_003")) # Pull the overcrowded households by race

ffx.overcrowding <- get_acs(
  geography = "tract",
  state = "VA",
  county = c("Fairfax County",
             "Fairfax City",
             "Falls Church City"),
  variables = c(overcrowding.vars, # Pull the variables above plus...
                "B25014_001", # Total occupants per room, denominator
                "B25014_005", # Owner occupied, 1 to 1.5 occupants per room
                "B25014_011", # Renter occupied, 1 to 1.5 occupants per room
                "B25014_006", # Owner occupied, 1.5 to 2 occupants per room
                "B25014_012", # Renter occupied, 1.5 to 2 occupants per room
                "B25014_007", # Owner occupied, 2 or more occupants per room
                "B25014_013" # Renter occupied, 2 or more occupants per room
                ),
  year = 2019, # What year?
  geometry = T # Get the geometry to make a shape file
  ) %>%
  select(GEOID, NAME, variable, estimate) %>% # Variables to keep
  spread(variable, estimate) %>% # Pivot the data on these
  rowwise() %>% # How to pivot
  mutate(
    pct_overcrowded_all = sum(c(B25014_007, # Add up all the households with more
                                B25014_006, # than one occupant per room
                                B25014_005,
                                B25014_011,
                                B25014_012,
                                B25014_013
                                )
                              ) / B25014_001 * 100, # Divide by the total and multiply by 100
    # White H, Hispanic I, B Black, D Asian, G Multiracial
    pct_overcrowded_white = B25014H_003 / B25014H_001 * 100,
    pct_overcrowded_hispanic = B25014I_003 / B25014I_001 * 100,
    pct_overcrowded_black = B25014B_003 / B25014B_001 * 100,
    pct_overcrowded_asian = B25014D_003 / B25014D_001 * 100,
    pct_overcrowded_multi = B25014G_003 / B25014G_001 * 100,
    GEOID = as.numeric(GEOID)
  ) %>%
  select(
    GEOID,
    NAME,
    pct_overcrowded_all,
    pct_overcrowded_white,
    pct_overcrowded_hispanic,
    pct_overcrowded_black,
    pct_overcrowded_asian,
    pct_overcrowded_multi
  )

map.1 <- tm_shape(ffx.overcrowding) +
  tm_polygons(col = c("pct_overcrowded_all",
                      "pct_overcrowded_white",
                      "pct_overcrowded_hispanic",
                      "pct_overcrowded_black",
                      "pct_overcrowded_asian",
                      "pct_overcrowded_multi"),
              title = "% of Households Overcrowded",
              legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")),
              style = "fixed",
              breaks = c(0,10,20,30,40,50,60,70,80,90,100)
              ) +
  tm_layout(panel.labels = c("All Residents",
                             "White",
                             "Hispanic",
                             "Black",
                             "Asian",
                             "Multiracial"),
            legend.outside = T
            ) +
  tm_compass(position = c("left","top"))
map.1

# Mapping poverty ----

poverty.vars <- c(paste0("B17020", c("H", "I", "C", "B", "D", "G"), "_001"),
          paste0("B17020", c("H", "I", "C", "B", "D", "G"), "_002"))

ffx.poverty <- get_acs(geography = "tract",
                       state = "VA",
                       county = c("Fairfax County",
                                  "Fairfax City",
                                  "Falls Church City"),
                       variables = c(poverty.vars,# Poverty denominator
                                     "B17020_001" # Total Overall
                                     ),
                       year = 2019, # What year
                       geometry = T # Get the geometry to make the map
                       ) %>%
  select(GEOID, NAME, variable, estimate) %>%
  spread(variable, estimate) %>%
  rowwise() %>%
  mutate(pct_poverty_all = sum(c(B17020H_002,
                                 B17020I_002,
                                 B17020B_002,
                                 B17020C_002,
                                 B17020D_002,
                                 B17020G_002)
                               ) / B17020_001 * 100,
    # White H, Hispanic I, B Black, D Asian, G Multiracial, C American Indian
    pct_poverty_white = B17020H_002 / B17020H_001 * 100,
    pct_poverty_hispanic = B17020I_002 / B17020I_001 * 100,
    pct_poverty_black = B17020B_002 / B17020B_001 * 100,
    pct_poverty_asian = B17020D_002 / B17020D_001 * 100,
    pct_poverty_multi = B17020G_002 / B17020G_001 * 100,
    pct_poverty_ai = B17020C_002 / B17020C_001 * 100,
    GEOID = as.numeric(GEOID)) %>%
  select(GEOID,
         NAME,
         pct_poverty_all,
         pct_poverty_white,
         pct_poverty_hispanic,
         pct_poverty_black,
         pct_poverty_asian,
         pct_poverty_multi,
         pct_poverty_ai
         )

# Make the map of poverty by census tract ----

map.2 <- tm_shape(ffx.poverty) +
  tm_polygons(col = c("pct_poverty_all",
                      "pct_poverty_white",
                      "pct_poverty_hispanic",
                      "pct_poverty_black",
                      "pct_poverty_asian",
                      "pct_poverty_multi"),
              title = "% of Households in Poverty",
              legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")),
              style = "fixed",
              breaks = c(0,10,20,30,40,50,60,70,80,90,100)
              ) +
  tm_layout(panel.labels = c("All Residents",
                             "White",
                             "Hispanic",
                             "Black",
                             "Asian",
                             "Multiracial"),
            legend.outside = T
            ) +
  tm_compass(position = c("left","top"))
map.2

# Mapping poverty in Frederick County, Maryland ----

fred.poverty <- get_acs(geography = "tract",
                       state = "MD",
                       county = c("Frederick County"),
                       variables = c(poverty.vars,# Poverty denominator
                                     "B17020_001" # Total Overall
                       ),
                       year = 2019, # What year
                       geometry = T # Get the geometry to make the map
) %>%
  select(GEOID, NAME, variable, estimate) %>%
  spread(variable, estimate) %>%
  rowwise() %>%
  mutate(pct_poverty_all = sum(c(B17020H_002,
                                 B17020I_002,
                                 B17020B_002,
                                 B17020C_002,
                                 B17020D_002,
                                 B17020G_002)
  ) / B17020_001 * 100,
  # White H, Hispanic I, B Black, D Asian, G Multiracial, C American Indian
  pct_poverty_white = B17020H_002 / B17020H_001 * 100,
  pct_poverty_hispanic = B17020I_002 / B17020I_001 * 100,
  pct_poverty_black = B17020B_002 / B17020B_001 * 100,
  pct_poverty_asian = B17020D_002 / B17020D_001 * 100,
  pct_poverty_multi = B17020G_002 / B17020G_001 * 100,
  pct_poverty_ai = B17020C_002 / B17020C_001 * 100,
  GEOID = as.numeric(GEOID)) %>%
  select(GEOID,
         NAME,
         pct_poverty_all,
         pct_poverty_white,
         pct_poverty_hispanic,
         pct_poverty_black,
         pct_poverty_asian,
         pct_poverty_multi,
         pct_poverty_ai
  )

# Make the map of poverty by census tract ----

map.2.5 <- tm_shape(fred.poverty) +
  tm_polygons(col = c("pct_poverty_all",
                      "pct_poverty_white",
                      "pct_poverty_hispanic",
                      "pct_poverty_black",
                      "pct_poverty_asian",
                      "pct_poverty_multi"),
              title = "% of Households in Poverty",
              legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")),
              style = "fixed",
              breaks = c(0,10,20,30,40,50,60,70,80,90,100)
  ) +
  tm_layout(main.title = "Poverty in Frederick County, Maryland",
            
            panel.labels = c("All Residents",
                             "White",
                             "Hispanic",
                             "Black",
                             "Asian",
                             "Multiracial"),
            legend.outside = T
  ) +
  tm_compass(position = c("left","top")) +
  tm_credits("2019 ACS 5-Year Estimate",
             position=c("right", "bottom"),
             size = 0.5)
map.2.5

# Poverty by ZIP code in all of Virginia ----

# Getting data on poverty  by ZIP----

ffx.poverty.zip <- get_acs(geography = "zcta", # ZIP Code tabulation area
                           state = "VA",
                           variables = c(poverty.vars,
                                         "B17020_001" # Total Overall
                                         ),
                           year = 2019, # What year?
                           geometry = T # Get the geometry to make the maps
                           ) %>%
  dplyr::select(GEOID, NAME, variable, estimate) %>%
  spread(variable, estimate) %>%
  rowwise() %>%
  mutate(
    pct_poverty_all = sum(c(B17020H_002,
                            B17020I_002,
                            B17020B_002,
                            B17020C_002,
                            B17020D_002,
                            B17020G_002)
                          ) / B17020_001 * 100,
    # White H, Hispanic I, B Black, D Asian, G Multiracial, C American Indian
    pct_poverty_white = B17020H_002 / B17020H_001 * 100,
    pct_poverty_hispanic = B17020I_002 / B17020I_001 * 100,
    pct_poverty_black = B17020B_002 / B17020B_001 * 100,
    pct_poverty_asian = B17020D_002 / B17020D_001 * 100,
    pct_poverty_multi = B17020G_002 / B17020G_001 * 100,
    pct_poverty_ai = B17020C_002 / B17020C_001 * 100,
    GEOID = as.numeric(GEOID)
    ) %>%
  dplyr::select(
    GEOID,
    NAME,
    pct_poverty_all,
    pct_poverty_white,
    pct_poverty_hispanic,
    pct_poverty_black,
    pct_poverty_asian,
    pct_poverty_multi,
    pct_poverty_ai
  )

map.3 <- tm_shape(ffx.poverty.zip) +
  tm_polygons(col = c("pct_poverty_all",
                      "pct_poverty_white",
                      "pct_poverty_hispanic",
                      "pct_poverty_black",
                      "pct_poverty_asian",
                      "pct_poverty_multi",
                      "pct_poverty_ai"),
              title = "% of Households in Poverty",
              legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")),
              style = "fixed",
              breaks = c(0,10,20,30,40,50,60,70,80,90,100)
              ) +
  tm_layout(panel.labels = c("All Residents",
                             "White",
                             "Hispanic",
                             "Black",
                             "Asian",
                             "Multiracial",
                             "Native American"),
            legend.outside = T
            ) +
  tm_compass(position = c("left","top")) +
  tm_scale_bar(position = c("left","top"))
map.3

# Poverty by County in all of Virginia ----

# Getting data on poverty  by County ----

va.poverty.county <- get_acs(geography = "county", # County
                           state = "VA", # In Virginia Only
                           variables = c(poverty.vars,
                                         "B17020_001" # Total Overall
                           ),
                           year = 2019, # What year?
                           geometry = T # Get the geometry to make the maps
) %>%
  dplyr::select(GEOID, NAME, variable, estimate) %>%
  spread(variable, estimate) %>%
  rowwise() %>%
  mutate(
    pct_poverty_all = sum(c(B17020H_002,
                            B17020I_002,
                            B17020B_002,
                            B17020C_002,
                            B17020D_002,
                            B17020G_002)
    ) / B17020_001 * 100,
    # White H, Hispanic I, B Black, D Asian, G Multiracial, C American Indian
    pct_poverty_white = B17020H_002 / B17020H_001 * 100,
    pct_poverty_hispanic = B17020I_002 / B17020I_001 * 100,
    pct_poverty_black = B17020B_002 / B17020B_001 * 100,
    pct_poverty_asian = B17020D_002 / B17020D_001 * 100,
    pct_poverty_multi = B17020G_002 / B17020G_001 * 100,
    pct_poverty_ai = B17020C_002 / B17020C_001 * 100,
    GEOID = as.numeric(GEOID)
  ) %>%
  dplyr::select(
    GEOID,
    NAME,
    pct_poverty_all,
    pct_poverty_white,
    pct_poverty_hispanic,
    pct_poverty_black,
    pct_poverty_asian,
    pct_poverty_multi,
    pct_poverty_ai
  )

map.4 <- tm_shape(va.poverty.county) +
  tm_polygons(col = c("pct_poverty_all",
                      "pct_poverty_white",
                      "pct_poverty_hispanic",
                      "pct_poverty_black",
                      "pct_poverty_asian",
                      "pct_poverty_multi",
                      "pct_poverty_ai"),
              title = "% of Households in Poverty",
              legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")),
              style = "fixed",
              breaks = c(0,10,20,30,40,50,60,70,80,90,100)
  ) +
  tm_layout(panel.labels = c("All Residents",
                             "White",
                             "Hispanic",
                             "Black",
                             "Asian",
                             "Multiracial",
                             "Native American"),
            legend.outside = T
  ) +
  tm_compass(position = c("left","top")) +
  tm_scale_bar(position = c("left","top"))
map.4
