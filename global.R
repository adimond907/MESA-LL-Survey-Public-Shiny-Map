library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyr)
library(sf)
library(here)

setwd(here())

# --- LOAD DATA & STATIC SHAPEFILES ---
raw_station_cpue <- read.csv("data/station_cpue.csv") %>% 
  select(-any_of("X"))


station_cpue <- raw_station_cpue %>%
  complete(
    nesting(Year, Survey, Station, Area_ID, Area, Latitude, Longitude),
    nesting(species_code, Species),
    fill = list(TotalCatch = 0) 
  )

area_cpue <- read.csv("data/area_cpue.csv") %>%
  select(-c(X))

# Load static management regions shapefile
regions_sf <- st_read("shapefiles/sablefish_management_regions.shp") %>%
  st_transform(4326) # Leaflet requires EPSG:4326

