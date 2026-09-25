library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(sf)
library(here)

setwd(here())

# --- 1. LOAD & PREPARE STATION DATA ---
raw_station_cpue <- read.csv("data/station_cpue.csv") %>% 
  select(-any_of("X"))

# Get all unique stations surveyed per year (with spatial metadata)
unique_stations <- raw_station_cpue %>%
  select(Year, Survey, Station, Area_ID, Area, Latitude, Longitude) %>%
  distinct()

# Get all unique species mapping
unique_species <- raw_station_cpue %>%
  select(species_code, Species) %>%
  distinct()

# Cross-join stations and species to form the full template
station_species_grid <- cross_join(unique_stations, unique_species)

# Merge back raw data and fill missing catches with 0
station_cpue <- station_species_grid %>%
  left_join(
    raw_station_cpue, 
    by = c("Year", "Survey", "Station", "Area_ID", "Area", "Latitude", "Longitude", "species_code", "Species")
  ) %>%
  mutate(TotalCatch = coalesce(TotalCatch, 0))

# --- 2. LOAD REGIONAL DATA & SHAPEFILES ---
area_cpue <- read.csv("data/area_cpue.csv") %>% 
  select(-any_of("X"))

regions_sf <- st_read("shapefiles/sablefish_management_regions.shp") %>%
  st_transform(4326) # Leaflet requires EPSG:4326