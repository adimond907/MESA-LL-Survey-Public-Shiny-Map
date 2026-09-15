library(akgfmaps)
library(sf)
library(dplyr)
library(rmapshaper)

# 1. Fetch NMFS areas with EPSG:4326
nmfs_raw <- get_nmfs_areas(set.crs = "EPSG:4326")

# 2. Map REP_AREA numbers to your 6 exact Area names
sable_regions <- nmfs_raw %>%
  mutate(REP_AREA = as.numeric(as.character(REP_AREA))) %>%
  mutate(Area = case_when(
    REP_AREA %in% c(541, 542, 543) ~ "Aleutians",
    REP_AREA %in% c(610)           ~ "Western Gulf of Alaska",
    REP_AREA %in% c(620, 630)      ~ "Central Gulf of Alaska",
    REP_AREA %in% c(640)           ~ "West Yakutat",
    REP_AREA %in% c(650)           ~ "East Yakutat/Southeast",
    REP_AREA >= 500 & REP_AREA < 541 ~ "Bering Sea",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Area)) %>%
  group_by(Area) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

# 3. Save clean shapefile
st_write(
  sable_regions,
  "shapefiles/sablefish_management_regions.shp",
  delete_dsn = TRUE
)


# Load full-resolution shapefile
regions_sf <- st_read("shapefiles/sablefish_management_regions.shp")

# Simplify geometries (keep 5% of vertices)
regions_simple <- ms_simplify(regions_sf, keep = 0.05, keep_shapes = TRUE)

# Save lightweight shapefile
st_write(
  regions_simple,
  "shapefiles/sablefish_management_regions.shp",
  delete_dsn = TRUE
)