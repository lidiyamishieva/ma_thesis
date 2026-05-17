# ==============================================================================
# Script:        05_aggregate_osm_data.R
# Purpose:       Aggregating OSM features within NUTS regions
# Project:       The use of OpenStreetMaps in small area estimation of social 
#                cohesion
# Author:        Lidiya Mishieva
# Created:       2026-05-13
# Last updated:  2026-05-13
# R version:     4.5.2 (2025-10-31)
# OS:            x86_64, linux-gnu
# ==============================================================================

# ---- Notes --------------------------------------------------------------------

# ---- Setup --------------------------------------------------------------------

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(sf)
library(giscoR)

# ---- Paths --------------------------------------------------------------------

input_dir <- file.path("00_data", "derived")
output_dir <- file.path("00_data", "derived")

# ---- Import data --------------------------------------------------------------

osm_data <- st_read(file.path(input_dir, "osm_data_combined_2023.geojson"))
europe_data <- st_read(file.path(input_dir, "estat_data.geojson"))
nuts <- giscoR::gisco_get_nuts(year = 2021, resolution = "10")

# ---- Data processing ----------------------------------------------------------

# check whether the projection is the same
st_crs(osm_data)
st_crs(nuts)

# align projections in both data sets
osm_data <- st_transform(osm_data, crs = st_crs(nuts))

# check again
st_crs(osm_data)==st_crs(nuts)

# write types of amenities available into a vector
amenities <- unique(osm_data$amenity)

# count amenities per administrative unit
counts_list <- map(amenities, function(am) {lengths(st_intersects(europe_data, filter(osm_data, amenity == am)))})

# give names to the list elements
names(counts_list) <- paste0(amenities, "_count")

# combine counts as variables in a dataset and append them to the administrative data
europe_data <- bind_cols(europe_data, as.data.frame(counts_list))

# combine amenities into categories
osm_data <- osm_data %>%
  mutate(
    amenity_category = case_when(
      amenity %in% c("college", "kindergarten", "library", "school", "university") ~ "Education",
      amenity %in% c("clinic", "doctors", "hospital", "nursing_home", "pharmacy", "social_facility") ~ "Healthcare",
      amenity %in% c("community_centre", "social_centre") ~ "Social places (gathering)",
      amenity %in% c("public_bath", "internet_cafe", "give_box", "kitchen", "marketplace", "nightclub") ~ "Social places (chance)", 
      amenity %in% c("place_of_worship") ~ "Religion",
      amenity %in% c("refugee_site", "shower") ~ "Other"
    )
  )

# define variable names
# education_vars <- paste0(c("college", "kindergarten", "library", "school", "university"), "_count")
# healthcare_vars <- paste0(c("clinic", "doctors", "hospital", "nursing_home", "pharmacy", "social_facility"), "_count")
# social_gathering_vars <- paste0(c("community_centre", "social_centre"), "_count")
# social_chance_vars <- paste0(c("shower", "public_bath", "internet_cafe", "give_box", "kitchen", "marketplace", "nightclub"), "_count")

# count amenity groups according to the defined categories
# produce per 1k inhabitant stats

europe_data <- europe_data %>% 
  mutate(
    across(all_of(paste0(amenities, "_count")), ~ .x / TOT_POPULATION * 1000, .names = "{.col}_per_1k_inhab")#,
    
    # education_count = rowSums(across(all_of(education_vars)), na.rm = TRUE),
    # healthcare_count = rowSums(across(all_of(healthcare_vars)), na.rm = TRUE),
    # social_gathering_count = rowSums(across(all_of(social_gathering_vars)), na.rm = TRUE),
    # social_chance_count = rowSums(across(all_of(social_chance_vars)), na.rm = TRUE),
    # 
    # across(ends_with("_count") & starts_with(c("education", "healthcare", "social_gathering", "social_chance")), ~ .x / TOT_POPULATION * 1000, .names = "{.col}_per_1k_inhab")
  )

europe_data <- europe_data %>%
  dplyr::select(-all_of(paste0(amenities, "_count")))

# ---- Export outputs -----------------------------------------------------------

# export the data containing both administrative and osm data
write_csv(europe_data, file.path(output_dir, "europe_admin_osm_data.csv"))
st_write(europe_data, file.path(output_dir, "europe_admin_osm_data.geojson"), append = FALSE, delete_dsn = TRUE, quiet = TRUE)

# ---- End ----------------------------------------------------------------------

