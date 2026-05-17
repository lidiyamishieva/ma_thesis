# ==============================================================================
# Script:        07_produce_final_data.R
# Purpose:       Prepare dataset for modeling
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

# ---- Paths --------------------------------------------------------------------

input_dir <- file.path("00_data", "derived")
output_dir <- file.path("00_data", "derived")

# ---- Import data --------------------------------------------------------------

europe_admin_osm_data <- st_read(file.path(input_dir, "europe_admin_osm_data.geojson"))
direct_estimates <- st_read(file.path(input_dir, "direct_estimates.geojson"))

# ---- Data processing ----------------------------------------------------------

europe_admin_osm_data <- rename(europe_admin_osm_data, region = geo)
all_data <- left_join(direct_estimates, as_tibble(europe_admin_osm_data), by=c("region", "geometry"))

# create dataset for SAE
mdata <- all_data %>%
  mutate(
    F1_direct_var = F1_SD^2,
    F2_direct_var = F2_SD^2,
    F3_direct_var = F3_SD^2,
    F4_direct_var = F4_SD^2,
    F5_direct_var = F5_SD^2
  ) %>% 
  dplyr::select(
    region,
    SampSize,
    NAME_LATN.x,
    SECONDARY_Y25.34,
    TERTIARY_Y25.34,
    SECONDARY_Y25.64,
    TERTIARY_Y25.64,
    POVERTY_RATE,
    EMPL_RATE_Y25.34,
    EMPL_RATE_Y20.64,
    FEMALE_RATE,
    ICCS0401_ROBBERY_RATE,
    ICCS0502_THEFT_RATE,
    POPDENSITY,
    contains("RATE_Y"),
    contains("Direct"),
    contains("direct_var"),
    contains("count_per_1k_inhab"),
    geometry
  )

# listwise deletion of missing values
mdata <- na.omit(mdata) 

# ---- Export outputs -----------------------------------------------------------

st_write(mdata, file.path(output_dir, "mdata.geojson"), append = FALSE, delete_dsn = TRUE, quiet = TRUE)
write_rds(mdata, file.path(output_dir, "mdata.Rds"))

# ---- End ----------------------------------------------------------------------
