# ==============================================================================
# Script:        03_transform_admin_data.R
# Purpose:       Transformation of administrative variables for further analysis
# Project:       The use of OpenStreetMaps in small area estimation of social 
#                cohesion
# Author:        Lidiya Mishieva
# Created:       2026-05-13
# Last updated:  2026-05-13
# R version:     4.5.2 (2025-10-31)
# OS:            x86_64, linux-gnu
# ==============================================================================

# ---- Notes --------------------------------------------------------------------

# International Standard Classification of Education (ISCED 2011)
# [ED0-2] Less than primary, primary and lower secondary education
# [ED3-8] Upper secondary, post-secondary non-tertiary and tertiary education
# [ED3_4] Upper secondary and post-secondary non-tertiary education
# [ED34_44] Upper secondary and post-secondary non-tertiary education - general
# [ED35_45] Upper secondary and post-secondary non-tertiary education - vocational
# [ED5-8] Tertiary education

# ESS regional levels available
# NUTS 1: Germany, Italy
# NUTS 2: Austria, Belgium, France, Greece, Netherlands, Norway, Poland, 
#         Portugal, Spain, Sweden, Switzerland
# NUTS 3: Bulgaria, Croatia, Finland, Hungary, Ireland, Slovakia

# ---- Setup --------------------------------------------------------------------

# clear environment
rm(list = ls())

library(tidyverse)
library(giscoR)
library(sf)

# ---- Paths --------------------------------------------------------------------

input_dir <- file.path("00_data", "raw", "admin")
output_dir <- file.path("00_data", "derived")

# ---- Import data --------------------------------------------------------------

data_list <- read_rds(file.path(input_dir, "raw_data_estat.Rds"))

nuts <- giscoR::gisco_get_nuts(
  year = 2021, resolution = "10", cache = TRUE, update_cache = TRUE)

# ---- Data processing ----------------------------------------------------------

## --- Filter -------------------------------------------------------------------

# eduction
education_nuts2 <- data_list$`Population in private households by educational attainment level and NUTS 2 region`$data %>%
  filter(TIME_PERIOD == 2023 & sex == "T" & isced11 %in% c("ED3_4", "ED5-8")) %>%
  dplyr::select(geo, isced11, age, values) %>%
  pivot_wider(
    names_from = c(isced11, age),
    values_from = values,
    names_sep = "_"
  )  %>%
  rename_with(~ gsub("ED3_4", "SECONDARY", .x), starts_with("ED3_4")) %>%
  rename_with(~ gsub("ED5-8", "TERTIARY", .x), starts_with("ED5-8"))

# employment
employment_nuts2 <- data_list$`Employment rates by NUTS 2 region`$data %>%
  filter(TIME_PERIOD == 2023 & sex == "T") %>%
  dplyr::select(geo, age, values) %>%
  pivot_wider(
    names_from = c(age),
    values_from = values,
    names_sep = "_"
  ) %>%
  rename_with(~ paste0("EMPL_RATE_", .x), starts_with("Y"))

# poverty
poverty_nuts2 <- data_list$`Persons at risk of poverty or social exclusion by NUTS 2 region`$data %>%
  filter(TIME_PERIOD == "2023") %>%
  dplyr::select(geo, values) %>%
  rename(POVERTY_RATE = values)

# gender
gender_nuts2 <- data_list$`Population on 1 January by age, sex and NUTS 2 region`$data %>%
  filter(TIME_PERIOD == 2023 & age == "TOTAL" & sex=="F") %>%
  dplyr::select(geo, values) %>%
  rename(FEMALE_nuts2 = values)

gender_nuts3 <- data_list$`Population on 1st January by age, sex, type of projection and NUTS 3 region`$data %>%
  filter(TIME_PERIOD == 2023 & age == "TOTAL" & sex == "F" & projection=="BSL") %>%
  dplyr::select(geo, values) %>%
  rename(FEMALE_nuts3 = values)

# public safety
police <- data_list$`Police-recorded offences by NUTS 3 region`$data %>%
  filter(
    # replace missings by the last available values
    (substr(geo, 1, 2) == "DE" & TIME_PERIOD == 2023) | (substr(geo, 1, 2) != "DE" & TIME_PERIOD == 2022)) %>%
  # 0401 is robbery, 0502 is theft
  filter(iccs %in% c("ICCS0401", "ICCS0502")) %>%
  filter(unit == "P_HTHAB") %>%
  dplyr::select(geo, iccs, values) %>%
  # transform to wide format
  pivot_wider(
    names_from = c(iccs),
    values_from = values,
    names_sep = "_"
  ) %>%
  rename(
    ICCS0502_THEFT = ICCS0502,
    ICCS0401_ROBBERY = ICCS0401
  )

# total population
population_nuts3 <- data_list$`Population on 1st January by age, sex, type of projection and NUTS 3 region`$data %>%
  filter(TIME_PERIOD == 2023 & age == "TOTAL" & sex=="T" & projection=="BSL") %>%
  dplyr::select(geo, values) %>%
  rename(TOT_POPULATION_nuts3 = values)

population_nuts2 <- data_list$`Population on 1 January by age, sex and NUTS 2 region`$data %>%
  filter(TIME_PERIOD == 2023 & age == "TOTAL" & sex=="T") %>%
  dplyr::select(geo, values) %>%
  rename(TOT_POPULATION_nuts2 = values)

# target population ess
population_target_nuts3 <- data_list$`Population on 1st January by age, sex, type of projection and NUTS 3 region`$data %>%
  filter(TIME_PERIOD == 2023 & age %in% c(paste0("Y", 15:99)) & sex=="T" & projection=="BSL") %>%
  dplyr::select(geo, values) %>%
  group_by(geo) %>%
  summarise(values = sum(values)) %>%
  rename(POPULATION_15TO99_nuts3 = values)

population_target_nuts2 <- data_list$`Population on 1 January by age, sex and NUTS 2 region`$data %>%
  filter(TIME_PERIOD == 2023 & age %in% c(paste0("Y", 15:99)) & sex=="T") %>%
  dplyr::select(geo, values) %>%
  group_by(geo) %>%
  summarise(values = sum(values)) %>%
  rename(POPULATION_15TO99_nuts2 = values)

# age groups
age_groups_nuts3 <- data_list$`Population on 1st January by age, sex, type of projection and NUTS 3 region`$data %>%
  filter(TIME_PERIOD == 2023, sex == "T", age %in% c("Y_LT1", paste0("Y", 1:99)), projection == "BSL") %>%
  mutate(
    age_group = case_when(
      age %in% c("Y_LT1", paste0("Y", 1:19)) ~ "Y_LT20",
      age %in% paste0("Y", 20:39)            ~ "Y20_39",
      age %in% paste0("Y", 40:59)            ~ "Y40_59",
      age %in% paste0("Y", 60:79)            ~ "Y60_79",
      age %in% paste0("Y", 80:99)            ~ "Y80_99"
    )
  ) %>%
  dplyr::select(c(geo, age_group, values)) %>%
  group_by(geo, age_group) %>%
  summarise(population = sum(values, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = age_group,
    values_from = population
  )

age_groups_nuts2 <- data_list$`Population on 1 January by age, sex and NUTS 2 region`$data %>%
  filter(TIME_PERIOD == 2023, sex == "T", age %in% c("Y_LT1", paste0("Y", 1:99))) %>%
  mutate(
    age_group = case_when(
      age %in% c("Y_LT1", paste0("Y", 1:19)) ~ "Y_LT20",
      age %in% paste0("Y", 20:39)            ~ "Y20_39",
      age %in% paste0("Y", 40:59)            ~ "Y40_59",
      age %in% paste0("Y", 60:79)            ~ "Y60_79",
      age %in% paste0("Y", 80:99)            ~ "Y80_99"
    )
  ) %>%
  dplyr::select(c(geo, age_group, values)) %>%
  group_by(geo, age_group) %>%
  summarise(population = sum(values, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = age_group,
    values_from = population
  )

# area
area <- data_list$`Area by NUTS 3 region`$data %>%
  filter(TIME_PERIOD == 2023 & landuse == "TOTAL") %>%
  rename(AREA_SQKM = values) %>%
  dplyr::select(c("geo", "AREA_SQKM"))

## --- Combine all variables ----------------------------------------------------

estat_dat <- nuts %>% dplyr::select(geo) %>%
  full_join(area, by = "geo") %>%
  full_join(population_nuts2, by = "geo") %>%
  full_join(population_target_nuts2, by = "geo") %>%
  full_join(population_nuts3, by = "geo") %>%
  full_join(population_target_nuts3, by = "geo") %>%
  full_join(police, by = "geo") %>%
  full_join(age_groups_nuts2, by = "geo") %>%
  full_join(age_groups_nuts3, by = "geo") %>%
  full_join(gender_nuts2, by = "geo") %>%
  full_join(gender_nuts3, by = "geo") %>%
  full_join(education_nuts2, by = "geo") %>%
  full_join(poverty_nuts2, by = "geo") %>%
  full_join(employment_nuts2, by = "geo")

## --- Variable transformations -------------------------------------------------

estat_dat <- estat_dat %>%
  mutate(
    TOT_POPULATION         = coalesce(TOT_POPULATION_nuts2, TOT_POPULATION_nuts3),
    POPULATION_15TO99      = coalesce(POPULATION_15TO99_nuts2, POPULATION_15TO99_nuts3),
    FEMALE                 = coalesce(FEMALE_nuts2, FEMALE_nuts3),
    Y_LT20                 = coalesce(Y_LT20.x, Y_LT20.y)*100,
    Y20_39                 = coalesce(Y20_39.x, Y20_39.y)*100,
    Y40_59                 = coalesce(Y40_59.x, Y40_59.y)*100,
    Y60_79                 = coalesce(Y60_79.x, Y60_79.y)*100,
    Y80_99                 = coalesce(Y80_99.x, Y80_99.y)*100,
    FEMALE_RATE            = FEMALE/TOT_POPULATION*100,
    ICCS0401_ROBBERY_RATE  = ICCS0401_ROBBERY/TOT_POPULATION*10000,
    ICCS0502_THEFT_RATE    = ICCS0502_THEFT/TOT_POPULATION*10000,
    POPDENSITY             = TOT_POPULATION/AREA_SQKM
  ) %>%
  # age rates
  mutate(across(starts_with("Y"), ~ .x / TOT_POPULATION, .names = "RATE_{.col}")) %>%
  dplyr::select(-c(TOT_POPULATION_nuts2, TOT_POPULATION_nuts3,
            POPULATION_15TO99_nuts2, POPULATION_15TO99_nuts3,
            FEMALE_nuts2, FEMALE_nuts3,
            ends_with(".x"), ends_with(".y")))

# exctract the names of the nuts2 level only variables
nuts2_only_vars <- unique(c(names(employment_nuts2), names(poverty_nuts2), names(education_nuts2)))
nuts2_only_vars <- nuts2_only_vars[2:length(nuts2_only_vars)] # remove "geo"

# for nuts2-level variables only, write the values of nuts2 variables 
# to the nuts3 regions within the nuts2 regions

estat_dat <- estat_dat %>%
  mutate(
    geo = as.character(geo),
    nuts2_id = substr(geo, 1, 4)
  ) %>%
  group_by(nuts2_id) %>%
  mutate(
    across(
      all_of(nuts2_only_vars), 
      ~ ifelse(nchar(geo) == 5, first(na.omit(.x)), .x))) %>%
  ungroup()

# country codes of ESS regions
ess_nuts1_reg <- c("DE", "IT")
ess_nuts2_reg <- c("AT", "BE", "FR", "EL", "NL", "NO", "PL", "PT", "ES", "SE", "CH")
ess_nuts3_reg <- c("BG", "HR", "FI", "HU", "IE", "SI")

# keep only regions that match the required NUTS level per country
estat_dat <- estat_dat %>%
  mutate(
    # extract country code
    country = str_sub(geo, 1, 2),
    # determine NUTS level based on length of geo code
    nuts_level = str_length(geo)
  ) %>%
  filter(
    (country %in% ess_nuts1_reg & nuts_level == 3) |
    (country %in% ess_nuts2_reg & nuts_level == 4) |
      (country %in% ess_nuts3_reg & nuts_level == 5)
  )

# produce an sf object containing all the geometries of the areas
estat_dat_sf <- left_join(as.data.frame(estat_dat), nuts)
estat_dat_sf <- st_as_sf(estat_dat_sf)

# remove variables that are not needed
estat_dat_sf <- estat_dat_sf %>% 
  dplyr::select(-c("LEVL_CODE", "URBN_TYPE", "CNTR_CODE", "MOUNT_TYPE", "COAST_TYPE",
                   "nuts2_id", "CAPT", "ISO3_CODE", "NAME_ENGL", "CC_STAT", "NAME_GERM",
                   "NUTS_NAME", "SVRG_UN", "NAME_FREN", "EFTA_STAT", "EU_STAT",  
                   "FEMALE", "Y_LT20", "Y20_39", "Y40_59", "Y60_79", "Y80_99", 
                   "ICCS0401_ROBBERY", "ICCS0502_THEFT"))

# ---- Export outputs -----------------------------------------------------------

sf::st_write(estat_dat_sf, 
             dsn = file.path(output_dir, "estat_data.geojson"), 
             layer = "estat_data_sf.geojson",
             append = FALSE, delete_dsn = TRUE, quiet = TRUE)

write_rds(as.data.frame(estat_dat_sf), file.path(output_dir, "estat_data.Rds"))

# ---- End ----------------------------------------------------------------------
