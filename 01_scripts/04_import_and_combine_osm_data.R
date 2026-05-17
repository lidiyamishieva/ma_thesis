# ==============================================================================
# Script:        04_import_and_combine_osm_data.R
# Purpose:       Importing raw OSM data and combine in one dataset
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
library(haven)
library(sf)

# ---- Paths --------------------------------------------------------------------

output_dir <- file.path("00_data", "derived")
input_dir <- file.path("00_data", "raw", "osm", "kml")

# collect the titles of all the separate osm files in a vector
# in order to read them in at once
file_names <- c(
  "AT_2023",
  "BE_2023",
  "BG_2023",
  "CH_2023",
  "DE_2023_education",
  "DE_2023_entertainment",
  "DE_2023_facilities",
  "DE_2023_healthcare",
  "DE_2023_other",
  "ES_2023_education",
  "ES_2023_entertainment",
  "ES_2023_facilities",
  "ES_2023_healthcare",
  "ES_2023_other",
  "FI_2023",
  "FR_2023_education",
  "FR_2023_entertainment",
  "FR_2023_facilities",
  "FR_2023_healthcare",
  "FR_2023_other",
  "GR_2023",
  "HR_2023",
  "HU_2023",
  "IE_2023",
  "IT_2023_education",
  "IT_2023_entertainment",
  "IT_2023_facilities",
  "IT_2023_healthcare",
  "IT_2023_other",
  "NL_2023",
  "NO_2023_education",
  "NO_2023_entertainment",
  "NO_2023_facilities",
  "NO_2023_healthcare",
  "NO_2023_other",
  "PL_2023_education",
  "PL_2023_entertainment",
  "PL_2023_facilities",
  "PL_2023_healthcare",
  "PL_2023_other",
  "PT_2023",
  "SE_2023_education",
  "SE_2023_entertainment",
  "SE_2023_facilities",
  "SE_2023_healthcare",
  "SE_2023_other",
  "SI_2023"
  )

# define path names for all osm files

path_names <- file.path(input_dir, paste0(file_names, ".kml"))

# ---- Import data --------------------------------------------------------------

# read in all the files into R
all_data <- sapply(path_names, sf::read_sf)

# ---- Data processing ----------------------------------------------------------

# for bigger countries data was downloaded in chunks and needs
# to be merged differently

# collect the country codes of those countries in a vector
big_countries <- c("DE", "ES", "FR", "IT", "NO", "PL", "SE")

# collect all other country codes
country_codes <- substr(file_names, 1, 2)

# extract dataframes from the data list directly into the environment
# by assigning them to objects named as as file names
for (i in 1:length(file_names)) {assign(file_names[i], all_data[path_names][[i]])}

# clean up
# now that we have the separate data frames we 
# do not need the list with all data frames anymore
rm(all_data)


# function for combining the separate datasets for the big countries
combine_datasets <- function(prefix, year) {
  base_name <- paste0(prefix, "_", year)
  
  datasets <- list(
    get(paste0(base_name, "_education")),
    as.data.frame(get(paste0(base_name, "_healthcare"))),
    as.data.frame(get(paste0(base_name, "_entertainment"))),
    as.data.frame(get(paste0(base_name, "_facilities"))),
    as.data.frame(get(paste0(base_name, "_other")))
  )
  
  combined <- datasets %>% reduce(full_join)
  
  rm(list = paste0(
    base_name, c("_education", "_healthcare", "_entertainment", "_facilities", "_other")),
    envir = .GlobalEnv)
  
  return(combined)
}

# apply the function to combine the separates datasets of the bigger countries
DE_2023 <- combine_datasets("DE", 2023)
ES_2023 <- combine_datasets("ES", 2023)
FR_2023 <- combine_datasets("FR", 2023)
IT_2023 <- combine_datasets("IT", 2023)
PL_2023 <- combine_datasets("PL", 2023)
SE_2023 <- combine_datasets("SE", 2023)
NO_2023 <- combine_datasets("NO", 2023)

# create a named list with all final datasets
dataset_names <- paste0(unique(country_codes), "_2023")
datasets_2023 <- lapply(dataset_names, get)
names(datasets_2023) <- dataset_names

# clean up
rm(list = dataset_names, envir = .GlobalEnv)

# create a country id within each country dataset
datasets_2023 <- lapply(names(datasets_2023), function(name) {
  df <- datasets_2023[[name]]
  df$country_id <- name
  df <- df[, c("country_id", setdiff(names(df), "country_id"))]
  df
})

# we have to provide the names again
names(datasets_2023) <- dataset_names 

# count the number of non missing variables in each dataset
# and collect them in a variable within each dataset
datasets_2023 <- lapply(datasets_2023, function(df) {
  df$valid_count <- rowSums(!is.na(df))
  df
})


# reduce the data to necessary variables
keep_vars <- c(
  "country_id",
  "Name",
  "_id",
  "wikidata",
  "amenity",
  "building",
  "place_of_worship",
  "valid_count"
)

datasets_2023_reduced <- lapply(datasets_2023, function(df) {
  df[, intersect(keep_vars, names(df)), drop = FALSE]
})

# tranform them all to data frames (they are sf objects now)
datasets_2023_reduced[2:length(datasets_2023_reduced)] <- lapply(
  datasets_2023_reduced[2:length(datasets_2023_reduced)], as.data.frame)

# merge all datasets into 1, and check for duplicates
combined_2023 <- reduce(datasets_2023_reduced, full_join)

# ---- Export outputs -----------------------------------------------------------

# export the data containing all osm tags
saveRDS(datasets_2023, file.path(output_dir, "osm_datasets_as_list_all_tags_retained.Rds"))

# export combined and reduced osm data
write_csv(combined_2023, file.path(output_dir, "osm_data_combined_2023.csv"))
st_write(combined_2023, file.path(output_dir, "osm_data_combined_2023.geojson"), 
         append = FALSE, delete_dsn = TRUE, quiet = TRUE)

# ---- End ----------------------------------------------------------------------


















