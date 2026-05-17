# ==============================================================================
# Script:        02_import_admin_data.R
# Purpose:       Eurostat data is downloaded from the Eurostat database using 
#                the package eurostat
# Project:       The use of OpenStreetMaps in small area estimation of social 
#                cohesion
# Author:        Lidiya Mishieva
# Created:       2026-05-13
# Last updated:  2026-05-13
# R version:     4.5.2 (2025-10-31)
# OS:            x86_64, linux-gnu
# ==============================================================================

# ---- Notes --------------------------------------------------------------------

# IT TAKES TIME TO LOAD ALL THE DATA
# THE OUTPUT IS ALREADY STORED IN THE OUTPUT DIRECTORY

# ---- Setup --------------------------------------------------------------------

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(sf)
library(giscoR)
library(geojsonsf)
library(eurostat)

# ---- Paths --------------------------------------------------------------------

output_dir <- file.path("00_data", "raw", "admin")

# ---- Import data --------------------------------------------------------------

# import data from eurostat

# collect the names of the required datasets to be imported
dataset_names <- c(
  "Population on 1st January by age, sex, type of projection and NUTS 3 region",
  "Population on 1 January by age, sex and NUTS 2 region",
  "Police-recorded offences by NUTS 3 region",
  "Area by NUTS 3 region",
  "Population in private households by educational attainment level and NUTS 2 region",
  "Employment rates by NUTS 2 region",
  "Persons at risk of poverty or social exclusion by NUTS 2 region"
)


# loop to import the data from eurostat using eurostat package
data_list <- lapply(dataset_names, function(name) {
  
  # look for the dataset in the eurostat database
  search_results <- search_eurostat(name, type = "dataset")
  
  # if multiple available, take the dataset with the most recent information
  search_results <- search_results %>% mutate(data.end = as.numeric(data.end))
  best_match <- search_results %>%
    filter(data.end == max(data.end, na.rm = TRUE)) %>%
    slice(1)
  
  # extract the dataset id
  id <- best_match$code
  
  # import the data from the database
  dat <- get_eurostat(id, time_format = "num", stringsAsFactors = TRUE)
  
  # write the imported dataset into a list
  list(
    dataset_name = name,
    dataset_id = id,
    max_year = best_match$data.end,
    data = dat
  )
})

# provide names to the list containing all the imported datasets
names(data_list) <- dataset_names

# ---- Data processing ----------------------------------------------------------

# ---- Export outputs -----------------------------------------------------------

write_rds(data_list, file.path(output_dir, "raw_data_estat.Rds"))

# ---- End ----------------------------------------------------------------------
