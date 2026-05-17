# ==============================================================================
# Script:        00_prepare_mplus_input_from_raw_ess_data.R
# Purpose:       This scripts imports raw ess, selects all is necessary for 
#                further analyses, and prepares Mplus import file
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

# set options
options(stringsAsFactors = FALSE)

# ---- Paths --------------------------------------------------------------------

# define paths
input_dir  <- file.path("00_data", "raw", "ess11")
output_dir <- file.path("00_data", "derived")
output_dir_mplus <- "02_mplus"

# ---- Import data --------------------------------------------------------------

ess11 <- read.csv(file.path(input_dir, "ESS11.csv"))

# ---- Data processing ----------------------------------------------------------

## --- ESS ----------------------------------------------------------------------

# collect all indicator names in a vector
indicators <- c(
  "ppltrst", "pplfair", "pplhlp", # interpersonal trust
  "sclmeet", "inprdsc", "sclact", # social relations
  "imbgeco", "imueclt", "imwbcnt", # openness
  "trstprl", "trstlgl", "trstplt", "trstprt", # institutional trust
  "stfgov", "stfdem", "stfedu", "stfhlth" # legitimacy institutions 
)

# extract all needed variables from the ess dataset
ess11 <- ess11[c("idno", "cntry", "region", "regunit", "dweight", "pweight", indicators)]

# create a unique id for each participant
ess11$unique_id <- paste0(ess11$cntry, ess11$idno)

## -- NUTS ----------------------------------------------------------------------

# IL does not use NUTS
# FI -> FI1D4  (Kainuu) in ESS & FI1D8 in NUTS 16/21/24 
# FI1D6 (Pohjois-Pohjanmaa) in ESS & FI1D9 in NUTS 16/21/24, 
# the ones in ESS are from NUTS (version 2013)

ess11 <- ess11 %>%
  # recode finnish regions as described above
  mutate(region = case_when(
    region == "FI1D4" ~ "FI1D8",
    region == "FI1D6" ~ "FI1D9",
    TRUE ~ region
  )) %>%
  # exclude regions from israel
  filter(!cntry=="IL")

# create variables containing all the higher level nuts regions for each respondent

ess11 <- ess11 %>%
  mutate(
    nuts1 = case_when(
      regunit == 1 ~ region,
      regunit %in% c(2, 3) ~ substr(region, 1, 3),
      TRUE ~ NA_character_
    ),
    nuts2 = case_when(
      regunit == 2 ~ region,
      regunit == 3 ~ substr(region, 1, 4),
      TRUE ~ NA_character_
    ),
    nuts3 = case_when(
      regunit == 3 ~ region,
      TRUE ~ NA_character_
    )
  )

# prepate export data for later use in mplus
# create numeric variables for country, nuts1, and rowid for later use in MPLUS
# nuts 1 will be the cluster variable in the multilevel cFA
ess11$nuts1_num <- as.numeric(as.factor(ess11$nuts1))
ess11$row_id <- 1:nrow(ess11)
ess11_mplus <- na.omit(ess11[c(indicators, "row_id", "nuts1_num")])

# ---- Export outputs -----------------------------------------------------------

write_csv(ess11, file.path(output_dir, "ess11_analysis_dataset.csv"))
write_csv(ess11_mplus, file.path(output_dir_mplus, "ess11_analysis_dataset_mplus.csv"))

# ---- End ----------------------------------------------------------------------






