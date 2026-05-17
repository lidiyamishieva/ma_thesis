# ==============================================================================
# Script:        import_and_clean_mplus_output.R
# Purpose:       import output from mplus and prepare the final individual
#                analysis dataset
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

# ---- Paths --------------------------------------------------------------------

# define paths
input_dir <- file.path("00_data", "derived")
input_dir_mplus  <- "02_mplus"
output_dir <- file.path("00_data", "derived")

# ---- Import data --------------------------------------------------------------

ess11 <- read.csv(file.path(input_dir, "ess11_analysis_dataset.csv"))
fscores <- read.csv(
  file.path(input_dir_mplus, 
            "ess11_analysis_dataset_mplus_nocolnames_fscores_nuts1.csv"))

# ---- Data processing ----------------------------------------------------------

# fix csv spacing
# transforming a single string column containing space-separated 
# values into a data frame with 44 columns
fscores <- as.data.frame(str_split_fixed(fscores[[1]], ' +', 45))

# collect all variable names in a vector
vars <- c(
  "empty",
  "PPLTRST", "PPLFAIR", "PPLHLP", 
  "SCLMEET", "INPRDSC", "SCLACT", 
  "IMBGECO", "IMUECLT", "IMWBCNT", 
  "TRSTPRL", "TRSTPLT", "TRSTPRT", 
  "STFGOV", "STFDEM", "STFEDU", "STFHLTH", 
  "row_id",
  "F1W", "F2W", "F3W", "F4W", "F5W", 
  "F1B", "F2B", "F3B", "F4B", "F5B",
  "B_PPLTRST", "B_PPLFAIR", "B_PPLHLP", 
  "B_SCLMEET", "B_INPRDSC", "B_SCLACT", 
  "B_IMBGECO", "B_IMUECLT", "B_IMWBCNT", 
  "B_TRSTPRL", "B_TRSTPLT", "B_TRSTPRT", 
  "B_STFGOV", "B_STFDEM", "B_STFEDU", "B_STFHLTH", 
  "NUTS1_NUM")


# give each column a name
names(fscores) <- vars

# recode the class of row_id back to integer
fscores$row_id <- as.integer(fscores$row_id)

# for one subject the scores were not produced:
# which(!(ess11_mplus$row_id %in% fscores$row_id))
# ess11_mplus[11148,] # row_id==12718

# merge factor scores with the rest of the data

ess11 <- ess11 %>%
  left_join(fscores, by = "row_id")

# listwise deletion ignoring nuts variables
nuts_vars <- c("nuts1", "nuts2", "nuts1_num", "NUTS1_NUM", "nuts3")
data_analysis <- ess11 %>% drop_na(-all_of(nuts_vars))

# remove duplicates and unnecessary columns
data_analysis <- subset(data_analysis, select = -c(row_id, nuts1_num, NUTS1_NUM,
                                                   idno, cntry, empty,
                                                   F1B, F2B, F3B, F4B, F5B,
                                                   PPLTRST, PPLFAIR, PPLHLP, 
                                                   SCLMEET, INPRDSC, SCLACT, 
                                                   IMBGECO, IMUECLT, IMWBCNT, 
                                                   TRSTPRL, TRSTPLT, TRSTPRT, 
                                                   STFGOV, STFDEM, STFEDU, STFHLTH,
                                                   B_PPLTRST, B_PPLFAIR, B_PPLHLP, 
                                                   B_SCLMEET, B_INPRDSC, B_SCLACT, 
                                                   B_IMBGECO, B_IMUECLT, B_IMWBCNT, 
                                                   B_TRSTPRL, B_TRSTPLT, B_TRSTPRT, 
                                                   B_STFGOV, B_STFDEM, B_STFEDU, B_STFHLTH))

# change the class of the factor scores to numeric
data_analysis <- data_analysis %>%
  mutate(across(all_of(c("F1W","F2W","F3W","F4W","F5W")),
                ~ as.numeric(.x)))

# ---- Export outputs -----------------------------------------------------------

write_csv(data_analysis, file.path(output_dir, "ess11_analysis_dataset.csv"))

# ---- End ----------------------------------------------------------------------







