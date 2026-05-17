# ==============================================================================
# Script:        06_produce_direct_estimates.R
# Purpose:       Produces direct estimates
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
library(sae)

# ---- Paths --------------------------------------------------------------------

input_dir <- file.path("00_data","derived")
output_dir <- file.path("00_data", "derived")

# ---- Import data --------------------------------------------------------------

# data containing factor scores
data_analysis <- read_csv(file.path(input_dir, "ess11_analysis_dataset.csv"))
# data containing auxiliary data
europe_data <- st_read(file.path(input_dir, "europe_admin_osm_data.geojson"))

nuts <- giscoR::gisco_get_nuts(year = 2021, resolution = "10", cache = TRUE, update_cache = TRUE)

# ---- Data processing ----------------------------------------------------------

# min max scale the factor scores

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

data_analysis <- data_analysis %>%
  mutate(across(all_of(
    c("F1W", "F2W", "F3W", "F4W", "F5W")), ~ range01(.x ), .names = "{.col}_scaled"))

write_csv(data_analysis, file.path(output_dir, "ess11_analysis_dataset.csv"))

# merge data
nuts <- nuts %>% rename(region = NUTS_ID)
data_analysis <- st_as_sf(left_join(data_analysis, nuts[c("region", "NAME_LATN")]))

# produce horvitz-thompson direct estimates

# compute population sizes per domain based on nuts region population sizes
N <- europe_data %>% 
  dplyr::select(c("geo", "POPULATION_15TO99")) %>% 
  rename(region=geo)

N <- N %>% as.data.frame() %>% dplyr::select(c("region", "POPULATION_15TO99"))

data_analysis <- left_join(data_analysis, N, by="region")

# produce a data frame containing the area and the population size
N <- data_analysis %>% 
  group_by(region) %>% 
  summarise(POPULATION_15TO99=unique(POPULATION_15TO99))

plot(N)

# remove regions without the population data or zero population

idx <- filter(N, is.na(POPULATION_15TO99)==TRUE | POPULATION_15TO99==0) %>% 
  dplyr::select(region)

N <- N[!N$region %in% idx$region, ]

# remove the rows from the individual dataset as well
data_analysis <- data_analysis[!data_analysis$region %in% idx$region, ]

# compute sample size and effective sample size in each region
data_analysis <- data_analysis %>% group_by(region) %>% mutate(n = n())
data_analysis <- data_analysis %>% group_by(region) %>% mutate(eff_n = sum(dweight))

# adjust the design weights to area population size
data_analysis$dweight_adj <- (data_analysis$POPULATION_15TO99/data_analysis$n)*data_analysis$dweight
data_analysis$dweight_adj2 <- data_analysis$pweight*data_analysis$dweight*10000

check_weights <- data_analysis %>%
  dplyr::select(region, dweight_adj, dweight_adj2) %>%
  group_by(region) %>%
  summarise(pop_based_on_dweight_adj = sum(dweight_adj),
            pop_based_on_dweight_adj2 = sum(dweight_adj2)) %>%
  left_join(N %>% dplyr::select(region, POPULATION_15TO99) %>% as_data_frame())

boxplot(
  check_weights$pop_based_on_dweight_adj - check_weights$POPULATION_15TO99,
  check_weights$pop_based_on_dweight_adj2 - check_weights$POPULATION_15TO99,
  names = c("Method 1", "Method 2"),
  ylab = "Difference"
)

# produce direct estimates using design weights
f1_weighted <- sae::direct(y=data_analysis$F1W_scaled, sweight = data_analysis$dweight_adj, domsize = as.data.frame(N[, c("region", "POPULATION_15TO99")]), dom=data_analysis$region)
f2_weighted <- sae::direct(y=data_analysis$F2W_scaled, sweight = data_analysis$dweight_adj, domsize = as.data.frame(N[, c("region", "POPULATION_15TO99")]), dom=data_analysis$region)
f3_weighted <- sae::direct(y=data_analysis$F3W_scaled, sweight = data_analysis$dweight_adj, domsize = as.data.frame(N[, c("region", "POPULATION_15TO99")]), dom=data_analysis$region)
f4_weighted <- sae::direct(y=data_analysis$F4W_scaled, sweight = data_analysis$dweight_adj, domsize = as.data.frame(N[, c("region", "POPULATION_15TO99")]), dom=data_analysis$region)
f5_weighted <- sae::direct(y=data_analysis$F5W_scaled, sweight = data_analysis$dweight_adj, domsize = as.data.frame(N[, c("region", "POPULATION_15TO99")]), dom=data_analysis$region)

hist(f1_weighted$Direct)
hist(f2_weighted$Direct)
hist(f3_weighted$Direct)
hist(f4_weighted$Direct)
hist(f5_weighted$Direct)

# CVs behave as expected
plot(f1_weighted[order(f1_weighted$SampSize),]$CV, main="CV% of Direct Estimates of F1", xlab = "Areas, ordered by growing area sample size", ylab = "CV%")
plot(f2_weighted[order(f2_weighted$SampSize),]$CV, main="CV% of Direct Estimates of F2", xlab = "Areas, ordered by growing area sample size", ylab = "CV%")
plot(f3_weighted[order(f3_weighted$SampSize),]$CV, main="CV% of Direct Estimates of F3", xlab = "Areas, ordered by growing area sample size", ylab = "CV%")
plot(f4_weighted[order(f4_weighted$SampSize),]$CV, main="CV% of Direct Estimates of F4", xlab = "Areas, ordered by growing area sample size", ylab = "CV%")
plot(f5_weighted[order(f5_weighted$SampSize),]$CV, main="CV% of Direct Estimates of F5", xlab = "Areas, ordered by growing area sample size", ylab = "CV%")

# combine all estimates

for(i in 1:5){
  
  # weighted
  df_w <- get(paste0("f", i, "_weighted"))
  names(df_w)[1] <- "region"
  names(df_w)[3:ncol(df_w)] <- 
    paste0("F", i, "_", names(df_w)[3:ncol(df_w)])
  assign(paste0("f", i, "_weighted"), df_w)
}

all_direct_estimates <- reduce(
  list(f1_weighted, f2_weighted, f3_weighted, f4_weighted, f5_weighted),
  function(x, y) bind_cols(x, y[, -c(1,2)])
)

# check the order of regions
all_direct_estimates[1:2]
table(data_analysis$region)

all_direct_estimates_sf <- left_join(all_direct_estimates, nuts[c("region", "NAME_LATN")])
all_direct_estimates_sf <- st_as_sf(all_direct_estimates_sf)

# ---- Export outputs -----------------------------------------------------------

saveRDS(check_weights, file.path(output_dir, "weights.Rds"))
st_write(all_direct_estimates_sf, file.path(output_dir, "direct_estimates.geojson"), append = FALSE, delete_dsn = TRUE, quiet = TRUE)
saveRDS(all_direct_estimates, file.path(output_dir, "direct_estimates.rds"))

# ---- End ----------------------------------------------------------------------
















