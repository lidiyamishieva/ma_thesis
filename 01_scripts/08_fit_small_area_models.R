# ==============================================================================
# Script:        08_fit_small_area_models.R
# Purpose:       Estimating small area models and model selection
# Project:       The use of OpenStreetMaps in small area estimation of social 
#                cohesion
# Author:        Lidiya Mishieva
# Created:       2026-05-13
# Last updated:  2026-05-13
# R version:     4.5.2 (2025-10-31)
# OS:            x86_64, linux-gnu
# ==============================================================================

# ---- Notes --------------------------------------------------------------------

# Note on operating systems:
# This script uses parallel::mclapply() to run the n_cores dependent-variable
# models in parallel. mclapply() relies on process forking, which is available
# on Unix-like systems such as Linux and macOS, but not on Windows. Therefore,
# this parallel version will not work as intended on non-Unix systems.
#
# On Windows, mclapply() falls back to sequential execution when mc.cores = 1,
# but it cannot use multiple cores via forking. To run this script in parallel
# on Windows, use a socket-based parallel backend instead, for example
# parallel::makeCluster() together with parLapply().

# steps for model fitting and selection
# a) all administrative variables + model selection
# b) all osm variables (ungrouped) + model selection
# c) union of best-fit variables from a and b + model selection
# n) null model (random effect only, no fixed covariates)

# ---- Setup --------------------------------------------------------------------

# clear environment
rm(list = ls())

library(tidyverse)
library(emdi)
library(sf)
library(parallel)

# reproducible RNG across forked workers
RNGkind("L'Ecuyer-CMRG")
set.seed(42)

n_cores <- 5L # use 5 or less, 1 for windows (see note above)

# ---- Paths --------------------------------------------------------------------

input_dir <- file.path("00_data", "derived")
output_dir <- file.path("03_output", "model_fits")

# ---- Import data --------------------------------------------------------------

mdata <- readRDS(file.path(input_dir, "mdata.Rds"))

# ---- Data processing ----------------------------------------------------------

all_osm_variables <- c(
  "university_count_per_1k_inhab", "school_count_per_1k_inhab", "college_count_per_1k_inhab", "kindergarten_count_per_1k_inhab", "library_count_per_1k_inhab",
  "hospital_count_per_1k_inhab", "nursing_home_count_per_1k_inhab", "social_facility_count_per_1k_inhab", "clinic_count_per_1k_inhab", "doctors_count_per_1k_inhab", "pharmacy_count_per_1k_inhab",
  "marketplace_count_per_1k_inhab", "public_bath_count_per_1k_inhab", "shower_count_per_1k_inhab", "give_box_count_per_1k_inhab",
  "community_centre_count_per_1k_inhab", "social_centre_count_per_1k_inhab", "nightclub_count_per_1k_inhab", "internet_cafe_count_per_1k_inhab", "kitchen_count_per_1k_inhab",
  "place_of_worship_count_per_1k_inhab")

all_admin_variables <- c(
  "POPDENSITY", "FEMALE_RATE",
  "RATE_Y_LT20", "RATE_Y20_39", "RATE_Y40_59", "RATE_Y60_79",
  "SECONDARY_Y25.34", "TERTIARY_Y25.34", "SECONDARY_Y25.64", "TERTIARY_Y25.64",
  "POVERTY_RATE", "EMPL_RATE_Y25.34", "EMPL_RATE_Y20.64",
  "ICCS0401_ROBBERY_RATE", "ICCS0502_THEFT_RATE")

mdata <- mdata %>%
  sf::st_drop_geometry() %>%
  as.data.frame()

# helper: extract term labels from a stepped fh fit
fit_terms <- function(fit) attr(terms(fit$fixed), "term.labels")

# process one dependent variable (runs in its own forked worker)
process_depvar <- function(i) {
  depvar <- paste0("F", i, "_Direct")
  vardir <- paste0("F", i, "_direct_var")
  
  # single shared call wrapper — avoids repeating the six fh arguments everywhere
  # bquote substitutes the literal string value of vardir (e.g. "F1_direct_var")
  # into the call so that step() can re-evaluate it without needing vardir in scope
  vd <- vardir
  fh_call <- function(formula) {
    eval(bquote(emdi::fh(
      fixed = .(formula), vardir = .(vd), domains = "region", combined_data = mdata,
      maxit = 1000, interval = c(0, 10), MSE = TRUE, method = "ml", B = c(0, 50)
    )))
  }
  
  savefh <- function(obj, prefix, spec) saveRDS(obj, file.path(output_dir, sprintf("%s_%d%s.rds", prefix, i, spec)))
  
  formula_admin <- as.formula(paste(depvar, "~ 0 +", paste(all_admin_variables, collapse = " + ")))
  formula_osm   <- as.formula(paste(depvar, "~ 0 +", paste(all_osm_variables,   collapse = " + ")))
  formula_null  <- as.formula(paste(depvar, "~ 1"))  # intercept-only: no covariates, pure random effect
  
  fit_n <- fh_call(formula_null);      savefh(fit_n, "fit", "n")
  best_fit_n <- fit_n;                 savefh(best_fit_n, "best_fit", "n")
  message(sprintf("[F%d] null done", i))
  
  fit_a <- fh_call(formula_admin);     savefh(fit_a, "fit", "a")
  best_fit_a <- step(fit_a, criteria = "KICb2"); savefh(best_fit_a, "best_fit", "a")
  message(sprintf("[F%d] admin done", i))
  
  fit_b <- fh_call(formula_osm);       savefh(fit_b, "fit", "b")
  best_fit_b <- step(fit_b, criteria = "KICb2"); savefh(best_fit_b, "best_fit", "b")
  message(sprintf("[F%d] osm done", i))
  
  # automatically derive combined formula from the union of best-fit variables
  combined_vars <- union(fit_terms(best_fit_a), fit_terms(best_fit_b))
  formula_c     <- as.formula(paste(depvar, "~ 0 +", paste(combined_vars, collapse = " + ")))
  
  fit_c <- fh_call(formula_c);         savefh(fit_c, "fit", "c")
  best_fit_c <- step(fit_c, criteria = "KICb2"); savefh(best_fit_c, "best_fit", "c")
  message(sprintf("[F%d] combined done", i))
  
  invisible(NULL)
}

# run all five dependent variables in parallel (results saved inside each worker)
mclapply(1:5, process_depvar, mc.cores = n_cores, mc.preschedule = FALSE)

loeffelfux_msg <- r"{

 ----------------------------- 
 Finally! All models finished! 
 -----------------------------
    \
      \
        \
           /\   /\   art by Todd Vargo
          //\\_//\\     ____
          \_     _/    /   /
           / * * \    /^^^]
           \_\O/_/    [   ]
            /   \_    [   /
            \     \_  /  /
             [ [ /  \/ _/
            _[ [ \  /_/
              _[ [ \  /_/
  
}"

cat(loeffelfux_msg)


# ---- Export outputs -----------------------------------------------------------

# output generated within the function above

# ---- End ----------------------------------------------------------------------


















