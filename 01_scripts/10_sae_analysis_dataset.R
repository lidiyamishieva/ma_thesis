# ==============================================================================
# Script:        10_sae_analysis_dataset.R
# Purpose:       Builds the shared long-format analysis dataset from all fitted 
#                model objects and saves it to evidence/analysis_data.rds for
#                use by 14_diagnostic_plots.R and 16_results_tables.R (loaded, 
#                not re-sourced).
# Project:       The use of OpenStreetMaps in small area estimation of social 
#                cohesion
# Author:        Lidiya Mishieva
# Created:       2026-05-13
# Last updated:  2026-05-13
# R version:     4.5.2 (2025-10-31)
# OS:            x86_64, linux-gnu
# ==============================================================================

# ---- Notes --------------------------------------------------------------------

## Outputs:
##   03_output/evidence/analysis_data.rds  — named list with keys:
##     all_data      — long data frame, one row per domain x factor x spec
##     specs         — character vector of active spec codes
##     model_labels  — named vector: spec code -> display label
##     model_colors  — named vector: spec code -> hex colour
##     factor_labels — named vector: "1"-"5" -> dimension name
##     direct_color  — hex colour for the direct estimator series
##     fit_dir       — path to best_fit_new/
##     sample_size   — named vector: domain -> sample size
##     cv_by_factor  — named list: factor -> region -> CV (proportion)

# ---- Setup --------------------------------------------------------------------

# clear environment
rm(list = ls())

suppressPackageStartupMessages(library(dplyr))

include_null_model <- TRUE
fit_dir            <- "best_fit_new"

specs <- c("a", "b", "c")
if (include_null_model) specs <- c("n", specs)

model_labels <- c(
  a = "M1 (admin)",
  b = "M2 (osm)",
  c = "M3 (admin + osm)",
  n = "M0 (null)"
)

model_colors <- c(
  a = "#1b9e77",
  b = "#d95f02",
  c = "#7570b3",
  n = "#e7298a"
)

direct_color <- "#aaaaaa"

factor_labels <- c(
  `1` = "Interpersonal trust",
  `2` = "Social relations",
  `3` = "Openness",
  `4` = "Institutional trust",
  `5` = "Legitimacy of institutions"
)

# ---- Paths --------------------------------------------------------------------

input_dir <- file.path("00_data", "derived")
fit_dir <- file.path("03_output", "model_fits")
output_dir <- file.path("03_output", "evidence")

# ---- Import data --------------------------------------------------------------

# load regional dataset used for all fits
mdata <- readRDS(file.path(input_dir, "mdata.Rds"))
if (inherits(mdata, "sf")) mdata <- sf::st_drop_geometry(mdata)
mdata <- as.data.frame(mdata)

# Load direct estimates
direct_est  <- readRDS(file.path(input_dir, "direct_estimates.rds"))
direct_est  <- direct_est[direct_est$region %in% mdata$region, ]

# ---- Data processing ----------------------------------------------------------

# ── Sample sizes ──────────────────────────────────────────────────────────
samp_col    <- intersect(c("SampSize","SampleSize","sample_size","n","N"), names(mdata))[1]
sample_size <- setNames(mdata[[samp_col]], mdata$region)

# Named list: cv_by_factor[["1"]] is a named vector (region -> CV as proportion)
cv_by_factor <- setNames(lapply(1:5, function(i) {
  cv_pct <- direct_est[[sprintf("F%d_CV", i)]]
  setNames(cv_pct / 100, direct_est$region)
}), as.character(1:5))

# ── Helper: extract per-domain data from one fit ──────────────────────────
extract_data <- function(i, spec) {
  path <- file.path(fit_dir, sprintf("best_fit_%d%s.rds", i, spec))
  if (!file.exists(path)) return(NULL)
  fit <- readRDS(path)
  ind <- as.data.frame(fit$ind)
  mse <- as.data.frame(fit$MSE)
  m <- merge(
    ind[, intersect(c("Domain","Direct","FH"), names(ind))],
    mse[, intersect(c("Domain","Direct","FH"), names(mse))],
    by = "Domain", suffixes = c("_est","_mse")
  )
  m$samp_size   <- sample_size[m$Domain]
  m$rmse_direct <- sqrt(pmax(m$Direct_mse, 0))
  m$rmse_fh     <- sqrt(pmax(m$FH_mse, 0))
  # CV = sqrt(design_var) / direct_est, sourced directly from mdata
  m$cv_direct   <- cv_by_factor[[as.character(i)]][m$Domain]
  m$efficiency  <- (m$rmse_fh - m$rmse_direct) / m$rmse_direct
  m$bias        <- m$FH_est - m$Direct_est
  m$spec        <- spec
  m$model       <- model_labels[spec]
  m$factor_id   <- as.character(i)
  m
}

# ── Build all_data ────────────────────────────────────────────────────────
all_data <- do.call(rbind, lapply(1:5, function(i)
  do.call(rbind, lapply(specs, function(s) extract_data(i, s)))
))

cat("10_sae_analysis_dataset.R: loaded", nrow(all_data), "rows across",
    length(specs), "specs and 5 factors.\n")

# ---- Export outputs -----------------------------------------------------------

# ── Save to disk ──────────────────────────────────────────────────────────
saveRDS(
  list(
    all_data      = all_data,
    specs         = specs,
    model_labels  = model_labels,
    model_colors  = model_colors,
    factor_labels = factor_labels,
    direct_color  = direct_color,
    fit_dir       = fit_dir,
    sample_size   = sample_size,
    cv_by_factor  = cv_by_factor
  ),
  file.path(output_dir, "analysis_data.rds")
)

cat(paste0("Saved ", file.path(output_dir, "analysis_data.rds"), "\n"))

# ---- End ----------------------------------------------------------------------
