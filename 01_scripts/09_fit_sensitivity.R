# ==============================================================================
# Script:        09_fit_sensitivity.R
# Purpose:       Re-fits the selected best-fit models (specs a/b/c) WITH an intercept
#                using the exact same covariates chosen by KICb2 stepping, then compares
#                performance against the no-intercept versions in best_fit_new/.
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

# ---- Setup --------------------------------------------------------------------

# clear environment
rm(list = ls())

library(emdi)
library(parallel)
library(sf)

RNGkind("L'Ecuyer-CMRG")
set.seed(42)

n_cores <- 5L # use 5 or less, 1 for windows (see note above)

# ---- Paths --------------------------------------------------------------------

input_dir <- file.path("00_data", "derived")
fit_dir <- file.path("03_output", "model_fits")
output_dir <- file.path("03_output", "evidence")

# ---- Import data --------------------------------------------------------------

mdata <- readRDS(file.path(input_dir, "mdata.Rds"))

mdata <- mdata %>%
  sf::st_drop_geometry() %>%
  as.data.frame()

# ---- Data processing ----------------------------------------------------------

fit_terms <- function(fit) attr(terms(fit$fixed), "term.labels")

# specs to compare — null (n) already has an intercept, skip it
specs_to_compare <- c("a", "b", "c")

spec_labels <- c(
  a = "M1 (admin)",
  b = "M2 (osm)",
  c = "M3 (admin + osm)"
)

process_one <- function(i) {
  depvar <- paste0("F", i, "_Direct")
  vardir <- paste0("F", i, "_direct_var")
  vd     <- vardir
  
  fh_call <- function(formula) {
    eval(bquote(emdi::fh(
      fixed         = .(formula),
      vardir        = .(vd),
      domains       = "region",
      combined_data = mdata,
      maxit         = 1000,
      interval      = c(0, 10),
      MSE           = TRUE,
      method        = "ml",
      B             = c(0, 50)
    )))
  }
  
  rows <- lapply(specs_to_compare, function(s) {
    
    path_noint <- file.path(fit_dir, sprintf("best_fit_%d%s.rds", i, s))
    if (!file.exists(path_noint)) {
      message(sprintf("[F%d %s] no-intercept fit not found, skipping", i, s))
      return(NULL)
    }
    
    fit_noint <- readRDS(path_noint)
    trms      <- fit_terms(fit_noint)
    
    # same covariates, but WITH intercept (no "0 +" prefix)
    formula_int <- as.formula(paste(depvar, "~", paste(trms, collapse = " + ")))
    
    fit_int <- tryCatch(
      fh_call(formula_int),
      error = function(e) {
        message(sprintf("[F%d %s] intercept fit failed: %s", i, s, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(fit_int)) return(NULL)
    
    path_int <- file.path(fit_dir, sprintf("best_fit_%d%s_int.rds", i, s))
    saveRDS(fit_int, path_int)
    message(sprintf("[F%d %s] intercept fit saved → %s", i, s, path_int))
    
    # ── comparison metrics ────────────────────────────────────────────────
    gamma_noint <- fit_noint$model$variance
    gamma_int   <- fit_int$model$variance
    
    mse_noint   <- mean(fit_noint$MSE$FH, na.rm = TRUE)
    mse_int     <- mean(fit_int$MSE$FH,   na.rm = TRUE)
    
    rho_noint   <- cor(fit_noint$ind$Direct, fit_noint$ind$FH, method = "spearman")
    rho_int     <- cor(fit_int$ind$Direct,   fit_int$ind$FH,   method = "spearman")
    
    # RMSE of FH vs Direct (across domains)
    rmse_noint  <- sqrt(mean((fit_noint$ind$FH - fit_noint$ind$Direct)^2, na.rm = TRUE))
    rmse_int    <- sqrt(mean((fit_int$ind$FH   - fit_int$ind$Direct)^2,   na.rm = TRUE))
    
    # mean(absolute diff) between int and noint predictions
    pred_mad    <- mean(abs(fit_int$ind$FH - fit_noint$ind$FH), na.rm = TRUE)
    pred_rho    <- cor(fit_int$ind$FH, fit_noint$ind$FH, method = "spearman")
    
    # AIC/BIC/KICb2 from model_select (present in stepped objects, NA otherwise)
    ms_ni <- fit_noint$model_select
    get_crit <- function(ms, col) {
      if (!is.null(ms) && col %in% names(ms)) ms[[col]][nrow(ms)] else NA_real_
    }
    aic_noint  <- get_crit(ms_ni, "AIC")
    bic_noint  <- get_crit(ms_ni, "BIC")
    kic_noint  <- get_crit(ms_ni, "KICb2")
    
    data.frame(
      factor          = i,
      spec            = s,
      model           = spec_labels[s],
      n_terms         = length(trms),
      # random effect variance
      gamma_noint     = round(gamma_noint, 6),
      gamma_int       = round(gamma_int,   6),
      # mean FH MSE (lower = better)
      mean_mse_noint  = round(mse_noint,   6),
      mean_mse_int    = round(mse_int,     6),
      mse_winner      = ifelse(mse_int < mse_noint, "int", "noint"),
      # RMSE of FH vs Direct
      rmse_vs_direct_noint = round(rmse_noint, 6),
      rmse_vs_direct_int   = round(rmse_int,   6),
      rmse_winner          = ifelse(rmse_int < rmse_noint, "int", "noint"),
      # Spearman rho of FH vs Direct
      rho_noint       = round(rho_noint,   4),
      rho_int         = round(rho_int,     4),
      # agreement between int and noint predictions
      pred_mad        = round(pred_mad,    6),
      pred_rho        = round(pred_rho,    4),
      # model-selection criteria (from no-intercept stepped fit; NA if unavailable)
      aic_noint       = round(aic_noint,   2),
      bic_noint       = round(bic_noint,   2),
      kicb2_noint     = round(kic_noint,   2),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, rows)
}

results <- mclapply(1:5, process_one, mc.cores = n_cores, mc.preschedule = FALSE)
tbl     <- do.call(rbind, results)

write.csv(tbl, file.path(output_dir, "intercept_comparison.csv"), row.names = FALSE)
cat("\nIntercept comparison saved to intercept_comparison.csv\n\n")

# ── quick console summary ─────────────────────────────────────────────────
cat(sprintf("%-3s %-2s %-20s  %6s %6s  mse_win  %6s %6s  rmse_win  rho_ni rho_int  pred_rho\n",
            "F", "sp", "model", "γ_ni", "γ_int",
            "mse_ni", "mse_in"))
cat(strrep("-", 100), "\n")
for (k in seq_len(nrow(tbl))) {
  r <- tbl[k, ]
  cat(sprintf("F%d  %-2s %-20s  %6.4f %6.4f  %-8s %6.4f %6.4f  %-9s %6.4f %6.4f  %6.4f\n",
              r$factor, r$spec, r$model,
              r$gamma_noint, r$gamma_int, r$mse_winner,
              r$mean_mse_noint, r$mean_mse_int, r$rmse_winner,
              r$rho_noint, r$rho_int, r$pred_rho))
}

# ---- Export outputs -----------------------------------------------------------

# output generated within the function above

# ---- End ----------------------------------------------------------------------
