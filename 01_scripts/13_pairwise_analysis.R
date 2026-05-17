# ==============================================================================
# Script:        13_pairwise_analysis.R
# Purpose:       Pairwise sign-consistency analysis across all FH model specs.
# Project:       The use of OpenStreetMaps in small area estimation of social 
#                cohesion
# Author:        Lidiya Mishieva
# Created:       2026-05-13
# Last updated:  2026-05-13
# R version:     4.5.2 (2025-10-31)
# OS:            x86_64, linux-gnu
# ==============================================================================

# ---- Notes --------------------------------------------------------------------

# Depends on 12_eblup_estimates.R having been run first, which produces:
#   validation_full.csv    -- all 233 domains, wide format; used to rebuild long-form data
#   validation_targets.csv -- identifies the handpicked target domain set
#
# Two analytical levels are run:
#
#   specific (target domains, ~120 regions):
#     Claim-level validation — do the regional contrasts highlighted in the
#     results section agree in direction across specs (admin/OSM/hybrid)?
#     Also flags admin-vs-OSM sign conflicts.
#
#   general (all 233 domains, 27,028 pairs × 5 dims = 135,140 rows):
#     Aggregate error rates cited in the discussion section,
#     e.g. "admin sign error rate 28% for F3, 25% for F1".
#
# Outputs (all written to output_dir):
#   validation_pairwise_targets.csv   -- all pairs among target domains
#   validation_pairwise_summary.csv   -- per-dimension summary for target domains
#   pairwise_all_domains.csv          -- all pairs among all 233 domains
#   pairwise_all_domains_smry.csv     -- per-dimension summary incl. error rates
#
# Design note:
#   load_validation_data() reads validation_full.csv (wide) and pivots back to
#   long format, recovering fh_est/fh_mse per spec from the pre-computed CSV.
#   load_g2_components() loads all 15 RDS model objects once at startup and
#   pre-computes the 233×233 Prasad-Rao g2-covariance matrices, which correct
#   the pairwise MSE for shared fixed-effects uncertainty (see Section 1 comment).
#   build_pairs() is a single function used for both analytical levels.

# ---- Setup --------------------------------------------------------------------

# clear environment
rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

n_resp       <- 5
active_specs <- c("a", "b", "c")
ci_level     <- 0.95
z            <- qnorm((1 + ci_level) / 2)

# ---- Paths --------------------------------------------------------------------

fit_dir <- file.path("03_output", "model_fits")
output_dir <- file.path("03_output", "evidence")

full_csv   <- file.path(output_dir, "validation_full.csv")
target_csv <- file.path(output_dir, "validation_targets.csv")

# ---- Import data --------------------------------------------------------------

# ---- Data processing ----------------------------------------------------------

# ──── load_g2_components ────────────────────────────────────────────────────
#
# For each model (response_index × spec_code), load the emdi FH object and
# pre-compute the n×n g2-covariance matrix:
#
#   g2_cov(i,j) = (1-γ_i)(1-γ_j) · x_i' · Var(β̂) · x_j
#
# This is the shared fixed-effects uncertainty term from the Prasad-Rao MSE
# decomposition.  It corrects the naive MSE_i + MSE_j used in the significance
# test for the difference FH_i − FH_j:
#
# MSE(FH_i − FH_j) = MSE_i + MSE_j − 2 · g2_cov(i,j)

load_g2_components <- function(fits_dir, n_resp, active_specs) {
  out <- list()
  for (ri in seq_len(n_resp)) {
    for (s in active_specs) {
      path <- file.path(fits_dir, sprintf("best_fit_%d%s.rds", ri, s))
      fit  <- readRDS(path)
      cd   <- fit$framework$combined_data
      X    <- model.matrix(fit$fixed, data = cd)
      gam  <- fit$model$gamma
      gamma_vec <- gam$Gamma[match(cd$region, gam$Domain)]
      domains   <- cd$region
      ## W[i, ] = (1 − γ_i) · x_i' ;  g2_mat = W · Var(β̂) · W'
      W      <- sweep(X, 1L, 1 - gamma_vec, "*")
      g2_mat <- tcrossprod(W %*% fit$model$beta_vcov, W)
      rownames(g2_mat) <- colnames(g2_mat) <- domains
      out[[paste0(ri, s)]] <- g2_mat
    }
  }
  out
}


# ──── load_validation_data ────────────────────────────────────────────────────
#
# Reads validation_full.csv (wide, one row per domain × dimension) and pivots
# to long format (one row per domain × dimension × spec_code), keeping only
# the columns needed for pairwise analysis:
#   domain, country, NAME_LATN.x, response_index
#   spec_code                -- a / b / c
#   fh_est, fh_mse           -- FH point estimate and MSE
#   direct_est, direct_mse   -- direct survey estimate and MSE
#
# The wide CSV stores fh_rmse (not fh_mse) and direct_se (not direct_mse);
# the squared values are recovered here.

load_validation_data <- function(full_csv_path) {
  if (!file.exists(full_csv_path)) {
    stop(sprintf(
      "Required input not found: %s\nRun 05_eblup_estimates.R first.", full_csv_path
    ))
  }
  full <- read.csv(full_csv_path, stringsAsFactors = FALSE)
  
  long <- full %>%
    dplyr::select(domain, country, NAME_LATN.x, response_index,
           direct_est, direct_se,
           a_fh_est, a_fh_rmse, b_fh_est, b_fh_rmse, c_fh_est, c_fh_rmse) %>%
    pivot_longer(
      cols          = matches("^[abc]_(fh_est|fh_rmse)$"),
      names_to      = c("spec_code", ".value"),
      names_pattern = "^(.)_(fh_.+)$"
    ) %>%
    mutate(
      direct_mse = direct_se^2,
      fh_mse     = fh_rmse^2
    )
  
  long
}

# ──── Load data ──────────────────────────────────────────────────────────────

cat("Reading pre-computed estimates from", full_csv, "\n")
all_rows <- load_validation_data(full_csv)
cat(sprintf("  %d rows x %d cols (domain x dim x spec)\n", nrow(all_rows), ncol(all_rows)))

if (!file.exists(target_csv)) {
  stop(sprintf(
    "Required input not found: %s\nRun 05_eblup_estimates.R first.", target_csv
  ))
}

target_domain_set <- unique(read.csv(target_csv, stringsAsFactors = FALSE)$domain)
cat(sprintf("  %d target domains loaded from %s\n", length(target_domain_set), target_csv))

cat("Loading model objects for g2 covariance correction ...\n")
g2_comps <- load_g2_components(fit_dir, n_resp, active_specs)
cat(sprintf("  g2 components ready for %d models\n", length(g2_comps)))


# ──── Core pairwise helper ────────────────────────────────────────────────────
#
# build_pairs(ri, domain_pool, all_rows, include_names)
#
# For a given response_index (ri) and domain set, compute all n*(n-1)/2 pairs.
# For each pair:
#   direct_diff / direct_se / direct_significant / direct_sign
#   For each spec s in active_specs:
#     s_sig              : |FH_i - FH_j| > z * sqrt(MSE_i + MSE_j)
#     s_flip             : FH sign != direct sign  (direction reversal)
#     s_spurious_weak    : sig & flip & direct non-significant
#     s_spurious_strong  : sig & flip & direct significant (worst case)
#     s_spurious         : any spurious (weak or strong)
#
# When include_names = TRUE (target analysis), also adds:
#   name_1, name_2, country_1, country_2
#   s_fh_diff, s_fh_se, s_sign  per spec
#   a_vs_b_flip, a_vs_b_both_sig_conflict

build_pairs <- function(ri, domain_pool, all_rows, include_names = FALSE,
                        g2_comps = NULL) {
  ri_long <- all_rows[all_rows$response_index == ri, ]
  dir_df  <- ri_long[ri_long$spec_code == "a",
                     c("domain", "NAME_LATN.x", "country", "direct_est", "direct_mse")]
  dir_df  <- dir_df[dir_df$domain %in% domain_pool, ]
  doms    <- sort(unique(dir_df$domain))
  n_doms  <- length(doms)
  if (n_doms < 2) return(NULL)
  
  idx    <- utils::combn(n_doms, 2)
  d1_vec <- doms[idx[1, ]]
  d2_vec <- doms[idx[2, ]]
  
  r1 <- dir_df[match(d1_vec, dir_df$domain), ]
  r2 <- dir_df[match(d2_vec, dir_df$domain), ]
  
  dir_diff <- r1$direct_est - r2$direct_est
  dir_se   <- sqrt(pmax(r1$direct_mse, 0) + pmax(r2$direct_mse, 0))
  dir_sig  <- abs(dir_diff) > z * dir_se
  dir_sgn  <- sign(dir_diff)
  
  out <- data.frame(
    response_index     = ri,
    domain_1           = d1_vec,
    domain_2           = d2_vec,
    direct_diff        = dir_diff,
    direct_se          = dir_se,
    direct_significant = dir_sig,
    direct_sign        = dir_sgn,
    stringsAsFactors   = FALSE
  )
  if (include_names) {
    out$name_1    <- r1$NAME_LATN.x
    out$country_1 <- r1$country
    out$name_2    <- r2$NAME_LATN.x
    out$country_2 <- r2$country
  } else {
    out$country_1 <- r1$country
    out$country_2 <- r2$country
  }
  
  for (s in active_specs) {
    fh_s    <- ri_long[ri_long$spec_code == s & ri_long$domain %in% doms,
                       c("domain", "fh_est", "fh_mse")]
    fe1     <- fh_s$fh_est[match(d1_vec, fh_s$domain)]
    fe2     <- fh_s$fh_est[match(d2_vec, fh_s$domain)]
    fm1     <- fh_s$fh_mse[match(d1_vec, fh_s$domain)]
    fm2     <- fh_s$fh_mse[match(d2_vec, fh_s$domain)]
    fh_diff <- fe1 - fe2
    # Analytical g2-covariance correction (Prasad-Rao cross-term).
    # Falls back to naive MSE_i + MSE_j when g2_comps is NULL.
    if (!is.null(g2_comps)) {
      gmat  <- g2_comps[[paste0(ri, s)]]
      idx1  <- match(d1_vec, rownames(gmat))
      idx2  <- match(d2_vec, colnames(gmat))
      g2cov <- gmat[cbind(idx1, idx2)]
      g2cov[is.na(g2cov)] <- 0
    } else {
      g2cov <- 0
    }
    fh_se   <- sqrt(pmax(pmax(fm1, 0) + pmax(fm2, 0) - 2 * g2cov, 0))
    fh_sig  <- !is.na(fh_diff) & !is.na(fh_se) & (abs(fh_diff) > z * fh_se)
    fh_sgn  <- sign(fh_diff)
    flip            <- !is.na(fh_sgn) & dir_sgn != 0 & fh_sgn != dir_sgn
    # spurious_weak:   sign flip but direct was non-significant (survey was ambiguous)
    # spurious_strong: sign flip AND direct was significant (model confidently contradicts
    #                  a survey that already pointed the other way — most problematic)
    spurious_weak   <- fh_sig & flip & !dir_sig
    spurious_strong <- fh_sig & flip & dir_sig
    
    if (include_names) {
      out[[paste0(s, "_fh_diff")]] <- fh_diff
      out[[paste0(s, "_fh_se")]]   <- fh_se
      out[[paste0(s, "_sign")]]    <- fh_sgn
    }
    out[[paste0(s, "_sig")]]             <- fh_sig
    out[[paste0(s, "_flip")]]            <- flip
    out[[paste0(s, "_spurious_weak")]]   <- spurious_weak
    out[[paste0(s, "_spurious_strong")]] <- spurious_strong
    out[[paste0(s, "_spurious")]]        <- spurious_weak | spurious_strong
  }
  
  if (include_names) {
    out$a_vs_b_flip <- !is.na(out$a_sign) & !is.na(out$b_sign) &
      out$a_sign != 0 & out$b_sign != 0 & out$a_sign != out$b_sign
    out$a_vs_b_both_sig_conflict <- out$a_sig & out$b_sig & out$a_vs_b_flip
  }
  out
}


# summarise_pairs: aggregate pairwise table to per-dimension summary with error rates
# error_rate = spurious / n_significant  (share of significant pairs that point wrong way)

summarise_pairs <- function(pairs_df) {
  pairs_df %>%
    group_by(response_index) %>%
    summarise(
      n_pairs           = n(),
      direct_sig        = sum(direct_significant,    na.rm = TRUE),
      a_sig             = sum(a_sig,                 na.rm = TRUE),
      b_sig             = sum(b_sig,                 na.rm = TRUE),
      c_sig             = sum(c_sig,                 na.rm = TRUE),
      a_flip            = sum(a_flip,                na.rm = TRUE),
      b_flip            = sum(b_flip,                na.rm = TRUE),
      c_flip            = sum(c_flip,                na.rm = TRUE),
      a_spurious_weak   = sum(a_spurious_weak,       na.rm = TRUE),
      a_spurious_strong = sum(a_spurious_strong,     na.rm = TRUE),
      a_spurious        = sum(a_spurious,            na.rm = TRUE),
      b_spurious_weak   = sum(b_spurious_weak,       na.rm = TRUE),
      b_spurious_strong = sum(b_spurious_strong,     na.rm = TRUE),
      b_spurious        = sum(b_spurious,            na.rm = TRUE),
      c_spurious_weak   = sum(c_spurious_weak,       na.rm = TRUE),
      c_spurious_strong = sum(c_spurious_strong,     na.rm = TRUE),
      c_spurious        = sum(c_spurious,            na.rm = TRUE),
      a_error_rate      = round(sum(a_spurious, na.rm=TRUE) / pmax(sum(a_sig, na.rm=TRUE), 1), 4),
      b_error_rate      = round(sum(b_spurious, na.rm=TRUE) / pmax(sum(b_sig, na.rm=TRUE), 1), 4),
      c_error_rate      = round(sum(c_spurious, na.rm=TRUE) / pmax(sum(c_sig, na.rm=TRUE), 1), 4),
      .groups = "drop"
    )
}

# ──── Specific pairwise: target domains ───────────────────────────────────────
#
# ~120 handpicked regions from validation_targets.csv.
# include_names = TRUE: adds name_1/name_2 and per-spec fh_diff/sign columns,
# and the admin-vs-OSM cross-spec conflict flags.

cat("\n-- Building target-domain pairwise table ... --\n")
pairwise_targets <- do.call(rbind,
                            Filter(Negate(is.null),
                                   lapply(seq_len(n_resp), build_pairs,
                                          domain_pool   = target_domain_set,
                                          all_rows      = all_rows,
                                          include_names = TRUE,
                                          g2_comps      = g2_comps)))
rownames(pairwise_targets) <- NULL

pairwise_targets_smry <- pairwise_targets %>%
  group_by(response_index) %>%
  summarise(
    n_pairs              = n(),
    direct_sig           = sum(direct_significant,       na.rm = TRUE),
    a_sig                = sum(a_sig,                    na.rm = TRUE),
    b_sig                = sum(b_sig,                    na.rm = TRUE),
    c_sig                = sum(c_sig,                    na.rm = TRUE),
    a_flip_vs_direct     = sum(a_flip,                   na.rm = TRUE),
    b_flip_vs_direct     = sum(b_flip,                   na.rm = TRUE),
    c_flip_vs_direct     = sum(c_flip,                   na.rm = TRUE),
    a_spurious_weak      = sum(a_spurious_weak,          na.rm = TRUE),
    a_spurious_strong    = sum(a_spurious_strong,        na.rm = TRUE),
    a_spurious           = sum(a_spurious,               na.rm = TRUE),
    b_spurious_weak      = sum(b_spurious_weak,          na.rm = TRUE),
    b_spurious_strong    = sum(b_spurious_strong,        na.rm = TRUE),
    b_spurious           = sum(b_spurious,               na.rm = TRUE),
    c_spurious_weak      = sum(c_spurious_weak,          na.rm = TRUE),
    c_spurious_strong    = sum(c_spurious_strong,        na.rm = TRUE),
    c_spurious           = sum(c_spurious,               na.rm = TRUE),
    a_vs_b_flip          = sum(a_vs_b_flip,              na.rm = TRUE),
    a_vs_b_both_sig_conf = sum(a_vs_b_both_sig_conflict, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(pairwise_targets, file.path(output_dir, "validation_pairwise_targets.csv"), row.names = FALSE)
write.csv(pairwise_targets_smry, file.path(output_dir, "validation_pairwise_summary.csv"), row.names = FALSE)
cat(sprintf("Saved target pairs:   %s  (%d rows)\n", file.path(output_dir, "validation_pairwise_targets.csv"), nrow(pairwise_targets)))
cat(sprintf("Saved target summary: %s  (%d rows)\n", file.path(output_dir, "validation_pairwise_summary.csv"), nrow(pairwise_targets_smry)))

cat("\n-- Pairwise sign-flip summary (target domains, by dimension) --\n")
print(as.data.frame(pairwise_targets_smry), row.names = FALSE)

## admin-spurious: admin significant but direction contradicts direct estimate
spurious_rows <- pairwise_targets[pairwise_targets$a_spurious %in% TRUE,
                                  c("response_index", "domain_1", "name_1", "domain_2", "name_2",
                                    "direct_diff", "direct_significant",
                                    "a_fh_diff", "a_sig", "a_flip",
                                    "b_fh_diff", "b_sig", "b_flip")]
cat("\n-- Admin-spurious pairs (significant admin FH but sign contradicts direct) --\n")
if (nrow(spurious_rows) > 0) {
  print(spurious_rows, row.names = FALSE, digits = 3)
} else {
  cat("None found in target domain set.\n")
}

## admin-vs-OSM: both models significant but disagree on direction
conflict_rows <- pairwise_targets[pairwise_targets$a_vs_b_both_sig_conflict %in% TRUE,
                                  c("response_index", "domain_1", "name_1", "domain_2", "name_2",
                                    "direct_sign",
                                    "a_fh_diff", "a_sig", "a_sign",
                                    "b_fh_diff", "b_sig", "b_sign")]
cat("\n-- Admin-vs-OSM both-significant sign conflicts --\n")
if (nrow(conflict_rows) > 0) {
  print(conflict_rows, row.names = FALSE, digits = 3)
} else {
  cat("None found in target domain set.\n")
}

# ──── General pairwise: all 233 domains ───────────────────────────────────────
#
# All in-sample domains, all n*(n-1)/2 = 27,028 pairs x 5 dims = ~135k rows.
# include_names = FALSE: leaner output (no name columns, no per-spec diffs).
# Produces the aggregate error rates cited in the results discussion.

all_domains <- unique(all_rows$domain)
cat(sprintf("\n-- Building all-domain pairwise table (%d domains x 5 dims) ... --\n", length(all_domains)))

pairwise_all <- do.call(rbind,
                        Filter(Negate(is.null),
                               lapply(seq_len(n_resp), build_pairs,
                                      domain_pool   = all_domains,
                                      all_rows      = all_rows,
                                      include_names = FALSE,
                                      g2_comps      = g2_comps)))
rownames(pairwise_all) <- NULL

pairwise_all_smry <- summarise_pairs(pairwise_all)

write.csv(pairwise_all, file.path(output_dir, "pairwise_all_domains.csv"), row.names = FALSE)
write.csv(pairwise_all_smry, file.path(output_dir, "pairwise_all_domains_smry.csv"), row.names = FALSE)
cat(sprintf("Saved all-domain pairs:   %s  (%d rows)\n", file.path(output_dir, "pairwise_all_domains.csv"), nrow(pairwise_all)))
cat(sprintf("Saved all-domain summary: %s  (%d rows)\n", file.path(output_dir, "pairwise_all_domains_smry.csv"), nrow(pairwise_all_smry)))

cat("\n-- All-domain sign-flip summary (g2-corrected) --\n")
cat("(error_rate = spurious / sig; all in-sample domains, Prasad-Rao g2 covariance correction)\n")
print(as.data.frame(pairwise_all_smry), row.names = FALSE)


# ──── Naive vs g2-corrected comparison ────────────────────────────────────────
#
# Re-run the all-domain summary without the g2 correction (naive: MSE_i + MSE_j
# only) and save both summaries side-by-side for verification.
#
# Reference for the g2 correction:
#   Prasad, N. G. N. & Rao, J. N. K. (1990). The estimation of the mean squared
#   error of small-area estimators. JASA, 85(409), 163–171.  [g1/g2/g3 decomposition]
#   Rao, J. N. K. & Molina, I. (2015). Small Area Estimation (2nd ed.). Wiley.
#   Section 4.2 gives the full PR MSE estimator; the off-diagonal covariance
#   Cov(EBLUP_i, EBLUP_j) = (1-γ_i)(1-γ_j) x_i' Var(β̂) x_j follows directly,
#   since the only shared randomness across areas is β̂ (direct errors are independent).
#   emdi computes only the marginal (diagonal) MSE per domain; pairwise covariances
#   are an extension for contrast inference and are not part of emdi's standard output.

cat("\n-- Re-running all-domain pairs without g2 correction (naive) for comparison --\n")
pairwise_all_naive <- do.call(rbind,
                              Filter(Negate(is.null),
                                     lapply(seq_len(n_resp), build_pairs,
                                            domain_pool   = all_domains,
                                            all_rows      = all_rows,
                                            include_names = FALSE,
                                            g2_comps      = NULL)))
rownames(pairwise_all_naive) <- NULL
pairwise_all_smry_naive <- summarise_pairs(pairwise_all_naive)

write.csv(pairwise_all_naive, file.path(output_dir, "pairwise_all_domains_naive.csv"), row.names = FALSE)
write.csv(pairwise_all_smry_naive, file.path(output_dir, "pairwise_all_domains_smry_naive.csv"), row.names = FALSE)
cat(sprintf("Saved naive pairs:   %s  (%d rows)\n", file.path(output_dir, "pairwise_all_domains_naive.csv"), nrow(pairwise_all_naive)))
cat(sprintf("Saved naive summary: %s\n", file.path(output_dir, "pairwise_all_domains_smry_naive.csv")))

# Significance-change table: the meaningful criterion.
# For each spec x dimension, count pairs that:
#   gained_sig  : naive non-sig -> g2-corrected sig  (correction tightens SE -> more powerful)
#   lost_sig    : naive sig     -> g2-corrected non-sig  (rare: only when cross-term is negative)
# Separately for within-country and between-country pairs.

cat("\n-- Significance changes: naive vs g2-corrected (all domains) --\n")
cat("The sensible criterion: how many pairs change their significance call?\n")
cat("(+ = correction adds significance; − = correction removes significance)\n\n")

# join on domain_1 / domain_2 / response_index
key_vars <- c("response_index", "domain_1", "domain_2")
merged <- merge(
  pairwise_all_naive[, c(key_vars, "country_1", "country_2", paste0(active_specs, "_sig"))],
  pairwise_all[, c(key_vars, paste0(active_specs, "_sig"))],
  by = key_vars, suffixes = c("_naive", "_g2")
)
merged$within <- merged$country_1 == merged$country_2

flip_rows <- list()
for (ri in seq_len(n_resp)) {
  for (w in c(TRUE, FALSE)) {
    sub <- merged[merged$response_index == ri & merged$within == w, ]
    for (s in active_specs) {
      naive_col <- paste0(s, "_sig_naive")
      g2_col    <- paste0(s, "_sig_g2")
      gained <- sum(!sub[[naive_col]] &  sub[[g2_col]], na.rm = TRUE)
      lost   <- sum( sub[[naive_col]] & !sub[[g2_col]], na.rm = TRUE)
      flip_rows[[length(flip_rows) + 1]] <- data.frame(
        response_index = ri,
        within_country = w,
        spec           = s,
        n_pairs        = nrow(sub),
        gained_sig     = gained,
        lost_sig       = lost,
        net            = gained - lost,
        stringsAsFactors = FALSE
      )
    }
  }
}
flip_tbl <- do.call(rbind, flip_rows)
rownames(flip_tbl) <- NULL

write.csv(flip_tbl, file.path(output_dir, "pairwise_sig_changes.csv"), row.names = FALSE)
cat(sprintf("Saved significance-change table: %s\n", file.path(output_dir, "pairwise_sig_changes.csv")))
cat("\n")
print(flip_tbl, row.names = FALSE)

# Legacy: side-by-side sig count + error rate comparison
cat("\n-- Supplementary: sig counts and error rates naive vs g2-corrected --\n")
for (ri in seq_len(n_resp)) {
  n  <- pairwise_all_smry_naive[pairwise_all_smry_naive$response_index == ri, ]
  g  <- pairwise_all_smry[pairwise_all_smry$response_index == ri, ]
  for (s in active_specs) {
    sig_n  <- n[[paste0(s, "_sig")]]
    sig_g  <- g[[paste0(s, "_sig")]]
    err_n  <- n[[paste0(s, "_error_rate")]]
    err_g  <- g[[paste0(s, "_error_rate")]]
    cat(sprintf("  F%d %s: sig %d -> %d (%+d)   error_rate %.4f -> %.4f (%+.4f)\n",
                ri, s, sig_n, sig_g, sig_g - sig_n, err_n, err_g, err_g - err_n))
  }
}


# ---- Export outputs -----------------------------------------------------------

# ---- End ----------------------------------------------------------------------
