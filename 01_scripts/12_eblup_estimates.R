# ==============================================================================
# Script:        12_eblup_estimates.R
# Purpose:       For every in-sample domain (all 233), extract Direct and FH 
#                estimates for all pecs and dimensions.
#                Computes ranks, quintiles (breakpoint-based), 95% CI spans,
#                and flags whether CIs exclude the EU or within-country FH mean.
#                Also filters to the handpicked target_spec regions for 
#                claim-level validation.
#                These are analysis results for 13_pairwise_analysis.R.
# Project:       The use of OpenStreetMaps in small area estimation of social 
#                cohesion
# Author:        Lidiya Mishieva
# Created:       2026-05-13
# Last updated:  2026-05-13
# R version:     4.5.2 (2025-10-31)
# OS:            x86_64, linux-gnu
# ==============================================================================

# ---- Notes --------------------------------------------------------------------

# Spec labels:
#   a  = admin-only (M1): administrative variables + stepwise KICb2 selection
#   b  = OSM-only  (M2): OSM variables only + stepwise KICb2 selection
#   c  = hybrid    (M3): admin + selected OSM variables + stepwise KICb2 selection
#
# Outputs:
#   evidence/validation_full.csv    -- all 233 domains, all dimensions, all specs
#   evidence/validation_targets.csv -- filtered to target regions (with claims column)

# ---- Setup --------------------------------------------------------------------

# clear environment
rm(list = ls())

suppressPackageStartupMessages({
  library(emdi)
  library(dplyr)
  library(tidyr)
  library(sf)
})

mdata_file  <- "mdata.Rds"
n_resp      <- 5
active_specs <- c("a", "b", "c")
ci_level    <- 0.95
z           <- qnorm((1 + ci_level) / 2)

# Dimension labels (F1–F5).
# Fill in once the F-to-dimension mapping is confirmed.

dimension_labels <- c(
  F1 = "F1",
  F2 = "F2",
  F3 = "F3",
  F4 = "F4",
  F5 = "F5"
)

# Target regions
# Add / remove rows freely. The script will extract ALL listed domain codes
# from the full table. Use country prefix (e.g. "BG") to include all regions
# of that country; use a full code (e.g. "EL30") for a single region.
# The 'claim' column documents which substantive claim the row relates to.

# These claims were made based on the the maps, and the resulting table was
# used to verify the claims. The claims stated in this table were not all
# verified.

target_spec <- tibble::tribble(
  ~filter_type, ~filter_value,        ~claim,
  ## Institutional trust / legitimacy
  "country",    "BG",    "institutional: Bulgaria low, strong internal variation",
  "domain",     "BG411", "institutional: Sofia exception within Bulgaria",
  "domain",     "BG312", "institutional: Montana exception within Bulgaria",
  "country",    "ES",    "institutional: Spain low",
  "country",    "HR",    "institutional: Croatia low",
  "country",    "EL",    "institutional: Greece low",
  "domain",     "EL30",  "institutional: Attica high (Greek outlier)",
  "domain",     "EL41",  "institutional: North Aegean low",
  "domain",     "EL42",  "institutional: South Aegean low",
  "country",    "CH",    "institutional: Switzerland uniformly high",
  "domain",     "CH07",  "institutional: Ticino uncertain exception",
  ## Interpersonal trust
  "country",    "CH",    "interpersonal: Switzerland high",
  "country",    "FI",    "interpersonal: Finland high, south lower",
  "country",    "BG",    "interpersonal: Bulgaria low",
  "country",    "EL",    "interpersonal: Greece low",
  "country",    "HU",    "interpersonal: Hungary low",
  "country",    "HR",    "interpersonal: Croatia low",
  "country",    "NL",    "interpersonal: Netherlands unexpectedly low",
  ## Social relations (density)
  "country",    "BG",    "social_relations: Bulgaria high, heterogeneous",
  "country",    "HU",    "social_relations: Hungary low",
  "domain",     "HU110", "social_relations: Budapest exception",
  "country",    "PL",    "social_relations: Poland low",
  "domain",     "PL81",  "social_relations: Lubelskie positive exception",
  "domain",     "PL43",  "social_relations: Lubuskie positive exception",
  ## Openness towards migration
  "country",    "IE",    "openness: Ireland high, variable",
  "country",    "BG",    "openness: Bulgaria low",
  "country",    "HU",    "openness: Hungary low",
  "country",    "HR",    "openness: Croatia low",
  "domain",     "EL41",  "openness: North Aegean extremely low"
)


# ---- Paths --------------------------------------------------------------------

input_dir <- file.path("00_data", "derived")
fit_dir <- file.path("03_output", "model_fits")
output_dir <- file.path("03_output", "evidence")

# ---- Import data --------------------------------------------------------------

mdata_raw <- readRDS(file.path(input_dir, mdata_file))
if (inherits(mdata_raw, "sf")) mdata_raw <- sf::st_drop_geometry(mdata_raw)

region_names <- mdata_raw %>%
  dplyr::select(region, NAME_LATN.x, SampSize)

# ---- Data processing ----------------------------------------------------------


# ──── Load fits and extract estimates ──────────────────────────────────────────


extract_domain_rows <- function(resp_i, spec_code) {
  fname <- file.path(fit_dir, sprintf("best_fit_%d%s.rds", resp_i, spec_code))
  if (!file.exists(fname)) {
    warning(sprintf("File not found: %s", fname))
    return(NULL)
  }
  fit <- readRDS(fname)
  
  ind <- tryCatch(as.data.frame(fit$ind), error = function(e) NULL)
  mse <- tryCatch(as.data.frame(fit$MSE), error = function(e) NULL)
  if (is.null(ind) || is.null(mse)) return(NULL)
  
  req_cols <- c("Domain", "Direct", "FH")
  if (!all(req_cols %in% names(ind)) || !all(req_cols %in% names(mse))) return(NULL)
  
  ## remove out-of-sample domains if flagged
  if ("Out" %in% names(ind)) {
    in_sample <- !as.logical(ind$Out)
    ind <- ind[in_sample, ]
    mse <- mse[mse$Domain %in% ind$Domain, ]
  }
  
  merged <- merge(
    ind[, c("Domain", "Direct", "FH")],
    mse[, c("Domain", "Direct", "FH")],
    by = "Domain", suffixes = c("_est", "_mse")
  )
  
  merged$response_index <- resp_i
  merged$dimension      <- dimension_labels[paste0("F", resp_i)]
  merged$spec_code      <- spec_code
  merged
}

all_rows <- lapply(seq_len(n_resp), function(i) {
  lapply(active_specs, function(s) extract_domain_rows(i, s))
})

all_rows <- do.call(rbind, Filter(Negate(is.null), unlist(all_rows, recursive = FALSE)))

# ───── Compute derived columns ─────────────────────────────────────────────────

all_rows <- all_rows %>%
  rename(
    domain          = Domain,
    direct_est      = Direct_est,
    fh_est          = FH_est,
    direct_mse      = Direct_mse,
    fh_mse          = FH_mse
  ) %>%
  mutate(
    direct_se    = sqrt(pmax(direct_mse, 0)),
    fh_rmse      = sqrt(pmax(fh_mse, 0)),
    direct_ci_lo = direct_est - z * direct_se,
    direct_ci_hi = direct_est + z * direct_se,
    fh_ci_lo     = fh_est    - z * fh_rmse,
    fh_ci_hi     = fh_est    + z * fh_rmse,
    country      = substr(domain, 1, 2)
  )

# join region names
all_rows <- left_join(all_rows, region_names, by = c("domain" = "region"))

# quintiles and ranks (over all 233 in-sample domains per dimension×spec)
all_rows <- all_rows %>%
  group_by(response_index, spec_code) %>%
  mutate(
    direct_rank     = rank(-direct_est, ties.method = "average"),  # rank 1 = highest
    fh_rank         = rank(-fh_est,     ties.method = "average"),
    direct_quintile = dplyr::ntile(direct_est, 5),  # 1 = bottom, 5 = top
    fh_quintile     = dplyr::ntile(fh_est,     5)
  ) %>%
  ungroup()

# European mean FH (per dimension × spec)
all_rows <- all_rows %>%
  group_by(response_index, spec_code) %>%
  mutate(eu_mean_fh = mean(fh_est, na.rm = TRUE)) %>%
  ungroup()

# Country mean FH (per country × dimension × spec)
all_rows <- all_rows %>%
  group_by(country, response_index, spec_code) %>%
  mutate(country_mean_fh = mean(fh_est, na.rm = TRUE)) %>%
  ungroup()

# CI excludes reference
all_rows <- all_rows %>%
  mutate(
    ci_excludes_eu_mean      = (fh_ci_lo > eu_mean_fh)      | (fh_ci_hi < eu_mean_fh),
    ci_above_eu_mean         = fh_ci_lo > eu_mean_fh,
    ci_below_eu_mean         = fh_ci_hi < eu_mean_fh,
    ci_excludes_country_mean = (fh_ci_lo > country_mean_fh) | (fh_ci_hi < country_mean_fh),
    ci_above_country_mean    = fh_ci_lo > country_mean_fh,
    ci_below_country_mean    = fh_ci_hi < country_mean_fh
  )

# ──── Quintile breakpoints and CI quintile spans ────────────────────────────

# The point estimate falls in quintile Q, but the 95% CI can straddle quintile
# boundaries.  fh_quintile_span / dir_quintile_span = number of quintiles the CI
# covers (1 = fully within one quintile, 2 = spans a boundary, etc.).
# fh_ci_lo_quintile / fh_ci_hi_quintile = quintile of the lower/upper CI bound.
# Quintile 1 = bottom fifth, quintile 5 = top fifth of the European distribution.

fh_qtile_breaks <- all_rows %>%
  group_by(response_index, spec_code) %>%
  summarise(
    fh_q20 = quantile(fh_est, 0.20, na.rm = TRUE),
    fh_q40 = quantile(fh_est, 0.40, na.rm = TRUE),
    fh_q60 = quantile(fh_est, 0.60, na.rm = TRUE),
    fh_q80 = quantile(fh_est, 0.80, na.rm = TRUE),
    .groups = "drop"
  )

direct_qtile_breaks <- all_rows %>%
  filter(spec_code == "a") %>%
  group_by(response_index) %>%
  summarise(
    dir_q20 = quantile(direct_est, 0.20, na.rm = TRUE),
    dir_q40 = quantile(direct_est, 0.40, na.rm = TRUE),
    dir_q60 = quantile(direct_est, 0.60, na.rm = TRUE),
    dir_q80 = quantile(direct_est, 0.80, na.rm = TRUE),
    .groups = "drop"
  )

# assign quintile (1–5) of value x given the four quantile breakpoints
q_of <- function(x, q20, q40, q60, q80) {
  dplyr::case_when(
    is.na(x) ~ NA_integer_,
    x <= q20  ~ 1L,
    x <= q40  ~ 2L,
    x <= q60  ~ 3L,
    x <= q80  ~ 4L,
    TRUE       ~ 5L
  )
}

all_rows <- all_rows %>%
  left_join(fh_qtile_breaks,     by = c("response_index", "spec_code")) %>%
  left_join(direct_qtile_breaks, by = "response_index") %>%
  mutate(
    fh_ci_lo_quintile  = q_of(fh_ci_lo,     fh_q20,  fh_q40,  fh_q60,  fh_q80),
    fh_ci_hi_quintile  = q_of(fh_ci_hi,     fh_q20,  fh_q40,  fh_q60,  fh_q80),
    fh_quintile_span   = fh_ci_hi_quintile - fh_ci_lo_quintile + 1L,
    dir_ci_lo_quintile = q_of(direct_ci_lo, dir_q20, dir_q40, dir_q60, dir_q80),
    dir_ci_hi_quintile = q_of(direct_ci_hi, dir_q20, dir_q40, dir_q60, dir_q80),
    dir_quintile_span  = dir_ci_hi_quintile - dir_ci_lo_quintile + 1L
  )

## ──── Wide format: one row per domain×dimension, columns per spec ─────────────

wide <- all_rows %>%
  dplyr::select(
    domain, country, NAME_LATN.x, response_index, dimension, spec_code,
    SampSize,
    direct_est, direct_se, direct_ci_lo, direct_ci_hi,
    direct_rank, direct_quintile,
    fh_est, fh_rmse, fh_ci_lo, fh_ci_hi, fh_rank, fh_quintile,
    eu_mean_fh, country_mean_fh,
    ci_excludes_eu_mean, ci_above_eu_mean, ci_below_eu_mean,
    ci_excludes_country_mean, ci_above_country_mean, ci_below_country_mean
  ) %>%
  ## keep direct columns only once (they are spec-invariant); pivot FH columns
  ## First: save direct info (same across specs per domain×dimension)
  group_by(domain, response_index) %>%
  mutate(
    direct_est_chk = direct_est,
    direct_se_chk  = direct_se
  ) %>%
  ungroup()

## build per-spec wide columns
fh_cols <- c("fh_est", "fh_rmse", "fh_ci_lo", "fh_ci_hi", "fh_rank", "fh_quintile",
             "fh_ci_lo_quintile", "fh_ci_hi_quintile", "fh_quintile_span",
             "eu_mean_fh", "country_mean_fh",
             "ci_excludes_eu_mean", "ci_above_eu_mean", "ci_below_eu_mean",
             "ci_excludes_country_mean", "ci_above_country_mean", "ci_below_country_mean")

wide_fh <- all_rows %>%
  dplyr::select(domain, response_index, dimension, spec_code, all_of(fh_cols)) %>%
  pivot_wider(
    names_from  = spec_code,
    values_from = all_of(fh_cols),
    names_glue  = "{spec_code}_{.value}"
  )

direct_once <- all_rows %>%
  filter(spec_code == "a") %>%   # direct is same across specs; take from spec a
  dplyr::select(
    domain, country, NAME_LATN.x, SampSize,
    response_index, dimension,
    direct_est, direct_se, direct_ci_lo, direct_ci_hi,
    direct_rank, direct_quintile,
    dir_ci_lo_quintile, dir_ci_hi_quintile, dir_quintile_span
  )

full_table <- left_join(direct_once, wide_fh, by = c("domain", "response_index", "dimension"))


## OSM–admin difference (c minus a)
full_table <- full_table %>%
  mutate(
    osm_admin_diff = b_fh_est - a_fh_est
  )

## tidy column order
full_table <- full_table %>%
  dplyr::select(
    domain, country, NAME_LATN.x, SampSize,
    response_index, dimension,
    direct_est, direct_se, direct_ci_lo, direct_ci_hi, direct_rank, direct_quintile,
    dir_ci_lo_quintile, dir_ci_hi_quintile, dir_quintile_span,
    ## admin spec (a / M1)
    a_fh_est, a_fh_rmse, a_fh_ci_lo, a_fh_ci_hi, a_fh_rank, a_fh_quintile,
    a_fh_ci_lo_quintile, a_fh_ci_hi_quintile, a_fh_quintile_span,
    a_eu_mean_fh, a_country_mean_fh,
    a_ci_excludes_eu_mean, a_ci_above_eu_mean, a_ci_below_eu_mean,
    a_ci_excludes_country_mean, a_ci_above_country_mean, a_ci_below_country_mean,
    ## OSM spec (b / M2)
    b_fh_est, b_fh_rmse, b_fh_ci_lo, b_fh_ci_hi, b_fh_rank, b_fh_quintile,
    b_fh_ci_lo_quintile, b_fh_ci_hi_quintile, b_fh_quintile_span,
    b_eu_mean_fh, b_country_mean_fh,
    b_ci_excludes_eu_mean, b_ci_above_eu_mean, b_ci_below_eu_mean,
    b_ci_excludes_country_mean, b_ci_above_country_mean, b_ci_below_country_mean,
    ## hybrid spec (c / M3)
    c_fh_est, c_fh_rmse, c_fh_ci_lo, c_fh_ci_hi, c_fh_rank, c_fh_quintile,
    c_fh_ci_lo_quintile, c_fh_ci_hi_quintile, c_fh_quintile_span,
    c_eu_mean_fh, c_country_mean_fh,
    c_ci_excludes_eu_mean, c_ci_above_eu_mean, c_ci_below_eu_mean,
    c_ci_excludes_country_mean, c_ci_above_country_mean, c_ci_below_country_mean,
    ## summary
    osm_admin_diff
  ) %>%
  arrange(response_index, country, domain)

# ──── Filter to target regions ───────────────────────────────────────────────

# Expand target_spec: "country" rows become all regions with that prefix
target_domains <- purrr::map_dfr(seq_len(nrow(target_spec)), function(i) {
  row <- target_spec[i, ]
  if (row$filter_type == "country") {
    matching <- full_table %>%
      filter(country == row$filter_value) %>%
      dplyr::select(domain) %>%
      distinct()
    matching$claim <- row$claim
    matching
  } else {
    tibble::tibble(domain = row$filter_value, claim = row$claim)
  }
})

# one domain can appear under multiple claims (e.g. BG appears for institutional and interpersonal)
# keep all claim associations but deduplicate domain×response_index rows for the table itself
target_table <- full_table %>%
  filter(domain %in% target_domains$domain) %>%
  arrange(response_index, country, domain)

# attach all associated claim descriptions as a single string per domain
claim_map <- target_domains %>%
  group_by(domain) %>%
  summarise(claims = paste(unique(claim), collapse = " | "), .groups = "drop")

target_table <- left_join(target_table, claim_map, by = "domain")

# ---- Export outputs -----------------------------------------------------------

full_path   <- file.path(output_dir, "validation_full.csv")
target_path   <- file.path(output_dir, "validation_targets.csv")

write.csv(full_table,   full_path,   row.names = FALSE)
write.csv(target_table, target_path, row.names = FALSE)

cat(sprintf("Saved full table:    %s  (%d rows x %d cols)\n",
            full_path,   nrow(full_table),   ncol(full_table)))
cat(sprintf("Saved target table:  %s  (%d rows x %d cols)\n",
            target_path, nrow(target_table), ncol(target_table)))

# ---- End ----------------------------------------------------------------------
