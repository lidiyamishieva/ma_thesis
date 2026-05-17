# ==============================================================================
# Script:        16_results_tables.R
# Purpose:       Manuscript tables only, all for the results section/appendices
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

library(tidyverse)
library(modelsummary)

fmt <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_character_)
  sprintf("%.3f (%.3f); %.3f [%.3f–%.3f]",
          mean(x), sd(x), median(x), min(x), max(x))
}

# ---- Paths --------------------------------------------------------------------

output_dir <- file.path("03_output", "tables", "results")
fit_dir <- file.path("03_output", "model_fits")
evidence_dir <- file.path("03_output", "evidence")

# ---- Import data --------------------------------------------------------------

# Load shared analysis dataset (produced by 03_analysis_dataset.R)
cache <- readRDS(file.path(evidence_dir, "analysis_data.rds"))
list2env(cache, envir = environment())

# ---- Data processing ----------------------------------------------------------

# build table
tbl <- do.call(rbind, lapply(1:5, function(i) {
  do.call(rbind, lapply(specs, function(s) {
    d    <- all_data[all_data$factor_id == i & all_data$spec == s, ]
    path <- file.path(fit_dir, sprintf("best_fit_%d%s.rds", i, s))
    ms   <- if (file.exists(path)) readRDS(path)$model$model_select else NULL
    data.frame(
      Factor           = i,
      Factor_label     = factor_labels[as.character(i)],
      Model            = model_labels[s],
      Direct_estimate  = fmt(d$Direct_est),
      CV_direct        = fmt(d$cv_direct),
      EBLUP            = fmt(d$FH_est),
      Spearman_rho     = round(cor(d$Direct_est, d$FH_est, method = "spearman"), 3),
      MSE_direct       = fmt(d$Direct_mse),
      MSE_EBLUP        = fmt(d$FH_mse),
      RMSE_direct      = fmt(d$rmse_direct),
      RMSE_EBLUP       = fmt(d$rmse_fh),
      RMSE_over_Direct = fmt(d$rmse_direct / d$Direct_est),
      RMSE_over_EBLUP  = fmt(d$rmse_fh / d$FH_est),
      Efficiency       = fmt(d$efficiency),
      KICb2            = if (!is.null(ms)) round(ms$KICb2, 2) else NA_real_,
      FH_R2            = if (!is.null(ms)) round(ms$FH_R2, 3) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}))

write.csv(tbl, file.path(output_dir, "model_summary_table.csv"), row.names = FALSE)
cat("Summary table written:", nrow(tbl), "rows.\n")

# Direct estimates summary (per factor, deduplicated across specs) 
direct_unique <- all_data[all_data$spec == all_data$spec[1], ]   # any one spec; directs identical
direct_unique <- all_data[!duplicated(paste(all_data$factor_id, all_data$Domain)), ]

direct_summary <- do.call(rbind, lapply(1:5, function(i) {
  d <- direct_unique[direct_unique$factor_id == i, ]
  data.frame(
    Factor        = i,
    Factor_label  = factor_labels[as.character(i)],
    n_domains     = nrow(d),
    Direct_est    = fmt(d$Direct_est),
    RMSE_direct   = fmt(d$rmse_direct),
    CV_direct     = fmt(d$cv_direct),
    stringsAsFactors = FALSE
  )
}))

write.csv(direct_summary, file.path(output_dir, "direct_summary.csv"), row.names = FALSE)
cat("Direct summary written.\n")

direct_kbl <- direct_summary
names(direct_kbl) <- c("F", "Dimension", "*n*", "Direct estimate", "RMSE (direct)", "CV (direct)")
saveRDS(direct_kbl, file.path(output_dir, "direct_summary.rds"))

direct_md <- c(
  "**Direct estimate distribution by dimension** *(mean (SD), median [min–max])*", "",
  knitr::kable(direct_kbl, format = "pipe", row.names = FALSE)
)

writeLines(direct_md, file.path(output_dir, "direct_summary.md"), useBytes = FALSE)
cat("Direct summary markdown written.\n")

# Markdown version
model_order <- c("M0 (null)", "M1 (admin)", "M2 (osm)", "M3 (admin + osm)")
col_names   <- c("M0", "M1 (admin)", "M2 (OSM)", "M3 (admin+OSM)")

fmt_cell <- function(v, col) {
  if (length(v) == 0 || all(is.na(v))) return("—")
  if (col %in% c("Spearman_rho", "FH_R2")) return(sprintf("%.3f", as.numeric(v)))
  if (col == "KICb2")                       return(sprintf("%.2f", as.numeric(v)))
  as.character(v)
}

md_metrics <- list(
  "EBLUP"      = "EBLUP",
  "RMSE"       = "RMSE_EBLUP",
  "Rel. RMSE"  = "RMSE_over_EBLUP",
  "Efficiency" = "Efficiency",
  "Spearman ρ" = "Spearman_rho",
  "KICb2"      = "KICb2",
  "FH-R²"      = "FH_R2"
)

md_df <- do.call(rbind, lapply(1:5, function(fi) {
  sub <- tbl[tbl$Factor == fi, ]
  fl  <- sub$Factor_label[1]
  header <- setNames(
    as.data.frame(t(c(sprintf("**F%d: %s**", fi, fl), "", "", "", "")), stringsAsFactors = FALSE),
    c("Metric", col_names)
  )
  metric_rows <- do.call(rbind, lapply(names(md_metrics), function(mname) {
    col  <- md_metrics[[mname]]
    vals <- sapply(model_order, function(m) fmt_cell(sub[sub$Model == m, col], col))
    setNames(as.data.frame(t(c(mname, vals)), stringsAsFactors = FALSE), c("Metric", col_names))
  }))
  rbind(header, metric_rows)
}))

footnote <- paste0(
  "*Values: mean (SD), median [min–max]. ",
  "Efficiency = (RMSE_EBLUP − RMSE_Direct) / RMSE_Direct. ",
  "Spearman ρ = rank correlation between direct estimates and EBLUPs across all NUTS-2 regions. ",
  "KICb2 = Kullback information criterion (bias-corrected, version 2); lower = better fit. ",
  "FH-R² = 1 − σ²ᵤ/σ²₀; negative values expected for M2.*"
)
md_lines <- c(knitr::kable(md_df, format = "pipe", row.names = FALSE), "", footnote)

saveRDS(md_df, file.path(output_dir, "model_summary_table.rds"))
writeLines(md_lines, file.path(output_dir, "model_summary_table.md"))
cat("Markdown table written.\n")
cat("\nOutputs in", output_dir, "\n")

# Pairwise within/between-country distinguishability table
# Reads pairwise_all_domains.csv, splits by within-country vs between-country,
# and writes CSV / TXT / MD outputs for Table YYY in the results section.


PAIRWISE_IN      <- file.path(evidence_dir, "pairwise_all_domains.csv")
PAIRWISE_OUT_CSV <- file.path(output_dir, "pairwise_within_between_country.csv")
PAIRWISE_OUT_TXT <- file.path(output_dir, "pairwise_within_between_country.txt")
PAIRWISE_OUT_MD  <- file.path(output_dir, "pairwise_within_between_country.md")

cat("\nReading", PAIRWISE_IN, "...\n")
pw <- read.csv(PAIRWISE_IN, stringsAsFactors = FALSE)
pw$within <- pw$country_1 == pw$country_2

summarise_pw <- function(d) {
  data.frame(
    n_pairs          = nrow(d),
    direct_sig       = sum(d$direct_significant, na.rm = TRUE),
    a_sig            = sum(d$a_sig,   na.rm = TRUE),
    b_sig            = sum(d$b_sig,   na.rm = TRUE),
    c_sig            = sum(d$c_sig,   na.rm = TRUE),
    a_spurious       = sum(d$a_spurious, na.rm = TRUE),
    b_spurious       = sum(d$b_spurious, na.rm = TRUE),
    c_spurious       = sum(d$c_spurious, na.rm = TRUE),
    direct_share_sig = mean(d$direct_significant, na.rm = TRUE),
    a_share_sig      = mean(d$a_sig,  na.rm = TRUE),
    b_share_sig      = mean(d$b_sig,  na.rm = TRUE),
    c_share_sig      = mean(d$c_sig,  na.rm = TRUE),
    a_error_rate     = sum(d$a_spurious, na.rm = TRUE) / pmax(sum(d$a_sig, na.rm = TRUE), 1),
    b_error_rate     = sum(d$b_spurious, na.rm = TRUE) / pmax(sum(d$b_sig, na.rm = TRUE), 1),
    c_error_rate     = sum(d$c_spurious, na.rm = TRUE) / pmax(sum(d$c_sig, na.rm = TRUE), 1),
    stringsAsFactors = FALSE
  )
}

pw_out <- pw %>%
  group_by(response_index, within) %>%
  group_modify(~ summarise_pw(.x)) %>%
  ungroup() %>%
  mutate(scope = ifelse(within, "within_country", "between_country")) %>%
  dplyr::select(response_index, scope, everything(), -within) %>%
  arrange(response_index, desc(scope))

# Round shares to 4 decimal places for CSV
pw_csv <- pw_out
share_cols <- grep("share|error_rate", names(pw_csv), value = TRUE)
pw_csv[share_cols] <- lapply(pw_csv[share_cols], round, digits = 4)
write.csv(pw_csv, PAIRWISE_OUT_CSV, row.names = FALSE)
cat("Wrote", PAIRWISE_OUT_CSV, "\n")


sink(PAIRWISE_OUT_TXT)
cat("Pairwise distinguishability split by scope (within-country vs between-country)\n")
cat("Source: pairwise_all_domains.csv (all 27,028 pairs per dimension)\n\n")
cat("Spec codes: a = admin (M1), b = OSM (M2), c = combined (M3)\n")
cat("share_sig  = share of pairs distinguishable at 95%\n")
cat("error_rate = share of distinguishable pairs whose sign contradicts the direct estimate\n\n")
print(as.data.frame(pw_csv), row.names = FALSE, digits = 3)
sink()
cat("Wrote", PAIRWISE_OUT_TXT, "\n")

# Markdown table via knitr::kable
dim_labels <- c("F1", "F2", "F3", "F4", "F5")

pw_md_df <- pw_out %>%
  mutate(
    Dim.          = dim_labels[response_index],
    Scope         = ifelse(scope == "between_country", "between", "within"),
    `direct sig.` = sprintf("%.1f%%", round(direct_share_sig * 100, 1)),
    `M1 sig.`     = sprintf("%.1f%%", round(a_share_sig  * 100, 1)),
    `M2 sig.`     = sprintf("%.1f%%", round(b_share_sig  * 100, 1)),
    `M3 sig.`     = sprintf("%.1f%%", round(c_share_sig  * 100, 1)),
    `M1 err.`     = sprintf("%.1f%%", round(a_error_rate * 100, 1)),
    `M2 err.`     = sprintf("%.1f%%", round(b_error_rate * 100, 1)),
    `M3 err.`     = sprintf("%.1f%%", round(c_error_rate * 100, 1))
  ) %>%
  dplyr::select(Dim., Scope, `direct sig.`, `M1 sig.`, `M2 sig.`, `M3 sig.`,
         `M1 err.`, `M2 err.`, `M3 err.`)
saveRDS(pw_md_df, sub("[.]csv$", ".rds", PAIRWISE_OUT_CSV))

pw_footnote <- paste0(
  "Sig. = share of pairs distinguishable at 95 %; ",
  "err. = share of distinguishable pairs whose sign contradicts the direct estimator.*"
)
pw_md <- c(
  "**Table YYY: Pairwise distinguishability and sign-error rate by scope**", "",
  knitr::kable(pw_md_df, format = "pipe", align = c("l", "l", rep("r", 7))),
  "", pw_footnote
)
writeLines(pw_md, PAIRWISE_OUT_MD, useBytes = FALSE)
cat("Wrote", PAIRWISE_OUT_MD, "\n")

# ── Appendix table: final covariate sets for all 15 fitted models ─────────
# (5 factors × M1/M2/M3; M0 is intercept-only and noted in the caption)

coef_raw <- read.csv(file.path(evidence_dir, "model_coefficients.csv"), stringsAsFactors = FALSE)

# Human-readable variable labels
var_labels <- c(
  # Eurostat / demographic (spec a)
  POPDENSITY                        = "Population density",
  FEMALE_RATE                       = "Female share",
  TERTIARY_Y25.34                   = "Tertiary educ. (25\u201334)",
  TERTIARY_Y25.64                   = "Tertiary educ. (25\u201364)",
  SECONDARY_Y25.34                  = "Secondary educ. (25\u201334)",
  RATE_Y_LT20                       = "Share aged <20",
  RATE_Y60_79                       = "Share aged 60\u201379",
  POVERTY_RATE                      = "Poverty rate",
  ICCS0401_ROBBERY_RATE             = "Robbery rate",
  ICCS0502_THEFT_RATE               = "Theft rate",
  # OSM POI (spec b / c)
  university_count_per_1k_inhab     = "Universities/1k",
  school_count_per_1k_inhab         = "Schools/1k",
  college_count_per_1k_inhab        = "Colleges/1k",
  kindergarten_count_per_1k_inhab   = "Kindergartens/1k",
  hospital_count_per_1k_inhab       = "Hospitals/1k",
  library_count_per_1k_inhab        = "Libraries/1k",
  social_facility_count_per_1k_inhab = "Social facilities/1k",
  doctors_count_per_1k_inhab        = "Doctors/1k",
  marketplace_count_per_1k_inhab    = "Marketplaces/1k",
  shower_count_per_1k_inhab         = "Showers/1k",
  social_centre_count_per_1k_inhab  = "Social centres/1k",
  place_of_worship_count_per_1k_inhab = "Places of worship/1k"
)

model_spec_labels <- c(
  a = "M1 (admin)",
  b = "M2 (OSM)",
  c = "M3 (admin+OSM)"
)

covariate_tbl <- do.call(rbind, lapply(1:5, function(ri) {
  do.call(rbind, lapply(c("a", "b", "c"), function(sc) {
    terms <- coef_raw[coef_raw$response_index == ri &
                        coef_raw$spec_code == sc, "term"]
    # map to readable labels; fall back to raw name if not in lookup
    pretty <- ifelse(terms %in% names(var_labels), var_labels[terms], terms)
    data.frame(
      Dimension = paste0("F", ri, ": ", factor_labels[as.character(ri)]),
      Model     = model_spec_labels[sc],
      k         = length(terms),
      Covariates = paste(pretty, collapse = "; "),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }))
}))

row.names(covariate_tbl) <- NULL

write.csv(covariate_tbl, file.path(output_dir, "model_covariate_sets.csv"), row.names = FALSE)
cov_kbl <- covariate_tbl
names(cov_kbl) <- c("Dimension", "Model", "*k*", "Covariates")
saveRDS(cov_kbl, file.path(output_dir, "model_covariate_sets.rds"))
cat("Covariate sets CSV written.\n")

cov_md_caption <- paste0(
  "**Appendix Table A1: Final covariate sets for all 15 fitted models.**  ",
  "*k* = number of selected covariates. ",
  "M0 (null) contains only an intercept and is not listed. ",
  "Admin covariates (M1, M3) are drawn from Eurostat regional statistics; ",
  "OSM covariates (M2, M3) are POI counts per 1,000 inhabitants derived from OpenStreetMap."
)

cov_md <- c(cov_md_caption, "", knitr::kable(cov_kbl, format = "pipe", align = c("l", "l", "r", "l")))
writeLines(cov_md, file.path(output_dir, "model_covariate_sets.md"), useBytes = FALSE)

cat("Covariate sets markdown written.\n")

# Regression coefficient tables via modelsummary
# One table per dimension (F1–F5), columns = M1 / M2 / M3.
# M0 (intercept-only) is excluded — nothing interesting to show.

# Minimal broom-style S3 methods so modelsummary can handle emdi fh objects
tidy.fh <- function(x, ...) {
  d <- as.data.frame(x$model$coefficients)
  data.frame(
    term      = rownames(d),
    estimate  = d$coefficients,
    std.error = d$std.error,
    statistic = d$t.value,
    p.value   = d$p.value,
    stringsAsFactors = FALSE
  )
}

glance.fh <- function(x, ...) {
  ms <- x$model$model_select
  data.frame(
    Observations = nrow(x$ind),
    sigma2_u     = round(x$model$variance, 5),
    KICb2        = round(ms$KICb2, 2),
    FH_R2        = round(ms$FH_R2, 3),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# register as S3 methods for this session
registerS3method("tidy",   "fh", tidy.fh)
registerS3method("glance", "fh", glance.fh)

# readable variable names (reuse var_labels defined above)
coef_rename <- var_labels   # same named vector, term → pretty label

gof_map <- data.frame(
  raw     = c("Observations", "sigma2_u", "KICb2", "FH_R2"),
  clean   = c("Observations", "\u03c3\u00b2\u1d64", "KICb2", "FH-R\u00b2"),
  fmt     = c("%.0f",         "%.5f",     "%.2f",   "%.3f"),
  stringsAsFactors = FALSE
)

for (fi in 1:5) {
  fl <- factor_labels[as.character(fi)]
  models <- list(
    "M1 (admin)"      = tryCatch(readRDS(file.path(fit_dir, sprintf("best_fit_%da.rds", fi))), error = function(e) NULL),
    "M2 (OSM)"        = tryCatch(readRDS(file.path(fit_dir, sprintf("best_fit_%db.rds", fi))), error = function(e) NULL),
    "M3 (admin+OSM)"  = tryCatch(readRDS(file.path(fit_dir, sprintf("best_fit_%dc.rds", fi))), error = function(e) NULL)
  )
  models <- Filter(Negate(is.null), models)
  
  out_path <- file.path(output_dir, sprintf("coef_table_F%d.md", fi))
  caption  <- sprintf("**Appendix Table A2.%d: Regression coefficients — F%d: %s**  \n*SE in parentheses. +p<0.1, *p<0.05, **p<0.01, ***p<0.001.*", fi, fi, fl)
  
  ms_df <- modelsummary(
    models,
    output     = "dataframe",
    estimate   = "{estimate}{stars}",
    statistic  = "({std.error})",
    coef_rename = coef_rename,
    gof_map    = gof_map,
    stars      = c(`+` = 0.1, `*` = 0.05, `**` = 0.01, `***` = 0.001)
  )
  # drop internal columns modelsummary adds
  ms_df <- ms_df[, !names(ms_df) %in% c("part", "statistic"), drop = FALSE]
  # blank the term label on SE rows (every second row in the covariate block)
  coef_rows <- which(ms_df[[1]] %in% c(names(coef_rename), unname(coef_rename),
                                       coef_raw$term, unname(var_labels)))
  se_rows   <- coef_rows[seq(2, length(coef_rows), by = 2)]
  ms_df[se_rows, 1] <- ""
  tab <- c(
    caption, "",
    knitr::kable(ms_df, format = "pipe", row.names = FALSE,
                 col.names = c("", names(ms_df)[-1]))
  )
  writeLines(tab, out_path, useBytes = FALSE)
  
  # kable-ready RDS: strip stars, plain ASCII GOF labels
  ms_clean <- ms_df
  # strip significance stars from estimate cells (all cols except term col)
  ms_clean[-1] <- lapply(ms_clean[-1], function(x) gsub("[+*]+$", "", x))
  # rename Unicode GOF row labels to plain ASCII
  ms_clean[[1]] <- gsub("\u03c3\u00b2\u1d64", "sigma2_u",          ms_clean[[1]], fixed = TRUE)
  ms_clean[[1]] <- gsub("FH-R\u00b2",         "prop. var. explained", ms_clean[[1]], fixed = TRUE)
  saveRDS(ms_clean, file.path(output_dir, sprintf("coef_table_F%d.rds", fi)))
  cat(sprintf("Coefficient table written: coef_table_F%d.md\n", fi))
}

# ── Combined: all 5 dimensions in one file ────────────────────────────────────
all_coef_lines <- c()
coef_list      <- list()
for (fi in 1:5) {
  path <- file.path(output_dir, sprintf("coef_table_F%d.md", fi))
  rds  <- file.path(output_dir, sprintf("coef_table_F%d.rds", fi))
  if (file.exists(path))
    all_coef_lines <- c(all_coef_lines, readLines(path, warn = FALSE), "", "---", "")
  if (file.exists(rds))
    coef_list[[sprintf("F%d", fi)]] <- readRDS(rds)
}
writeLines(all_coef_lines, file.path(output_dir, "coef_tables_all.md"), useBytes = FALSE)
saveRDS(coef_list, file.path(output_dir, "coef_tables_all.rds"))
cat("Combined coefficient table written: coef_tables_all.md\n")

# ---- Export outputs -----------------------------------------------------------

# ---- End ----------------------------------------------------------------------
