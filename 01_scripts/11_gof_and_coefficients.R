# ==============================================================================
# Script:        11_gof_and_coefficients.R
# Purpose:       Loads all fitted model objects from 03_output/model_fits/ and 
#                extracts evidence: GOF metrics (AIC/BIC/KIC variants, FH-R²), 
#                shrinkage (γ) per domain, residual diagnostics, regression 
#                coefficients, and model ranking by KICb2/FH-R².
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

if (!requireNamespace("emdi", quietly = TRUE)) {
  stop("Package 'emdi' is required to extract model summaries. Install it before running this script.")
}

library(tidyverse)
library(emdi)

excluded_spec_codes <- character(0)   # a/b/c/n all included


# ---- Paths --------------------------------------------------------------------

fit_dir <- file.path("03_output", "model_fits")
output_dir <- file.path("03_output", "evidence")

# ---- Import data --------------------------------------------------------------

# ---- Data processing ----------------------------------------------------------

# ── 1. Text-parsing helpers ───────────────────────────────────────────────────
parse_fit_filename <- function(path) {
  fit_key <- tools::file_path_sans_ext(basename(path))
  match_parts <- regexec("^best_fit_([0-9]+)([[:alpha:]]+)$", fit_key)
  parts <- regmatches(fit_key, match_parts)[[1]]
  
  if (length(parts) != 3) {
    stop(sprintf("Unexpected fit filename: %s", basename(path)))
  }
  
  data.frame(
    file_path = path,
    fit_key = fit_key,
    response_index = as.integer(parts[2]),
    spec_code = parts[3],
    stringsAsFactors = FALSE
  )
}

extract_line_value <- function(lines, pattern) {
  match_idx <- grep(pattern, lines, perl = TRUE)
  
  if (length(match_idx) == 0) {
    return(NA_character_)
  }
  
  sub(pattern, "\\1", lines[match_idx[1]], perl = TRUE)
}

parse_named_numeric_block <- function(lines, header) {
  start_idx <- grep(paste0("^", header, "\\s*$"), lines, perl = TRUE)
  
  if (length(start_idx) == 0) {
    return(setNames(list(), character()))
  }
  
  block_lines <- character()
  for (line in lines[(start_idx[1] + 1):length(lines)]) {
    trimmed <- trimws(line)
    if (trimmed == "") {
      break
    }
    block_lines <- c(block_lines, trimmed)
  }
  
  parsed_values <- list()
  if (length(block_lines) < 2) {
    return(parsed_values)
  }
  
  for (index in seq(1, length(block_lines) - 1, by = 2)) {
    parsed_chunk <- tryCatch(
      read.table(
        text = paste(block_lines[index], block_lines[index + 1], sep = "\n"),
        header = TRUE,
        check.names = FALSE
      ),
      error = function(e) NULL
    )
    
    if (is.null(parsed_chunk) || nrow(parsed_chunk) == 0) {
      next
    }
    
    for (column_name in names(parsed_chunk)) {
      parsed_values[[column_name]] <- suppressWarnings(as.numeric(parsed_chunk[[column_name]][1]))
    }
  }
  
  parsed_values
}

parse_residual_diagnostics <- function(lines) {
  start_idx <- grep("^Residual diagnostics:\\s*$", lines, perl = TRUE)
  
  if (length(start_idx) == 0 || (start_idx[1] + 2) > length(lines)) {
    return(setNames(list(), character()))
  }
  
  header_line <- gsub("\\s+", " ", trimws(lines[start_idx[1] + 1]))
  column_names <- strsplit(header_line, " ", fixed = TRUE)[[1]]
  parsed_values <- list()
  
  for (line in lines[(start_idx[1] + 2):length(lines)]) {
    trimmed <- trimws(line)
    if (trimmed == "") {
      break
    }
    
    parts <- strsplit(gsub("\\s+", " ", trimmed), " ", fixed = TRUE)[[1]]
    row_name <- parts[1]
    row_values <- suppressWarnings(as.numeric(parts[-1]))
    
    if (length(row_values) != length(column_names)) {
      next
    }
    
    for (index in seq_along(column_names)) {
      parsed_values[[paste(row_name, column_names[index], sep = ".")]] <- row_values[index]
    }
  }
  
  parsed_values
}

# ── 2. Evidence extractors ────────────────────────────────────────────────────
extract_coefficients <- function(fit, fit_meta) {
  coefficient_table <- tryCatch(as.data.frame(fit$model$coefficients), error = function(e) NULL)
  
  if (is.null(coefficient_table) || nrow(coefficient_table) == 0) {
    return(NULL)
  }
  
  coefficient_table$term <- rownames(coefficient_table)
  rownames(coefficient_table) <- NULL
  
  coefficient_table <- coefficient_table[, c("term", setdiff(names(coefficient_table), "term"))]
  coefficient_table$fit_key <- fit_meta$fit_key
  coefficient_table$model_index <- fit_meta$model_index
  coefficient_table$response_index <- fit_meta$response_index
  coefficient_table$spec_index <- fit_meta$spec_index
  coefficient_table$spec_code <- fit_meta$spec_code
  
  coefficient_table[, c(
    "fit_key", "model_index", "response_index", "spec_index", "spec_code",
    setdiff(names(coefficient_table), c("fit_key", "model_index", "response_index", "spec_index", "spec_code"))
  )]
}

extract_shrinkage_info <- function(fit, fit_meta) {
  gamma_table <- tryCatch(as.data.frame(fit$model$gamma), error = function(e) NULL)
  
  # Fallback for models where gamma is not stored (e.g. null model with sigma^2_u = 0):
  # compute gamma_i = sigma^2_u / (sigma^2_u + psi_i) from the variance component
  # and the domain-level sampling variances (= Direct MSE).
  if (is.null(gamma_table) || nrow(gamma_table) == 0) {
    sigma2u   <- tryCatch(as.numeric(fit$model$variance), error = function(e) NULL)
    mse_table <- tryCatch(as.data.frame(fit$MSE),         error = function(e) NULL)
    if (!is.null(sigma2u) && length(sigma2u) == 1 &&
        !is.null(mse_table) && all(c("Domain","Direct") %in% names(mse_table))) {
      psi         <- mse_table$Direct
      gamma_vals  <- sigma2u / (sigma2u + psi)
      gamma_table <- data.frame(Domain = mse_table$Domain, Gamma = gamma_vals,
                                stringsAsFactors = FALSE)
    } else {
      return(list(summary_row = NULL, gamma_rows = NULL))
    }
  }
  
  gamma_table$domain <- rownames(gamma_table)
  rownames(gamma_table) <- NULL
  
  numeric_columns <- names(gamma_table)[vapply(gamma_table, is.numeric, logical(1))]
  if (length(numeric_columns) == 0) {
    return(list(summary_row = NULL, gamma_rows = NULL))
  }
  
  gamma_column <- if ("Gamma" %in% numeric_columns) "Gamma" else numeric_columns[1]
  domain_column <- if ("Domain" %in% names(gamma_table)) "Domain" else "domain"
  if (!(domain_column %in% names(gamma_table))) {
    gamma_table$domain <- rownames(gamma_table)
  }
  gamma_values <- gamma_table[[gamma_column]]
  
  response_var <- all.vars(fit$fixed)[1]
  
  gamma_rows <- data.frame(
    fit_key = fit_meta$fit_key,
    model_index = fit_meta$model_index,
    response_index = fit_meta$response_index,
    response_var = response_var,
    spec_index = fit_meta$spec_index,
    spec_code = fit_meta$spec_code,
    domain = as.character(gamma_table[[domain_column]]),
    gamma = gamma_values,
    stringsAsFactors = FALSE
  )
  
  summary_row <- data.frame(
    fit_key = fit_meta$fit_key,
    model_index = fit_meta$model_index,
    response_index = fit_meta$response_index,
    response_var = response_var,
    spec_index = fit_meta$spec_index,
    spec_code = fit_meta$spec_code,
    gamma_mean = mean(gamma_values, na.rm = TRUE),
    gamma_median = median(gamma_values, na.rm = TRUE),
    gamma_sd = stats::sd(gamma_values, na.rm = TRUE),
    gamma_min = min(gamma_values, na.rm = TRUE),
    gamma_max = max(gamma_values, na.rm = TRUE),
    share_gamma_lt_0_2 = mean(gamma_values < 0.2, na.rm = TRUE),
    share_gamma_0_2_to_0_8 = mean(gamma_values >= 0.2 & gamma_values <= 0.8, na.rm = TRUE),
    share_gamma_gt_0_8 = mean(gamma_values > 0.8, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  list(summary_row = summary_row, gamma_rows = gamma_rows)
}

# ── 3. Model comparison helpers ─────────────────────────────────────────────────
rank_metric <- function(values, decreasing = FALSE) {
  if (all(is.na(values))) return(rep(NA_integer_, length(values)))
  if (decreasing) rank(-values, ties.method = "min", na.last = "keep")
  else            rank( values, ties.method = "min", na.last = "keep")
}

format_best_specs <- function(df, metric_col, decreasing = FALSE) {
  vals <- df[[metric_col]]
  if (all(is.na(vals)))
    return(list(spec_codes = NA_character_, fit_keys = NA_character_, metric_value = NA_real_))
  best <- if (decreasing) max(vals, na.rm = TRUE) else min(vals, na.rm = TRUE)
  br   <- df[!is.na(vals) & vals == best, ]
  list(spec_codes = paste(br$spec_code, collapse = ", "),
       fit_keys   = paste(br$fit_key,   collapse = ", "),
       metric_value = best)
}

build_comparison_tables <- function(evidence_rows) {
  cols <- c("response_index", "response_var", "model_index", "spec_index",
            "spec_code", "fit_key", "KICb2", "FH_R2")
  cmp  <- evidence_rows[, cols]
  cmp  <- cmp[!(cmp$spec_code %in% excluded_spec_codes), ]
  if (nrow(cmp) == 0) stop("No models remain after excluding specs.")
  
  by_dep <- lapply(split(cmp, cmp$response_index), function(df) {
    df <- df[order(df$spec_index), ]
    df$KICb2_rank <- rank_metric(df$KICb2, decreasing = FALSE)
    df$FH_R2_rank <- rank_metric(df$FH_R2, decreasing = TRUE)
    df
  })
  comp_tbl <- do.call(rbind, by_dep)
  rownames(comp_tbl) <- NULL
  
  best_tbl <- do.call(rbind, lapply(by_dep, function(df) {
    bk <- format_best_specs(df, "KICb2", decreasing = FALSE)
    br <- format_best_specs(df, "FH_R2", decreasing = TRUE)
    data.frame(
      response_index   = df$response_index[1], response_var = df$response_var[1],
      excluded_specs   = paste(excluded_spec_codes, collapse = ", "),
      best_KICb2_specs = bk$spec_codes, best_KICb2_fits = bk$fit_keys, best_KICb2_value = bk$metric_value,
      best_FH_R2_specs = br$spec_codes, best_FH_R2_fits = br$fit_keys, best_FH_R2_value = br$metric_value,
      stringsAsFactors = FALSE
    )
  }))
  
  smry <- unlist(lapply(by_dep, function(df) {
    bk <- format_best_specs(df, "KICb2", decreasing = FALSE)
    br <- format_best_specs(df, "FH_R2", decreasing = TRUE)
    c(sprintf("Dependent variable %d (%s)", df$response_index[1], df$response_var[1]),
      sprintf("  Excluded specs: %s", paste(excluded_spec_codes, collapse = ", ")),
      sprintf("  Best KICb2: spec %s (%s) | KICb2=%.4f", bk$spec_codes, bk$fit_keys, bk$metric_value),
      sprintf("  Best FH-R²: spec %s (%s) | FH-R²=%.4f", br$spec_codes, br$fit_keys, br$metric_value),
      "")
  }), use.names = FALSE)
  
  list(comparison_table = comp_tbl, best_by_criterion = best_tbl, summary_lines = smry)
}

# ── 4. Master extractor ───────────────────────────────────────────────────────
extract_fit_evidence <- function(fit, fit_meta) {
  summary_lines <- tryCatch(capture.output(summary(fit)), error = function(e) sprintf("summary() failed: %s", conditionMessage(e)))
  explanatory_measures <- parse_named_numeric_block(summary_lines, "Explanatory measures:")
  residual_diagnostics <- parse_residual_diagnostics(summary_lines)
  
  method_variance <- if (is.list(fit$method) && "method" %in% names(fit$method)) {
    as.character(fit$method$method)
  } else {
    as.character(fit$method)
  }
  
  method_mse <- if (is.list(fit$method) && "MSE_method" %in% names(fit$method)) {
    as.character(fit$method$MSE_method)
  } else {
    extract_line_value(summary_lines, "^MSE method:\\s*(.+?)\\s*$")
  }
  
  variance_component <- suppressWarnings(as.numeric(
    extract_line_value(summary_lines, "^Estimated variance component\\(s\\):\\s*([0-9eE+\\.-]+)\\s*$")
  ))
  shrinkage_info <- extract_shrinkage_info(fit, fit_meta)
  
  list(
    evidence_row = data.frame(
      fit_key = fit_meta$fit_key,
      model_index = fit_meta$model_index,
      response_index = fit_meta$response_index,
      response_var = all.vars(fit$fixed)[1],
      spec_index = fit_meta$spec_index,
      spec_code = fit_meta$spec_code,
      file_path = fit_meta$file_path,
      fit_class = paste(class(fit), collapse = "; "),
      formula = paste(deparse(fit$fixed), collapse = " "),
      variance_method = method_variance,
      mse_method = method_mse,
      out_of_sample_domains = suppressWarnings(as.integer(
        extract_line_value(summary_lines, "^Out-of-sample domains:\\s*([0-9]+)\\s*$")
      )),
      in_sample_domains = suppressWarnings(as.integer(
        extract_line_value(summary_lines, "^In-sample domains:\\s*([0-9]+)\\s*$")
      )),
      variance_component = variance_component,
      coefficient_count = tryCatch(nrow(as.data.frame(fit$model$coefficients)), error = function(e) NA_integer_),
      loglike = if ("loglike" %in% names(explanatory_measures)) explanatory_measures$loglike else NA_real_,
      AIC = if ("AIC" %in% names(explanatory_measures)) explanatory_measures$AIC else NA_real_,
      AICc = if ("AICc" %in% names(explanatory_measures)) explanatory_measures$AICc else NA_real_,
      AICb1 = if ("AICb1" %in% names(explanatory_measures)) explanatory_measures$AICb1 else NA_real_,
      AICb2 = if ("AICb2" %in% names(explanatory_measures)) explanatory_measures$AICb2 else NA_real_,
      BIC = if ("BIC" %in% names(explanatory_measures)) explanatory_measures$BIC else NA_real_,
      KIC = if ("KIC" %in% names(explanatory_measures)) explanatory_measures$KIC else NA_real_,
      KICc = if ("KICc" %in% names(explanatory_measures)) explanatory_measures$KICc else NA_real_,
      KICb1 = if ("KICb1" %in% names(explanatory_measures)) explanatory_measures$KICb1 else NA_real_,
      KICb2 = if ("KICb2" %in% names(explanatory_measures)) explanatory_measures$KICb2 else NA_real_,
      AdjR2 = if ("AdjR2" %in% names(explanatory_measures)) explanatory_measures$AdjR2 else NA_real_,
      FH_R2 = if ("FH_R2" %in% names(explanatory_measures)) explanatory_measures$FH_R2 else NA_real_,
      gamma_mean = if (!is.null(shrinkage_info$summary_row)) shrinkage_info$summary_row$gamma_mean else NA_real_,
      gamma_median = if (!is.null(shrinkage_info$summary_row)) shrinkage_info$summary_row$gamma_median else NA_real_,
      gamma_sd = if (!is.null(shrinkage_info$summary_row)) shrinkage_info$summary_row$gamma_sd else NA_real_,
      gamma_min = if (!is.null(shrinkage_info$summary_row)) shrinkage_info$summary_row$gamma_min else NA_real_,
      gamma_max = if (!is.null(shrinkage_info$summary_row)) shrinkage_info$summary_row$gamma_max else NA_real_,
      share_gamma_lt_0_2 = if (!is.null(shrinkage_info$summary_row)) shrinkage_info$summary_row$share_gamma_lt_0_2 else NA_real_,
      share_gamma_0_2_to_0_8 = if (!is.null(shrinkage_info$summary_row)) shrinkage_info$summary_row$share_gamma_0_2_to_0_8 else NA_real_,
      share_gamma_gt_0_8 = if (!is.null(shrinkage_info$summary_row)) shrinkage_info$summary_row$share_gamma_gt_0_8 else NA_real_,
      std_resid_skewness = if ("Standardized_Residuals.Skewness" %in% names(residual_diagnostics)) residual_diagnostics[["Standardized_Residuals.Skewness"]] else NA_real_,
      std_resid_kurtosis = if ("Standardized_Residuals.Kurtosis" %in% names(residual_diagnostics)) residual_diagnostics[["Standardized_Residuals.Kurtosis"]] else NA_real_,
      std_resid_shapiro_w = if ("Standardized_Residuals.Shapiro_W" %in% names(residual_diagnostics)) residual_diagnostics[["Standardized_Residuals.Shapiro_W"]] else NA_real_,
      std_resid_shapiro_p = if ("Standardized_Residuals.Shapiro_p" %in% names(residual_diagnostics)) residual_diagnostics[["Standardized_Residuals.Shapiro_p"]] else NA_real_,
      random_effects_skewness = if ("Random_effects.Skewness" %in% names(residual_diagnostics)) residual_diagnostics[["Random_effects.Skewness"]] else NA_real_,
      random_effects_kurtosis = if ("Random_effects.Kurtosis" %in% names(residual_diagnostics)) residual_diagnostics[["Random_effects.Kurtosis"]] else NA_real_,
      random_effects_shapiro_w = if ("Random_effects.Shapiro_W" %in% names(residual_diagnostics)) residual_diagnostics[["Random_effects.Shapiro_W"]] else NA_real_,
      random_effects_shapiro_p = if ("Random_effects.Shapiro_p" %in% names(residual_diagnostics)) residual_diagnostics[["Random_effects.Shapiro_p"]] else NA_real_,
      stringsAsFactors = FALSE
    ),
    shrinkage_summary = shrinkage_info$summary_row,
    gamma_rows = shrinkage_info$gamma_rows,
    coefficient_rows = extract_coefficients(fit, fit_meta),
    summary_lines = c(
      sprintf("===== Model %d | Response %d | Spec %s | %s =====", fit_meta$model_index, fit_meta$response_index, fit_meta$spec_code, fit_meta$fit_key),
      summary_lines,
      ""
    )
  )
}

# Find fit files only, e.g. best_fit_1a.rds
rds_files <- list.files(
  fit_dir,
  pattern = "^best_fit_[0-9]+[[:alpha:]]+\\.rds$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(rds_files) == 0) {
  message("No .rds files found in '", fit_dir, "'.")
} else {
  fit_meta <- do.call(rbind, lapply(rds_files, parse_fit_filename))
  fit_meta$spec_index <- match(fit_meta$spec_code, sort(unique(fit_meta$spec_code)))
  fit_meta <- fit_meta[order(fit_meta$response_index, fit_meta$spec_index), ]
  fit_meta$model_index <- seq_len(nrow(fit_meta))
  rds_files <- fit_meta$file_path
  
  # Read each file safely; return NULL on failure
  best_fits_list <- setNames(
    lapply(rds_files, function(f) {
      tryCatch({
        readRDS(f)
      }, error = function(e) {
        warning(sprintf("Failed to read '%s': %s", f, conditionMessage(e)))
        NULL
      })
    }),
    tools::file_path_sans_ext(basename(rds_files))
  )
  
  # Drop failed reads (NULL entries)
  if (length(best_fits_list) > 0) {
    best_fits_list <- best_fits_list[!vapply(best_fits_list, is.null, logical(1))]
  }
  
  fit_meta <- fit_meta[fit_meta$fit_key %in% names(best_fits_list), ]
  fit_meta <- fit_meta[match(names(best_fits_list), fit_meta$fit_key), ]
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  evidence_bundle <- Map(
    f = function(fit, meta_row) extract_fit_evidence(fit, meta_row),
    fit = best_fits_list,
    meta_row = split(fit_meta, seq_len(nrow(fit_meta)))
  )
  
  evidence_rows <- do.call(rbind, lapply(evidence_bundle, function(item) item$evidence_row))
  shrinkage_rows <- do.call(
    rbind,
    Filter(Negate(is.null), lapply(evidence_bundle, function(item) item$shrinkage_summary))
  )
  gamma_rows <- do.call(
    rbind,
    Filter(Negate(is.null), lapply(evidence_bundle, function(item) item$gamma_rows))
  )
  if (!is.null(shrinkage_rows)) {
    shrinkage_rows <- shrinkage_rows[!(shrinkage_rows$spec_code %in% excluded_spec_codes), ]
  }
  if (!is.null(gamma_rows)) {
    gamma_rows <- gamma_rows[!(gamma_rows$spec_code %in% excluded_spec_codes), ]
  }
  coefficient_rows <- do.call(
    rbind,
    Filter(Negate(is.null), lapply(evidence_bundle, function(item) item$coefficient_rows))
  )
  summary_text <- unlist(lapply(evidence_bundle, function(item) item$summary_lines), use.names = FALSE)
  comparison_outputs <- build_comparison_tables(evidence_rows)
  
  write.csv(fit_meta, file.path(output_dir, "fit_order.csv"), row.names = FALSE)
  write.csv(evidence_rows, file.path(output_dir, "model_evidence.csv"), row.names = FALSE)
  if (!is.null(shrinkage_rows)) {
    write.csv(shrinkage_rows, file.path(output_dir, "model_shrinkage_summary.csv"), row.names = FALSE)
  }
  if (!is.null(gamma_rows)) {
    write.csv(gamma_rows, file.path(output_dir, "model_gamma_by_domain.csv"), row.names = FALSE)
  }
  if (!is.null(coefficient_rows)) {
    write.csv(coefficient_rows, file.path(output_dir, "model_coefficients.csv"), row.names = FALSE)
  }
  writeLines(summary_text, con = file.path(output_dir, "all_model_summaries.txt"))
  write.csv(comparison_outputs$comparison_table,  file.path(output_dir, "model_comparison_by_depvar.csv"), row.names = FALSE)
  write.csv(comparison_outputs$best_by_criterion, file.path(output_dir, "model_best_by_criterion.csv"),   row.names = FALSE)
  writeLines(comparison_outputs$summary_lines, con = file.path(output_dir, "model_comparison_summary.txt"))
  saveRDS(
    list(
      fit_order             = fit_meta,
      model_evidence        = evidence_rows,
      model_shrinkage_summary = shrinkage_rows,
      model_gamma_by_domain = gamma_rows,
      model_coefficients    = coefficient_rows,
      model_comparison      = comparison_outputs$comparison_table,
      best_by_criterion     = comparison_outputs$best_by_criterion,
      comparison_summary    = comparison_outputs$summary_lines,
      summaries             = summary_text
    ),
    file = file.path(output_dir, "all_model_evidence.rds")
  )
  
  # Save the loaded list for reproducibility
  save_path <- file.path(fit_dir, "loaded_best_fits.rds")
  saveRDS(best_fits_list, file = save_path)
  
  message(
    "Loaded ", length(fit_dir), " fit(s) into `best_fits_list`, saved the ordered list to '",
    save_path,
    "', and wrote evidence files to '", output_dir, "'."
  )
}

# ---- Export outputs -----------------------------------------------------------

# output generated within the function above

# ---- End ----------------------------------------------------------------------
