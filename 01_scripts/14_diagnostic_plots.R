# ==============================================================================
# Script:        14_diagnostic_plots.R
# Purpose:       Produces all diagnostic plots
# Project:       The use of OpenStreetMaps in small area estimation of social 
#                cohesion
# Author:        Lidiya Mishieva
# Created:       2026-05-13
# Last updated:  2026-05-13
# R version:     4.5.2 (2025-10-31)
# OS:            x86_64, linux-gnu
# ==============================================================================

# ---- Notes --------------------------------------------------------------------

# Outputs → plots/diagnostics/.
# Sources: 03_output/evidence/analysis_data.rds (10_sae_analysis_dataset.R) 
# for per-model RMSE/scatter/efficiency/bias/QQ plots; 
# 03_output/evidence/model_gamma_by_domain.csv (11_gof_and_coefficients.R) + 
# 00_data/derived/mdata.rds for the shrinkage vs. sample-size panel.

# ---- Setup --------------------------------------------------------------------

# clear environment
rm(list = ls())

library(sf)
library(ggplot2)
library(patchwork)

# ---- Paths --------------------------------------------------------------------

# Load shared analysis dataset (produced by 03_analysis_dataset.R)
input_dir <- file.path("03_output", "evidence")
input_dir2 <- file.path("00_data", "derived")
out_dir  <- file.path("03_output", "plots", "diagnostics")

gamma_csv <- file.path(input_dir, "model_gamma_by_domain.csv")
mdata_csv <- file.path(input_dir2, "mdata.Rds")

# ---- Import data --------------------------------------------------------------
# Load shared analysis dataset (produced by 03_analysis_dataset.R)

cache <- readRDS(file.path(input_dir, "analysis_data.rds"))
list2env(cache, envir = environment())

# ---- Data processing ----------------------------------------------------------

# ── switches (plots only) ─────────────────────────────────────────────────

pt_alpha <- 0.45   # point transparency for overlapping series

# ── fixed axis limits (shared across all factors for comparability) ───────
rmse_ylim <- range(c(all_data$rmse_fh, all_data$rmse_direct), na.rm = TRUE)
eff_ylim  <- range(all_data$efficiency, na.rm = TRUE)
bias_ylim <- range(all_data$bias, na.rm = TRUE)
cv_ylim   <- range(all_data$cv_direct[is.finite(all_data$cv_direct)], na.rm = TRUE)
est_range <- range(c(all_data$Direct_est, all_data$FH_est), na.rm = TRUE)
x_lim     <- range(all_data$samp_size, na.rm = TRUE)

th_cell <- theme_minimal(base_size = 10) +
  theme(axis.line = element_line(color = "grey60", linewidth = 0.4))

for (i in 1:5) {
  fi_all <- all_data[all_data$factor_id == i, ]   # all models for this factor
  for (s in specs) {
    d <- all_data[all_data$factor_id == i & all_data$spec == s, ]
    if (nrow(d) == 0) next
    ftitle <- factor_labels[as.character(i)]
    mtitle <- model_labels[[s]]
    mcol   <- model_colors[[s]]
    
    # ── RMSE vs sample size ───────────────────────────────────────────────
    rmse_long <- rbind(
      data.frame(samp_size = d$samp_size, rmse = d$rmse_direct, series = "Direct"),
      data.frame(samp_size = d$samp_size, rmse = d$rmse_fh,     series = mtitle)
    )
    rmse_long$series <- factor(rmse_long$series, levels = c("Direct", mtitle))
    p_r <- ggplot(rmse_long, aes(x = samp_size, y = rmse, color = series)) +
      geom_point(size = 0.6, alpha = pt_alpha) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 0.5,
                  linetype = "dashed", formula = y ~ x) +
      scale_color_manual(
        values = c(Direct = direct_color, setNames(mcol, mtitle)),
        name   = NULL
      ) +
      scale_x_continuous(limits = x_lim) +
      scale_y_continuous(limits = rmse_ylim) +
      labs(x = "Sample size", y = "RMSE") +
      th_cell +
      theme(legend.position = "top")
    ggsave(file.path(out_dir, sprintf("F%d%s_rmse.png", i, s)),
           p_r, width = 4, height = 3.5, dpi = 200)
    
    # ── Scatter: Direct vs EBLUP ──────────────────────────────────────────
    rho <- cor(d$Direct_est, d$FH_est, method = "spearman")
    p_s <- ggplot(d, aes(x = Direct_est, y = FH_est)) +
      geom_point(size = 0.6, alpha = pt_alpha, color = mcol) +
      geom_smooth(method = "loess", se = FALSE, color = mcol,
                  linewidth = 0.5, linetype = "dashed", formula = y ~ x) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                  color = "grey40", linewidth = 0.5) +
      annotate("text", x = est_range[1], y = est_range[2],
               label = sprintf("\u03c1 = %.2f", rho),
               hjust = 0, vjust = 1, size = 3.2, color = "grey30") +
      scale_x_continuous(limits = est_range) +
      scale_y_continuous(limits = est_range) +
      coord_fixed() +
      labs(x = "Direct estimate", y = "FH (EBLUP)") +
      th_cell
    ggsave(file.path(out_dir, sprintf("F%d%s_scatter.png", i, s)),
           p_s, width = 4, height = 4, dpi = 200)
    
    # ── Efficiency vs sample size ─────────────────────────────────────────
    p_e <- ggplot(d, aes(x = samp_size, y = efficiency)) +
      geom_point(size = 0.6, alpha = pt_alpha, color = mcol) +
      geom_smooth(method = "loess", se = FALSE, color = mcol,
                  linewidth = 0.5, linetype = "dashed", formula = y ~ x) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40",
                 linewidth = 0.5) +
      scale_x_continuous(limits = x_lim) +
      scale_y_continuous(limits = eff_ylim) +
      labs(x = "Sample size", y = "Efficiency") +
      th_cell
    ggsave(file.path(out_dir, sprintf("F%d%s_efficiency.png", i, s)),
           p_e, width = 4, height = 3.5, dpi = 200)
    
    # ── Bias vs sample size ───────────────────────────────────────────────
    p_b <- ggplot(d, aes(x = samp_size, y = bias)) +
      geom_point(size = 0.6, alpha = pt_alpha, color = mcol) +
      geom_smooth(method = "loess", se = FALSE, color = mcol,
                  linewidth = 0.5, linetype = "dashed", formula = y ~ x) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40",
                 linewidth = 0.5) +
      scale_x_continuous(limits = x_lim) +
      scale_y_continuous(limits = bias_ylim) +
      labs(x = "Sample size", y = "Bias") +
      th_cell
    ggsave(file.path(out_dir, sprintf("F%d%s_bias.png", i, s)),
           p_b, width = 4, height = 3.5, dpi = 200)
  }
}

cat("Per-cell plots written.\n")


# ──── Diagnostic plots (A4 grids) ─────────────────────────────────────────────
# Layout: rows = dependent variable (F1–F5), cols = [QQ random effects, Res vs fitted]
# Outputs:
#   diag_{s}.png          — one grid per model (col = panel type, row = factor)
#   diag_combined.png     — all models overlaid with color, same layout

# helper: extract diagnostic data for one fit
extract_diag <- function(i, s) {
  path <- file.path(fit_dir, sprintf("best_fit_%d%s.rds", i, s))
  if (!file.exists(path)) return(NULL)
  fit      <- readRDS(path)
  direct   <- as.numeric(fit$ind$Direct)
  fh       <- as.numeric(fit$ind$FH)
  psi      <- as.numeric(fit$MSE$Direct)
  gamma    <- as.numeric(fit$model$gamma$Gamma)
  sigma2_u <- as.numeric(fit$model$variance)
  std_res  <- (direct - fh) / sqrt(pmax(psi, .Machine$double.eps))
  denom    <- pmax(1 - gamma, 1e-8)
  u_hat    <- gamma / denom * (direct - fh)
  std_ran  <- u_hat / sqrt(max(sigma2_u, .Machine$double.eps))
  data.frame(
    factor_id    = as.character(i),
    factor_label = factor(factor_labels[as.character(i)], levels = factor_labels),
    spec         = s,
    model        = model_labels[s],
    fh           = fh,
    std_res      = std_res,
    std_ran      = std_ran,
    stringsAsFactors = FALSE
  )
}

diag_all <- do.call(rbind, lapply(1:5, function(i)
  do.call(rbind, lapply(specs, function(s) extract_diag(i, s)))
))
diag_all$model_f <- factor(diag_all$model, levels = model_labels[specs])

th_diag <- theme_minimal(base_size = 8) +
  theme(
    axis.line    = element_line(color = "grey60", linewidth = 0.3),
    strip.text.x = element_text(size = 7, face = "bold"),
    strip.text.y = element_text(size = 6.5, angle = 0),
    panel.spacing = unit(0.35, "lines"),
    plot.title   = element_text(size = 9, face = "bold"),
    axis.text    = element_text(size = 6),
    axis.title   = element_text(size = 7)
  )

cat("Generating diagnostic grid plots...\n")

# ── per-model grids ───────────────────────────────────────────────────────
for (s in specs) {
  dd   <- diag_all[diag_all$spec == s, ]
  mcol <- model_colors[[s]]
  
  # QQ random effects: facet_wrap rows = factor
  # use stat_qq inside facet_wrap — need long format with quantiles per group
  qq_data <- do.call(rbind, lapply(levels(dd$factor_label), function(fl) {
    x <- dd$std_ran[dd$factor_label == fl]
    x <- x[is.finite(x)]
    n <- length(x)
    probs <- ppoints(n)
    data.frame(
      factor_label = factor(fl, levels = factor_labels),
      theoretical  = qnorm(probs),
      sample       = sort(x)
    )
  }))
  
  # pooled QQ data across all models (used as combined reference)
  p_qq <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
    geom_point(size = 0.5, alpha = pt_alpha, color = mcol) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "grey40", linewidth = 0.3) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(18))) +
    labs(x = "Theoretical", y = "Sample quantile") +
    th_diag
  
  res_data <- dd[is.finite(dd$std_res) & is.finite(dd$fh), ]
  p_res <- ggplot(res_data, aes(x = fh, y = std_res)) +
    geom_point(size = 0.5, alpha = pt_alpha, color = mcol) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.3) +
    geom_smooth(method = "loess", se = FALSE, color = mcol,
                linewidth = 0.5, linetype = "dashed", formula = y ~ x) +
    facet_wrap(~ factor_label, ncol = 1, scales = "free_x",
               labeller = labeller(factor_label = label_wrap_gen(18))) +
    labs(x = "FH (EBLUP)", y = "Std. residual") +
    th_diag
  
  p_out <- p_qq | p_res
  ggsave(file.path(out_dir, sprintf("diag_%s.png", s)),
         p_out, width = 6.30, height = 9.72, dpi = 300)
  cat(sprintf("  wrote diag_%s.png\n", s))
}

# ── combined grid: all models overlaid ───────────────────────────────────
mc_named <- setNames(unname(model_colors[specs]), model_labels[specs])

qq_comb <- do.call(rbind, lapply(specs, function(s) {
  do.call(rbind, lapply(levels(diag_all$factor_label), function(fl) {
    x <- diag_all$std_ran[diag_all$spec == s & diag_all$factor_label == fl]
    x <- x[is.finite(x)]
    n <- length(x)
    probs <- ppoints(n)
    data.frame(
      factor_label = factor(fl, levels = factor_labels),
      model_f      = factor(model_labels[s], levels = model_labels[specs]),
      theoretical  = qnorm(probs),
      sample       = sort(x)
    )
  }))
}))

# pooled QQ across all models for combined plot
p_qq_comb <- ggplot(qq_comb, aes(x = theoretical, y = sample, color = model_f)) +
  geom_point(size = 0.45, alpha = pt_alpha) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey40", linewidth = 0.3) +
  scale_color_manual(values = mc_named, name = NULL) +
  facet_wrap(~ factor_label, ncol = 1,
             labeller = labeller(factor_label = label_wrap_gen(18))) +
  labs(x = "Theoretical", y = "Sample quantile") +
  th_diag

res_comb <- diag_all[is.finite(diag_all$std_res) & is.finite(diag_all$fh), ]
p_res_comb <- ggplot(res_comb, aes(x = fh, y = std_res, color = model_f)) +
  geom_point(size = 0.45, alpha = pt_alpha) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.3) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.5,
              linetype = "dashed", formula = y ~ x) +
  scale_color_manual(values = mc_named, name = NULL) +
  facet_wrap(~ factor_label, ncol = 1, scales = "free_x",
             labeller = labeller(factor_label = label_wrap_gen(18))) +
  labs(x = "FH (EBLUP)", y = "Std. residual") +
  th_diag +
  theme(strip.text = element_blank())

p_comb_out <- (p_qq_comb | p_res_comb) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.text = element_text(size = 6))
ggsave(file.path(out_dir, "combined_diag.png"),
       p_comb_out, width = 6.30, height = 9.72, dpi = 300)
cat("  wrote combined_diag.png\n")
cat("Diagnostic plots written.\n")

# ── per-model RMSE + bias combined (A4 portrait, rows = factor) ────────────
for (s in specs) {
  mcol <- model_colors[[s]]
  dd   <- all_data[all_data$spec == s, ]
  dd$factor_label <- factor(factor_labels[dd$factor_id], levels = factor_labels)
  
  p_rmse_m <- ggplot(dd, aes(x = samp_size)) +
    geom_point(aes(y = rmse_direct), color = direct_color,
               size = 0.5, alpha = pt_alpha) +
    geom_smooth(aes(y = rmse_direct), method = "loess", se = FALSE,
                color = direct_color, linewidth = 0.5, linetype = "dashed",
                formula = y ~ x) +
    geom_point(aes(y = rmse_fh), color = mcol, size = 0.5, alpha = pt_alpha) +
    geom_smooth(aes(y = rmse_fh), method = "loess", se = FALSE,
                color = mcol, linewidth = 0.5, linetype = "dashed",
                formula = y ~ x) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = rmse_ylim) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Sample size", y = "RMSE") +
    th_diag
  
  p_bias_m <- ggplot(dd, aes(x = samp_size, y = bias)) +
    geom_point(size = 0.5, alpha = pt_alpha, color = mcol) +
    geom_smooth(method = "loess", se = FALSE, color = mcol,
                linewidth = 0.5, linetype = "dashed", formula = y ~ x) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.3) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = bias_ylim) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Sample size", y = "Bias") +
    th_diag +
    theme(strip.text = element_blank())
  
  p_rb <- p_rmse_m | p_bias_m
  ggsave(file.path(out_dir, sprintf("rmse_bias_%s.png", s)),
         p_rb, width = 6.30, height = 9.00, dpi = 300)
  cat(sprintf("  wrote rmse_bias_%s.png\n", s))
}

# ── combined RMSE + bias: all models overlaid ─────────────────────────────
{
  all_data$factor_label <- factor(factor_labels[all_data$factor_id],
                                  levels = factor_labels)
  all_data$model_f <- factor(all_data$model, levels = model_labels[specs])
  mc_named_rb <- setNames(unname(model_colors[specs]), model_labels[specs])
  
  p_rmse_comb <- ggplot(all_data, aes(x = samp_size)) +
    geom_point(aes(y = rmse_direct), color = direct_color,
               size = 0.45, alpha = pt_alpha) +
    geom_smooth(aes(y = rmse_direct), method = "loess", se = FALSE,
                color = direct_color, linewidth = 0.4, linetype = "dashed",
                formula = y ~ x) +
    geom_point(aes(y = rmse_fh, color = model_f), size = 0.45, alpha = pt_alpha) +
    geom_smooth(aes(y = rmse_fh, color = model_f), method = "loess", se = FALSE,
                linewidth = 0.4, linetype = "dashed", formula = y ~ x) +
    scale_color_manual(values = mc_named_rb, name = NULL) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = rmse_ylim) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Sample size", y = "RMSE") +
    th_diag
  
  p_bias_comb <- ggplot(all_data, aes(x = samp_size, y = bias,
                                      color = model_f)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.4,
                linetype = "dashed", formula = y ~ x) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.3) +
    scale_color_manual(values = mc_named_rb, name = NULL) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = bias_ylim) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Sample size", y = "Bias") +
    th_diag +
    theme(strip.text = element_blank())
  
  p_rb_comb <- (p_rmse_comb | p_bias_comb) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.text = element_text(size = 6))
  ggsave(file.path(out_dir, "combined_rmse_bias.png"),
         p_rb_comb, width = 6.30, height = 9.00, dpi = 300)
  cat("  wrote combined_rmse_bias.png\n")
}
cat("RMSE+bias plots written.\n")

# ── per-model scatter + efficiency combined ────────────────────────────────
for (s in specs) {
  mcol <- model_colors[[s]]
  dd   <- all_data[all_data$spec == s, ]
  dd$factor_label <- factor(factor_labels[dd$factor_id], levels = factor_labels)
  
  p_scat_m <- ggplot(dd, aes(x = Direct_est, y = FH_est)) +
    geom_point(size = 0.5, alpha = pt_alpha, color = mcol) +
    geom_smooth(method = "loess", se = FALSE, color = mcol,
                linewidth = 0.5, linetype = "dashed", formula = y ~ x) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "grey40", linewidth = 0.4) +
    scale_x_continuous(limits = est_range) +
    scale_y_continuous(limits = est_range) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Direct estimate", y = "FH (EBLUP)") +
    th_diag
  
  p_eff_m <- ggplot(dd, aes(x = samp_size, y = efficiency)) +
    geom_point(size = 0.5, alpha = pt_alpha, color = mcol) +
    geom_smooth(method = "loess", se = FALSE, color = mcol,
                linewidth = 0.5, linetype = "dashed", formula = y ~ x) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.3) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = eff_ylim) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Sample size", y = "Efficiency") +
    th_diag
  
  p_scat_m <- p_scat_m + theme(strip.text = element_blank())
  
  p_se <- p_eff_m | p_scat_m
  ggsave(file.path(out_dir, sprintf("scatter_eff_%s.png", s)),
         p_se, width = 6.30, height = 9.00, dpi = 300)
  cat(sprintf("  wrote scatter_eff_%s.png\n", s))
}

# ── combined scatter + efficiency: all models overlaid ────────────────────
{
  all_data$factor_label <- factor(factor_labels[all_data$factor_id],
                                  levels = factor_labels)
  all_data$model_f <- factor(all_data$model, levels = model_labels[specs])
  mc_named_se <- setNames(unname(model_colors[specs]), model_labels[specs])
  
  p_scat_comb <- ggplot(all_data, aes(x = Direct_est, y = FH_est,
                                      color = model_f)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.4,
                linetype = "dashed", formula = y ~ x) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "grey40", linewidth = 0.4) +
    scale_color_manual(values = mc_named_se, name = NULL) +
    scale_x_continuous(limits = est_range) +
    scale_y_continuous(limits = est_range) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Direct estimate", y = "FH (EBLUP)") +
    th_diag
  
  p_eff_comb <- ggplot(all_data, aes(x = samp_size, y = efficiency,
                                     color = model_f)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.4,
                linetype = "dashed", formula = y ~ x) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.3) +
    scale_color_manual(values = mc_named_se, name = NULL) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = eff_ylim) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Sample size", y = "Efficiency") +
    th_diag
  
  p_scat_comb <- p_scat_comb + theme(strip.text = element_blank())
  
  p_se_comb <- (p_eff_comb | p_scat_comb) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.text = element_text(size = 6))
  ggsave(file.path(out_dir, "combined_scatter_eff.png"),
         p_se_comb, width = 6.30, height = 9.00, dpi = 300)
  cat("  wrote combined_scatter_eff.png\n")
}
cat("Scatter+efficiency plots written.\n")

# ── grid plots (A4 portrait): rows = factor, cols = model ─────────────────
{
  all_data$factor_label <- factor(
    factor_labels[all_data$factor_id],
    levels = factor_labels
  )
  all_data$model_f <- factor(all_data$model, levels = model_labels[specs])
  
  mc_named <- setNames(unname(model_colors[specs]), model_labels[specs])
  th_grid  <- theme_minimal(base_size = 8) +
    theme(
      axis.line       = element_line(color = "grey60", linewidth = 0.3),
      legend.position = "none",
      strip.text.x    = element_text(size = 7, face = "bold"),
      strip.text.y    = element_text(size = 6.5, angle = 0),
      panel.spacing   = unit(0.3, "lines"),
      plot.title      = element_text(size = 9, face = "bold"),
      axis.text       = element_text(size = 6),
      axis.title      = element_text(size = 7)
    )
  
  # ── grid RMSE ────────────────────────────────────────────────────────
  p_grid_rmse <- ggplot(all_data, aes(x = samp_size)) +
    geom_point(aes(y = rmse_direct), color = direct_color,
               size = 0.45, alpha = pt_alpha) +
    geom_smooth(aes(y = rmse_direct), method = "loess", se = FALSE,
                color = direct_color, linewidth = 0.4, linetype = "dashed",
                formula = y ~ x) +
    geom_point(aes(y = rmse_fh, color = model_f),
               size = 0.45, alpha = pt_alpha) +
    geom_smooth(aes(y = rmse_fh, color = model_f), method = "loess", se = FALSE,
                linewidth = 0.4, linetype = "dashed", formula = y ~ x) +
    scale_color_manual(values = mc_named) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = rmse_ylim) +
    facet_grid(factor_label ~ model_f,
               labeller = labeller(factor_label = label_wrap_gen(12))) +
    labs(x = "Sample size", y = "RMSE") +
    th_grid
  
  ggsave(file.path(out_dir, "combined_grid_rmse.png"),
         p_grid_rmse, width = 6.30, height = 9.72, dpi = 300)
  cat("Grid RMSE written.\n")
  
  # ── grid scatter ──────────────────────────────────────────────────────
  rho_ann <- do.call(rbind, lapply(1:5, function(i) {
    do.call(rbind, lapply(specs, function(s) {
      d <- all_data[all_data$factor_id == i & all_data$spec == s, ]
      rho <- cor(d$Direct_est, d$FH_est, method = "spearman")
      data.frame(
        factor_label     = factor(factor_labels[as.character(i)], levels = factor_labels),
        model_f          = factor(model_labels[s], levels = model_labels[specs]),
        label            = sprintf("\u03c1=%.2f", rho),
        x                = est_range[1],
        y                = est_range[2],
        stringsAsFactors = FALSE
      )
    }))
  }))
  
  p_grid_scat <- ggplot(all_data, aes(x = Direct_est, y = FH_est, color = model_f)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.4,
                linetype = "dashed", formula = y ~ x) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "grey40", linewidth = 0.3) +
    geom_text(data = rho_ann, aes(x = x, y = y, label = label),
              hjust = 0, vjust = 1, size = 2, color = "grey20",
              inherit.aes = FALSE) +
    scale_color_manual(values = mc_named) +
    scale_x_continuous(limits = est_range) +
    scale_y_continuous(limits = est_range) +
    facet_grid(factor_label ~ model_f,
               labeller = labeller(factor_label = label_wrap_gen(12))) +
    labs(x = "Direct estimate", y = "FH (EBLUP)") +
    th_grid
  
  ggsave(file.path(out_dir, "combined_grid_scatter.png"),
         p_grid_scat, width = 6.30, height = 9.72, dpi = 300)
  cat("Grid scatter written.\n")
  
  # ── grid efficiency ───────────────────────────────────────────────────
  p_grid_eff <- ggplot(all_data, aes(x = samp_size, y = efficiency,
                                     color = model_f)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.4,
                linetype = "dashed", formula = y ~ x) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.3) +
    scale_color_manual(values = mc_named) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = eff_ylim) +
    facet_grid(factor_label ~ model_f,
               labeller = labeller(factor_label = label_wrap_gen(12))) +
    labs(x = "Sample size", y = "Efficiency") +
    th_grid
  
  ggsave(file.path(out_dir, "combined_grid_efficiency.png"),
         p_grid_eff, width = 6.30, height = 9.72, dpi = 300)
  cat("Grid efficiency written.\n")
  
  # ── grid bias ─────────────────────────────────────────────────────────
  p_grid_bias <- ggplot(all_data, aes(x = samp_size, y = bias,
                                      color = model_f)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.4,
                linetype = "dashed", formula = y ~ x) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.3) +
    scale_color_manual(values = mc_named) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = bias_ylim) +
    facet_grid(factor_label ~ model_f,
               labeller = labeller(factor_label = label_wrap_gen(12))) +
    labs(x = "Sample size", y = "Bias") +
    th_grid
  ggsave(file.path(out_dir, "combined_grid_bias.png"),
         p_grid_bias, width = 6.30, height = 9.72, dpi = 300)
  cat("Grid bias written.\n")
  
  # ── collapsed plots: all models in one panel per factor (→ 5 rows, no model col)
  th_coll <- th_grid + theme(legend.position = "bottom",
                             legend.text = element_text(size = 6))
  
  # RMSE collapsed
  rmse_coll <- do.call(rbind, lapply(1:5, function(i) {
    fi <- all_data[all_data$factor_id == i, ]
    dp <- fi[fi$spec == specs[1], c("samp_size","rmse_direct","factor_label")]
    fh <- fi[, c("samp_size","rmse_fh","model_f","factor_label")]
    rbind(
      data.frame(samp_size = dp$samp_size, rmse = dp$rmse_direct,
                 series = "Direct", factor_label = dp$factor_label,
                 model_f = NA),
      data.frame(samp_size = fh$samp_size, rmse = fh$rmse_fh,
                 series = as.character(fh$model_f),
                 factor_label = fh$factor_label, model_f = as.character(fh$model_f))
    )
  }))
  rmse_coll$series <- factor(rmse_coll$series,
                             levels = c("Direct", model_labels[specs]))
  rmse_coll_col <- c(Direct = direct_color, mc_named)
  
  p_coll_rmse <- ggplot(rmse_coll[!is.na(rmse_coll$model_f), ],
                        aes(x = samp_size, y = rmse, color = series)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.5,
                linetype = "dashed", formula = y ~ x) +
    geom_point(data = rmse_coll[is.na(rmse_coll$model_f), ],
               aes(x = samp_size, y = rmse), color = direct_color,
               size = 0.45, alpha = pt_alpha, inherit.aes = FALSE) +
    geom_smooth(data = rmse_coll[is.na(rmse_coll$model_f), ],
                aes(x = samp_size, y = rmse), method = "loess", se = FALSE,
                color = direct_color, linewidth = 0.5, linetype = "dashed",
                formula = y ~ x, inherit.aes = FALSE) +
    scale_color_manual(values = rmse_coll_col, name = NULL) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = rmse_ylim) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Sample size", y = "RMSE") +
    th_coll
  ggsave(file.path(out_dir, "combined_collapsed_rmse.png"),
         p_coll_rmse, width = 3.15, height = 9.72, dpi = 300)
  cat("Collapsed RMSE written.\n")
  
  # Scatter collapsed
  rho_ann_coll <- do.call(rbind, lapply(1:5, function(i) {
    do.call(rbind, lapply(specs, function(s) {
      d <- all_data[all_data$factor_id == i & all_data$spec == s, ]
      data.frame(
        factor_label = factor(factor_labels[as.character(i)], levels = factor_labels),
        model_f      = factor(model_labels[s], levels = model_labels[specs]),
        label        = sprintf("ρ=%.2f", cor(d$Direct_est, d$FH_est,
                                             method = "spearman")),
        x            = est_range[1], y = est_range[2] - diff(est_range) *
          0.08 * (match(s, specs) - 1),
        stringsAsFactors = FALSE
      )
    }))
  }))
  
  p_coll_scat <- ggplot(all_data, aes(x = Direct_est, y = FH_est,
                                      color = model_f)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.5,
                linetype = "dashed", formula = y ~ x) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "grey40", linewidth = 0.3) +
    geom_text(data = rho_ann_coll, aes(x = x, y = y, label = label,
                                       color = model_f),
              hjust = 0, vjust = 1, size = 1.8, inherit.aes = FALSE) +
    scale_color_manual(values = mc_named, name = NULL) +
    scale_x_continuous(limits = est_range) +
    scale_y_continuous(limits = est_range) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Direct estimate", y = "FH (EBLUP)") +
    th_coll
  ggsave(file.path(out_dir, "combined_collapsed_scatter.png"),
         p_coll_scat, width = 3.15, height = 9.72, dpi = 300)
  cat("Collapsed scatter written.\n")
  
  # Efficiency collapsed
  p_coll_eff <- ggplot(all_data, aes(x = samp_size, y = efficiency,
                                     color = model_f)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.5,
                linetype = "dashed", formula = y ~ x) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.3) +
    scale_color_manual(values = mc_named, name = NULL) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = eff_ylim) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Sample size", y = "Efficiency") +
    th_coll
  ggsave(file.path(out_dir, "combined_collapsed_efficiency.png"),
         p_coll_eff, width = 3.15, height = 9.72, dpi = 300)
  cat("Collapsed efficiency written.\n")
  
  # Bias collapsed
  p_coll_bias <- ggplot(all_data, aes(x = samp_size, y = bias,
                                      color = model_f)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.5,
                linetype = "dashed", formula = y ~ x) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.3) +
    scale_color_manual(values = mc_named, name = NULL) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = bias_ylim) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Sample size", y = "Bias") +
    th_coll
  ggsave(file.path(out_dir, "combined_collapsed_bias.png"),
         p_coll_bias, width = 3.15, height = 9.72, dpi = 300)
  cat("Collapsed bias written.\n")
  
  # ── grid QQ (rows = factor, cols = model) ──────────────────────────────
  p_grid_qq <- ggplot(qq_comb, aes(x = theoretical, y = sample,
                                   color = model_f)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "grey40", linewidth = 0.3) +
    scale_color_manual(values = mc_named, guide = "none") +
    facet_grid(factor_label ~ model_f,
               labeller = labeller(factor_label = label_wrap_gen(12))) +
    labs(x = "Theoretical quantile", y = "Sample quantile") +
    th_grid
  ggsave(file.path(out_dir, "combined_grid_diag_qq.png"),
         p_grid_qq, width = 6.30, height = 9.72, dpi = 300)
  cat("Grid diag QQ written.\n")
  
  # ── grid residuals (rows = factor, cols = model) ──────────────────────
  p_grid_res <- ggplot(res_comb, aes(x = fh, y = std_res,
                                     color = model_f)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.3) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.4,
                linetype = "dashed", formula = y ~ x) +
    scale_color_manual(values = mc_named, guide = "none") +
    facet_grid(factor_label ~ model_f, scales = "free_x",
               labeller = labeller(factor_label = label_wrap_gen(12))) +
    labs(x = "FH (EBLUP)", y = "Std. residual") +
    th_grid
  ggsave(file.path(out_dir, "combined_grid_diag_res.png"),
         p_grid_res, width = 6.30, height = 9.72, dpi = 300)
  cat("Grid diag residuals written.\n")
  
  # ── collapsed QQ ─────────────────────────────────────────────────────
  p_coll_qq <- ggplot(qq_comb, aes(x = theoretical, y = sample,
                                   color = model_f)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "grey40", linewidth = 0.3) +
    scale_color_manual(values = mc_named, name = NULL) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Theoretical quantile", y = "Sample quantile") +
    th_coll
  ggsave(file.path(out_dir, "combined_collapsed_diag_qq.png"),
         p_coll_qq, width = 3.15, height = 9.72, dpi = 300)
  cat("Collapsed diag QQ written.\n")
  
  # ── collapsed residuals ───────────────────────────────────────────────
  p_coll_res <- ggplot(res_comb, aes(x = fh, y = std_res,
                                     color = model_f)) +
    geom_point(size = 0.45, alpha = pt_alpha) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.3) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.5,
                linetype = "dashed", formula = y ~ x) +
    scale_color_manual(values = mc_named, name = NULL) +
    facet_wrap(~ factor_label, ncol = 1, scales = "free_x",
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "FH (EBLUP)", y = "Std. residual") +
    th_coll
  ggsave(file.path(out_dir, "combined_collapsed_diag_res.png"),
         p_coll_res, width = 3.15, height = 9.72, dpi = 300)
  cat("Collapsed diag residuals written.\n")
}

# ── CV (direct) vs sample size ────────────────────────────────────────────
# CV is a property of the direct estimate only (independent of model),
# so we deduplicate to one row per domain × factor.
{
  cv_data <- all_data[all_data$spec == specs[1], ]
  cv_data$factor_label <- factor(factor_labels[cv_data$factor_id],
                                 levels = factor_labels)
  
  p_cv <- ggplot(cv_data, aes(x = samp_size, y = cv_direct)) +
    geom_point(size = 0.5, alpha = pt_alpha, color = direct_color) +
    geom_smooth(method = "loess", se = FALSE, color = direct_color,
                linewidth = 0.5, linetype = "dashed", formula = y ~ x) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = cv_ylim) +
    facet_wrap(~ factor_label, ncol = 1,
               labeller = labeller(factor_label = label_wrap_gen(20))) +
    labs(x = "Sample size", y = "CV (direct)") +
    th_coll
  ggsave(file.path(out_dir, "cv_direct_by_samplesize.png"),
         p_cv, width = 3.15, height = 9.72, dpi = 300)
  cat("CV vs sample size plot written.\n")
  
  # ── per-factor separate CV plots ────────────────────────────────────────
  for (i in 1:5) {
    d_cv <- cv_data[cv_data$factor_id == i, ]
    ftitle <- factor_labels[as.character(i)]
    p_cv_i <- ggplot(d_cv, aes(x = samp_size, y = cv_direct)) +
      geom_point(size = 0.6, alpha = pt_alpha, color = direct_color) +
      geom_smooth(method = "loess", se = FALSE, color = direct_color,
                  linewidth = 0.5, linetype = "dashed", formula = y ~ x) +
      scale_x_continuous(limits = x_lim) +
      scale_y_continuous(limits = cv_ylim) +
      labs(x = "Sample size", y = "CV (direct)",
           title = sprintf("F%d: %s", i, ftitle)) +
      th_cell
    ggsave(file.path(out_dir, sprintf("F%d_cv_direct.png", i)),
           p_cv_i, width = 4, height = 3.5, dpi = 200)
  }
  cat("Per-factor CV plots written.\n")
}

# ── Distribution plots: Direct_est / RMSE_direct / CV_direct ─────────────
# Uses one row per domain × factor (deduplicated, spec-independent for directs)
{
  dist_data <- all_data[all_data$spec == specs[1], ]
  dist_data$factor_label <- factor(factor_labels[dist_data$factor_id],
                                   levels = factor_labels)
  
  # colour palette: one per factor
  fac_colors <- setNames(
    c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e"),
    factor_labels
  )
  
  th_dist <- theme_minimal(base_size = 9) +
    theme(axis.line = element_line(color = "grey60", linewidth = 0.3),
          legend.position = "bottom",
          legend.text = element_text(size = 7),
          panel.spacing = unit(0.3, "lines"))
  
  vars <- list(
    list(col = "Direct_est",  xlab = "Direct estimate",  file = "dist_direct_est"),
    list(col = "rmse_direct", xlab = "RMSE (direct)",    file = "dist_rmse_direct"),
    list(col = "cv_direct",   xlab = "CV (direct)",      file = "dist_cv_direct")
  )
  
  for (v in vars) {
    # ── unconditional density overlay ──────────────────────────────────
    p_dens <- ggplot(dist_data, aes_string(x = v$col, color = "factor_label",
                                           fill = "factor_label")) +
      geom_density(alpha = 0.10, linewidth = 0.5) +
      scale_color_manual(values = fac_colors, name = NULL) +
      scale_fill_manual(values  = fac_colors, name = NULL) +
      labs(x = v$xlab, y = "Density",
           title = paste0("Distribution of ", v$xlab, " (all 233 areas)")) +
      th_dist
    ggsave(file.path(out_dir, paste0(v$file, "_density.png")),
           p_dens, width = 5.5, height = 3.5, dpi = 200)
    
    # ── conditional by sample-size quartile ────────────────────────────
    dist_data$ss_quartile <- cut(dist_data$samp_size,
                                 breaks = quantile(dist_data$samp_size, probs = 0:4/4, na.rm = TRUE),
                                 include.lowest = TRUE,
                                 labels = c("Q1 (small)", "Q2", "Q3", "Q4 (large)"))
    
    p_box <- ggplot(dist_data,
                    aes_string(x = "factor_label", y = v$col,
                               fill = "factor_label")) +
      geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.4, linewidth = 0.35) +
      scale_fill_manual(values = fac_colors, guide = "none") +
      facet_wrap(~ ss_quartile, nrow = 1) +
      labs(x = NULL, y = v$xlab,
           title = paste0(v$xlab, " by sample-size quartile")) +
      th_dist +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 6))
    ggsave(file.path(out_dir, paste0(v$file, "_by_ss_quartile.png")),
           p_box, width = 8, height = 3.8, dpi = 200)
  }
  cat("Distribution plots written.\n")
}

# ── Shrinkage (gamma) vs sample size ─────────────────────────────────────────

if (file.exists(gamma_csv)) {
  gamma_df  <- read.csv(gamma_csv, stringsAsFactors = FALSE)
  mdata_raw <- readRDS(mdata_csv)
  if (inherits(mdata_raw, "sf")) mdata_raw <- sf::st_drop_geometry(mdata_raw)
  mdata_raw <- as.data.frame(mdata_raw)
  samp_candidates <- c("SampSize", "SampleSize", "sample_size", "n", "N")
  samp_col <- samp_candidates[samp_candidates %in% names(mdata_raw)][1]
  if (!is.na(samp_col) && "region" %in% names(mdata_raw)) {
    ssize_df <- data.frame(
      domain      = as.character(mdata_raw$region),
      sample_size = mdata_raw[[samp_col]],
      stringsAsFactors = FALSE
    )
    plot_df <- merge(gamma_df, ssize_df, by = "domain", all.x = FALSE)
    if (nrow(plot_df) > 0) {
      spec_colors   <- c(a = "#1b9e77", b = "#d95f02", c = "#7570b3", n = "#e7298a")
      resp_levels   <- sort(unique(plot_df$response_index))
      n_panels      <- length(resp_levels)
      n_cols        <- 2L
      n_rows        <- ceiling(n_panels / n_cols)
      png(file.path(out_dir, "shrinkage_vs_sample_size.png"),
          width = 1800, height = 900 * n_rows, res = 180)
      op <- par(mfrow = c(n_rows, n_cols), mar = c(4.5, 4.5, 3, 1), oma = c(0, 0, 2, 0))
      on.exit({ par(op); dev.off() }, add = TRUE)
      for (ri in resp_levels) {
        pd <- plot_df[plot_df$response_index == ri, ]
        pd <- pd[order(pd$spec_index, pd$sample_size), ]
        plot(pd$sample_size, pd$gamma,
             col = spec_colors[pd$spec_code], pch = 16,
             xlab = "Sample size", ylab = "Gamma (weight on direct estimate)",
             main = unique(pd$response_var))
        grid()
        present <- names(spec_colors)[names(spec_colors) %in% pd$spec_code]
        legend("bottomright", legend = present, col = spec_colors[present],
               pch = 16, title = "Spec", bty = "n")
      }
      mtext("Shrinkage vs. Sample Size", outer = TRUE, cex = 1.2)
      cat("Shrinkage vs. sample size plot written.\n")
    }
  }
}

cat("\nAll outputs in", out_dir, "\n")


# ---- Export outputs -----------------------------------------------------------

# ---- End ----------------------------------------------------------------------
