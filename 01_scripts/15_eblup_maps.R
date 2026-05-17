# ==============================================================================
# Script:        15_eblup_maps.R
# Purpose:       Produces maps & country-ordered boxplots for all model × factor
#                combinations
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
library(sf)
library(patchwork)


# ── switch: include intercept-only null model (M0) in plots? ──────────────
include_null_model <- TRUE   # set TRUE to add M0 (null/intercept-only)

# ---- Paths --------------------------------------------------------------------

input_dir <- file.path("00_data", "derived")
output_dir <- file.path("03_output", "plots", "eblup_maps")
fit_dir <- file.path("03_output", "model_fits")

# ---- Import data --------------------------------------------------------------

mdata <- readRDS(file.path(input_dir, "mdata.Rds"))

# load fits 
load_fit <- function(i, spec) {
  readRDS(file.path(fit_dir, sprintf("best_fit_%d%s.rds", i, spec)))
}

make_fits_for_factor <- function(i) {
  fl <- list(
    "M1 (admin)"       = load_fit(i, "a"),
    "M2 (osm)"         = load_fit(i, "b"),
    "M3 (admin + osm)" = load_fit(i, "c")
  )
  if (include_null_model) {
    fl <- c(list("M0 (null)" = load_fit(i, "n")), fl)
  }
  fl
}


# ---- Data processing ----------------------------------------------------------

fits_all <- list(
  F1 = make_fits_for_factor(1),
  F2 = make_fits_for_factor(2),
  F3 = make_fits_for_factor(3),
  F4 = make_fits_for_factor(4),
  F5 = make_fits_for_factor(5)
)

factor_labels <- c(
  F1 = "Interpersonal trust",
  F2 = "Social relations",
  F3 = "Openness",
  F4 = "Institutional trust",
  F5 = "Legitimacy of institutions"
)

make_eblup_df <- function(fits_all, mdata, area_var = NULL, factor_labels = NULL) {
  
  out <- lapply(names(fits_all), function(factor_id) {
    
    fits_factor <- fits_all[[factor_id]]
    
    # extract Direct estimates once from the first fit in this factor
    first_fit <- fits_factor[[1]]
    direct_vals <- first_fit$ind$Direct
    area_id_direct <- if (!is.null(area_var)) mdata[[area_var]] else seq_along(direct_vals)
    direct_row <- data.frame(
      Area = area_id_direct,
      Factor = factor_id,
      Factor_label = if (!is.null(factor_labels)) factor_labels[factor_id] else factor_id,
      Model = "Direct",
      EBLUP = as.numeric(direct_vals),
      stringsAsFactors = FALSE
    )
    
    model_rows <- lapply(names(fits_factor), function(model_name) {
      
      fit <- fits_factor[[model_name]]
      eblup <- fit$ind$FH
      
      if (!is.null(area_var)) {
        area_id <- mdata[[area_var]]
      } else {
        area_id <- seq_along(eblup)
      }
      
      data.frame(
        Area = area_id,
        Factor = factor_id,
        Factor_label = if (!is.null(factor_labels)) factor_labels[factor_id] else factor_id,
        Model = model_name,
        EBLUP = as.numeric(eblup),
        stringsAsFactors = FALSE
      )
    })
    
    c(list(direct_row), model_rows)
  })
  
  do.call(rbind, unlist(out, recursive = FALSE))
}


eblup_df <- make_eblup_df(
  fits_all = fits_all,
  mdata = mdata,
  area_var = "region",
  factor_labels = factor_labels
)

mdata_sf <- st_as_sf(mdata)
mdata_sf$Area <- mdata_sf$region


factor_long_labels <- c(
  F1 = "Interpersonal trust",
  F2 = "Density of social relations",
  F3 = "Openness towards migration",
  F4 = "Institutional trust",
  F5 = "Legitimacy of institutions"
)

# which models to plot
plot_models <- c("Direct", "M1 (admin)", "M2 (osm)", "M3 (admin + osm)")
if (include_null_model) plot_models <- c(plot_models, "M0 (null)")

# safe tag for filenames: "M0 (null)" → "M0", "M1 (admin)" → "M1", etc.
model_tag <- function(m) sub("\\s.*", "", m)

for (model_name in plot_models) {
  
  mtag <- model_tag(model_name)
  
  # build wide sf for this model
  eblup_wide <- eblup_df |>
    filter(Model == model_name) |>
    dplyr::select(Area, Factor, EBLUP) |>
    pivot_wider(names_from = Factor, values_from = EBLUP,
                names_prefix = paste0("EBLUP_", mtag, "_"))
  
  eblup_wide_sf <- mdata_sf |>
    dplyr::select(Area) |>
    left_join(eblup_wide, by = "Area")
  
  factor_vars   <- paste0("EBLUP_", mtag, "_", names(factor_long_labels))
  factor_labels <- setNames(factor_long_labels,
                            paste0("EBLUP_", mtag, "_", names(factor_long_labels)))
  
  for (v in factor_vars) {
    
    if (!v %in% names(eblup_wide_sf)) next
    
    #---------------------------
    # BOXPLOT DATA
    #---------------------------
    
    box_df <- eblup_wide_sf |>
      st_drop_geometry() |>
      mutate(country = substr(Area, 1, 2)) |>
      dplyr::select(country, Area, Value = all_of(v)) |>
      filter(!is.na(Value))
    
    # country ordering by median
    country_order <- box_df |>
      group_by(country) |>
      summarise(med = median(Value, na.rm = TRUE), .groups = "drop") |>
      arrange(med) |>
      pull(country)
    
    box_df <- box_df |>
      mutate(country = factor(country, levels = country_order))
    
    # rank for gradient colouring
    country_rank <- box_df |>
      group_by(country) |>
      summarise(med = median(Value, na.rm = TRUE), .groups = "drop") |>
      arrange(med) |>
      mutate(rank = row_number())
    
    box_df <- box_df |>
      left_join(country_rank, by = "country")
    
    # overall median
    overall_median <- median(box_df$Value, na.rm = TRUE)
    
    #---------------------------
    # BOXPLOT
    #---------------------------
    
    # overall mean
    overall_mean <- mean(box_df$Value, na.rm = TRUE)
    
    p_box <- ggplot(
      box_df,
      aes(x = country, y = Value, fill = NA)
    ) +
      geom_boxplot(
        outlier.alpha = 0.5,
        linewidth = 0.4,
        width = 0.55
      ) +
      geom_hline(
        yintercept = overall_mean,
        linetype = "dashed",
        linewidth = 0.6,
        color = "black"
      ) +
      coord_flip() +
      scale_y_continuous(
        breaks = seq(0, 1, by = 0.1)
      ) +
      labs(
        x = NULL,
        y = "EBLUP estimate",
        title = "A"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold")
      )
    
    #---------------------------
    # MAP DATA (QUANTILES)
    #---------------------------
    
    x <- eblup_wide_sf[[v]]
    
    brks <- unique(quantile(
      x,
      probs = seq(0, 1, length.out = 6),
      na.rm = TRUE
    ))
    
    labs_q <- paste0(
      round(brks[-length(brks)], 3),
      "–",
      round(brks[-1], 3)
    )
    
    map_df <- eblup_wide_sf |>
      mutate(
        q_class = cut(
          .data[[v]],
          breaks = brks,
          include.lowest = TRUE,
          labels = labs_q
        )
      )
    
    #---------------------------
    # MAP
    #---------------------------
    
    p_map <- ggplot(map_df) +
      geom_sf(
        aes(fill = q_class),
        color = "black",
        linewidth = 0.1
      ) +
      scale_fill_viridis_d(
        option = "C",
        name = "Quintile",
        drop = FALSE
      ) +
      labs(
        title = "B"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = c(0.15, 0.9),
        legend.background = element_rect(
          fill = scales::alpha("white", 0.7),
          color = NA
        ),
        legend.key.height = unit(0.5, "cm"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10)
      )
    #---------------------------
    # COMBINE
    #---------------------------
    
    p_combined <- p_box + p_map +
      plot_layout(widths = c(1, 1.5)) +
      plot_annotation(
        title = paste0(factor_labels[[v]], " — ", model_name, "\n"),
        theme = theme(
          plot.title = element_text(face = "bold", size = 12)
        )
      )
    
    print(p_combined)
    
    #---------------------------
    # SAVE
    #---------------------------
    
    ggsave(
      filename = file.path(output_dir, paste0(v, "_boxplot_map.png")),
      plot = p_combined,
      width = 12,
      height = 7,
      dpi = 300
    )
  }   # end factor loop
}     # end model loop


# ---- Export outputs -----------------------------------------------------------

# ---- End ----------------------------------------------------------------------
