# ==============================================================================
# Script:        17_describe_data_tbls_plots.R
# Purpose:       Creates descriptives tables and plots included mainly in the
#                data subsection of the manuscript. Also descriptive summaries
#                of adjusted weights (methods section) & factor scores (results)
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

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(sf)

ess_items <- c(
  "ppltrst", "pplfair", "pplhlp",
  "sclmeet", "inprdsc", "sclact",
  "imbgeco", "imueclt", "imwbcnt",
  "trstprl", "trstlgl", "trstplt", "trstprt",
  "stfgov", "stfdem", "stfedu", "stfhlth"
)

item_labels <- c(
  ppltrst = "Most people can be trusted",
  pplfair = "Most people try to be fair",
  pplhlp = "People mostly try to be helpful",
  sclmeet = "Social meetings",
  inprdsc = "Discuss intimate/personal matters",
  sclact = "Social activities",
  imbgeco = "Immigration good for economy",
  imueclt = "Immigration enriches cultural life",
  imwbcnt = "Immigrants make country better place",
  trstprl = "Trust in parliament",
  trstlgl = "Trust in legal system",
  trstplt = "Trust in politicians",
  trstprt = "Trust in political parties",
  stfgov = "Satisfaction with government",
  stfdem = "Satisfaction with democracy",
  stfedu = "State of education",
  stfhlth = "State of health services"
)

dimension_lookup <- tibble(
  Variable = ess_items,
  Dimension = c(
    rep("Interpersonal trust", 3),
    rep("Density of social relations", 3),
    rep("Openness towards migration", 3),
    rep("Institutional trust", 4),
    rep("Legitimacy of institutions", 4)
  )
)

factor_vars <- c(
  "F1W_scaled",
  "F2W_scaled",
  "F3W_scaled",
  "F4W_scaled",
  "F5W_scaled"
)

factor_labels <- c(
  F1W_scaled = "Interpersonal trust",
  F2W_scaled = "Density of social relations",
  F3W_scaled = "Openness towards migration",
  F4W_scaled = "Institutional trust",
  F5W_scaled = "Legitimacy of institutions"
)

all_admin_variables <- c(
  "POPDENSITY", "FEMALE_RATE",
  "RATE_Y_LT20", "RATE_Y20_39", "RATE_Y40_59", "RATE_Y60_79",
  "SECONDARY_Y25.34", "TERTIARY_Y25.34",
  "SECONDARY_Y25.64", "TERTIARY_Y25.64", 
  "POVERTY_RATE",
  "EMPL_RATE_Y25.34", "EMPL_RATE_Y20.64", 
  "ICCS0401_ROBBERY_RATE", "ICCS0502_THEFT_RATE"
)

admin_lookup <- tibble(
  Variable = all_admin_variables,
  Variable_label = c(
    "Population density (Inh./km²)",
    "Female population share (%)",
    "Population aged under 20 (%)",
    "Population aged 20–39 (%)",
    "Population aged 40–59 (%)",
    "Population aged 60–79 (%)",
    "Population aged 25–34 with secondary education (%)",
    "Population aged 25–34 with tertiary education (%)",
    "Population aged 25–64 with secondary education (%)",
    "Population aged 25–64 with tertiary education (%)",
    "Persons at risk of poverty or social exclusion (%)",
    "Employment rate among persons aged 25–34 (%)",
    "Employment rate among persons aged 20–64 (%)",
    "Robbery rate (count per 10.000 inh.)",
    "Theft rate (count pre 10.000 inh.)"
  )
)


osm_variables <- c(
  "university_count_per_1k_inhab",
  "school_count_per_1k_inhab",
  "place_of_worship_count_per_1k_inhab",
  "kindergarten_count_per_1k_inhab",
  "library_count_per_1k_inhab",
  "hospital_count_per_1k_inhab",
  "nursing_home_count_per_1k_inhab",
  "social_facility_count_per_1k_inhab",
  "marketplace_count_per_1k_inhab",
  "community_centre_count_per_1k_inhab",
  "college_count_per_1k_inhab",
  "clinic_count_per_1k_inhab",
  "doctors_count_per_1k_inhab",
  "pharmacy_count_per_1k_inhab",
  "public_bath_count_per_1k_inhab",
  "shower_count_per_1k_inhab",
  "nightclub_count_per_1k_inhab",
  "social_centre_count_per_1k_inhab",
  "refugee_site_count_per_1k_inhab",
  "give_box_count_per_1k_inhab",
  "internet_cafe_count_per_1k_inhab",
  "kitchen_count_per_1k_inhab"
)

osm_lookup <- tibble(
  Variable = osm_variables,
  Type = c(
    "Education", "Education", "Other", "Education", "Education",
    "Healthcare", "Healthcare", "Healthcare",
    "Other", "Cultural/social gathering", "Education",
    "Healthcare", "Healthcare", "Healthcare",
    "Other", "Facilities", "Cultural/social gathering",
    "Cultural/social gathering", "Other", "Facilities",
    "Other", "Other"
  ),
  Variable_label = c(
    "Universities",
    "Schools",
    "Places of worship",
    "Kindergartens",
    "Libraries",
    "Hospitals",
    "Nursing homes",
    "Social facilities",
    "Marketplaces",
    "Community centres",
    "Colleges",
    "Clinics",
    "Doctors",
    "Pharmacies",
    "Public baths",
    "Showers",
    "Nightclubs",
    "Social centres",
    "Refugee sites",
    "Give boxes",
    "Internet cafés",
    "Kitchens"
  )
)

type_order <- c(
  "Education",
  "Healthcare",
  "Cultural/social gathering",
  "Facilities",
  "Other"
)


osm_labels <- c(
  university_count_per_1k_inhab = "Universities per 1,000 inhabitants",
  school_count_per_1k_inhab = "Schools per 1,000 inhabitants",
  place_of_worship_count_per_1k_inhab = "Places of worship per 1,000 inhabitants",
  kindergarten_count_per_1k_inhab = "Kindergartens per 1,000 inhabitants",
  library_count_per_1k_inhab = "Libraries per 1,000 inhabitants",
  hospital_count_per_1k_inhab = "Hospitals per 1,000 inhabitants",
  nursing_home_count_per_1k_inhab = "Nursing homes per 1,000 inhabitants",
  social_facility_count_per_1k_inhab = "Social facilities per 1,000 inhabitants",
  marketplace_count_per_1k_inhab = "Marketplaces per 1,000 inhabitants",
  community_centre_count_per_1k_inhab = "Community centres per 1,000 inhabitants",
  college_count_per_1k_inhab = "Colleges per 1,000 inhabitants",
  clinic_count_per_1k_inhab = "Clinics per 1,000 inhabitants",
  doctors_count_per_1k_inhab = "Doctors per 1,000 inhabitants",
  pharmacy_count_per_1k_inhab = "Pharmacies per 1,000 inhabitants",
  public_bath_count_per_1k_inhab = "Public baths per 1,000 inhabitants",
  shower_count_per_1k_inhab = "Showers per 1,000 inhabitants",
  nightclub_count_per_1k_inhab = "Nightclubs per 1,000 inhabitants",
  social_centre_count_per_1k_inhab = "Social centres per 1,000 inhabitants",
  refugee_site_count_per_1k_inhab = "Refugee sites per 1,000 inhabitants",
  give_box_count_per_1k_inhab = "Give boxes per 1,000 inhabitants",
  internet_cafe_count_per_1k_inhab = "Internet cafés per 1,000 inhabitants",
  kitchen_count_per_1k_inhab = "Kitchens per 1,000 inhabitants"
)

# ---- Paths --------------------------------------------------------------------

input_dir <- file.path("00_data", "derived")

# ---- Import data --------------------------------------------------------------

weights <- readRDS(file.path(input_dir, "weights.Rds"))

mdata <- readRDS(file.path(input_dir, "mdata.Rds"))

final_dataset_factor_scores <-  read_csv(
  file.path(input_dir, "ess11_analysis_dataset.csv"))

# ---- Data processing ----------------------------------------------------------

## sample size and sampling fraction table -----
sampling_summary <- mdata %>%
  sf::st_drop_geometry() %>%
  dplyr::select(region, SampSize) %>%
  left_join(
    weights %>%
      sf::st_drop_geometry() %>%
      dplyr::select(region, POPULATION_15TO99),
    by = "region"
  ) %>%
  mutate(
    `Sampling fraction` = SampSize / POPULATION_15TO99
  ) %>%
  dplyr::select(
    `Sample size` = SampSize,
    `Sampling fraction`
  ) %>%
  summarise(across(
    everything(),
    list(
      Min = ~min(.x, na.rm = TRUE),
      `1st Q` = ~quantile(.x, 0.25, na.rm = TRUE),
      Median = ~median(.x, na.rm = TRUE),
      Mean = ~mean(.x, na.rm = TRUE),
      `3rd Q` = ~quantile(.x, 0.75, na.rm = TRUE),
      Max = ~max(.x, na.rm = TRUE)
    )
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "^(.*)_(Min|1st Q|Median|Mean|3rd Q|Max)$"
  ) %>%
  pivot_wider(
    names_from = Statistic,
    values_from = value
  )

sampling_summary_fmt <- sampling_summary %>%
  mutate(across(
    -Variable,
    ~ ifelse(
      Variable == "Sample size",
      sprintf("%.0f", .x),
      sprintf("%.5f", .x)
    )
  ))

knitr::kable(sampling_summary_fmt, booktabs = TRUE)

## plot weigthts adjustment -----


weights_long <- weights %>%
  sf::st_drop_geometry() %>%
  mutate(
    `Adjusted weights` = (pop_based_on_dweight_adj - POPULATION_15TO99) / 1000,
    `ESS composite weights` = (pop_based_on_dweight_adj2 - POPULATION_15TO99) / 1000
  ) %>%
  dplyr::select(`Adjusted weights`, `ESS composite weights`) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Weight type",
    values_to = "Deviation"
  )

p_weights <- ggplot(weights_long, aes(x = `Weight type`, y = Deviation)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_boxplot(width = 0.45, linewidth = 0.3, outlier.size = 0.7) +
  scale_y_continuous(
    breaks = seq(
      floor(min(weights_long$Deviation, na.rm = TRUE) / 500) * 500,
      ceiling(max(weights_long$Deviation, na.rm = TRUE) / 500) * 500,
      by = 500
    )
  ) +
  labs(
    x = NULL,
    y = "Deviation from area population (in thousands)"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 8),
    plot.margin = margin(4, 4, 4, 4)
  )

## table summary of individual indicators -----

item_summary <- final_dataset_factor_scores %>%
  as.data.frame() %>%
  dplyr::select(all_of(ess_items)) %>%
  summarise(across(
    everything(),
    list(
      Mean = ~mean(.x, na.rm = TRUE),
      SD = ~sd(.x, na.rm = TRUE),
      Min = ~min(.x, na.rm = TRUE),
      `1st Q` = ~quantile(.x, 0.25, na.rm = TRUE),
      Median = ~median(.x, na.rm = TRUE),
      `3rd Q` = ~quantile(.x, 0.75, na.rm = TRUE),
      Max = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "^(.*)_(Mean|SD|Min|1st Q|Median|3rd Q|Max)$"
  ) %>%
  pivot_wider(
    names_from = Statistic,
    values_from = value
  ) %>%
  left_join(dimension_lookup, by = "Variable") %>%
  mutate(
    Indicator = recode(Variable, !!!item_labels),
    `Mean (SD)` = sprintf("%.2f (%.2f)", Mean, SD)
  ) %>%
  dplyr::select(
    Dimension,
    Indicator,
    `Mean (SD)`,
    Min,
    `1st Q`,
    Median,
    `3rd Q`,
    Max
  ) %>%
  mutate(across(
    c(Min, `1st Q`, Median, `3rd Q`, Max),
    ~sprintf("%.2f", .x)
  ))

# show dimension name only once per block
item_summary_display <- item_summary %>%
  group_by(Dimension) %>%
  mutate(
    Dimension = if_else(row_number() == 1, Dimension, "")
  ) %>%
  ungroup()

knitr::kable(item_summary_display, booktabs = TRUE)

## table summary of factor scores used for direct estimation -----

factor_summary <- final_dataset_factor_scores %>%
  as.data.frame() %>%
  dplyr::select(all_of(factor_vars)) %>%
  summarise(across(
    everything(),
    list(
      Mean = ~mean(.x, na.rm = TRUE),
      SD = ~sd(.x, na.rm = TRUE),
      Min = ~min(.x, na.rm = TRUE),
      `1st Q` = ~quantile(.x, 0.25, na.rm = TRUE),
      Median = ~median(.x, na.rm = TRUE),
      `3rd Q` = ~quantile(.x, 0.75, na.rm = TRUE),
      Max = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "^(.*)_(Mean|SD|Min|1st Q|Median|3rd Q|Max)$"
  ) %>%
  pivot_wider(
    names_from = Statistic,
    values_from = value
  ) %>%
  mutate(
    Dimension = recode(Variable, !!!factor_labels),
    `Mean (SD)` = sprintf("%.2f (%.2f)", Mean, SD)
  ) %>%
  dplyr::select(
    Dimension,
    `Mean (SD)`,
    Min,
    `1st Q`,
    Median,
    `3rd Q`,
    Max
  ) %>%
  mutate(across(
    c(Min, `1st Q`, Median, `3rd Q`, Max),
    ~sprintf("%.2f", .x)
  ))

knitr::kable(factor_summary, booktabs = TRUE)

## summary table of administrative data -----

admin_summary <- mdata %>%
  sf::st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(
    FEMALE_RATE = FEMALE_RATE,
    RATE_Y_LT20 = RATE_Y_LT20,
    RATE_Y20_39 = RATE_Y20_39,
    RATE_Y40_59 = RATE_Y40_59,
    RATE_Y60_79 = RATE_Y60_79,
    ICCS0401_ROBBERY_RATE = ICCS0401_ROBBERY_RATE,
    ICCS0502_THEFT_RATE = ICCS0502_THEFT_RATE
  ) %>%
  dplyr::select(all_of(all_admin_variables)) %>%
  summarise(across(
    everything(),
    list(
      Mean = ~mean(.x, na.rm = TRUE),
      SD = ~sd(.x, na.rm = TRUE),
      Min = ~min(.x, na.rm = TRUE),
      `1st Q` = ~quantile(.x, 0.25, na.rm = TRUE),
      Median = ~median(.x, na.rm = TRUE),
      `3rd Q` = ~quantile(.x, 0.75, na.rm = TRUE),
      Max = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "^(.*)_(Mean|SD|Min|1st Q|Median|3rd Q|Max)$"
  ) %>%
  pivot_wider(
    names_from = Statistic,
    values_from = value
  ) %>%
  left_join(admin_lookup, by = "Variable") %>%
  mutate(
    `Mean (SD)` = sprintf("%.2f (%.2f)", Mean, SD)
  ) %>%
  dplyr::select(
    Variable = Variable_label,
    `Mean (SD)`,
    Min,
    `1st Q`,
    Median,
    `3rd Q`,
    Max
  ) %>%
  mutate(across(
    c(Min, `1st Q`, Median, `3rd Q`, Max),
    ~sprintf("%.2f", .x)
  ))

knitr::kable(admin_summary, booktabs = TRUE)

## summary table of osm variables -----


osm_summary <- mdata %>%
  sf::st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(all_of(osm_variables)) %>%
  summarise(across(
    everything(),
    list(
      Mean = ~mean(.x, na.rm = TRUE),
      SD = ~sd(.x, na.rm = TRUE),
      Min = ~min(.x, na.rm = TRUE),
      `1st Q` = ~quantile(.x, 0.25, na.rm = TRUE),
      Median = ~median(.x, na.rm = TRUE),
      `3rd Q` = ~quantile(.x, 0.75, na.rm = TRUE),
      Max= ~max(.x, na.rm = TRUE),
      `Areas with zero count` = ~sum(.x == 0, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "^(.*)_(Mean|SD|Min|1st Q|Median|3rd Q|Max|Areas with zero count)$"
  ) %>%
  pivot_wider(
    names_from = Statistic,
    values_from = value
  ) %>%
  left_join(osm_lookup, by = "Variable") %>%
  mutate(
    Variable = Variable_label,
    `Mean (SD)` = sprintf("%.3f (%.3f)", Mean, SD),
    `Areas with zero count` = sprintf("%.0f", `Areas with zero count`)
  ) %>%
  dplyr::select(
    Type,
    Variable,
    `Mean (SD)`,
    Min,
    `1st Q`,
    Median,
    `3rd Q`,
    Max,
    `Areas with zero count`
  ) %>%
  mutate(across(
    c(Min, `1st Q`, Median, `3rd Q`, Max),
    ~sprintf("%.3f", .x)
  ))

osm_summary_display <- osm_summary %>%
  mutate(Type = factor(Type, levels = type_order)) %>%
  arrange(Type, Variable) %>%
  group_by(Type) %>%
  mutate(Type = if_else(row_number() == 1, as.character(Type), "")) %>%
  ungroup()

knitr::kable(osm_summary_display, booktabs = TRUE)

## maps for osm variables -----


make_osm_plot <- function(v, data = mdata) {
  
  var_label <- osm_labels[[v]]
  
  plot_df <- data |>
    mutate(country = substr(region, 1, 2))
  
  overall_mean <- mean(plot_df[[v]], na.rm = TRUE)
  
  # BOXPLOT BY COUNTRY
  
  p_box <- ggplot(
    plot_df,
    aes(
      y = reorder(country, .data[[v]], median, na.rm = TRUE),
      x = .data[[v]]
    )
  ) +
    geom_vline(
      xintercept = overall_mean,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    geom_boxplot(outlier.alpha = 0.5, linewidth = 0.3) +
    labs(
      title = "A",
      x = var_label,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # MAP DATA: QUINTILES
  
  x <- plot_df[[v]]
  
  brks <- unique(quantile(
    x,
    probs = seq(0, 1, length.out = 6),
    na.rm = TRUE
  ))
  
  # fallback if too many equal values cause duplicate breaks
  if (length(brks) < 3) {
    
    map_df <- plot_df |>
      mutate(
        q_class = factor(
          dplyr::ntile(.data[[v]], 5),
          levels = 1:5,
          labels = paste0("Q", 1:5)
        )
      )
    
  } else {
    
    labs_q <- paste0(
      round(brks[-length(brks)], 3),
      "–",
      round(brks[-1], 3)
    )
    
    map_df <- plot_df |>
      mutate(
        q_class = cut(
          .data[[v]],
          breaks = brks,
          include.lowest = TRUE,
          labels = labs_q
        )
      )
  }
  
  # MAP
  
  p_map <- ggplot(map_df) +
    geom_sf(
      aes(fill = q_class),
      color = "black",
      linewidth = 0.1
    ) +
    scale_fill_viridis_d(
      option = "C",
      name = "Quantile",
      drop = FALSE,
      na.value = "grey80"
    ) +
    labs(title = "B") +
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
  
  # COMBINE
  
  p_combined <- p_box + p_map +
    plot_layout(widths = c(1.3, 2.7)) +
    plot_annotation(
      title = var_label
    )
  
  return(p_combined)
}

## 2x2 plot for the paper ----

vars <- c(
  "kindergarten_count_per_1k_inhab",
  "give_box_count_per_1k_inhab",
  "doctors_count_per_1k_inhab",
  "university_count_per_1k_inhab"
)

var_labels <- c(
  kindergarten_count_per_1k_inhab = "Kindergartens per 1,000 inhabitants",
  give_box_count_per_1k_inhab = "Give boxes per 1,000 inhabitants",
  doctors_count_per_1k_inhab = "Doctors per 1,000 inhabitants",
  university_count_per_1k_inhab = "Universities per 1,000 inhabitants"
)

panel_labels <- c("A", "B", "C", "D")

make_map_only <- function(data, v, panel_label) {
  
  x <- data[[v]]
  
  brks <- unique(quantile(
    x,
    probs = seq(0, 1, length.out = 6),
    na.rm = TRUE
  ))
  
  # fallback in case quantile breaks are not unique
  if (length(brks) < 3) {
    map_df <- data |>
      mutate(
        q_class = factor(
          dplyr::ntile(.data[[v]], 5),
          levels = 1:5,
          labels = paste0("Q", 1:5)
        )
      )
  } else {
    labs_q <- paste0(
      round(brks[-length(brks)], 3),
      "–",
      round(brks[-1], 3)
    )
    
    map_df <- data |>
      mutate(
        q_class = cut(
          .data[[v]],
          breaks = brks,
          include.lowest = TRUE,
          labels = labs_q
        )
      )
  }
  
  ggplot(map_df) +
    geom_sf(
      aes(fill = q_class),
      color = "black",
      linewidth = 0.1
    ) +
    scale_fill_viridis_d(
      option = "C",
      name = "Quantile",
      drop = FALSE,
      na.value = "grey80"
    ) +
    labs(
      title = panel_label,
      subtitle = var_labels[[v]]
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10),
      legend.position = c(0.18, 0.82),
      legend.background = element_rect(
        fill = scales::alpha("white", 0.7),
        color = NA
      ),
      legend.key.height = unit(0.4, "cm"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9)
    )
}

plots <- Map(
  function(v, lab) make_map_only(mdata, v, lab),
  vars,
  panel_labels
)

p_4panel <- (plots[[1]] + plots[[2]]) /
  (plots[[3]] + plots[[4]])

p_4panel



# ---- Export outputs -----------------------------------------------------------

saveRDS(sampling_summary, 
        file.path("03_output", "tables", "descriptives", "sampling_summary_table.rds"))

ggsave(
  filename = file.path("03_output", "plots", "weight_adjustment_deviation_boxplot.png"),
  plot = p_weights,
  width = 3.7,
  height = 2.8,
  dpi = 300,
  units = "in"
)

saveRDS(item_summary, 
        file.path("03_output", "tables", "descriptives", "data_table_summary_indicator_items.rds"))

saveRDS(factor_summary, 
        file.path("03_output", "tables", "descriptives", "data_table_summary_factor_scores.rds"))

saveRDS(admin_summary, 
        file.path("03_output", "tables", "descriptives", "table_summary_admin.rds"))

saveRDS(osm_summary_display, 
        file.path("03_output", "tables", "descriptives", "table_summary_osm.rds"))

for (v in osm_variables) {
  
  p <- make_osm_plot(v)
  
  file_name <- file.path(
    "03_output", "plots", "osm_predictors_maps",
    paste0(str_remove(v, "_count_per_1k_inhab"), "_map_boxplot.png")
  )
  
  ggsave(
    filename = file_name,
    plot = p,
    width = 12,
    height = 7,
    dpi = 300
  )
}

for (v in osm_variables) {
  
  p <- make_osm_plot(v)
  
  file_name <- file.path(
    "03_output", "plots", "osm_predictors_maps",
    paste0(str_remove(v, "_count_per_1k_inhab"), "_map_boxplot.pdf")
  )
  
  ggsave(
    filename = file_name,
    plot = p,
    width = 12,
    height = 7
  )
}

ggsave(
  file.path("03_output", "plots", "osm_predictors_maps", "osm_maps_4panel.png"),
  plot = p_4panel,
  width = 8,
  height = 10,
  dpi = 300
)


# ---- End ----------------------------------------------------------------------
