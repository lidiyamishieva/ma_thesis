# The use of OpenStreetMap in small area estimation of social cohesion

## Introduction

This repository contains the data and code needed to reproduce the findings of my master's thesis, which examines whether OpenStreetMap data can serve as a new source of auxiliary information for producing small area estimates of social cohesion across European sub-national regions. The study responds to the need for reliable evidence on regional differences in social cohesion for policy-making. While survey data are well suited for measuring social indicators, they often provide too few observations within individual regions to support stable sub-national estimates. Small area estimation (SAE) offers a methodological tool for producing more efficient regional estimates via a model-based approach. OpenStreetMap is examined as a new auxiliary data source because it provides geo-referenced high-resolution information that may help explain regional variation in social cohesion.

In the first step, a multilevel confirmatory factor analysis model is estimated using individual survey data from the European Social Survey (ESS) Round 11. Individual factor scores for each of the five latent social cohesion dimensions are then predicted and aggregated to the sub-national level through direct estimation. In the second step, Fay-Herriot small area models are fitted for each dimension using three auxiliary-data specifications: administrative data only, OpenStreetMap data only, and both sources combined. The models are compared in terms of efficiency gains, agreement with direct estimates, and validation against external evidence.

The results show that OpenStreetMap-based specifications improve the precision of the estimates markedly less than specifications based on administrative data. However, OSM-based models preserve the direct survey signal more closely and avoid some of the stronger regional reorderings introduced by administrative models. The thesis therefore argues that low-explanatory-power models can still be
valuable in SAE when stronger models risk misrepresenting regional patterns.




# TBA

- Short summary of the aim of this repo / Intro
- Folder structure
- Instructions on the Mplus step 
- Rproj file
- Prerequisites for reproducing results
- More details about the scripts
- Ethics
- License
- Permission and access

## Reproducibility instructions

Please keep the repository folder structure unchanged. All scripts should be run from the repository root directory.

## Running the scripts

The scripts in `01_scripts/` require outputs produced by previous scripts. Therefore, they should be run in the following order:

``` text
00_prepare_mplus_input_from_raw_ess_data.R
01_import_and_clean_mplus_output.R
02_import_admin_data.R
03_transform_admin_data.R
04_import_and_combine_osm_data.R
05_aggregate_osm_data.R
06_produce_direct_estimates.R
07_produce_final_data.R
08_fit_small_area_models.R
09_fit_sensitivity.R
10_sae_analysis_dataset.R
11_gof_and_coefficients.R
12_eblup_estimates.R
13_pairwise_analysis.R
14_diagnostic_plots.R
15_eblup_maps.R
16_results_tables.R
17_describe_data_tbls_plots.R
```

Scripts `08_fit_small_area_models.R` and `09_fit_sensitivity.R` use multicore processing on Unix-like systems. If running the project on Windows, the parallel processing setup may need to be adjusted. Details on how to do this are included in the relevant scripts.

## Data files not included in this repository

The following files are **not included** in this repository due to GitHub file size restrictions:

``` text
00_data/derived/osm_data_combined_2023.csv
00_data/derived/osm_data_combined_2023.geojson
00_data/raw/admin/raw_data_estat.Rds
00_data/raw/ess11/ESS11.csv
```

Only one file needs to be downloaded manually from an external source:

``` text
00_data/raw/ess11/ESS11.csv
```

Download the ESS11 integrated file from the European Social Survey website:

<https://ess.sikt.no/en/datafile/242aaa39-3bbb-40f5-98bf-bfb1ce53d8ef>

After downloading, place the file in:

``` text
00_data/raw/ess11/
```

and rename it to:

``` text
ESS11.csv
```

Please cite the dataset as follows:

> European Social Survey European Research Infrastructure (ESS ERIC) (2026).\
> *ESS11 - integrated file, edition 4.1* [Data set].\
> Sikt - Norwegian Agency for Shared Services in Education and Research.\
> <https://doi.org/10.21338/ess11e04_1>

All other files listed above are generated automatically when running the project scripts in the required order.

## R package environment

This project uses `renv` to record the R package environment used for the analysis.

After cloning the repository, install the required R packages by running the following command from the project root:

```r
renv::restore()
```

This will install the package versions recorded in `renv.lock`.
