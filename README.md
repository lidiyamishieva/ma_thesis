# The use of OpenStreetMap in small area estimation of social cohesion

# Master's thesis in Methodology & Statistics

Dear guest,

To support reproducibility, the materials required to reproduce the analysis in my thesis will be made available in this repository on 18 May 2026.

The repository will contain the relevant data, analysis scripts, and output files. For now, only some files are uploaded: the files referred to as Supplementary Materials in the thesis itself.

Detailed instructions for reproducing the results will be provided in this README once the repository is complete.

Have a great day!

Lidiya

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
