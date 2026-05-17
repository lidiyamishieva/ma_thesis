# The use of OpenStreetMaps in small area estimation of social cohesion
# Master's thesis in Methodology & Statistics

Dear guest, 

To support reproducibility, the materials required to reproduce the analysis in my thesis will be made available in this repository on 18 May 2026.
The repository will contain the relevant data, analysis scripts, and output files. 
Detailed instructions for reproducing the results will be provided in the repository README file.

For now, only some files are uploaded, the ones referred to as Supplementary Materials in the paper itself.

Have a great day!

Lidiya

## TBA


The following files are **not included** in this repository due to GitHub file size restrictions:

```text
00_data/derived/osm_data_combined_2023.csv
00_data/derived/osm_data_combined_2023.geojson
00_data/raw/admin/raw_data_estat.Rds
00_data/raw/ess11/ESS11.csv
```

Only one file needs to be downloaded manually from an external source:

```text
00_data/raw/ess11/ESS11.csv
```

Download the ESS11 integrated file from the European Social Survey website:

https://ess.sikt.no/en/datafile/242aaa39-3bbb-40f5-98bf-bfb1ce53d8ef

After downloading, place the file in:

```text
00_data/raw/ess11/
```

and rename it to:

```text
ESS11.csv
```

Please cite the dataset as follows:

> European Social Survey European Research Infrastructure (ESS ERIC) (2026).  
> *ESS11 - integrated file, edition 4.1* [Data set].  
> Sikt - Norwegian Agency for Shared Services in Education and Research.  
> https://doi.org/10.21338/ess11e04_1

All other files listed above are generated automatically when running the project scripts in the required order.
