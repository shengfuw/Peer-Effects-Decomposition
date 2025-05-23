# Replication Code for 
> **"Decomposing Peer Effects on Academic Achievement: The Mediating Role of Educational Expectations"**  
 
- Author: **Shengfu Wang** (Department of Sociology, National Taiwan University)
- Contact: [r11325008@ntu.edu.tw](emailto:r11325008@ntu.edu.tw)

## 📄 Overview
This repository contains the replication code for my Master’s thesis. It includes:

* Stata and R code for extracting data, cleaning variables, conducting analyses, and generating tables and figures.
* Some output tables and figures.
* Instructions for replication steps and example project file structure (⚠️ real data is not included).



## 📊 Reproducing the Results
Due to data access restrictions, the raw data cannot be shared.  To get the data and replicate results, please follow these steps:

1. Apply for access to the Add Health restricted-use dataset through [Add Health Data Access](https://addhealth.cpc.unc.edu/data/).
2. Run `do/01 initialize.do` to create the whole project structure.
3. Run `do/02 import_data.do` to convert the original SAS datasets to Stata datasets.
5. Place the Stata datasets in the `rawdata/` directory.
6. Run code scripts in `do/` in order (`03` → `07`).
7. Results will be saved to `output/` and figures to `figures/`.

## ⌨️ Code Guide
| File | Description |
|------|-------------|
| `do/01 initialize.do` | Initializes project paths |
| `do/02 import_data.do` | Converts raw Add Health data to Stata datasets|
| `do/03 clean_data.do` | Cleans variables in Stata |
| `do/04 clean_data.R` | Creates peer measures in R |
| `do/05 pull_sample.do` | Prepares analytic sample |
| `do/06 analysis_mediation.R` | Runs four-way decomposition |
| `do/07 generate_figure.R` | Creates figures |
| `do/self_defined_functions.R` | Defines functions for `04.clean_data.R`|

## 📂 File Organization
- `do`: Contains all Stata and R scripts required for replication.
- `log`: Stores execution log files.
- `output`: Holds all resulting figures and tables generated during the analysis.
- `rawdata`: Places the original dataset.
- `workdata`: Contains intermediate datasets generated by the scripts. This folder is initially empty and will be automatically populated as the scripts are executed.

