# Data directory

This folder is intended to store input data for the project.

## Structure

The expected structure is:

data/
└── raw/
    └── restore4life/
        ├── historical/
        ├── ssp126/
        ├── ssp245/
        └── ssp585/

Each scenario folder should contain subfolders for individual GCMs, for example:

ssp126/
└── cnrm_esm2_1/
    ├── discharge_daily.csv
    ├── Precipitation_areaavg_annualtot.csv
    ├── ETRef_areaavg_annualtot.csv
    └── runoff_areaavg_annualtot.csv

## Notes

- Raw data is not included in this repository.
- File names must match those expected by the scripts.
- Data originates from CWATM model simulations.

## How to use

Place your input data in the correct structure before running:

source("scripts/01_run_pipeline.R")
