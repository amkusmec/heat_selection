## A genetic tradeoff for tolerance to moderate and severe heat stress in US hybrid maize

### Introduction

This project uses 81 years of data on public maize yield trials in the US Midwest to study changes in hybrid maize heat tolerance for 1934-2014. A [paper](https://doi.org/10.1371/journal.pgen.1010799) describing the results has been published at **PLoS Genetics**.

### Description

Prior to running any of the scripts in the repository, please run `00.install_packages.R` to install all R packages required to reproduce these analyses. The necessary directory structure has been preserved through the use of `.gitignore` files.

For the curated trial data and combined yield and weather datasets, see [this repository](https://doi.org/10.25380/iastate.21965093). Weather data can be reconstructed from public sources using the scripts in this repository.

1. **munge_yield**: These scripts process curated trial data. They also standardize county names and geo-locate them for linking with weather data.
2. **munge_weather**: These scripts assemble weather data for each trial location and year in the yield dataset. They pull data from the GHCN hosted by NOAA and the PRISM project at Oregon State University.
3. **model_temp**: These scripts fit a functional linear mixed effects model using B-splines and back-transform the results onto the original measurement scale.
4. **temp_analysis**: These scripts analyze the possibility of selection for heat tolerance at various temperatures and characterize genetic variation and constraints for heat tolerance.
5. **figures**: These scripts construct the main text and supplementary figures.

### License

This repository is free and open source for use in research and is licensed under the terms of [GNU GPLv3](https://choosealicense.com/licenses/gpl-3.0/#).
