# OWF Cumulative Impact Analysis with OSMOSE-EEC

**Author**: Yansong Huang  
**Date**: 2025-06-25  


# Introduction

This project aims to:

1. Run the **OSMOSE-EEC** model under a factorial plan combining:
   - Offshore Wind Farm (**OWF**) deployment scenarios,
   - Fishing regulation scenarios,
   - Climate change scenarios.

2. Analyse the cumulative impact of OWF deployment on the Eastern English Channel (EEC) ecosystem.

The project includes model configuration, model simulations, result visualisation through ecological indicators such as biomass, yield, and Large Fish Index (LFI).

---

# Folder Structure

The project is organised into the following folders:

## `input/`

Configuration of the OSMOSE-EEC model

## `data/`

Contains spatial and alternative input data for the scenario simulations:

- Maps of OWF locations in the OSMOSE-EEC grid;
- Species distribution maps under scenarios;
- Fishing effort maps under scenarios;
- Prey group maps under climate change projections.

## `indicators/`

Contains example model outputs

## `scripts_analysis/`

Scripts for post-simulation analysis and visualisation:

- Indicator time series and heatmap plotting;
- Statistical testing of scenario effects;
- Spatial analysis (e.g., OWF-specific biomass).

## `scripts_simulation/`

Scripts for preparing and running the factorial design simulations:

- Input file checking and consistency;
- Launching simulation batches across scenario combinations;
- Output management and replication control.

---

# Notes

All scripts are written in R and rely on tidyverse-compatible libraries. 

For the OSMOSE user guide, please refers to this page: https://github.com/osmose-model/osmose/wiki/User-Guide

To reproduce the full modelling and analysis pipeline:

1. Start from the scripts in `scripts_simulation/` to configure and run simulations;
2. Then use `scripts_analysis/` to process outputs and generate figures;

---

## Citation

If you use this repository or refer to the associated model configuration and analysis scripts, please cite:

Huang, Y., & Oliveros-Ramos, R. (2025). *OSMOSE-EEC factorial plan simulation for offshore wind farm cumulative impact assessment* (v0.9) [Pre-release]. Zenodo. https://doi.org/10.5281/zenodo.15740531

This is a pre-release accompanying a manuscript currently under peer review. A final version will be released upon publication.


## Contact

For questions, suggestions, or collaboration opportunities:

**Yansong Huang**  
[ORCID 0009-0004-7989-4154](https://orcid.org/0009-0004-7989-4154)  
[GitHub Profile](https://github.com/Yansong-Huang)

---



