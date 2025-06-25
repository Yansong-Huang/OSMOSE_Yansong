---
title: "OWF Cumulative Impact Analysis with OSMOSE-EEC"
author: "Yansong Huang"
date: "`r Sys.Date()`"
output: 
  html_document:
    toc: true
    toc_depth: 2
    number_sections: true
    theme: cosmo
---

# Introduction

This project aims to:

1. Run the **OSMOSE-EEC** model under a factorial plan combining:
   - Offshore Wind Farm (**OWF**) deployment scenarios,
   - Fishing regulation scenarios,
   - Climate change scenarios.

2. Analyse the cumulative impact of OWF deployment on the Eastern English Channel (EEC) ecosystem.

The project includes model simulations, sensitivity analysis, and result visualisation through ecological indicators such as biomass, yield, and Large Fish Indicator (LFI).

---

# Folder Structure

The project is organised into the following folders:

## 📁 `input/`

Configuration of the OSMOSE-EEC model

## 📁 `data/`

Contains spatial and alternative input data for the scenario simulations:

- Maps of OWF locations in the OSMOSE-EEC grid;
- Species distribution under fishing scenarios;
- Fishing effort maps;
- Prey group dynamics under climate change projections.

## 📁 `indicators/`

Contains example model outputs:

- Time series of biomass, yield, and LFI;
- Intermediate indicators calculated from simulation outputs;
- Visualisations used in exploratory analysis.

## 📁 `scripts_analysis/`

Scripts for post-simulation analysis and visualisation:

- Indicator time series plotting;
- Statistical testing of scenario effects;
- Spatial analysis (e.g., OWF-specific biomass).

## 📁 `scripts_simulation/`

Scripts for preparing and running the factorial design simulations:

- Input file checking and consistency;
- Launching simulation batches across scenario combinations;
- Output management and replication control.

---

# Notes

All scripts are written in R and rely on tidyverse-compatible libraries. Simulation outputs follow the structure of OSMOSE v4 and are processed to produce reproducible and publication-ready figures.

If you'd like to reproduce the full pipeline, start by reviewing the `scripts_simulation/` scripts, followed by `scripts_analysis/`.

---

