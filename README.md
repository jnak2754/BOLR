# BOLR
Bayesian ordinal logistic regression model blending physics-based synthetic data and empirical windstorm performance data

# Wind Damage Bayesian Modeling: Synthetic and Empirical Data Analysis

This repository contains the full analytical workflow for evaluating Bayesian ordinal logistic regression models to predict residential wind damage. The analysis combines synthetic damage data from fragility functions with real-world post-disaster observations from the WiSPD dataset.

## Project Overview

The study compares four modeling strategies—synthetic-only, empirical-only, sequential updating, and hierarchical integration—under hurricane and tornado conditions. The goal is to assess predictive accuracy, understand posterior behavior, and explore how sampling and damage state definitions affect inference.

## Repository Structure

- `scripts/`
  - `Synthetic_Data_Generator.py`: Generates the synthetic dataset and standardizes WiSPD predictor features.
  - `Empirical_OLRmodel.R`: Runs baseline empirical and synthetic ordinal logistic regressions to explore model structure.
  - `OLRandFigures.R`: Runs the final full analysis including model training, figure generation, and summary metrics.
- `data/`: Instructions or placeholders for accessing WiSPD and synthetic datasets.
- `outputs/`: Auto-generated figures and model summaries.


## How to Reproduce the Analysis

1. **Ensure all required data is available:**
   - Place `WiSPD.csv` and `Fragility_DB.csv` in the `data/` folder.

2. **Generate standardized datasets:**
   ```bash
   python scripts/Synthetic_Data_Generator.py

