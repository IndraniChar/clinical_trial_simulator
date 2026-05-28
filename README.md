# Clinical Trial Simulator for Rare Diseases

## 🧬 AI-Powered Clinical Trial Simulation Platform

An interactive Shiny application that simulates clinical trials in-silico using Monte Carlo methods and PK/PD modeling.

## Features
- Virtual patient population generation
- Sigmoid Emax PK/PD dose-response modeling
- Monte Carlo simulation (1000+ virtual trials)
- Treatment vs Placebo comparison with statistical significance
- Effect size (Cohen's d) and NNT calculation

## Run Locally
```r
shiny::runApp("app.R")