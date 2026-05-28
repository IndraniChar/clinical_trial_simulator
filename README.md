# 🧬 Clinical Trial Simulator for Rare Diseases

## 🚀 AI-Powered Clinical Trial Simulation Platform

An interactive Shiny application that simulates clinical trials **in-silico** using Monte Carlo methods and PK/PD modeling.

---

## 🌐 Live Demo

### 👉 [**Click here to launch the Clinical Trial Simulator**](https://indranichar.shinyapps.io/clinical_trial_simulator/)

> **No installation needed!** Runs directly in your browser.

---

## ✨ Features

| Feature | Description |
|---------|-------------|
| 🧬 **Virtual Patients** | Generates realistic patient populations with demographics |
| 📊 **PK/PD Modeling** | Sigmoid Emax dose-response model for accurate predictions |
| 🔄 **Monte Carlo Simulation** | Runs 1000+ virtual trials for robust statistics |
| 📈 **Statistical Analysis** | P-values, effect sizes (Cohen's d), and NNT calculation |
| 💊 **Treatment vs Placebo** | Direct comparison with visualization |

---

## 📊 What You Can Do

1. **Set trial parameters** (patients per arm, drug dose, simulations)
2. **Run Monte Carlo simulation** to predict outcomes
3. **Compare treatment vs placebo** with statistical significance
4. **Explore dose-response curves** to find optimal dosing
5. **View virtual patient demographics** and distributions

---

## 🖥️ Run Locally

```r
# Clone the repository
git clone https://github.com/IndraniChar/clinical_trial_simulator.git

# Open R and run:
shiny::runApp("app.R")
