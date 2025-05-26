This repository contains the code for the paper:

**_"Regression in Quotient Metric Spaces with a Focus on Elastic Curves"_**  

## 📁 Contents

- `simulations/`  
  Scripts to reproduce simulation studies from the paper.

- `application_hippocampus/`  
  Scripts and data for applying the quotient regression model to hippocampus shape data.

## 🚀 How to Use

### 1. Clone the Repository

```bash
git clone https://github.com/steyerli/Code_elastic_regression_JMVA.git
cd Code_elastic_regression_JMVA
```

### 2. Install Dependencies

Make sure you have **R** installed. Install the required R packages, especially:

- [`elasdics`](https://github.com/steyerli/elasdics): Implements the quotient regression model.

## 🔬 Simulations

Navigate to the `simulations/` folder.

### 🔹 1. Model Performance Comparison

- `perform_simulation.R`  
  Runs the main simulations comparing model performance.

- `make_sim_plot_n_table.R`  
  Generates plots and tables (e.g., MSE results) as displayed in the paper.

### 🔹 2. Inference Based on Spline Coefficients

- `test_model_coefs.R`  
  Runs simulations for testing inference on spline coefficients.

- `plot_boot_sim.R`  
  Generates plots from the bootstrap simulation results.

⚠️ **Note**:  
The folders `bootstrap_sim_data` and `bootstrap_sim_data2` must be populated by first running `test_model_coefs.R`.

---

## 🧠 Hippocampus Application

Navigate to the `application_hippocampus/` folder.

Key scripts:

- `fit_regression_models.R`  
  Fits the quotient regression model using different covariate combinations.

- `plot_regression_model.R`  
  Creates plots for the paper and its online supplement.

---

## 📄 License

This project is released under the **MIT License**.
