# Non-Linear Dynamics of Bitcoin and Gold: A Threshold VAR Analysis
*This R framework models non-linear Bitcoin and Gold dynamics via volatility-driven Threshold Vector Autoregression.*

---

## 🎯 Overview
This project analyzes the regime-dependent interdependence between cryptocurrencies, safe-haven assets, and global macro-financial variables using non-linear time series models.

**Objectives**
- Identify volatility regimes using the VIX index as a threshold variable.
- Estimate non-linear responses of Bitcoin and Gold to macro-financial shocks.
- Compare predictive performance between linear VAR and regime-switching TVAR specifications.

---

## 🗄️ Data
- **Source:** Yahoo Finance (BTC-USD, Gold Futures), FRED (Dollar Index, VIX), Proprietary (MSCI World).
- **Time Period / Size:** 2014-10 to 2025-12, Monthly frequency.
- **Target Variable:** Bitcoin (BTC-USD), Gold (GC=F).
- **Key Predictors / Features:** Dollar Index, VIX (Volatility Index), MSCI World.
- **Preprocessing:** Log-returns (multiplied by 100), Stationarity checks (ADF, Phillips-Perron, Zivot-Andrews).
- **Data Availability:** Public APIs (Yahoo Finance, FRED) and local Excel datasets.
- **Note:** The file `msci.xlsx` is listed in `.gitignore` because it contains proprietary data.

---

## 🧠 Methodology
- **Theoretical Approach:** Regime-Switching Models (Threshold Models).
- **Mathematical Framework:** Threshold Vector Autoregression (TVAR) with Cholesky Decomposition.
- **Evaluation Strategy:** AIC/BIC Information Criteria, Likelihood Ratio Tests, Impulse Response Functions (IRF).

---

## ⚙️ Features
- **Import Financial Data**: Automate data retrieval from Yahoo Finance and FRED APIs.
- **Detect Structural Breaks**: Identify additive outliers and level shifts using `tsoutliers`.
- **Test Stationarity**: Execute ADF, Phillips-Perron, and Zivot-Andrews unit root tests.
- **Estimate Regimes**: Model volatility regimes based on endogenously estimated VIX thresholds.
- **Simulate Shocks**: Compute regime-dependent Impulse Response Functions (IRF) via bootstrap.

---

## 🧰 Tech Stack
- **Language:** R
- **Data Engineering & Acquisition:** `readxl`, `writexl`
- **Numerical Computing & Data Manipulation:** `tidyverse`, `xts`, `zoo`
- **Econometrics & Statistical Inference:** `fBasics`, `tseries`, `FinTS`
- **Time Series Analysis:** `tsDyn`, `vars`, `urca`, `tsoutliers`, `seastests`
- **Quantitative Finance:** `quantmod`
- **Data Visualization:** `ggplot2` (via tidyverse), Base R

---

## 📦 Installation

```bash
git clone https://github.com/floriancrochet/master-year2-multivariate-time-series.git
cd master-year2-multivariate-time-series
Rscript -e 'install.packages(c("quantmod", "tidyverse", "tsDyn", "vars", "urca", "tsoutliers", "readxl", "writexl", "fBasics", "seastests", "tseries", "FinTS"))'
```

---

## 💻 Usage Example

### Reproducing the Analysis / Execution Pipeline

```r
source("project.R")
# The script automatically fetches data, trains models, and generates plots.
```

---

## 📂 Project Structure

```text
master-year2-multivariate-time-series/
│
├── data/
│   └── data.xlsx                                  # Global equity market index
├── report/
│   └── report.pdf
├── LICENSE
├── README.md
├── master-year2-multivariate-time-series.Rproj
└── project.R                                      # TVAR modeling pipeline
```

---

## 📈 Results

### Key Findings
- **Regime Identification:** The model delineates market states into "Calm" and "Stress" regimes based on VIX thresholds.
- **Shock Propagation:** Impulse Response Functions illustrate the varying sensitivity of Bitcoin and Gold to macro-financial shocks across regimes.
- **Variance Decomposition:** FEVD analysis isolates the contribution of dollar strength and equity volatility to asset returns during stress periods.

---

## 🚧 Limitations & Future Work
- **Recursive Identification:** The Cholesky decomposition assumes a specific ordering of variables (Dollar -> MSCI -> VIX -> Gold -> Bitcoin) which impacts impulse responses.
- **Seasonality Assumptions:** The model relies on the Webel-Ollech test to rule out seasonal adjustments for financial variables.

---

## 📚 References
- Chen, C. & Liu, L.-M., *Joint Estimation of Model Parameters and Outlier Effects in Time Series* (1993)

---

## 📜 License
This project is released under the MIT License.
© 2026 Djayan Daëron and Florian Crochet

---

## 👤 Authors
**Djayan Daëron**  
[GitHub Profile](https://github.com/Djayan-D)  

**Florian Crochet**  
[GitHub Profile](https://github.com/floriancrochet)

*Master 1 – Econometrics & Statistics, Applied Econometrics Track*  

---

## 🤝 Acknowledgments
This work was conducted as part of the Multivariate Time Series course, supervised by Moussa Zakaria.
