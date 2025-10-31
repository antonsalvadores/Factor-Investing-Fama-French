# Factor Investing Models: Fama-French vs. Carhart

This project, developed for the M.Sc. in Quantitative Banking and Finance, conducts an empirical comparison of the Fama-French 3-Factor model and the Carhart 4-Factor model to explain European stock returns.

---
### 📄 Key Project Documents
* **[View the Final Paper (PDF)](docs/Factor_Investing_Paper.pdf)**
* **[View the Assignment Guidelines (PDF)](docs/Assignment_Guidelines.pdf)**

### 📊 Key Results
* **[OLS Regressions (3-Factor Model)](results/3_Factor_Model_OLS.html)**
* **[OLS Regressions (4-Factor Carhart Model)](results/4_Factor_Carhart_OLS.html)**
---

## Project Objectives

[cite_start]The goal was to determine which model provides a better explanation for the returns of 25 European portfolios, sorted by size (ME) and book-to-market (B/M)[cite: 1834].

The project compares two key asset pricing models:
1.  **Fama-French 3-Factor Model:** Uses Market (Mkt-RF), Size (SMB), and Value (HML) factors.
2.  **Carhart 4-Factor Model:** Adds a Momentum (WML) factor to the Fama-French model.

## Methodology & Tools

* **Language & Tools:** R
* **Core Packages:** `tidyfinance`, `frenchdata` (for data import), `slider` (for rolling regressions), `stargazer` (for output tables).

## Key Analyses Performed

The analysis was conducted in two main stages:

### 1. OLS Time-Series Regressions
* Standard OLS regressions were run for each of the 25 portfolios against the 3-Factor and 4-Factor models.
* Full diagnostic tests were performed for heteroskedasticity (White test), autocorrelation (Breusch-Godfrey, Durbin-Watson), and normality (Jarque-Bera).
* **[See OLS regression tables here](results/)**

### 2. Fama-MacBeth Two-Stage Regression
To address issues of cross-sectional correlation and non-constant betas, the **Fama-MacBeth (1973) method** was implemented:
* **Stage 1 (Rolling Betas):** Time-series betas for each factor were estimated using a 60-month rolling window to capture their time-varying nature.
* **Stage 2 (Cross-Sectional Regression):** Monthly cross-sectional regressions were run to estimate the **risk premium (gamma)** associated with each factor.
* The stability and significance of these risk premiums were then analyzed to determine which factors (including Momentum) consistently provided explanatory power for returns.
