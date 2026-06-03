# ⚾ Baseball Player Salary Analysis
### Multiple Linear Regression & Logistic Regression — Group 6

This project analyses the factors influencing **MLB (Major League Baseball) player salaries** using the well-known **Hitters dataset** from the 1986–1987 season. The analysis was conducted as part of the **ST 3008 Applied Statistical Models** module and involves comprehensive EDA, regression modelling, and model adequacy checking using R.

---

## 📁 Project Structure

```
baseball-player-salary-analysis/
│
├── Fitted_Models_Group_6.R          # All R code for model fitting and analysis
├── Hitters.csv                      # Dataset (322 MLB players, 20 variables)
├── Group6_ST3008_Final_Report.pdf   # Full project report
└── README.md                        # Project documentation
```

---

## 📋 Project Overview

The **Hitters dataset** contains records of 322 MLB players from the 1986–1987 season, including performance metrics, career statistics, and salary information. The goal is to identify the key predictors of player salary and build reliable regression models.

### Dataset Details
- **Source:** StatLib library (Carnegie Mellon University), originally from Sports Illustrated (April 1987)
- **Observations:** 322 players (263 with salary data — 59 missing values)
- **Variables:** 20 total — 16 numeric, 3 categorical, 1 response (Salary)
- **Response Variable:** 1987 annual salary (in $1,000s)

---

## 🔍 Analysis Steps

### 1. Descriptive & Bivariate Analysis
- Summary statistics for all variables
- Correlation analysis — last season performance vs career performance
- Visualisations — histograms, scatterplots, boxplots
- Identified right-skewed salary distribution and multicollinearity among predictors

### 2. Multiple Linear Regression (MLR)
- **Forward variable selection** using ANOVA F-tests
- **VIF diagnostics** to detect and resolve multicollinearity
- Categorical variable testing (League, Division) using nested F-tests
- Two-way interaction term testing
- **Final MLR model:**
```
Salary ~ CRBI + Hits + PutOuts + Walks + Division + CRBI*Hits + Hits*Walks + CRBI*PutOuts
R² = 0.5935
```

### 3. Refined MLR Model (Log Transformation)
- Applied **log transformation** to Salary to fix heteroscedasticity
- Rebuilt model through forward selection
- **Final log-transformed model:**
```
log(Salary) ~ Hits + Years + PutOuts + Walks + Division
R² = 0.5169, Adjusted R² = 0.5075
```
- Diagnostic plots confirmed assumptions satisfied after transformation

### 4. Logistic Regression (Salary Classification)
- Salary categorised into **High / Low** using median as threshold
- 80/20 stratified train-test split
- **Final logistic model:**
```
Y ~ AtBat + Hits + PutOuts + Walks + Runs + AtBat*CRuns
Accuracy: 86.54% | AUC: 0.9747
```

---

## 📊 Key Findings

- **Years of experience** is the strongest single predictor of salary
- Career statistics correlate more strongly with salary than single-season performance
- **Hits, PutOuts, Walks, and Division** are consistent significant predictors across models
- Log transformation successfully resolved heteroscedasticity in the MLR model
- Logistic regression achieves consistently high AUC (above 0.90) across runs

---

## 🛠️ Tools & Libraries

- **R** — Core programming language
- **Base R** — `lm()`, `glm()`, `step()` for model fitting
- **car** — VIF diagnostics
- **caret** — Confusion matrix and classification metrics
- **pROC** — ROC curve and AUC
- **ggplot2** — Visualisations

---

## ▶️ How to Run

1. Open **RStudio**
2. Open `Fitted_Models_Group_6.R`
3. Make sure `Hitters.csv` is in the **same folder**
4. Run the script section by section

---

## 📄 Full Report

The complete analysis report including all outputs, diagnostic plots, and interpretations is available in:
👉 [`Group6_ST3008_Final_Report.pdf`](Group6_ST3008_Final_Report.pdf)
