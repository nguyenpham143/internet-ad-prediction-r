# Predicting Internet Advertisements Using Statistical Methods

An end-to-end statistical analysis and machine learning pipeline in **R** designed to classify web page images as internet advertisements (`ad.`) or non-advertisements (`nonad.`). Developed as the group assignment for the **Probability and Statistics (MT1013)** course at Ho Chi Minh City University of Technology (HCMUT - VNU-HCM), Semester 252.

## Overview

Online banner detection is crucial for ad-blocking and content recommendation. Using the **UCI Internet Advertisements Dataset** (1,560 initial attributes), this project implements a rigorous three-stage statistical workflow:

1. **Data Cleaning & Preprocessing (`data_cleaning.r`)**:
   - Extracts essential geometric dimensions: `Height`, `Width`, `Aspect_ratio` (`Width / Height`), and origin flag `Local`.
   - Cleans anomalous strings (`?`) and applies robust missing-value handling: **median imputation** for continuous skewed features (`Height`, `Width`) and **mode imputation** for binary attributes (`Local`).
   - Converts target labels into binary indicators: `1 = ad.`, `0 = nonad.`.

2. **Exploratory Data Analysis & Descriptive Statistics (`descriptive_statistics.r`)**:
   - Calculates central tendency and dispersion metrics (Mean, Median, Standard Deviation, IQR, Quantiles).
   - Examines severe class imbalance between banner ads and non-ad content.
   - Generates publication-ready visualizations via `ggplot2` (histograms, boxplots, stacked proportion charts).

3. **Inferential Statistics & Predictive Modeling (`inferential_statistics.r`)**:
   - Performs an 80/20 stratified train/test split.
   - Fits and compares nested **Logistic Regression (GLM Binomial)** models via Deviance Analysis / Likelihood Ratio Tests (`anova(..., test = "Chisq")`).
   - Evaluates multicollinearity via Variance Inflation Factors (**VIF**), odds ratios, and 95% confidence intervals.
   - Validates predictive power using Confusion Matrix metrics (Accuracy, Sensitivity, Specificity) and **ROC Curve / Area Under the Curve (AUC)**.

## Project Structure

```text
.
├── data/
│   ├── add.csv                       # Raw dataset from UCI (1,560 features)
│   └── preprocessed.csv              # Imputed & preprocessed dataset
├── src/
│   ├── data_cleaning.r               # Extraction & imputation module
│   ├── descriptive_statistics.r       # Descriptive statistics & ggplot2 visualizations
│   ├── inferential_statistics.r       # Logistic regression & hypothesis testing
│   └── main.r                        # Full end-to-end execution pipeline
├── report/
│   └── XSTK_CC02_Group20.pdf         # Final compiled report (Group 20)
├── .gitignore                        # Git ignore rules
├── LICENSE                           # MIT License
├── README.md                         # Project documentation
└── run.sh                            # Execution script
```

## Contributors

**Class**: CC02 — **Group**: 20  
**Advisor**: Dr. Nguyen Tien Dung

| Student ID | Full Name | Assigned Tasks |
|:---:|:---|:---|
| 2452347 | Le Vo Nghia Hiep | Inferential Statistics |
| 2411630 | Nguyen Dinh Dang Khoa | Data Cleaning |
| 2412372 | Pham Vu Khoi Nguyen | Merge code, Final Report |
| 2453210 | Phan The Thong | Inferential Statistics |
| 2453286 | Pham Bui Phuong Tran | Descriptive Statistics |

## Getting Started

### Prerequisites

Ensure **R** (>= 4.0.0) is installed along with the required packages:

```R
install.packages(c("ggplot2", "dplyr", "caTools", "caret", "pROC", "car"))
```

### Running the Pipeline

Execute the full pipeline from the terminal:

```bash
Rscript src/main.r
```

Or run via the automated shell script:

```bash
bash run.sh
```

Individual modules can also be run sequentially:

```bash
Rscript src/data_cleaning.r
Rscript src/descriptive_statistics.r
Rscript src/inferential_statistics.r
```

## License

This project is licensed under the [MIT License](LICENSE).
