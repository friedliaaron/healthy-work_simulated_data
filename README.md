# 🏥 Healthy@Work Analytics Dashboard

**Interactive Shiny Dashboard for Occupational Health Analyses**

[![DEMO VERSION](https://img.shields.io/badge/STATUS-DEMO%20VERSION-red?style=for-the-badge)](https://aaronfriedli.shinyapps.io/healthyworksimulated/)
---

## ⚠️ IMPORTANT NOTICE

**This version uses EXCLUSIVELY simulated, fictional data!**

- ❌ All displayed numbers, results, and analyses are **NOT real**
- ❌ The data was randomly generated and does **not represent a real study**
- ❌ **No conclusions** can be drawn about real banks, organisations, or individuals
- ✅ This version serves **exclusively as a demonstration** of the dashboard's functionality

---

## 📊 About the Project

This dashboard was developed as part of the **Healthy@Work Study** and enables interactive analyses of occupational health data in Swiss banks.

### Analysed Dimensions:
- 🧠 **Working Conditions**: Autonomy, Resources, Stressors
- 💪 **Coping Strategies**: Self-care, Risky self-endangerment
- 😊 **Well-being**: Engagement, Work-Life Balance, Exhaustion

### Dashboard Features:
- **7 interactive tabs** with various analysis types
- **Multilevel Models** (MLM) accounting for organisational hierarchy
- **Predictor Calculator** with waterfall and sensitivity analyses
- **EMM Analyses** for fair comparisons between organisational units
- **Correlation analyses** and descriptive statistics
- **Report Export** (Word, PDF, HTML)

---

## 🚀 Installation & Getting Started

### Prerequisites

```r
# R Version >= 4.0.0
R.version.string
```

### Required Packages

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets",
  "tidyverse", "DT", "plotly", 
  "lme4", "lmerTest", "nlme", "performance", "emmeans",
  "corrplot", "viridis", "Hmisc", "kableExtra",
  "readxl", "here", "rmarkdown", "knitr", "RColorBrewer"
))
```

### Starting the Dashboard

```r
# Navigate to the project folder
setwd("path/to/healthy_at_work_dashboard")

# Start the app
shiny::runApp()
```

The dashboard will open in your browser at `http://127.0.0.1:XXXX`

---

## 📁 Project Structure

```
healthy_at_work_dashboard/
│
├── app.R                    # Main app (UI + Server)
├── helpers.R                # Analysis functions
├── data_processing.R        # Data processing & simulation
├── report_template.Rmd      # R Markdown template for reports
│
├── www/
│   └── fhnw.jpg            # FHNW logo
│
└── README.md               # This file
```

---

## 📖 Dashboard Tabs in Detail

### 1️⃣ Overview
- Sample overview (N, banks, average per bank)
- Dataset information
- **Variable overview** with all constructs used

### 2️⃣ Descriptive Statistics
- Frequency tables for categorical variables
- Boxplots by group (age, gender, etc.)
- **Active filters**: Age, Gender, Bank

### 3️⃣ Clustering & ICC
- **Intraclass Correlation Coefficients** (ICC)
- Analysis of variance between vs. within organisations
- Significance tests

### 4️⃣ Inferential Statistics
- **Multilevel Models** (Mixed Effects Models)
- Controls for organisational membership
- Coefficient tables + visualisations
- **Men & women only** (automatic gender filter)

### 5️⃣ Predictor Calculator
- Interactive predictions based on personal characteristics
- **Waterfall Chart**: How is the value composed?
- **Sensitivity Analysis**: What-if scenarios

### 6️⃣ Organisational Units
- **Estimated Marginal Means** (EMMs)
- Adjusted comparisons between units
- Pairwise contrasts

### 7️⃣ Correlations
- Correlation matrix
- Interactive scatter plots
- Detailed correlation tables

### 8️⃣ Export Report
- **Selectable sections**
- **Formats**: Word (.docx), PDF, HTML
- Configurable filter settings

---

## 🎨 Design

The dashboard follows the **FHNW Corporate Design**:
- Primary colour: **FHNW Yellow** (#FDE70E)
- Accent colour: Black (#000000)
- Minimalist, professional design
- Responsive layout

---

## 📊 Simulated Data

Data is generated in `data_processing.R` > `load_simulated_data()`:

- **N = 1500** simulated observations
- **3 waves** (2022/2023, 2023/2024, 2024/2025)
- **15 organisations** (banks)
- **Realistic distributions** for all variables
- **Correlation structure** modelled on real studies

---

## 🔬 Statistical Methods

### Multilevel Models (MLM)
```r
nlme::lme(
  fixed = outcome ~ predictors,
  random = ~ 1 | organisation,
  data = df
)
```

**Why MLM?**
- Accounts for dependencies within organisations
- Separates variance between vs. within banks
- Robust standard errors

### Estimated Marginal Means (EMMs)
```r
emmeans::emmeans(model, ~ organisational_unit)
```

**Why EMMs?**
- Fair comparison between units
- Controls for confounding variables (age, gender, etc.)
- "Equal conditions" for all units

---

## 👥 Developed by

**FHNW - University of Applied Sciences and Arts Northwestern Switzerland**  
Institute for Cooperation Research and Development

**Project**: Healthy@Work Study  
**Contact**: [FHNW Website](https://www.fhnw.ch)

---

## 📄 Licence

This project was created for **demonstration and educational purposes**.  
For use with real data, please contact FHNW.

---

## 🙏 Acknowledgements

Built with:
- [R Shiny](https://shiny.rstudio.com/)
- [shinydashboard](https://rstudio.github.io/shinydashboard/)
- [tidyverse](https://www.tidyverse.org/)
- [lme4](https://github.com/lme4/lme4) & [nlme](https://cran.r-project.org/package=nlme)

---

**For questions or feedback:**  
📧 Contact the FHNW project team

---

*Last updated: March 2026*
