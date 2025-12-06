# Personality Traits and Drug Use  
P8105 Final Project — Fall 2025  

Team Members:
- Sijda Ahmed
- Ruicheng Yang
- Xiling Wang
- Haixin Jin
- Maryam Khalid

---

## Overview
This project investigates relationships between **personality traits** and **patterns of drug use** using the *Drug Consumption Dataset*. We examine:

- How personality profiles differ between drug users and non-users  
- Whether trait levels predict drug-use frequency  
- Demographic factors (e.g., age, education, country) associated with use  

Our work combines **data cleaning**, **exploratory visualization**, and **formal statistical testing**, and is presented as a fully reproducible R Markdown website.

---

## Repository Structure

```
P8105_FinalProject/
│
├── data/                          # Raw dataset and cleaned versions
├── figs/                          # Generated figures used in visualizations and report
├── index.html                     # Website home page
├── proposal.html                  # Project proposal
├── dataset.html                   # Data page with summaries
├── EDA.html                       # Exploratory data analysis
├── visualization.html             # Additional plots
├── results.html                   # Results page (statistical tests + interpretation)
│
├── FP_Proposal.Rmd                # Original proposal source file
├── data_cleaning.rmd              # Data import and preprocessing steps
├── eda.Rmd                        # Exploratory analysis R Markdown
├── drug_consumption_analysis.Rmd  # Formal analysis (statistical tests)
│
├── README.md                      # Project documentation
├── P8105_FinalProject.Rproj       # RStudio project file
├── style.css                      # Website styling
├── script.js                      # JavaScript for UI enhancements
└── .gitignore                     # Git ignore file
```

---

## How to Reproduce Our Website

1. **Clone the repository:**
```bash
git clone https://github.com/<TEAM>/P8105_FinalProject.git
```

2. **Open the R Project in RStudio:**
```
P8105_FinalProject.Rproj
```

3. **Install required R packages:**
```r
install.packages(c(
  "tidyverse", "janitor", "lubridate", "ggplot2", "patchwork",
  "here", "rmarkdown", "knitr"
))
```

4. **Re-run all analysis and rebuild the website:**
```r
rmarkdown::render_site()
```

All HTML pages will be created inside the project directory and viewable locally or deployed via GitHub Pages.

## Analytic Plan

The project includes both exploratory and formal statistical analyses:

### **Exploratory Analysis**
- Distributions of personality traits  
- Heatmaps and density plots of drug use  
- Demographic comparisons  

### **Formal Statistical Analyses**
- **Chi-square tests**:  
  - Education × user vs non-user  
  - Country × user vs non-user  

- **ANOVA / t-tests**:  
  - Personality trait differences between usage groups  

- **Correlation analyses**:  
  - Relationships between trait levels and usage frequency  

Results are summarized on the **Results** page and discussed more deeply in the **Full Report**.

---

## Final Report

The final written report (`final_report.html`) integrates:

- Motivation  
- Related work  
- Data cleaning steps  
- Exploratory findings  
- Statistical analyses  
- Interpretation and discussion  
- Limitations and further work  

---

## Website 
The project website includes:

- Project Overview  
- Dataset description  
- Exploratory visualizations  
- Formal results  
- Embedded screencast  
- Full written report  

## Contributions
| **Maryam Khalid** | Screencast recording, interpretation, final report analyses & cleanup |
| **Sijda Ahmed & Ruicheng Yang** | Statistical analysis and data visualization |
| **Xiling Wang & Haixin Jin** | Webpage development, documentation, and editing the final report |


## Notes for Reproducibility

- All work is written in `.Rmd` files and can be recomputed from scratch.  
- All data required for the project is contained in the `data/` folder.  
- The repository contains no absolute file paths.  
- Running `render_site()` regenerates the entire website.  

---

## Course Information

This project was completed for **P8105: Data Science I**, taught at Columbia University Mailman School of Public Health. The repository is intended for instructional and academic use only.
