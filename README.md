# Repository of the article entitled "Legume intensification leads to social ecological win-win outcomes globally

## 1. System Requirements

### Software Dependencies
- R 4.3.1 or later
- R packages: `DescTools`, `lme4`,`ggpubr`,`performance`,`dplyr`,`ggplot2`,`tidyverse`,`gridExtra`
- Operating System: Linux, macOS, or Windows 10/11

### Tested Versions
- R: 4.3.1
- OS: Windows 11

### Required Non-standard Hardware
- No specific hardware requirements beyond a standard desktop or laptop computer.

---

## 2. Installation Guide

### Step 1: 
Install R from CRAN: (https://cran.r-project.org/bin/windows/base/)

Recommended version: R 4.3.1 or later.

### Step 2: 
Install RStudio: (https://posit.co/download/rstudio-desktop/)

### Step 3:
Install the required packages by running the following command in R:

```
install.packages(c("ggplot2", "gridExtra", "DescTools", "lme4", "performance", "ggpubr", "dplyr"))
```

### Typical Install Time
On a typical desktop or laptop, installation should take approximately 5-10 minutes, depending on your internet speed and system performance.


---

## 3. Demo

### Instructions to Run the Demo

After installation, you can run the codes "Aggregated_models.R" and "Disaggregated_models.R" by copying it into your R script editor (or RStudio) and executing the script. The code processes multiple datasets (available on the respository as.csv files) to fit and plot models different models specified in Figures 3 and 5 of the paper submitted.

The .csv files that need to be loaded into Rstudio are the following (make sure to insert the filepath of the folder where yoou are storing these data files):

- "Econ_wellbeing_allservices_average_transf_2.csv"         
- "Econ_wellbeing_biocarb_services_average_transf_2.csv"   
- "Econ_wellbeing_prov_services_average_transf_2.csv"
- "Econ_wellbeing_regul_services_average_transf_2.csv"     
- "Econ_wellbeing_supp_services_average_transf_2.csv"
- "Nonecon_wellbeing_Allservices_average_transf_2.csv"     
- "Nonecon_wellbeing_biocarb_services_average_transf_2.csv"
- "Nonecon_wellbeing_prov_services_average_transf_2.csv"   
- "Nonecon_wellbeing_regul_services_average_transf_2.csv"
- "Nonecon_wellbeing_supp_services_average_transf_2.csv"   
- "Primary_production_profit_econ2.csv"
- "Soil_organic_carbon_profit_econ2.csv"                   
- "Yieldeqcumul_food_security_nonecon2.csv"
- "Yieldeqcumul_natural_capital_nonecon2.csv"              
- "Yieldeqcumul_profit_econ2.csv"
- "Yieldeqcumul_savings_labour_cost_econ2.csv" 

### Expected Run Time
The demo should complete in under 2 minutes on a standard desktop computer.

---

## 4. Instructions for Use

### Running the Software on Your Data
