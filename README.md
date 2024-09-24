# Repository of the article entitled "Legume intensification leads to social ecological win-win outcomes globally"

## 1. System Requirements

### Software Dependencies
- R 4.3.1 or later
- R packages: `DescTools`, `lme4`,`ggpubr`,`performance`,`dplyr`,`ggplot2`,`tidyverse`,`gridExtra`
- Operating System: Linux, macOS, or Windows 10/11
- R (CRAN)
- Code Ocean (https://codeocean.com/): RStudio Cloud Workstation (RStudio 2024.04.2-764) `DescTools`, `lme4`,`ggpubr`,`performance`,`dplyr`,`ggplot2`,`tidyverse`,`gridExtra`, `MatrixModels`, `car`, `quantreg`, `rstatix`


### Tested Versions
- R: 4.3.1
- OS: Windows 11
- Code Ocean (https://codeocean.com/): RStudio Cloud Workstation (RStudio 2024.04.2-764)

### Required Non-standard Hardware
- No specific hardware requirements beyond a standard desktop or laptop computer.

---

## 2. Installation Guide in RStudio

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

## 3. Running the code in R Studio

### Instructions to Run the code in R studio

After installation, you can run the codes "Aggregated_models.R" and "Disaggregated_models.R" by copying it into your R script editor (or RStudio) and executing the script. The code processes multiple datasets (available on the respository as.csv files) to fit and plot the models specified in Figures 3 and 5 of the paper submitted.

The .csv files that need to be loaded into Rstudio are the following (make sure to insert the filepath of the folder where yoou are storing these data files):

- "Econ_wellbeing_allservices_average_transf_2.csv"         
- "Econ_wellbeing_biocarb_services_average_transf_2.csv"   
- "Econ_wellbeing_prov_services_average_transf_2.csv"
- "Econ_wellbeing_regul_services_average_transf_2.csv"     
- "Econ_wellbeing_supp_services_average_transf_2.csv"
- "Nonecon_wellbeing_allservices_average_transf_2.csv"     
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

### Expected Outputs

- Results from GLM and LM models, including p values
- Joint impacts of legume intensification on ecosystem services or biodiversity and well-being, with each dot representing an individual case. Change is represented as a percentage, where intervention outcome is related to control (baseline). WB = well-being (economic or non-economic). Trend line is visible when a significant correlation was found (p<0.001).
- Significant correlations between specific indicators of ecosystem services and well-being (WB) (each dot representing an individual case).

### Expected Run Time
The demo should complete in under 2 minutes on a standard desktop computer.

---

## 4. Running the code via Code Ocean, using RStudio Cloud Workstation (Online Alternative)

- Access the Code Ocean project: (https://codeocean.com/)
- Using the RStudio Cloud Workstation (RStudio 2024.04.2-764) make sure the following packages are installed: `DescTools`, `lme4`,`ggpubr`,`performance`,`dplyr`,`ggplot2`,`tidyverse`,`gridExtra`, `MatrixModels`, `car`, `quantreg`, `rstatix`. In addition, the package `cmake` also needs to be loaded from apt-get on the Code Ocean Environment.
- Once the RStudio Cloud Workstation has opened, select the code entitled **"Models.R"**
- Run the entire code

### Expected Outputs
- Same outputs that are generated on the desktop version, as described earlier.
  
### Expected Run Time
The demo should complete in under 2 minutes on a standard desktop computer.

---

## 5. Other files in this repository

The file "DataExtraction_Legumes_Master_withMetadata_240703" includes all data extracted from the results of a systematic review of the peer-reviewed literature on grain legume intercropping and rotations in agri-food systems.
- The sheet "DataExtractionArticles" provides details for all 183 articles (382 cases) that reported at least one ecological outcome (ecosystem services and/or biodiversity) and one human well-being outcome from introducing legumes into an agri-food production system.
- The sheet "Metadata" provides descriptions of all columns in the DataExtractionArticles sheet
- The sheet "Systems" provides analyses of the different agri-food systems represented by the cases reviewed
- The sheet "Quality assessment codes" shows the Quality Assessment Table used to evaluate each article for study design and reporting quality, and risk of bias.


