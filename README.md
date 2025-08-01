# Repository of the article entitled "Legume intensification leads to social ecological win-win outcomes globally" (DAPR - NCOMMS-24-55796-T) UPDATED 2025

## 1. System Requirements

### Software Dependencies
- R 4.3.1 or later
- R packages: `DescTools`, `lme4`,`ggpubr`,`performance`,`dplyr`,`ggplot2`,`tidyverse`,`gridExtra`,`scales`,`ggalluvial`,`sp`, `sf`,`tmap`,`stars`,`raster`,`viridis`,`cowplot`,`ggspatial`, `Jtools`
- Operating System: Linux, macOS, or Windows 10/11
- Code Ocean (https://codeocean.com/): RStudio Cloud Workstation (RStudio 2024.04.2-764) `DescTools`, `lme4`,`ggpubr`,`performance`,`dplyr`,`ggplot2`,`tidyverse`,`gridExtra`, `MatrixModels`, `car`, `quantreg`, `rstatix`,`scales`,`ggalluvial`,`sp`, `sf`,`tmap`,`stars`,`raster`,`viridis`,`cowplot`,`ggspatial`, `Jtools`


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
install.packages(c("DescTools", "lme4", "ggpubr", "performance", "dplyr", "ggplot2", "tidyverse", "gridExtra", "scales", "ggalluvial", "sp", "sf", "tmap", "stars", "raster", "viridis", "cowplot", "ggspatial", "Jtools"))
```

### Typical Install Time
On a typical desktop or laptop, installation should take approximately 5-10 minutes, depending on your internet speed and system performance.


---

## 3. Running the code in R Studio

### Instructions to Run the code in R studio

After installation, you can run the codes "Aggregated_models_2025.R", and "Disaggregated_models_2025.R" by copying it into your R script editor (or RStudio) and executing the script. The codes process multiple datafiles from the folder named "Data", to fit and plot the models and generate visualisations included on the paper submitted.

### Expected Run Time
The demo should complete in under 2 minutes on a standard desktop computer.

---

## 4. Running the code via Code Ocean, using RStudio Cloud Workstation (Online Alternative)

- Access the Code Ocean project: (https://codeocean.com/)
- Using the RStudio Cloud Workstation (RStudio 2024.04.2-764) make sure the following packages are installed: `DescTools`, `lme4`,`ggpubr`,`performance`,`dplyr`,`ggplot2`,`tidyverse`,`gridExtra`, `MatrixModels`, `car`, `quantreg`, `rstatix`,`scales`,`ggalluvial`,`sp`, `sf`,`tmap`,`stars`,`raster`,`viridis`,`cowplot`,`ggspatial`, `Jtools`. In addition, the package `cmake` also needs to be loaded from apt-get on the Code Ocean Environment.
- Once the RStudio Cloud Workstation has opened, select the code entitled **"Models_and_visualisations.R"**
- Run the entire code

### Expected Outputs
- Same outputs that are generated on the desktop version, as described earlier.
  
### Expected Run Time
The demo should complete in under 2 minutes on a standard desktop computer.

---

## 5. Other files in this repository

The file "Master_dataset_legumes_2025.csv" includes all data extracted from the results of a systematic review of the peer-reviewed literature on grain legume intercropping and rotations in agri-food systems.
- The file "References_Legumes_2025" provides details for all papers and cases that reported at least one ecological outcome (ecosystem services and/or biodiversity) and one human well-being outcome from introducing legumes into an agri-food production system.
- The file "Metadata_and_quality_assessment_codes_2025" provides descriptions of all columns in the DataExtractionArticles sheet, as well as the Quality Assessment Table used to evaluate each article for study design, reporting quality, and risk of bias.
- The folder "Data" includes the filtered datasets needed to run the R codes.


