################################################################################
# Disaggregated  (pairwise) models for Social Ecological Data on Legume Introduction Food Systems
# 2026
################################################################################
#-------------------------------------------------------------------------------
### Load libraries ###
library(lme4)
library(ggplot2)
library(lattice)
library(dplyr)
library(devtools)
library(labdsv)
library(tibble)
library(nlme)
library(lmerTest)
library(DescTools)
library(jtools)
library(coefplot)
library(forestplot)
library(performance)
library(ggeffects)
library(tidyverse)
library(interactions)
library(car)
library(rcompanion)
library(bestNormalize)
library(mgcv)
library(psych)
library(broom)

### Set working directory ###
workingdir <- "insert filepath here"
setwd(workingdir)
getwd()

#-------------------------------------------------------------------------------
# Helper function 
run_model <- function(data, x_var, y_var, x_label = NULL, y_label = NULL) {
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- y_var
  
  formula_obj <- as.formula(paste0("`", y_var, "` ~ `", x_var, "`"))
  mod <- lm(formula_obj, data = data)
  print(summary(mod))
  
  # R-squared
  RSS_p <- sum(residuals(mod)^2)
  TSS   <- sum((data[[y_var]] - mean(data[[y_var]]))^2)
  R2    <- 1 - (RSS_p / TSS)
  cat("R-squared:", R2, "\n")
  cat("AIC:", AIC(mod), "\n")
  
  # Prediction sequence
  s <- seq(min(data[[x_var]], na.rm = TRUE),
           max(data[[x_var]], na.rm = TRUE),
           length.out = 100)
  
  newdat <- setNames(data.frame(s), x_var)
  pred <- predict(mod, newdata = newdat, interval = "confidence", level = 0.95)
  
  # Main regression plot 
  dev.new()
  par(mar = c(5, 5, 4, 2) + 0.1)
  par(cex.axis = 1.5)
  
  plot(data[[x_var]], data[[y_var]],
       pch = 16,
       col = "#147746",
       xlab = x_label,
       ylab = y_label,
       cex.axis = 2,
       cex.lab = 2,
       xlim = range(data[[x_var]], na.rm = TRUE),
       ylim = range(data[[y_var]], na.rm = TRUE),
       axes = FALSE)
  
  shade <- cbind(s, pred[, 2], pred[, 3])
  
  polygon(c(shade[, 1], rev(shade[, 1])),
          c(shade[, 2], rev(shade[, 3])),
          col = "#FDE72580",
          border = NA)
  
  lines(s, pred[, 1], col = "#1DB954", lwd = 3)
  axis(1)
  axis(2)
  
  return(mod)
}

#-------------------------------------------------------------------------------

load_and_transform <- function(filename, x_var, y_var, rename_yield = FALSE) {
  dat <- read.csv(filename, header = TRUE)
  
  if (rename_yield && "Yield" %in% names(dat)) {
    dat <- dat %>% rename(!!x_var := Yield)
  }
  
  dat[[x_var]] <- dat[[x_var]] * 100 - 100
  dat[[y_var]] <- dat[[y_var]] * 100 - 100
  
  return(dat)
}

#===============================================================================
# ECONOMIC MODELS (mod1–mod15)
#===============================================================================

mydat1 <- load_and_transform("filtered_economic_Climate_regulation_Profit.csv",
                             "Climate_regulation", "Profit")
mod1 <- run_model(mydat1, "Climate_regulation", "Profit")

mydat2 <- load_and_transform("filtered_economic_Nutrient_cycling_Financial_security.csv",
                             "Nutrient_cycling", "Financial_security")
mod2 <- run_model(mydat2, "Nutrient_cycling", "Financial_security")

mydat3 <- load_and_transform("filtered_economic_Nutrient_cycling_Profit.csv",
                             "Nutrient_cycling", "Profit")
mod3 <- run_model(mydat3, "Nutrient_cycling", "Profit")

mydat4 <- load_and_transform("filtered_economic_Nutrient_cycling_Savings_labour_cost.csv",
                             "Nutrient_cycling", "Savings_labour_cost")
mod4 <- run_model(mydat4, "Nutrient_cycling", "Savings_labour_cost")

mydat5 <- load_and_transform("filtered_economic_Pest_regulation_Profit.csv",
                             "Pest_regulation", "Profit")
mod5 <- run_model(mydat5, "Pest_regulation", "Profit")

mydat6 <- load_and_transform("filtered_economic_Primary_production_Financial_security.csv",
                             "Primary_production", "Financial_security")
mod6 <- run_model(mydat6, "Primary_production", "Financial_security")

mydat7 <- load_and_transform("filtered_economic_Primary_production_Profit.csv",
                             "Primary_production", "Profit")
mod7 <- run_model(mydat7, "Primary_production", "Profit")

mydat8 <- load_and_transform("filtered_economic_Primary_production_Savings_labour_cost.csv",
                             "Primary_production", "Savings_labour_cost")
mod8 <- run_model(mydat8, "Primary_production", "Savings_labour_cost")

mydat9 <- load_and_transform("filtered_economic_Soil_formation_Profit.csv",
                             "Soil_formation", "Profit")
mod9 <- run_model(mydat9, "Soil_formation", "Profit")

mydat10 <- load_and_transform("filtered_economic_Soil_formation_Savings_labour_cost.csv",
                              "Soil_formation", "Savings_labour_cost")
mod10 <- run_model(mydat10, "Soil_formation", "Savings_labour_cost")

mydat11 <- load_and_transform("filtered_economic_Water_regulation_Profit.csv",
                              "Water_regulation", "Profit")
mod11 <- run_model(mydat11, "Water_regulation", "Profit")

mydat12 <- load_and_transform("filtered_economic_Yield_Financial_security.csv",
                              "Yieldeqcumul", "Financial_security", rename_yield = TRUE)
mod12 <- run_model(mydat12, "Yieldeqcumul", "Financial_security",
                   x_label = "Equivalent & Cumulative Yield")

mydat13 <- load_and_transform("filtered_economic_Yield_Physical_capital.csv",
                              "Yieldeqcumul", "Physical_capital", rename_yield = TRUE)
mod13 <- run_model(mydat13, "Yieldeqcumul", "Physical_capital",
                   x_label = "Equivalent & Cumulative Yield")

mydat14 <- load_and_transform("filtered_economic_Yield_Profit.csv",
                              "Yieldeqcumul", "Profit", rename_yield = TRUE)
mod14 <- run_model(mydat14, "Yieldeqcumul", "Profit",
                   x_label = "Equivalent & Cumulative Yield")

mydat15 <- load_and_transform("filtered_economic_Yield_Savings_labour_cost.csv",
                              "Yieldeqcumul", "Savings_labour_cost", rename_yield = TRUE)
mod15 <- run_model(mydat15, "Yieldeqcumul", "Savings_labour_cost",
                   x_label = "Equivalent & Cumulative Yield")

#===============================================================================
# NON-ECONOMIC MODELS (mod16–mod28)
#===============================================================================

mydat16 <- load_and_transform("filtered_Non_economic_Climate_regulation_Natural_capital.csv",
                              "Climate_regulation", "Natural_capital")
mod16 <- run_model(mydat16, "Climate_regulation", "Natural_capital")

mydat17 <- load_and_transform("filtered_Non_economic_Nutrient_cycling_Health.csv",
                              "Nutrient_cycling", "Health")
mod17 <- run_model(mydat17, "Nutrient_cycling", "Health")

mydat18 <- load_and_transform("filtered_Non_economic_Nutrient_cycling_Natural_capital.csv",
                              "Nutrient_cycling", "Natural_capital")
mod18 <- run_model(mydat18, "Nutrient_cycling", "Natural_capital")

mydat19 <- load_and_transform("filtered_Non_economic_Pest_regulation_Natural_capital.csv",
                              "Pest_regulation", "Natural_capital")
mod19 <- run_model(mydat19, "Pest_regulation", "Natural_capital")

mydat20 <- load_and_transform("filtered_Non_economic_Primary_production_Food_security.csv",
                              "Primary_production", "Food_security")
mod20 <- run_model(mydat20, "Primary_production", "Food_security")

mydat21 <- load_and_transform("filtered_Non_economic_Primary_production_Health.csv",
                              "Primary_production", "Health")
mod21 <- run_model(mydat21, "Primary_production", "Health")

mydat22 <- load_and_transform("filtered_Non_economic_Primary_production_Natural_capital.csv",
                              "Primary_production", "Natural_capital")
mod22 <- run_model(mydat22, "Primary_production", "Natural_capital")

mydat23 <- load_and_transform("filtered_Non_economic_Water_regulation_Employment.csv",
                              "Water_regulation", "Employment")
mod23 <- run_model(mydat23, "Water_regulation", "Employment")

mydat24 <- load_and_transform("filtered_Non_economic_Water_regulation_Natural_capital.csv",
                              "Water_regulation", "Natural_capital")
mod24 <- run_model(mydat24, "Water_regulation", "Natural_capital")

mydat25 <- load_and_transform("filtered_Non_economic_Yield_Employment.csv",
                              "Yieldeqcumul", "Employment", rename_yield = TRUE)
mod25 <- run_model(mydat25, "Yieldeqcumul", "Employment",
                   x_label = "Equivalent & Cumulative Yield")

mydat26 <- load_and_transform("filtered_Non_economic_Yield_Food_security.csv",
                              "Yieldeqcumul", "Food_security", rename_yield = TRUE)
mod26 <- run_model(mydat26, "Yieldeqcumul", "Food_security",
                   x_label = "Equivalent & Cumulative Yield")

mydat27 <- load_and_transform("filtered_Non_economic_Yield_Health.csv",
                              "Yieldeqcumul", "Health", rename_yield = TRUE)
mod27 <- run_model(mydat27, "Yieldeqcumul", "Health",
                   x_label = "Equivalent & Cumulative Yield")

mydat28 <- load_and_transform("filtered_Non_economic_Yield_Natural_capital.csv",
                              "Yieldeqcumul", "Natural_capital", rename_yield = TRUE)
mod28 <- run_model(mydat28, "Yieldeqcumul", "Natural_capital",
                   x_label = "Equivalent & Cumulative Yield")

#===============================================================================
## SUMMARY OF ALL MODELS
#===============================================================================

model_list <- list(
  mod1,  mod2,  mod3,  mod4,  mod5,  mod6,  mod7,
  mod8,  mod9,  mod10, mod11, mod12, mod13, mod14,
  mod15, mod16, mod17, mod18, mod19, mod20, mod21,
  mod22, mod23, mod24, mod25, mod26, mod27, mod28
)

economic_models <- 1:15

# File 1: Full model summaries + significant variables
sink("model_summaries.txt")

for (i in seq_along(model_list)) {
  cat("Summary of mod", i, ":\n")
  print(summary(model_list[[i]]))
  
  p_values <- summary(model_list[[i]])$coefficients[, 4]
  significant_vars <- names(p_values[p_values < 0.05])
  
  if (length(significant_vars) > 0) {
    cat("\nSignificant variables (p < 0.05):",
        paste(significant_vars, collapse = ", "), "\n")
  } else {
    cat("\nNo significant variables found in mod", i, "\n")
  }
  
  cat("\n----------------------------------\n")
}

sink()

# File 2: Models with at least one significant independent variable
sink("significant_models_summary_2.txt")

for (i in seq_along(model_list)) {
  model_formula    <- formula(model_list[[i]])
  dependent_var    <- as.character(model_formula[[2]])
  independent_vars <- all.vars(model_formula[[3]])
  p_values         <- summary(model_list[[i]])$coefficients[, 4]
  
  significant_vars <- names(p_values[p_values < 0.05 & names(p_values) != "(Intercept)"])
  
  if (length(significant_vars) > 0) {
    cat("Model", i, ":\n")
    cat("Dependent variable:", dependent_var, "\n")
    cat("Independent variables:", paste(independent_vars, collapse = ", "), "\n")
    cat("Significant independent variables:", paste(significant_vars, collapse = ", "), "\n")
    cat("\n----------------------------------\n")
  }
}

sink()

