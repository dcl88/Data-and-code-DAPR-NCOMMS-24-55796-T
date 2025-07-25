################################################################################
# Disaggregated  (pairwise) models for Social Ecological Data on Legume Introduction Food Systems
# 2025
################################################################################

#-------------------------------------------------------------------------------
###Load data###
library(lme4)
library(ggplot2)
library(lattice)
library(dplyr)
library(devtools)
library(labdsv) #for function matrify()
library(dplyr)
library(tibble)
library(nlme) # to get P values for models
library(lmerTest) # to get P values for models
library(DescTools) #Pseudo R2
library(jtools) #get model outputs in nice formats
library(coefplot) #plot model coefficients
library(forestplot) #forestplot of coefficients
library(performance) #model performance statistics like nagakawa's R2
library(ggeffects)
library(tidyverse)
library(interactions)
library(car)
library(rcompanion)
library(bestNormalize)
library(mgcv)
library(psych)



  
  ###Load data###
  ##set working directory
  workingdir<-"insert filepath here"
  setwd(workingdir)
  getwd()
  
  #mod1-------------------------------------------------------------------------------
  
  ##read in mydat1 (csv file)
  mydat1<- read.csv("filtered_economic_Biodiversity_functions_processes_soil_organic_carbon_Financial_security.csv", header=T)
  head(mydat1)
  names(mydat1) #gives the headings of every column
  
  #Transforming data to percentage
  mydat1$Biodiversity_functions_processes_soil_organic_carbon <- mydat1$Biodiversity_functions_processes_soil_organic_carbon*100-100
  mydat1$Financial_security <- mydat1$Financial_security*100-100
  
  # Fit the nonlinear regression model 
  mod1 <- lm((Financial_security)~ (Biodiversity_functions_processes_soil_organic_carbon), data = mydat1)
  summary(mod1)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod1)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat1$Financial_security  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod1 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod1)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod1, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Biodiversity_functions_processes_soil_organic_carbon'
  s <- seq(min(mydat1$Biodiversity_functions_processes_soil_organic_carbon), max(mydat1$Biodiversity_functions_processes_soil_organic_carbon), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod1 <- predict(mod1, newdata = data.frame(Biodiversity_functions_processes_soil_organic_carbon = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat1$Biodiversity_functions_processes_soil_organic_carbon, mydat1$Financial_security, pch = 16, col = "#147746", 
       xlab = "Biodiversity_functions_processes_soil_organic_carbon", ylab = "Financial_security", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat1$Biodiversity_functions_processes_soil_organic_carbon, na.rm = TRUE), 
       ylim = range(mydat1$Financial_security, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod1[,2], predicted_values_mod1[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod1[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
 
  #mod2-------------------------------------------------------------------------------
  
  ##read in mydat2 (csv file)
  mydat2<- read.csv("filtered_economic_Biodiversity_functions_processes_soil_organic_carbon_Profit.csv", header=T)
  head(mydat2)
  names(mydat2) #gives the headings of every column
  
  #Transforming data to percentage
  mydat2$Biodiversity_functions_processes_soil_organic_carbon <- mydat2$Biodiversity_functions_processes_soil_organic_carbon*100-100
  mydat2$Profit <- mydat2$Profit*100-100
  
  # Fit the nonlinear regression model 
  mod2 <- lm((Profit)~ (Biodiversity_functions_processes_soil_organic_carbon), data = mydat2)
  summary(mod2)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod2)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat2$Profit  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod2 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod2)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod2, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Biodiversity_functions_processes_soil_organic_carbon'
  s <- seq(min(mydat2$Biodiversity_functions_processes_soil_organic_carbon), max(mydat2$Biodiversity_functions_processes_soil_organic_carbon), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod2 <- predict(mod2, newdata = data.frame(Biodiversity_functions_processes_soil_organic_carbon = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat2$Biodiversity_functions_processes_soil_organic_carbon, mydat2$Profit, pch = 16, col = "#147746", 
       xlab = "Biodiversity_functions_processes_soil_organic_carbon", ylab = "Profit", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat2$Biodiversity_functions_processes_soil_organic_carbon, na.rm = TRUE), 
       ylim = range(mydat2$Profit, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod2[,2], predicted_values_mod2[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod2[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod3-------------------------------------------------------------------------------
  
  ##read in mydat3 (csv file)
  mydat3<- read.csv("filtered_economic_Biodiversity_functions_processes_soil_organic_carbon_Savings_labour_cost.csv", header=T)
  head(mydat3)
  names(mydat3) #gives the headings of every column
  
  #Transforming data to percentage
  mydat3$Biodiversity_functions_processes_soil_organic_carbon <- mydat3$Biodiversity_functions_processes_soil_organic_carbon*100-100
  mydat3$Savings_labour_cost <- mydat3$Savings_labour_cost*100-100
  
  # Fit the nonlinear regression model 
  mod3 <- lm((Savings_labour_cost)~ (Biodiversity_functions_processes_soil_organic_carbon), data = mydat3)
  summary(mod3)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod3)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat3$Savings_labour_cost  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod3 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod3)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod3, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Biodiversity_functions_processes_soil_organic_carbon'
  s <- seq(min(mydat3$Biodiversity_functions_processes_soil_organic_carbon), max(mydat3$Biodiversity_functions_processes_soil_organic_carbon), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod3 <- predict(mod3, newdata = data.frame(Biodiversity_functions_processes_soil_organic_carbon = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat3$Biodiversity_functions_processes_soil_organic_carbon, mydat3$Savings_labour_cost, pch = 16, col = "#147746", 
       xlab = "Biodiversity_functions_processes_soil_organic_carbon", ylab = "Savings_labour_cost", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat3$Biodiversity_functions_processes_soil_organic_carbon, na.rm = TRUE), 
       ylim = range(mydat3$Savings_labour_cost, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod3[,2], predicted_values_mod3[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod3[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  #mod4-------------------------------------------------------------------------------
  
  ##read in mydat4 (csv file)
  mydat4<- read.csv("filtered_economic_Climate_regulation_Profit.csv", header=T)
  head(mydat4)
  names(mydat4) #gives the headings of every column
  
  #Transforming data to percentage
  mydat4$Climate_regulation <- mydat4$Climate_regulation*100-100
  mydat4$Profit <- mydat4$Profit*100-100
  
  # Fit the nonlinear regression model 
  mod4 <- lm((Profit)~ (Climate_regulation), data = mydat4)
  summary(mod4)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod4)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat4$Profit  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod4 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod4)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod4, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Climate_regulation'
  s <- seq(min(mydat4$Climate_regulation), max(mydat4$Climate_regulation), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod4 <- predict(mod4, newdata = data.frame(Climate_regulation = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat4$Climate_regulation, mydat4$Profit, pch = 16, col = "#147746", 
       xlab = "Climate_regulation", ylab = "Profit", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat4$Climate_regulation, na.rm = TRUE), 
       ylim = range(mydat4$Profit, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod4[,2], predicted_values_mod4[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod4[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  #mod5-------------------------------------------------------------------------------
  
  ##read in mydat5 (csv file)
  mydat5<- read.csv("filtered_economic_Equivalent_cumulative_yield_Financial_security.csv", header=T)
  head(mydat5)
  names(mydat5) #gives the headings of every column
  mydat5 <- mydat5 %>% rename(Yieldeqcumul = Yield) #renamed Yield to Yieldeqcumul
  
  #Transforming data to percentage
  mydat5$Yieldeqcumul <- mydat5$Yieldeqcumul*100-100
  mydat5$Financial_security <- mydat5$Financial_security*100-100
  
  # Fit the nonlinear regression model 
  mod5 <- lm((Financial_security)~ (Yieldeqcumul), data = mydat5)
  summary(mod5)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod5)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat5$Financial_security  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod5 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod5)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod5, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Yieldeqcumul'
  s <- seq(min(mydat5$Yieldeqcumul), max(mydat5$Yieldeqcumul), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod5 <- predict(mod5, newdata = data.frame(Yieldeqcumul = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat5$Yieldeqcumul, mydat5$Financial_security, pch = 16, col = "#147746", 
       xlab = "Equivalent & Cumulative Yield", ylab = "Financial_security", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat5$Yieldeqcumul, na.rm = TRUE), 
       ylim = range(mydat5$Financial_security, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod5[,2], predicted_values_mod5[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod5[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod6-------------------------------------------------------------------------------
  
  ##read in mydat6 (csv file)
  mydat6<- read.csv("filtered_economic_Equivalent_cumulative_yield_Physical_capital.csv", header=T)
  head(mydat6)
  names(mydat6) #gives the headings of every column
  mydat6 <- mydat6 %>% rename(Yieldeqcumul = Yield) #renamed Yield to Yieldeqcumul
  
  #Transforming data to percentage
  mydat6$Yieldeqcumul <- mydat6$Yieldeqcumul*100-100
  mydat6$Physical_capital <- mydat6$Physical_capital*100-100
  
  # Fit the nonlinear regression model 
  mod6 <- lm((Physical_capital)~ (Yieldeqcumul), data = mydat6)
  summary(mod6)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod6)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat6$Physical_capital  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod6 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod6)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod6, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Yieldeqcumul'
  s <- seq(min(mydat6$Yieldeqcumul), max(mydat6$Yieldeqcumul), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod6 <- predict(mod6, newdata = data.frame(Yieldeqcumul = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat6$Yieldeqcumul, mydat6$Physical_capital, pch = 16, col = "#147746", 
       xlab = "Equivalent & Cumulative Yield", ylab = "Physical_capital", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat6$Yieldeqcumul, na.rm = TRUE), 
       ylim = range(mydat6$Physical_capital, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod6[,2], predicted_values_mod6[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod6[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  #mod7-------------------------------------------------------------------------------
  
  ##read in mydat7 (csv file)
  mydat7<- read.csv("filtered_economic_Equivalent_cumulative_yield_Profit.csv", header=T)
  head(mydat7)
  names(mydat7) #gives the headings of every column
  mydat7 <- mydat7 %>% rename(Yieldeqcumul = Yield) #renamed Yield to Yieldeqcumul
  
  #Transforming data to percentage
  mydat7$Yieldeqcumul <- mydat7$Yieldeqcumul*100-100
  mydat7$Profit <- mydat7$Profit*100-100
  
 # Fit the nonlinear regression model 
  mod7 <- lm((Profit)~ (Yieldeqcumul), data = mydat7)
  summary(mod7)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod7)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat7$Profit  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod7 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod7)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod7, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Yieldeqcumul'
  s <- seq(min(mydat7$Yieldeqcumul), max(mydat7$Yieldeqcumul), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod7 <- predict(mod7, newdata = data.frame(Yieldeqcumul = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
    par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat7$Yieldeqcumul, mydat7$Profit, pch = 16, col = "#147746", 
       xlab = "Equivalent & Cumulative Yield", ylab = "Profit", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat7$Yieldeqcumul, na.rm = TRUE), 
       ylim = range(mydat7$Profit, na.rm = TRUE),
       axes = FALSE)
  
 
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod7[,2], predicted_values_mod7[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod7[,1], col = "#1DB954", lwd = 3)
  
# Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  

  
  #mod8-------------------------------------------------------------------------------
  
  ##read in mydat8 (csv file)
  mydat8<- read.csv("filtered_economic_Equivalent_cumulative_yield_Savings_labour_cost.csv", header=T)
  head(mydat8)
  names(mydat8) #gives the headings of every column
  mydat8 <- mydat8 %>% rename(Yieldeqcumul = Yield) #renamed Yield to Yieldeqcumul
  
  #Transforming data to percentage
  mydat8$Yieldeqcumul <- mydat8$Yieldeqcumul*100-100
  mydat8$Savings_labour_cost <- mydat8$Savings_labour_cost*100-100
  
  # Fit the nonlinear regression model 
  mod8 <- lm((Savings_labour_cost)~ (Yieldeqcumul), data = mydat8)
  summary(mod8)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod8)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat8$Savings_labour_cost  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod8 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod8)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod8, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Yieldeqcumul'
  s <- seq(min(mydat8$Yieldeqcumul), max(mydat8$Yieldeqcumul), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod8 <- predict(mod8, newdata = data.frame(Yieldeqcumul = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat8$Yieldeqcumul, mydat8$Savings_labour_cost, pch = 16, col = "#147746", 
       xlab = "Equivalent & Cumulative Yield", ylab = "Savings_labour_cost", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat8$Yieldeqcumul, na.rm = TRUE), 
       ylim = range(mydat8$Savings_labour_cost, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod8[,2], predicted_values_mod8[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod8[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod9-------------------------------------------------------------------------------
  
  ##read in mydat9 (csv file)
  mydat9<- read.csv("filtered_economic_Non_legume_yield_only_Profit.csv", header=T)
  head(mydat9)
  names(mydat9) #gives the headings of every column
  mydat9 <- mydat9 %>% rename(Non_legume_yield_only = Yield) #renamed Yield to Non_legume_yield_only
  
  #Transforming data to percentage
  mydat9$Non_legume_yield_only <- mydat9$Non_legume_yield_only*100-100
  mydat9$Profit <- mydat9$Profit*100-100
  
  # Fit the nonlinear regression model 
  mod9 <- lm((Profit)~ (Non_legume_yield_only), data = mydat9)
  summary(mod9)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod9)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat9$Profit  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod9 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod9)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod9, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Non_legume_yield_only'
  s <- seq(min(mydat9$Non_legume_yield_only), max(mydat9$Non_legume_yield_only), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod9 <- predict(mod9, newdata = data.frame(Non_legume_yield_only = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat9$Non_legume_yield_only, mydat9$Profit, pch = 16, col = "#147746", 
       xlab = "Non_legume_yield_only", ylab = "Profit", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat9$Non_legume_yield_only, na.rm = TRUE), 
       ylim = range(mydat9$Profit, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod9[,2], predicted_values_mod9[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod9[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod10-------------------------------------------------------------------------------
  
  ##read in mydat10 (csv file)
  mydat10<- read.csv("filtered_economic_Nutrient_cycling_Financial_security.csv", header=T)
  head(mydat10)
  names(mydat10) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat10$Nutrient_cycling <- mydat10$Nutrient_cycling*100-100
  mydat10$Financial_security <- mydat10$Financial_security*100-100
  
  # Fit the nonlinear regression model 
  mod10 <- lm((Financial_security)~ (Nutrient_cycling), data = mydat10)
  summary(mod10)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod10)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat10$Financial_security  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod10 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod10)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod10, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Nutrient_cycling'
  s <- seq(min(mydat10$Nutrient_cycling), max(mydat10$Nutrient_cycling), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod10 <- predict(mod10, newdata = data.frame(Nutrient_cycling = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat10$Nutrient_cycling, mydat10$Financial_security, pch = 16, col = "#147746", 
       xlab = "Nutrient_cycling", ylab = "Financial_security", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat10$Nutrient_cycling, na.rm = TRUE), 
       ylim = range(mydat10$Financial_security, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod10[,2], predicted_values_mod10[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod10[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod11-------------------------------------------------------------------------------
  
  ##read in mydat11 (csv file)
  mydat11<- read.csv("filtered_economic_Nutrient_cycling_Profit.csv", header=T)
  head(mydat11)
  names(mydat11) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat11$Nutrient_cycling <- mydat11$Nutrient_cycling*100-100
  mydat11$Profit <- mydat11$Profit*100-100
  
  # Fit the nonlinear regression model 
  mod11 <- lm((Profit)~ (Nutrient_cycling), data = mydat11)
  summary(mod11)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod11)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat11$Profit  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod11 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod11)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod11, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Nutrient_cycling'
  s <- seq(min(mydat11$Nutrient_cycling), max(mydat11$Nutrient_cycling), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod11 <- predict(mod11, newdata = data.frame(Nutrient_cycling = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat11$Nutrient_cycling, mydat11$Profit, pch = 16, col = "#147746", 
       xlab = "Nutrient_cycling", ylab = "Profit", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat11$Nutrient_cycling, na.rm = TRUE), 
       ylim = range(mydat11$Profit, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod11[,2], predicted_values_mod11[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod11[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod12-------------------------------------------------------------------------------
  
  ##read in mydat12 (csv file)
  mydat12<- read.csv("filtered_economic_Nutrient_cycling_Savings_labour_cost.csv", header=T)
  head(mydat12)
  names(mydat12) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat12$Nutrient_cycling <- mydat12$Nutrient_cycling*100-100
  mydat12$Savings_labour_cost <- mydat12$Savings_labour_cost*100-100
  
  # Fit the nonlinear regression model 
  mod12 <- lm((Savings_labour_cost)~ (Nutrient_cycling), data = mydat12)
  summary(mod12)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod12)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat12$Savings_labour_cost  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod12 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod12)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod12, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Nutrient_cycling'
  s <- seq(min(mydat12$Nutrient_cycling), max(mydat12$Nutrient_cycling), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod12 <- predict(mod12, newdata = data.frame(Nutrient_cycling = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat12$Nutrient_cycling, mydat12$Savings_labour_cost, pch = 16, col = "#147746", 
       xlab = "Nutrient_cycling", ylab = "Savings_labour_cost", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat12$Nutrient_cycling, na.rm = TRUE), 
       ylim = range(mydat12$Savings_labour_cost, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod12[,2], predicted_values_mod12[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod12[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod13-------------------------------------------------------------------------------
  
  ##read in mydat13 (csv file)
  mydat13<- read.csv("filtered_economic_Pest_regulation_Profit.csv", header=T)
  head(mydat13)
  names(mydat13) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat13$Pest_regulation <- mydat13$Pest_regulation*100-100
  mydat13$Profit <- mydat13$Profit*100-100
  
  # Fit the nonlinear regression model 
  mod13 <- lm((Profit)~ (Pest_regulation), data = mydat13)
  summary(mod13)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod13)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat13$Profit  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod13 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod13)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod13, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Pest_regulation'
  s <- seq(min(mydat13$Pest_regulation), max(mydat13$Pest_regulation), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod13 <- predict(mod13, newdata = data.frame(Pest_regulation = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat13$Pest_regulation, mydat13$Profit, pch = 16, col = "#147746", 
       xlab = "Pest_regulation", ylab = "Profit", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat13$Pest_regulation, na.rm = TRUE), 
       ylim = range(mydat13$Profit, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod13[,2], predicted_values_mod13[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod13[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  #mod14-------------------------------------------------------------------------------
  
  ##read in mydat14 (csv file)
  mydat14<- read.csv("filtered_economic_Primary_production_Financial_security.csv", header=T)
  head(mydat14)
  names(mydat14) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat14$Primary_production <- mydat14$Primary_production*100-100
  mydat14$Financial_security <- mydat14$Financial_security*100-100
  
  # Fit the nonlinear regression model 
  mod14 <- lm((Financial_security)~ (Primary_production), data = mydat14)
  summary(mod14)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod14)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat14$Financial_security  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod14 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod14)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod14, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Primary_production'
  s <- seq(min(mydat14$Primary_production), max(mydat14$Primary_production), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod14 <- predict(mod14, newdata = data.frame(Primary_production = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat14$Primary_production, mydat14$Financial_security, pch = 16, col = "#147746", 
       xlab = "Primary_production", ylab = "Financial_security", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat14$Primary_production, na.rm = TRUE), 
       ylim = range(mydat14$Financial_security, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod14[,2], predicted_values_mod14[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod14[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  #mod15-------------------------------------------------------------------------------
  
  ##read in mydat15 (csv file)
  mydat15<- read.csv("filtered_economic_Primary_production_Profit.csv", header=T)
  head(mydat15)
  names(mydat15) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat15$Primary_production <- mydat15$Primary_production*100-100
  mydat15$Profit <- mydat15$Profit*100-100
  
  # Fit the nonlinear regression model 
  mod15 <- lm((Profit)~ (Primary_production), data = mydat15)
  summary(mod15)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod15)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat15$Profit  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod15 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod15)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod15, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Primary_production'
  s <- seq(min(mydat15$Primary_production), max(mydat15$Primary_production), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod15 <- predict(mod15, newdata = data.frame(Primary_production = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat15$Primary_production, mydat15$Profit, pch = 16, col = "#147746", 
       xlab = "Primary_production", ylab = "Profit", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat15$Primary_production, na.rm = TRUE), 
       ylim = range(mydat15$Profit, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod15[,2], predicted_values_mod15[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod15[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  #mod16-------------------------------------------------------------------------------
  
  ##read in mydat16 (csv file)
  mydat16<- read.csv("filtered_economic_Primary_production_Savings_labour_cost.csv", header=T)
  head(mydat16)
  names(mydat16) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat16$Primary_production <- mydat16$Primary_production*100-100
  mydat16$Savings_labour_cost <- mydat16$Savings_labour_cost*100-100
  
  # Fit the nonlinear regression model 
  mod16 <- lm((Savings_labour_cost)~ (Primary_production), data = mydat16)
  summary(mod16)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod16)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat16$Savings_labour_cost  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod16 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod16)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod16, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Primary_production'
  s <- seq(min(mydat16$Primary_production), max(mydat16$Primary_production), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod16 <- predict(mod16, newdata = data.frame(Primary_production = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat16$Primary_production, mydat16$Savings_labour_cost, pch = 16, col = "#147746", 
       xlab = "Primary_production", ylab = "Savings_labour_cost", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat16$Primary_production, na.rm = TRUE), 
       ylim = range(mydat16$Savings_labour_cost, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod16[,2], predicted_values_mod16[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod16[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  #mod17-------------------------------------------------------------------------------
  
  ##read in mydat17 (csv file)
  mydat17<- read.csv("filtered_economic_Soil_formation_Profit.csv", header=T)
  head(mydat17)
  names(mydat17) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat17$Soil_formation <- mydat17$Soil_formation*100-100
  mydat17$Profit <- mydat17$Profit*100-100
  
  # Fit the nonlinear regression model 
  mod17 <- lm((Profit)~ (Soil_formation), data = mydat17)
  summary(mod17)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod17)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat17$Profit  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod17 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod17)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod17, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Soil_formation'
  s <- seq(min(mydat17$Soil_formation), max(mydat17$Soil_formation), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod17 <- predict(mod17, newdata = data.frame(Soil_formation = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat17$Soil_formation, mydat17$Profit, pch = 16, col = "#147746", 
       xlab = "Soil_formation", ylab = "Profit", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat17$Soil_formation, na.rm = TRUE), 
       ylim = range(mydat17$Profit, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod17[,2], predicted_values_mod17[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod17[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod18-------------------------------------------------------------------------------
  
  ##read in mydat18 (csv file)
  mydat18<- read.csv("filtered_economic_Soil_formation_Savings_labour_cost.csv", header=T)
  head(mydat18)
  names(mydat18) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat18$Soil_formation <- mydat18$Soil_formation*100-100
  mydat18$Savings_labour_cost <- mydat18$Savings_labour_cost*100-100
  
  # Fit the nonlinear regression model 
  mod18 <- lm((Savings_labour_cost)~ (Soil_formation), data = mydat18)
  summary(mod18)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod18)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat18$Savings_labour_cost  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod18 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod18)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod18, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Soil_formation'
  s <- seq(min(mydat18$Soil_formation), max(mydat18$Soil_formation), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod18 <- predict(mod18, newdata = data.frame(Soil_formation = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat18$Soil_formation, mydat18$Savings_labour_cost, pch = 16, col = "#147746", 
       xlab = "Soil_formation", ylab = "Savings_labour_cost", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat18$Soil_formation, na.rm = TRUE), 
       ylim = range(mydat18$Savings_labour_cost, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod18[,2], predicted_values_mod18[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod18[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod19-------------------------------------------------------------------------------
  
  ##read in mydat19 (csv file)
  mydat19<- read.csv("filtered_economic_Water_regulation_Profit.csv", header=T)
  head(mydat19)
  names(mydat19) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat19$Water_regulation <- mydat19$Water_regulation*100-100
  mydat19$Profit <- mydat19$Profit*100-100
  
  # Fit the nonlinear regression model 
  mod19 <- lm((Profit)~ (Water_regulation), data = mydat19)
  summary(mod19)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod19)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat19$Profit  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod19 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod19)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod19, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Water_regulation'
  s <- seq(min(mydat19$Water_regulation), max(mydat19$Water_regulation), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod19 <- predict(mod19, newdata = data.frame(Water_regulation = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat19$Water_regulation, mydat19$Profit, pch = 16, col = "#147746", 
       xlab = "Water_regulation", ylab = "Profit", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat19$Water_regulation, na.rm = TRUE), 
       ylim = range(mydat19$Profit, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod19[,2], predicted_values_mod19[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod19[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod20-------------------------------------------------------------------------------
  
  ##read in mydat20 (csv file)
  mydat20<- read.csv("filtered_Non_economic_Biodiversity_functions_processes_soil_organic_carbon_Employment.csv", header=T)
  head(mydat20)
  names(mydat20) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat20$Biodiversity_functions_processes_soil_organic_carbon <- mydat20$Biodiversity_functions_processes_soil_organic_carbon*100-100
  mydat20$Employment <- mydat20$Employment*100-100
  
  # Fit the nonlinear regression model 
  mod20 <- lm((Employment)~ (Biodiversity_functions_processes_soil_organic_carbon), data = mydat20)
  summary(mod20)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod20)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat20$Employment  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod20 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod20)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod20, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Biodiversity_functions_processes_soil_organic_carbon'
  s <- seq(min(mydat20$Biodiversity_functions_processes_soil_organic_carbon), max(mydat20$Biodiversity_functions_processes_soil_organic_carbon), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod20 <- predict(mod20, newdata = data.frame(Biodiversity_functions_processes_soil_organic_carbon = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat20$Biodiversity_functions_processes_soil_organic_carbon, mydat20$Employment, pch = 16, col = "#147746", 
       xlab = "Biodiversity_functions_processes_soil_organic_carbon", ylab = "Employment", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat20$Biodiversity_functions_processes_soil_organic_carbon, na.rm = TRUE), 
       ylim = range(mydat20$Employment, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod20[,2], predicted_values_mod20[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod20[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod21-------------------------------------------------------------------------------
  
  ##read in mydat21 (csv file)
  mydat21<- read.csv("filtered_Non_economic_Biodiversity_functions_processes_soil_organic_carbon_Food_security.csv", header=T)
  head(mydat21)
  names(mydat21) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat21$Biodiversity_functions_processes_soil_organic_carbon <- mydat21$Biodiversity_functions_processes_soil_organic_carbon*100-100
  mydat21$Food_security <- mydat21$Food_security*100-100
  
  # Fit the nonlinear regression model 
  mod21 <- lm((Food_security)~ (Biodiversity_functions_processes_soil_organic_carbon), data = mydat21)
  summary(mod21)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod21)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat21$Food_security  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod21 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod21)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod21, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Biodiversity_functions_processes_soil_organic_carbon'
  s <- seq(min(mydat21$Biodiversity_functions_processes_soil_organic_carbon), max(mydat21$Biodiversity_functions_processes_soil_organic_carbon), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod21 <- predict(mod21, newdata = data.frame(Biodiversity_functions_processes_soil_organic_carbon = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat21$Biodiversity_functions_processes_soil_organic_carbon, mydat21$Food_security, pch = 16, col = "#147746", 
       xlab = "Biodiversity_functions_processes_soil_organic_carbon", ylab = "Food_security", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat21$Biodiversity_functions_processes_soil_organic_carbon, na.rm = TRUE), 
       ylim = range(mydat21$Food_security, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod21[,2], predicted_values_mod21[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod21[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
 
  
  #mod22-------------------------------------------------------------------------------
  
  ##read in mydat22 (csv file)
  mydat22<- read.csv("filtered_Non_economic_Biodiversity_functions_processes_soil_organic_carbon_Natural_capital.csv", header=T)
  head(mydat22)
  names(mydat22) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat22$Biodiversity_functions_processes_soil_organic_carbon <- mydat22$Biodiversity_functions_processes_soil_organic_carbon*100-100
  mydat22$Natural_capital <- mydat22$Natural_capital*100-100
  
  # Fit the nonlinear regression model 
  mod22 <- lm((Natural_capital)~ (Biodiversity_functions_processes_soil_organic_carbon), data = mydat22)
  summary(mod22)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod22)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat22$Natural_capital  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod22 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod22)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod22, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Biodiversity_functions_processes_soil_organic_carbon'
  s <- seq(min(mydat22$Biodiversity_functions_processes_soil_organic_carbon), max(mydat22$Biodiversity_functions_processes_soil_organic_carbon), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod22 <- predict(mod22, newdata = data.frame(Biodiversity_functions_processes_soil_organic_carbon = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat22$Biodiversity_functions_processes_soil_organic_carbon, mydat22$Natural_capital, pch = 16, col = "#147746", 
       xlab = "Biodiversity_functions_processes_soil_organic_carbon", ylab = "Natural_capital", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat22$Biodiversity_functions_processes_soil_organic_carbon, na.rm = TRUE), 
       ylim = range(mydat22$Natural_capital, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod22[,2], predicted_values_mod22[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod22[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  #mod23-------------------------------------------------------------------------------
  
  ##read in mydat23 (csv file)
  mydat23<- read.csv("filtered_Non_economic_Climate_regulation_Natural_capital.csv", header=T)
  head(mydat23)
  names(mydat23) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat23$Climate_regulation <- mydat23$Climate_regulation*100-100
  mydat23$Natural_capital <- mydat23$Natural_capital*100-100
  
  # Fit the nonlinear regression model 
  mod23 <- lm((Natural_capital)~ (Climate_regulation), data = mydat23)
  summary(mod23)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod23)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat23$Natural_capital  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod23 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod23)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod23, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Climate_regulation'
  s <- seq(min(mydat23$Climate_regulation), max(mydat23$Climate_regulation), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod23 <- predict(mod23, newdata = data.frame(Climate_regulation = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat23$Climate_regulation, mydat23$Natural_capital, pch = 16, col = "#147746", 
       xlab = "Climate_regulation", ylab = "Natural_capital", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat23$Climate_regulation, na.rm = TRUE), 
       ylim = range(mydat23$Natural_capital, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod23[,2], predicted_values_mod23[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod23[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  
  #mod24-------------------------------------------------------------------------------
  
  ##read in mydat24 (csv file)
  mydat24<- read.csv("filtered_Non_economic_Equivalent_cumulative_yield_Employment.csv", header=T)
  head(mydat24)
  names(mydat24) #gives the headings of every column
  mydat24 <- mydat24 %>% rename(Yieldeqcumul = Yield) #renamed Yield to Yieldeqcumul
  
  #Transforming data to percentage
  mydat24$Yieldeqcumul <- mydat24$Yieldeqcumul*100-100
  mydat24$Employment <- mydat24$Employment*100-100
  
  # Fit the nonlinear regression model 
  mod24 <- lm((Employment)~ (Yieldeqcumul), data = mydat24)
  summary(mod24)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod24)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat24$Employment  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod24 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod24)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod24, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Yieldeqcumul'
  s <- seq(min(mydat24$Yieldeqcumul), max(mydat24$Yieldeqcumul), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod24 <- predict(mod24, newdata = data.frame(Yieldeqcumul = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat24$Yieldeqcumul, mydat24$Employment, pch = 16, col = "#147746", 
       xlab = "Equivalent & Cumulative Yield", ylab = "Employment", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat24$Yieldeqcumul, na.rm = TRUE), 
       ylim = range(mydat24$Employment, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod24[,2], predicted_values_mod24[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod24[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
 
  
  #mod25-------------------------------------------------------------------------------
  
  ##read in mydat25 (csv file)
  mydat25<- read.csv("filtered_Non_economic_Equivalent_cumulative_yield_Food_security.csv", header=T)
  head(mydat25)
  names(mydat25) #gives the headings of every column
  mydat25 <- mydat25 %>% rename(Yieldeqcumul = Yield) #renamed Yield to Yieldeqcumul
  
  #Transforming data to percentage
  mydat25$Yieldeqcumul <- mydat25$Yieldeqcumul*100-100
  mydat25$Food_security <- mydat25$Food_security*100-100
  
  # Fit the nonlinear regression model 
  mod25 <- lm((Food_security)~ (Yieldeqcumul), data = mydat25)
  summary(mod25)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod25)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat25$Food_security  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod25 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod25)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod25, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Yieldeqcumul'
  s <- seq(min(mydat25$Yieldeqcumul), max(mydat25$Yieldeqcumul), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod25 <- predict(mod25, newdata = data.frame(Yieldeqcumul = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat25$Yieldeqcumul, mydat25$Food_security, pch = 16, col = "#147746", 
       xlab = "Equivalent & Cumulative Yield", ylab = "Food_security", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat25$Yieldeqcumul, na.rm = TRUE), 
       ylim = range(mydat25$Food_security, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod25[,2], predicted_values_mod25[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod25[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  #mod26-------------------------------------------------------------------------------
  
  ##read in mydat26 (csv file)
  mydat26<- read.csv("filtered_Non_economic_Equivalent_cumulative_yield_Health.csv", header=T)
  head(mydat26)
  names(mydat26) #gives the headings of every column
  mydat26 <- mydat26 %>% rename(Yieldeqcumul = Yield) #renamed Yield to Yieldeqcumul
  
  #Transforming data to percentage
  mydat26$Yieldeqcumul <- mydat26$Yieldeqcumul*100-100
  mydat26$Health <- mydat26$Health*100-100
  
  # Fit the nonlinear regression model 
  mod26 <- lm((Health)~ (Yieldeqcumul), data = mydat26)
  summary(mod26)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod26)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat26$Health  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod26 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod26)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod26, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Yieldeqcumul'
  s <- seq(min(mydat26$Yieldeqcumul), max(mydat26$Yieldeqcumul), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod26 <- predict(mod26, newdata = data.frame(Yieldeqcumul = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat26$Yieldeqcumul, mydat26$Health, pch = 16, col = "#147746", 
       xlab = "Equivalent & Cumulative Yield", ylab = "Health", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat26$Yieldeqcumul, na.rm = TRUE), 
       ylim = range(mydat26$Health, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod26[,2], predicted_values_mod26[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod26[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod27-------------------------------------------------------------------------------
  
  ##read in mydat27 (csv file)
  mydat27<- read.csv("filtered_Non_economic_Equivalent_cumulative_yield_Natural_capital.csv", header=T)
  head(mydat27)
  names(mydat27) #gives the headings of every column
  mydat27 <- mydat27 %>% rename(Yieldeqcumul = Yield) #renamed Yield to Yieldeqcumul
  
  #Transforming data to percentage
  mydat27$Yieldeqcumul <- mydat27$Yieldeqcumul*100-100
  mydat27$Natural_capital <- mydat27$Natural_capital*100-100
  
  # Fit the nonlinear regression model 
  mod27 <- lm((Natural_capital)~ (Yieldeqcumul), data = mydat27)
  summary(mod27)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod27)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat27$Natural_capital  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod27 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod27)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod27, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Yieldeqcumul'
  s <- seq(min(mydat27$Yieldeqcumul), max(mydat27$Yieldeqcumul), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod27 <- predict(mod27, newdata = data.frame(Yieldeqcumul = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat27$Yieldeqcumul, mydat27$Natural_capital, pch = 16, col = "#147746", 
       xlab = "Equivalent & Cumulative Yield", ylab = "Natural_capital", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat27$Yieldeqcumul, na.rm = TRUE), 
       ylim = range(mydat27$Natural_capital, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod27[,2], predicted_values_mod27[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod27[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
 
  
  #mod28-------------------------------------------------------------------------------
  
  ##read in mydat28 (csv file)
  mydat28<- read.csv("filtered_Non_economic_Non_legume_yield_only_Natural_capital.csv", header=T)
  head(mydat28)
  names(mydat28) #gives the headings of every column
  mydat28 <- mydat28 %>% rename(Non_legume_yield_only = Yield) #renamed Yield to Non_legume_yield_only
  
  #Transforming data to percentage
  mydat28$Non_legume_yield_only <- mydat28$Non_legume_yield_only*100-100
  mydat28$Natural_capital <- mydat28$Natural_capital*100-100
  
  # Fit the nonlinear regression model 
  mod28 <- lm((Natural_capital)~ (Non_legume_yield_only), data = mydat28)
  summary(mod28)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod28)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat28$Natural_capital  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod28 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod28)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod28, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Non_legume_yield_only'
  s <- seq(min(mydat28$Non_legume_yield_only), max(mydat28$Non_legume_yield_only), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod28 <- predict(mod28, newdata = data.frame(Non_legume_yield_only = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat28$Non_legume_yield_only, mydat28$Natural_capital, pch = 16, col = "#147746", 
       xlab = "Non_legume_yield_only", ylab = "Natural_capital", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat28$Non_legume_yield_only, na.rm = TRUE), 
       ylim = range(mydat28$Natural_capital, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod28[,2], predicted_values_mod28[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod28[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod29-------------------------------------------------------------------------------
  
  ##read in mydat29 (csv file)
  mydat29<- read.csv("filtered_Non_economic_Nutrient_cycling_Health.csv", header=T)
  head(mydat29)
  names(mydat29) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat29$Nutrient_cycling <- mydat29$Nutrient_cycling*100-100
  mydat29$Health <- mydat29$Health*100-100
  
  # Fit the nonlinear regression model 
  mod29 <- lm((Health)~ (Nutrient_cycling), data = mydat29)
  summary(mod29)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod29)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat29$Health  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod29 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod29)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod29, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Nutrient_cycling'
  s <- seq(min(mydat29$Nutrient_cycling), max(mydat29$Nutrient_cycling), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod29 <- predict(mod29, newdata = data.frame(Nutrient_cycling = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat29$Nutrient_cycling, mydat29$Health, pch = 16, col = "#147746", 
       xlab = "Nutrient_cycling", ylab = "Health", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat29$Nutrient_cycling, na.rm = TRUE), 
       ylim = range(mydat29$Health, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod29[,2], predicted_values_mod29[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod29[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  #mod30-------------------------------------------------------------------------------
  
  ##read in mydat30 (csv file)
  mydat30<- read.csv("filtered_Non_economic_Nutrient_cycling_Natural_capital.csv", header=T)
  head(mydat30)
  names(mydat30) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat30$Nutrient_cycling <- mydat30$Nutrient_cycling*100-100
  mydat30$Natural_capital <- mydat30$Natural_capital*100-100
  
  # Fit the nonlinear regression model 
  mod30 <- lm((Natural_capital)~ (Nutrient_cycling), data = mydat30)
  summary(mod30)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod30)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat30$Natural_capital  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod30 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod30)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod30, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Nutrient_cycling'
  s <- seq(min(mydat30$Nutrient_cycling), max(mydat30$Nutrient_cycling), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod30 <- predict(mod30, newdata = data.frame(Nutrient_cycling = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat30$Nutrient_cycling, mydat30$Natural_capital, pch = 16, col = "#147746", 
       xlab = "Nutrient_cycling", ylab = "Natural_capital", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat30$Nutrient_cycling, na.rm = TRUE), 
       ylim = range(mydat30$Natural_capital, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod30[,2], predicted_values_mod30[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod30[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  #mod31-------------------------------------------------------------------------------
  
  ##read in mydat31 (csv file)
  mydat31<- read.csv("filtered_Non_economic_Pest_regulation_Natural_capital.csv", header=T)
  head(mydat31)
  names(mydat31) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat31$Pest_regulation <- mydat31$Pest_regulation*100-100
  mydat31$Natural_capital <- mydat31$Natural_capital*100-100
  
  # Fit the nonlinear regression model 
  mod31 <- lm((Natural_capital)~ (Pest_regulation), data = mydat31)
  summary(mod31)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod31)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat31$Natural_capital  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod31 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod31)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod31, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Pest_regulation'
  s <- seq(min(mydat31$Pest_regulation), max(mydat31$Pest_regulation), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod31 <- predict(mod31, newdata = data.frame(Pest_regulation = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat31$Pest_regulation, mydat31$Natural_capital, pch = 16, col = "#147746", 
       xlab = "Pest_regulation", ylab = "Natural_capital", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat31$Pest_regulation, na.rm = TRUE), 
       ylim = range(mydat31$Natural_capital, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod31[,2], predicted_values_mod31[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod31[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  #mod32-------------------------------------------------------------------------------
  
  ##read in mydat32 (csv file)
  mydat32<- read.csv("filtered_Non_economic_Primary_production_Food_security.csv", header=T)
  head(mydat32)
  names(mydat32) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat32$Primary_production <- mydat32$Primary_production*100-100
  mydat32$Food_security <- mydat32$Food_security*100-100
  
  # Fit the nonlinear regression model 
  mod32 <- lm((Food_security)~ (Primary_production), data = mydat32)
  summary(mod32)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod32)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat32$Food_security  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod32 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod32)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod32, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Primary_production'
  s <- seq(min(mydat32$Primary_production), max(mydat32$Primary_production), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod32 <- predict(mod32, newdata = data.frame(Primary_production = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat32$Primary_production, mydat32$Food_security, pch = 16, col = "#147746", 
       xlab = "Primary_production", ylab = "Food_security", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat32$Primary_production, na.rm = TRUE), 
       ylim = range(mydat32$Food_security, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod32[,2], predicted_values_mod32[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod32[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  #mod33-------------------------------------------------------------------------------
  
  ##read in mydat33 (csv file)
  mydat33<- read.csv("filtered_Non_economic_Primary_production_Health.csv", header=T)
  head(mydat33)
  names(mydat33) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat33$Primary_production <- mydat33$Primary_production*100-100
  mydat33$Health <- mydat33$Health*100-100
  
  # Fit the nonlinear regression model 
  mod33 <- lm((Health)~ (Primary_production), data = mydat33)
  summary(mod33)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod33)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat33$Health  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod33 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod33)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod33, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Primary_production'
  s <- seq(min(mydat33$Primary_production), max(mydat33$Primary_production), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod33 <- predict(mod33, newdata = data.frame(Primary_production = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat33$Primary_production, mydat33$Health, pch = 16, col = "#147746", 
       xlab = "Primary_production", ylab = "Health", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat33$Primary_production, na.rm = TRUE), 
       ylim = range(mydat33$Health, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod33[,2], predicted_values_mod33[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod33[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  #mod34-------------------------------------------------------------------------------
  
  ##read in mydat34 (csv file)
  mydat34<- read.csv("filtered_Non_economic_Primary_production_Natural_capital.csv", header=T)
  head(mydat34)
  names(mydat34) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat34$Primary_production <- mydat34$Primary_production*100-100
  mydat34$Natural_capital <- mydat34$Natural_capital*100-100
  
  # Fit the nonlinear regression model 
  mod34 <- lm((Natural_capital)~ (Primary_production), data = mydat34)
  summary(mod34)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod34)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat34$Natural_capital  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod34 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod34)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod34, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Primary_production'
  s <- seq(min(mydat34$Primary_production), max(mydat34$Primary_production), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod34 <- predict(mod34, newdata = data.frame(Primary_production = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat34$Primary_production, mydat34$Natural_capital, pch = 16, col = "#147746", 
       xlab = "Primary_production", ylab = "Natural_capital", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat34$Primary_production, na.rm = TRUE), 
       ylim = range(mydat34$Natural_capital, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod34[,2], predicted_values_mod34[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod34[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  
  
  #mod35-------------------------------------------------------------------------------
  
  ##read in mydat35 (csv file)
  mydat35<- read.csv("filtered_Non_economic_Water_regulation_Employment.csv", header=T)
  head(mydat35)
  names(mydat35) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat35$Water_regulation <- mydat35$Water_regulation*100-100
  mydat35$Employment <- mydat35$Employment*100-100
  
  # Fit the nonlinear regression model 
  mod35 <- lm((Employment)~ (Water_regulation), data = mydat35)
  summary(mod35)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod35)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat35$Employment  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod35 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod35)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod35, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Water_regulation'
  s <- seq(min(mydat35$Water_regulation), max(mydat35$Water_regulation), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod35 <- predict(mod35, newdata = data.frame(Water_regulation = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat35$Water_regulation, mydat35$Employment, pch = 16, col = "#147746", 
       xlab = "Water_regulation", ylab = "Employment", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat35$Water_regulation, na.rm = TRUE), 
       ylim = range(mydat35$Employment, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod35[,2], predicted_values_mod35[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod35[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
 
  
  #mod36-------------------------------------------------------------------------------
  
  ##read in mydat36 (csv file)
  mydat36<- read.csv("filtered_Non_economic_Water_regulation_Natural_capital.csv", header=T)
  head(mydat36)
  names(mydat36) #gives the headings of every column
  
  
  #Transforming data to percentage
  mydat36$Water_regulation <- mydat36$Water_regulation*100-100
  mydat36$Natural_capital <- mydat36$Natural_capital*100-100
  
  # Fit the nonlinear regression model 
  mod36 <- lm((Natural_capital)~ (Water_regulation), data = mydat36)
  summary(mod36)
  
  # Calculate the residual sum of squares (RSS) for the model
  RSS_p <- sum(residuals(mod36)^2)
  
  # Calculate the total sum of squares (TSS) for the model
  y <- mydat36$Natural_capital  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure for the model
  R_squared_mod36 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(mod36)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(mod36, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- pred_int[, "lwr"]
  upper <- pred_int[, "upr"]
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Water_regulation'
  s <- seq(min(mydat36$Water_regulation), max(mydat36$Water_regulation), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the model
  predicted_values_mod36 <- predict(mod36, newdata = data.frame(Water_regulation = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  # mar = c(bottom, left, top, right)
  par(mar = c(7, 7, 4, 4) + 0.5)  # Increase the margin space if needed
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat36$Water_regulation, mydat36$Natural_capital, pch = 16, col = "#147746", 
       xlab = "Water_regulation", ylab = "Natural_capital", 
       cex.axis = 2, cex.lab = 2, 
       xlim = range(mydat36$Water_regulation, na.rm = TRUE), 
       ylim = range(mydat36$Natural_capital, na.rm = TRUE),
       axes = FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, predicted_values_mod36[,2], predicted_values_mod36[,3])
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  
  # Add the fitted model curve 
  lines(s, predicted_values_mod36[,1], col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
