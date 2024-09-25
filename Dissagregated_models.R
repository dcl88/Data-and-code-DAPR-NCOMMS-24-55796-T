################################################################################
# Linear models with logarithmic transformation (disaggregated models) for Social Ecological Data on Legume Introduction Food Systems
# Year: 2023-2024
# Authors: Anonymysed for double-blind peer review  
################################################################################

#-------------------------------------------------------------------------------
###Loading packages###
install.packages("ggplot2")
install.packages("dplyr")
install.packages("DescTools")
install.packages("tidyverse")
install.packages("gridExtra")

library(ggplot2)
library(dplyr)
library(DescTools)
library(tidyverse)
library(gridExtra)

  #1-------------------------------------------------------------------------------
  ###Load data###
  ##set working directory
  workingdir<-"Insert filepath here"
  setwd(workingdir)
  getwd()
  
  ##read in data (csv file)
  mydat1<- read.csv("Yieldeqcumul_profit_econ2.csv", header=T)
  head(mydat1)
  names(mydat1) 
  
  # Fit the linear regression model  
  pow1 <- lm(log(Profit) ~ log(Yieldeqcumul), data = mydat1)
  summary(pow1)
  
  # Calculate the residual sum of squares (RSS) 
  RSS_p <- sum(residuals(pow1)^2)
  
  # Calculate the total sum of squares (TSS) 
  y <- mydat1$Profit  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  
  # Calculate the R-squared measure
  R_squared_pow1 <- 1 - (RSS_p / TSS)
  
  # Calculate AIC
  AIC(pow1)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(pow1, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- exp(pred_int[, "lwr"])
  upper <- exp(pred_int[, "upr"])
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Yieldeqcumul'
  s <- seq(min(mydat1$Yieldeqcumul), max(mydat1$Yieldeqcumul), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals 
  predicted_values_pow1 <- predict(pow1, newdata = data.frame(Yieldeqcumul = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  par(mar = c(7, 7, 4, 4) + 0.5)  
  
  par(cex.axis = 1.5)  
  
  # Plot the data points
  plot(mydat1$Yieldeqcumul, mydat1$Profit, pch = 16, col = "#147746", 
       xlab = "Equivalent & Cumulative Yield", ylab = "Profit", 
       cex.axis = 2, cex.lab = 2, 
       xlim = c(0, max(mydat1$Yieldeqcumul)), ylim = c(0, 12),
       axes = FALSE)
  
 
  # Add shaded confidence intervals 
  shade <- cbind(s, exp(predicted_values_pow1[,2]), exp(predicted_values_pow1[,3]))
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  # Add the fitted curve 
  lines(s, exp(predicted_values_pow1[,1]), col = "#1DB954", lwd = 3)
  
# Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
 
  #2-------------------------------------------------------------------------------
  ###Load data###
  ##set working directory
  workingdir<-"Insert filepath here"
  setwd(workingdir)
  getwd()
  
  ##read in data (csv file)
  mydat2<- read.csv("Yieldeqcumul_Savings_labour_cost_econ2.csv", header=T)
  head(mydat2)
  names(mydat2) 
  
  
  # Fit the linear regression model 
  pow2 <- lm(log(Savings_labour_cost)~ log(Yieldeqcumul), data = mydat2)#new
  
  summary(pow2) #new
  
  # Calculate the residual sum of squares (RSS) 
  RSS_p <- sum(residuals(pow2)^2)
  RSS_p #new
  
  # Calculate the total sum of squares (TSS) 
  y <- mydat2$Savings_labour_cost  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  TSS #new
  
  # Calculate the R-squared measure 
  R_squared_pow2 <- 1 - (RSS_p / TSS)
  R_squared_pow2 #new
  
  #AIC
  AIC(pow2)
  
  ## Calculate confidence intervals for predictions ##
  
  pred_int <- predict(pow2, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- exp(pred_int[, "lwr"])
  upper <- exp(pred_int[, "upr"])
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Yieldeqcumul'
  s <- seq(min(mydat2$Yieldeqcumul), max(mydat2$Yieldeqcumul), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals 
  predicted_values_pow2 <- predict(pow2, newdata = data.frame(Yieldeqcumul = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  par(mar = c(7, 7, 4, 4) + 0.5)  
  
  par(cex.axis = 1.5)  
  
  # Plot the data points
  plot(mydat2$Yieldeqcumul, mydat2$Savings_labour_cost, pch = 16, col = "#147746",
       xlab = "Equivalent & Cumulative Yield", ylab = "Savings in Labour Cost", 
       cex.axis = 2, cex.lab = 2,
       xlim = c(0, 6), ylim = c(0, 2),axes=FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, exp(predicted_values_pow2[,2]), exp(predicted_values_pow2[,3]))
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  # Add the fitted cruve
  lines(s, exp(predicted_values_pow2[,1]), col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  #3-------------------------------------------------------------------------------
  ###Load data###
  ##set working directory
  workingdir<-"Insert filepath here"
  setwd(workingdir)
  getwd()
  
  ##read in data (csv file)
  mydat3<- read.csv("Yieldeqcumul_natural_capital_nonecon2.csv", header=T)
  head(mydat3)
  names(mydat3) 
  
  
  # Fit the linear regression model 
  pow3 <- lm(log(Natural_capital)~ log(Yieldeqcumul), data = mydat3)#new
  
  summary(pow3) #new
  
  # Calculate the residual sum of squares (RSS) 
  RSS_p <- sum(residuals(pow3)^2)
  RSS_p #new
  
  # Calculate the total sum of squares (TSS) 
  y <- mydat3$Natural_capital  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  TSS #new
  
  # Calculate the R-squared measure 
  R_squared_pow3 <- 1 - (RSS_p / TSS)
  R_squared_pow3 #new
  
  #AIC
  AIC(pow3)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(pow3, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- exp(pred_int[, "lwr"])
  upper <- exp(pred_int[, "upr"])
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Yieldeqcumul'
  s <- seq(min(mydat3$Yieldeqcumul), max(mydat3$Yieldeqcumul), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals 
  predicted_values_pow3 <- predict(pow3, newdata = data.frame(Yieldeqcumul = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels

  par(mar = c(7, 7, 4, 4) + 0.5)  
  
  par(cex.axis = 1.5)  # Increase the size of axis tick labels
  
  # Plot the data points
  plot(mydat3$Yieldeqcumul, mydat3$Natural_capital, pch = 16, col = "#147746", 
       xlab = "Equivalent & Cumulative Yield", ylab = "Natural Capital", 
       cex.axis = 1.2, cex.lab = 2, 
       xlim = c(0, max(mydat3$Yieldeqcumul)), ylim = c(0, 4), 
       axes=FALSE)
  
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, exp(predicted_values_pow3[,2]), exp(predicted_values_pow3[,3]))
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  # Add the fitted curve 
  lines(s, exp(predicted_values_pow3[,1]), col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  #4-------------------------------------------------------------------------------
  ###Load data###
  ##set working directory
  workingdir<-"Insert filepath here"
  setwd(workingdir)
  getwd()
  
  ##read in data (csv file)
  mydat4<- read.csv("Yieldeqcumul_food_security_nonecon2.csv", header=T)
  head(mydat4)
  names(mydat4) 
  
 
  # Fit the linear regression model 
  pow4 <- lm(log(Food_security)~ log(Yieldeqcumul), data = mydat4)#new
  
  summary(pow4) #new
  
  # Calculate the residual sum of squares (RSS) 
  RSS_p <- sum(residuals(pow4)^2)
  RSS_p #new
  
  # Calculate the total sum of squares (TSS) 
  y <- mydat4$Food_security  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  TSS #new
  
  # Calculate the R-squared measure 
  R_squared_pow4 <- 1 - (RSS_p / TSS)
  R_squared_pow4 #new
  
  #AIC
  AIC(pow4)
  
  ## Calculate confidence intervals for predictions ##
  
  pred_int <- predict(pow4, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- exp(pred_int[, "lwr"])
  upper <- exp(pred_int[, "upr"])
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Yieldeqcumul'
  s <- seq(min(mydat4$Yieldeqcumul), max(mydat4$Yieldeqcumul), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the pow4er relationship model
  predicted_values_pow4 <- predict(pow4, newdata = data.frame(Yieldeqcumul = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  par(mar = c(7, 7, 4, 4) + 0.5)  
  
  par(cex.axis = 1.5)  
  
  # Plot the data points
  plot(mydat4$Yieldeqcumul, mydat4$Food_security, pch = 16, col = "#147746", 
       xlab = "Equivalent & Cumulative Yield", ylab = "Food Security", 
       cex.axis = 1.2, cex.lab = 2, 
       xlim = c(0.5, max(mydat4$Yieldeqcumul)), ylim = c(0, 5),
       axes = FALSE)
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, exp(predicted_values_pow4[,2]), exp(predicted_values_pow4[,3]))
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  # Add the fitted curve 
  lines(s, exp(predicted_values_pow4[,1]), col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis

  #5-------------------------------------------------------------------------------
  ###Load data###
  ##set working directory
  workingdir<-"Insert filepath here"
  setwd(workingdir)
  getwd()
  
  ##read in data (csv file)
  mydat5<- read.csv("Primary_production_Profit_econ2.csv", header=T)
  head(mydat5)
  names(mydat5) #gives the headings of every column
  
  
  # Fit the linear regression model 
  pow5 <- lm(log(Profit)~ log(Primary_production), data = mydat5)#new
  
  # Display a summary 
  summary(pow5) #new
  
  # Calculate the residual sum of squares (RSS) 
  RSS_p <- sum(residuals(pow5)^2)
  RSS_p #new
  
  # Calculate the total sum of squares (TSS) 
  y <- mydat5$Profit  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  TSS #new
  
  # Calculate the R-squared measure 
  R_squared_pow5 <- 1 - (RSS_p / TSS)
  R_squared_pow5 #new
  
  #AIC
  AIC(pow5)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(pow5, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- exp(pred_int[, "lwr"])
  upper <- exp(pred_int[, "upr"])
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Primary_production'
  s <- seq(min(mydat5$Primary_production), max(mydat5$Primary_production), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the pow5er relationship model
  predicted_values_pow5 <- predict(pow5, newdata = data.frame(Primary_production = s), interval = "confidence", level = 0.95)
  
  # Adjust the plot margins to ensure space for larger labels
  par(mar = c(7, 7, 4, 4) + 0.5)  
  
  par(cex.axis = 1.5)  
  
  # Plot the data points
  plot(mydat5$Primary_production, mydat5$Profit, pch = 16, col = "#147746", 
       xlab = "Primary Production", ylab = "Profit", 
       cex.axis = 1.2, cex.lab = 2, 
       xlim = c(0.5,3.5), ylim = c(0, 12),
       axes = FALSE)
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, exp(predicted_values_pow5[,2]), exp(predicted_values_pow5[,3]))
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  # Add the fitted  curve 
  lines(s, exp(predicted_values_pow5[,1]), col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  #6-------------------------------------------------------------------------------
  ###Load data###
  ##set working directory
  workingdir<-"Insert filepath here"
  setwd(workingdir)
  getwd()
  
  ##read in data (csv file)
  mydat6<- read.csv("Soil_organic_carbon_Profit_econ2.csv", header=T)
  head(mydat6)
  names(mydat6) 
  
 
  # Fit the linear regression model  
  pow6 <- lm(log(Profit)~ log(Soil_organic_carbon), data = mydat6)#new
  
  summary(pow6) #new
  
  # Calculate the residual sum of squares (RSS)
  RSS_p <- sum(residuals(pow6)^2)
  RSS_p #new
  
  # Calculate the total sum of squares (TSS) 
  y <- mydat6$Profit  # Extract the dependent variable
  TSS <- sum((y - mean(y))^2)
  TSS #new
  
  # Calculate the R-squared measure 
  R_squared_pow6 <- 1 - (RSS_p / TSS)
  R_squared_pow6 #new
  
  #AIC
  AIC(pow6)
  
  ## Calculate confidence intervals for predictions ##
  pred_int <- predict(pow6, interval = "confidence", level = 0.95)
  
  # Extract lower and upper bounds
  lower <- exp(pred_int[, "lwr"])
  upper <- exp(pred_int[, "upr"])
  
  ## Plot with shaded confidence intervals ##
  # Create a sequence of values for the x-axis based on the range of 'Soil_organic_carbon'
  s <- seq(min(mydat6$Soil_organic_carbon), max(mydat6$Soil_organic_carbon), length.out = 100)
  
  # Predict the corresponding y-values and confidence intervals using the pow6er relationship model
  predicted_values_pow6 <- predict(pow6, newdata = data.frame(Soil_organic_carbon = s), interval = "confidence", level = 0.95)
  
  
  # Adjust the plot margins to ensure space for larger labels
  par(mar = c(7, 7, 4, 4) + 0.5) 
  
  par(cex.axis = 1.5)  
  
  # Plot the data points
  plot(mydat6$Soil_organic_carbon, mydat6$Profit, pch = 16, col = "#147746", 
       xlab = "Soil Organic Carbon", ylab = "Profit", 
       cex.axis = 1.2, cex.lab = 2, 
       xlim = c(0,7), ylim = c(0, 7),
       axes = FALSE)
  
  # Add shaded confidence intervals with increased transparency and yellow color
  shade <- cbind(s, exp(predicted_values_pow6[,2]), exp(predicted_values_pow6[,3]))
  polygon(c(shade[,1], rev(shade[,1])), c(shade[,2], rev(shade[,3])), col = "#FDE72580", border = NA)
  
  # Add the fitted pow6er curve 
  lines(s, exp(predicted_values_pow6[,1]), col = "#1DB954", lwd = 3)
  
  # Add only the visible y and x axes
  axis(1)  # x-axis
  axis(2)  # y-axis
  
  
  ##############################################################################
  ####Joining plots and customising the plots
  
  # Function for plot1
  plot1 <- function() {
    # Load data
    workingdir <- "Insert filepath here"
    setwd(workingdir)
    mydat1 <- read.csv("Yieldeqcumul_profit_econ2.csv", header = TRUE)
    
    # Fit the linear regression model
    pow1 <- lm(log(Profit) ~ log(Yieldeqcumul), data = mydat1)
    
    # Calculate confidence intervals for predictions
    pred_int <- predict(pow1, interval = "confidence", level = 0.95)
    
    # Create a ggplot object
    p <- ggplot(mydat1, aes(x = Yieldeqcumul, y = Profit)) +
      geom_point(color = "#147746") +
      geom_line(aes(y = exp(fitted(pow1)), color = "Fitted"), size = 1) +
      geom_ribbon(aes(ymin = exp(pred_int[, "lwr"]), ymax = exp(pred_int[, "upr"])), fill = "#FDE72580", alpha = 0.5) +
      labs(x = "Equivalent & Cumulative Yield (Provisioning Service)% Change", y = "Profit (Econ WB)% Change") +
      scale_color_manual(values = c("Fitted" = "#1DB954")) +
      theme_minimal()
    
    # customise the ggplot object
    p_a <- p +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(colour = "black"),
            legend.position = "null", 
            axis.title = element_text(size = 6.5), 
            axis.text = element_text(size = 6))
    
    return(p_a)
    
  }
  
  # Function for plot2
  plot2 <- function() {
    # Load data
    workingdir <- "Insert filepath here"
    setwd(workingdir)
    mydat2 <- read.csv("Yieldeqcumul_Savings_labour_cost_econ2.csv", header = TRUE)
    
    # Fit the linear regression model
    pow2 <- lm(log(Savings_labour_cost) ~ log(Yieldeqcumul), data = mydat2)
    
    # Calculate confidence intervals for predictions
    pred_int <- predict(pow2, interval = "confidence", level = 0.95)
    
    # Create a ggplot object
    p <- ggplot(mydat2, aes(x = Yieldeqcumul, y = Savings_labour_cost)) +
      geom_point(color = "#147746") +
      geom_line(aes(y = exp(fitted(pow2)), color = "Fitted"), size = 1) +
      geom_ribbon(aes(ymin = exp(pred_int[, "lwr"]), ymax = exp(pred_int[, "upr"])), fill = "#FDE72580", alpha = 0.5) +
      labs(x = "Equivalent & Cumulative Yield (Provisioning Service)% Change", y = "Savings Labour Cost (Econ WB)% Change") +
      scale_color_manual(values = c("Fitted" = "#1DB954")) +
      theme_minimal()
    
    # customise the ggplot object
    p_b <- p +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(colour = "black"),
            legend.position = "null",  
            axis.title = element_text(size = 6.5), 
            axis.text = element_text(size = 6))
    return(p_b)
  }
  
  # Function for plot3
  plot3 <- function() {
    # Load data
    workingdir <- "Insert filepath here"
    setwd(workingdir)
    mydat3 <- read.csv("Yieldeqcumul_natural_capital_nonecon2.csv", header = TRUE)
    
    # Fit the linear regression model
    pow3 <- lm(log(Natural_capital) ~ log(Yieldeqcumul), data = mydat3)
    
    # Calculate confidence intervals for predictions
    pred_int <- predict(pow3, interval = "confidence", level = 0.95)
    
    # Create a ggplot object
    p <- ggplot(mydat3, aes(x = Yieldeqcumul, y = Natural_capital)) +
      geom_point(color = "#147746") +
      geom_line(aes(y = exp(fitted(pow3)), color = "Fitted"), size = 1) +
      geom_ribbon(aes(ymin = exp(pred_int[, "lwr"]), ymax = exp(pred_int[, "upr"])), fill = "#FDE72580", alpha = 0.5) +
      labs(x = "Equivalent & Cumulative Yield (Provisioning Service)% Change", y = "Natural Capital (Non-econ WB)% Change") +
      scale_color_manual(values = c("Fitted" = "#1DB954")) +
      theme_minimal()
    
    # customise the ggplot object
    p_c <- p +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(colour = "black"),
            legend.position = "null", 
            axis.title = element_text(size = 6.5), 
            axis.text = element_text(size = 6))
    return(p_c)
  }
  
  # Function for plot4
  plot4 <- function() {
    # Load data
    workingdir <- "Insert filepath here"
    setwd(workingdir)
    mydat4 <- read.csv("Yieldeqcumul_food_security_nonecon2.csv", header = TRUE)
    
    # Fit the linear regression model
    pow4 <- lm(log(Food_security) ~ log(Yieldeqcumul), data = mydat4)
    
    # Calculate confidence intervals for predictions
    pred_int <- predict(pow4, interval = "confidence", level = 0.95)
    
    # Create a ggplot object
    p <- ggplot(mydat4, aes(x = Yieldeqcumul, y = Food_security)) +
      geom_point(color = "#147746") +
      geom_line(aes(y = exp(fitted(pow4)), color = "Fitted"), size = 1) +
      geom_ribbon(aes(ymin = exp(pred_int[, "lwr"]), ymax = exp(pred_int[, "upr"])), fill = "#FDE72580", alpha = 0.5) +
      labs(x = "Equivalent & Cumulative Yield (Provisioning Service) % Change", y = "Food Security (Non-econ WB)% Change") +
      scale_color_manual(values = c("Fitted" = "#1DB954")) +
      theme_minimal()
    
    # customise the ggplot object
    p_d <- p +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(colour = "black"),
            legend.position = "null", 
            axis.title = element_text(size = 6.5), 
            axis.text = element_text(size = 6))
    return(p_d)
  }
  
  # Function for plot5
  plot5 <- function() {
    # Load data
    workingdir <- "Insert filepath here"
    setwd(workingdir)
    mydat5 <- read.csv("Primary_production_Profit_econ2.csv", header = TRUE)
    
    # Fit the linear regression model
    pow5 <- lm(log(Profit) ~ log(Primary_production), data = mydat5)
    
    # Calculate confidence intervals for predictions
    pred_int <- predict(pow5, interval = "confidence", level = 0.95)
    
    # Create a ggplot object
    p <- ggplot(mydat5, aes(x = Primary_production, y = Profit)) +
      geom_point(color = "#147746") +
      geom_line(aes(y = exp(fitted(pow5)), color = "Fitted"), size = 1) +
      geom_ribbon(aes(ymin = exp(pred_int[, "lwr"]), ymax = exp(pred_int[, "upr"])), fill = "#FDE72580", alpha = 0.5) +
      labs(x = "Primary Production (Supporting Service)% Change", y = "Profit (Econ WB)% Change") +
      scale_color_manual(values = c("Fitted" = "#1DB954")) +
      theme_minimal()
    
    # customise the ggplot object
    p_e <- p +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(colour = "black"),
            legend.position = "null", 
            axis.title = element_text(size = 6.5), 
            axis.text = element_text(size = 6))
    return(p_e)
  }
  
  # Function for plot6
  plot6 <- function() {
    # Load data
    workingdir <- "Insert filepath here"
    setwd(workingdir)
    mydat6 <- read.csv("Soil_organic_carbon_Profit_econ2.csv", header = TRUE)
    
    # Fit the linear regression model
    pow6 <- lm(log(Profit) ~ log(Soil_organic_carbon), data = mydat6)
    
    # Calculate confidence intervals for predictions
    pred_int <- predict(pow6, interval = "confidence", level = 0.95)
    
    # Create a ggplot object
    p <- ggplot(mydat6, aes(x = Soil_organic_carbon, y = Profit)) +
      geom_point(color = "#147746") +
      geom_line(aes(y = exp(fitted(pow6)), color = "Fitted"), size = 1) +
      geom_ribbon(aes(ymin = exp(pred_int[, "lwr"]), ymax = exp(pred_int[, "upr"])), fill = "#FDE72580", alpha = 0.5) +
      labs(x = "Soil Organic Carbon % Change", y = "Profit (Econ WB)% Change") +
      scale_color_manual(values = c("Fitted" = "#1DB954")) +
      theme_minimal()
    
    # customise the ggplot object
    p_f <- p +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(colour = "black"),
            legend.position = "null", 
            axis.title = element_text(size = 6.5), 
            axis.text = element_text(size = 6))
    return(p_f)
  }
  
  # Arrange plots using grid.arrange
  grid.arrange(plot1(), plot2(), plot3(), plot4(), plot5(), plot6(), ncol = 2)
 
  
 