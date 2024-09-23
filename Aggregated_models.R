################################################################################
# GLMs (aggregated models) for Social Ecological Data on Legume Introduction Food Systems
# Year: 2023-2024
# Authors: Anonymysed for double-blind peer review  
################################################################################

#-------------------------------------------------------------------------------
###Loading packages###


install.packages("DescTools")
install.packages("lme4")
install.packages("ggpubr")
install.packages("performance")
install.packages("dplyr")
install.packages("ggplot2")


library(DescTools) 
library(lme4)
library(ggpubr) 
library(performance)
library(dplyr) 
library(ggplot2)


### Load and set working directory ###
##set working directory
workingdir<-"C:/Users/cruzlopd/Dropbox/Code/3.Final_data_code/Aggregated_models"
setwd(workingdir)
getwd()

### Read in datasets ###
mydat1<- read.csv("Econ_wellbeing_allservices_average_transf_2.csv", header=T)
head(mydat1)
names(mydat1) 

mydat2<- read.csv("Nonecon_wellbeing_Allservices_average_transf_2.csv", header=T)
head(mydat2)
names(mydat2) 

mydat3<- read.csv("Econ_wellbeing_prov_services_average_transf_2.csv", header=T)
head(mydat3)
names(mydat3) 

mydat4<- read.csv("Nonecon_wellbeing_prov_services_average_transf_2.csv", header=T)
head(mydat4)
names(mydat4) 

mydat5<- read.csv("Econ_wellbeing_biocarb_services_average_transf_2.csv", header=T)
head(mydat5)
names(mydat5) 

mydat6<- read.csv("Nonecon_wellbeing_biocarb_services_average_transf_2.csv", header=T)
head(mydat6)
names(mydat6) 

mydat7<- read.csv("Econ_wellbeing_supp_services_average_transf_2.csv", header=T)
head(mydat7)
names(mydat7) 

mydat8<- read.csv("Nonecon_wellbeing_supp_services_average_transf_2.csv", header=T)
head(mydat8)
names(mydat8) 

mydat9<- read.csv("Econ_wellbeing_regul_services_average_transf_2.csv", header=T)
head(mydat9)
names(mydat9) 

mydat10<- read.csv("Nonecon_wellbeing_regul_services_average_transf_2.csv", header=T)
head(mydat10)
names(mydat10) 



#########################################################################################
###MODELS ALL SERVICES

###1. Econ_wellbeing_average ~ All_services_average 
mydat1$Econ_wellbeing_average <- mydat1$Econ_wellbeing_average*100-100

#fit GLM model#1
model_any_ECON <- glm(Econ_wellbeing_average ~ All_services_average_percent_transf, data = mydat1)
summary(model_any_ECON)

#plot data points
plot(Econ_wellbeing_average ~ All_services_average_percent_transf, data = mydat1)

# Create a sequence of values for the independent variable
ecosystem_values_any_ECON <- seq(min(mydat1$All_services_average_percent_transf), max(mydat1$All_services_average_percent_transf), length = 100)

# Predict probabilities and confidence intervals for each model
pred_values1 <- predict(model_any_ECON, newdata = data.frame(All_services_average_percent_transf = ecosystem_values_any_ECON), type = "response")
ci_values1 <- predict(model_any_ECON, newdata = data.frame(All_services_average_percent_transf = ecosystem_values_any_ECON), se.fit = TRUE, type = "link")

# Convert confidence intervals to probability scale
ci_values1 <- (cbind(ci_values1$fit - ci_values1$se.fit, ci_values1$fit + ci_values1$se.fit))

# Create a data frame for visualization
plot_data_any_ECON <- data.frame(ecosystem_values_any_ECON, pred_values1, ci_values1)

###2. Non_econ_wellbeing_average ~ All_services_average 
mydat2$Nonecon_wellbeing_average <- mydat2$Nonecon_wellbeing_average*100-100

#fit GLM model #2
model_any_NONECON <- glm(Nonecon_wellbeing_average ~ All_services_average_percent_transf, data = mydat2)
summary(model_any_NONECON)

#plot data points
plot(Nonecon_wellbeing_average ~ All_services_average_percent_transf, data = mydat2)

# Create a sequence of values for the independent variable
ecosystem_values_any_NONECON <- seq(min(mydat1$All_services_average_percent_transf), max(mydat1$All_services_average_percent_transf), length = 100)

# Predict probabilities and confidence intervals for each model
pred_values2 <- predict(model_any_NONECON, newdata = data.frame(All_services_average_percent_transf = ecosystem_values_any_NONECON), type = "response")
ci_values2 <- predict(model_any_NONECON, newdata = data.frame(All_services_average_percent_transf = ecosystem_values_any_NONECON), se.fit = TRUE, type = "link")

# Convert confidence intervals to probability scale
ci_values2 <- (cbind(ci_values2$fit - ci_values2$se.fit, ci_values2$fit + ci_values2$se.fit))

# Create a data frame for visualization
plot_data_any_NONECON <- data.frame(ecosystem_values_any_NONECON, pred_values2, ci_values2)


#bIND RESULTS TOGETHER
names(plot_data_any_ECON) <- c("ecosystem_values","pred_values", "X1" , "X2" )
names(plot_data_any_NONECON) <- c("ecosystem_values","pred_values", "X1" , "X2" )
plot_data_any_ECON$WB <- "Economic"
plot_data_any_NONECON$WB <- "Non-economic"
plot_data_all_WB <- rbind(plot_data_any_ECON, plot_data_any_NONECON)
str(plot_data_all_WB)
head(plot_data_all_WB)

#########################################################################################
###MODELS PROVISIONING SERVICES
#Adjust dependent variable so dependent and independent are on same scale
mydat3$Econ_wellbeing_average <- mydat3$Econ_wellbeing_average*100-100
mydat4$Nonecon_wellbeing_average <- mydat4$Nonecon_wellbeing_average*100-100

###1. Econ_wellbeing_average ~ Prov_services_average
#fit GLM model 3
model_any_ECON3 <- glm(Econ_wellbeing_average ~ Prov_services_average_percent_transf, data = mydat3)
summary(model_any_ECON3)

#plot data points
plot(Econ_wellbeing_average ~ Prov_services_average_percent_transf, data = mydat3)

# Create a sequence of values for the independent variable
ecosystem_values_ECON3 <- seq(min(mydat3$Prov_services_average_percent_transf), max(mydat3$Prov_services_average_percent_transf), length = 100)

# Predict probabilities and confidence intervals for each model
pred_values3 <- predict(model_any_ECON3, newdata = data.frame(Prov_services_average_percent_transf = ecosystem_values_ECON3), type = "response")
ci_values3 <- predict(model_any_ECON3, newdata = data.frame(Prov_services_average_percent_transf = ecosystem_values_ECON3), se.fit = TRUE, type = "link")

# Convert confidence intervals to probability scale
ci_values3 <- (cbind(ci_values3$fit - ci_values3$se.fit, ci_values3$fit + ci_values3$se.fit))

# Create a data frame for visualization
plot_data_any_ECON3 <- data.frame(ecosystem_values_ECON3, pred_values3, ci_values3)


#fit GLM model 4 
model_any_NONECON4 <- glm(Nonecon_wellbeing_average ~ Prov_services_average_percent_transf, data = mydat4)
summary(model_any_NONECON4)

#plot data points
plot(Nonecon_wellbeing_average ~ Prov_services_average_percent_transf, data = mydat4)

# Create a sequence of values for the independent variable
ecosystem_values_NONECON4 <- seq(min(mydat4$Prov_services_average_percent_transf), max(mydat4$Prov_services_average_percent_transf), length = 100)

# Predict probabilities and confidence intervals for each model
pred_values4 <- predict(model_any_NONECON4, newdata = data.frame(Prov_services_average_percent_transf = ecosystem_values_NONECON4), type = "response")
ci_values4 <- predict(model_any_NONECON4, newdata = data.frame(Prov_services_average_percent_transf = ecosystem_values_NONECON4), se.fit = TRUE, type = "link")

# Convert confidence intervals to probability scale
ci_values4 <- (cbind(ci_values4$fit - ci_values4$se.fit, ci_values4$fit + ci_values4$se.fit))

# Create a data frame for visualization
plot_data_any_NONECON4 <- data.frame(ecosystem_values_NONECON4, pred_values4, ci_values4)

#bIND RESULTS TOGETHER
names(plot_data_any_ECON3) <- c("ecosystem_values","pred_values", "X1" , "X2" )
names(plot_data_any_NONECON4) <- c("ecosystem_values","pred_values", "X1" , "X2" )
plot_data_any_ECON3$WB <- "Economic"
plot_data_any_NONECON4$WB <- "Non-economic"
plot_data_all_WB2 <- rbind(plot_data_any_ECON3, plot_data_any_NONECON4)
str(plot_data_all_WB2)


#########################################################################################
###MODELS Biodiversity & soil organic carbon


#Adjust dependent variable so dependent and independent are on same scale
mydat5$Econ_wellbeing_average <- mydat5$Econ_wellbeing_average*100-100
mydat6$Nonecon_wellbeing_average <- mydat6$Nonecon_wellbeing_average*100-100

#fit GLM model 5
model_any_ECON5 <- glm(Econ_wellbeing_average ~ Biocarb_services_average_percent_transf, data = mydat5)
summary(model_any_ECON5)

#plot data points
plot(Econ_wellbeing_average ~ Biocarb_services_average_percent_transf, data = mydat5)

# Create a sequence of values for the independent variable
ecosystem_values_ECON5 <- seq(min(mydat5$Biocarb_services_average_percent_transf), max(mydat5$Biocarb_services_average_percent_transf), length = 100)

# Predict probabilities and confidence intervals for each model
pred_values5 <- predict(model_any_ECON5, newdata = data.frame(Biocarb_services_average_percent_transf = ecosystem_values_ECON5), type = "response")
ci_values5 <- predict(model_any_ECON5, newdata = data.frame(Biocarb_services_average_percent_transf = ecosystem_values_ECON5), se.fit = TRUE, type = "link")

# Convert confidence intervals to probability scale
ci_values5 <- (cbind(ci_values5$fit - ci_values5$se.fit, ci_values5$fit + ci_values5$se.fit))

# Create a data frame for visualization
plot_data_any_ECON5 <- data.frame(ecosystem_values_ECON5, pred_values5, ci_values5)


#fit GLM model 6
model_any_NONECON6 <- glm(Nonecon_wellbeing_average ~ Biocarb_services_average_percent_transf, data = mydat6)
summary(model_any_NONECON6)

#plot data points
plot(Nonecon_wellbeing_average ~ Biocarb_services_average_percent_transf, data = mydat6)

# Create a sequence of values for the independent variable
ecosystem_values_NONECON6 <- seq(min(mydat5$Biocarb_services_average_percent_transf), max(mydat5$Biocarb_services_average_percent_transf), length = 100)

# Predict probabilities and confidence intervals for each model
pred_values6 <- predict(model_any_NONECON6, newdata = data.frame(Biocarb_services_average_percent_transf = ecosystem_values_NONECON6), type = "response")
ci_values6 <- predict(model_any_NONECON6, newdata = data.frame(Biocarb_services_average_percent_transf = ecosystem_values_NONECON6), se.fit = TRUE, type = "link")

# Convert confidence intervals to probability scale
ci_values6 <- (cbind(ci_values6$fit - ci_values6$se.fit, ci_values6$fit + ci_values6$se.fit))

# Create a data frame for visualization
plot_data_any_NONECON6 <- data.frame(ecosystem_values_NONECON6, pred_values6, ci_values6)


#bIND RESULTS TOGETHER
names(plot_data_any_ECON5) <- c("ecosystem_values","pred_values", "X1" , "X2" )
names(plot_data_any_NONECON6) <- c("ecosystem_values","pred_values", "X1" , "X2" )
plot_data_any_ECON5$WB <- "Economic"
plot_data_any_NONECON6$WB <- "Non-economic"
plot_data_all_WB3 <- rbind(plot_data_any_ECON5, plot_data_any_NONECON6)
str(plot_data_all_WB3)

#########################################################################################
###MODELS SUPPORTING SERVICES

#Adjust dependent variable so dependent and independent are on same scale
mydat7$Econ_wellbeing_average <- mydat7$Econ_wellbeing_average*100-100
mydat8$Nonecon_wellbeing_average <- mydat8$Nonecon_wellbeing_average*100-100

###1. Econ_wellbeing_average ~ Supp_services_average
#fit GLM model 7
model_any_ECON7 <- glm(Econ_wellbeing_average ~ Supp_services_average_percent_transf, data = mydat7)
summary(model_any_ECON7)

#plot data points
plot(Econ_wellbeing_average ~ Supp_services_average_percent_transf, data = mydat7)

# Create a sequence of values for the independent variable
ecosystem_values_ECON7 <- seq(min(mydat7$Supp_services_average_percent_transf), max(mydat7$Supp_services_average_percent_transf), length = 100)

# Predict probabilities and confidence intervals for each model
pred_values7 <- predict(model_any_ECON7, newdata = data.frame(Supp_services_average_percent_transf = ecosystem_values_ECON7), type = "response")
ci_values7 <- predict(model_any_ECON7, newdata = data.frame(Supp_services_average_percent_transf = ecosystem_values_ECON7), se.fit = TRUE, type = "link")

# Convert confidence intervals to probability scale
ci_values7 <- (cbind(ci_values7$fit - ci_values7$se.fit, ci_values7$fit + ci_values7$se.fit))

# Create a data frame for visualization
plot_data_any_ECON7 <- data.frame(ecosystem_values_ECON7, pred_values7, ci_values7)


#fit GLM model 8
model_any_NONECON8 <- glm(Nonecon_wellbeing_average ~ Supp_services_average_percent_transf, data = mydat8)
summary(model_any_NONECON8)

#plot data points
plot(Nonecon_wellbeing_average ~ Supp_services_average_percent_transf, data = mydat8)

# Create a sequence of values for the independent variable
ecosystem_values_NONECON8 <- seq(min(mydat8$Supp_services_average_percent_transf), max(mydat8$Supp_services_average_percent_transf), length = 100)

# Predict probabilities and confidence intervals for each model
pred_values8 <- predict(model_any_NONECON8, newdata = data.frame(Supp_services_average_percent_transf = ecosystem_values_NONECON8), type = "response")
ci_values8 <- predict(model_any_NONECON8, newdata = data.frame(Supp_services_average_percent_transf = ecosystem_values_NONECON8), se.fit = TRUE, type = "link")

# Convert confidence intervals to probability scale
ci_values8 <- (cbind(ci_values8$fit - ci_values8$se.fit, ci_values8$fit + ci_values8$se.fit))

# Create a data frame for visualization
plot_data_any_NONECON8 <- data.frame(ecosystem_values_NONECON8, pred_values8, ci_values8)

#bIND RESULTS TOGETHER
names(plot_data_any_ECON7) <- c("ecosystem_values","pred_values", "X1" , "X2" )
names(plot_data_any_NONECON8) <- c("ecosystem_values","pred_values", "X1" , "X2" )
plot_data_any_ECON7$WB <- "Economic"
plot_data_any_NONECON8$WB <- "Non-economic"
plot_data_all_WB4 <- rbind(plot_data_any_ECON7, plot_data_any_NONECON8)
str(plot_data_all_WB4)

#########################################################################################
###MODELS REGULATING SERVICES

#Adjust dependent variable so dependent and independent are on same scale
mydat9$Econ_wellbeing_average <- mydat9$Econ_wellbeing_average*100-100
mydat10$Nonecon_wellbeing_average <- mydat10$Nonecon_wellbeing_average*100-100

###1. Econ_wellbeing_average ~ Supp_services_average
#fit GLM model
model_any_ECON9 <- glm(Econ_wellbeing_average ~ Regul_services_average_percent_transf, data = mydat9)
summary(model_any_ECON9)

#plot data points
plot(Econ_wellbeing_average ~ Regul_services_average_percent_transf, data = mydat9)

# Create a sequence of values for the independent variable
ecosystem_values_ECON9 <- seq(min(mydat9$Regul_services_average_percent_transf), max(mydat9$Regul_services_average_percent_transf), length = 100)

# Predict probabilities and confidence intervals for each model
pred_values9 <- predict(model_any_ECON9, newdata = data.frame(Regul_services_average_percent_transf = ecosystem_values_ECON9), type = "response")
ci_values9 <- predict(model_any_ECON9, newdata = data.frame(Regul_services_average_percent_transf = ecosystem_values_ECON9), se.fit = TRUE, type = "link")

# Convert confidence intervals to probability scale
ci_values9 <- (cbind(ci_values9$fit - ci_values9$se.fit, ci_values9$fit + ci_values9$se.fit))

# Create a data frame for visualization
plot_data_any_ECON9 <- data.frame(ecosystem_values_ECON9, pred_values9, ci_values9)


#fit GLM model 
model_any_NONECON10 <- glm(Nonecon_wellbeing_average ~ Regul_services_average_percent_transf, data = mydat10)
summary(model_any_NONECON10)

#plot data points
plot(Nonecon_wellbeing_average ~ Regul_services_average_percent_transf, data = mydat10)

# Create a sequence of values for the independent variable
ecosystem_values_NONECON10 <- seq(min(mydat9$Regul_services_average_percent_transf), max(mydat10$Regul_services_average_percent_transf), length = 100)

# Predict probabilities and confidence intervals for each model
pred_values10 <- predict(model_any_NONECON10, newdata = data.frame(Regul_services_average_percent_transf = ecosystem_values_NONECON10), type = "response")
ci_values10 <- predict(model_any_NONECON10, newdata = data.frame(Regul_services_average_percent_transf = ecosystem_values_NONECON10), se.fit = TRUE, type = "link")

# Convert confidence intervals to probability scale
ci_values10 <- (cbind(ci_values10$fit - ci_values10$se.fit, ci_values10$fit + ci_values10$se.fit))

# Create a data frame for visualization
plot_data_any_NONECON10 <- data.frame(ecosystem_values_NONECON10, pred_values10, ci_values10)

#bIND RESULTS TOGETHER
names(plot_data_any_ECON9) <- c("ecosystem_values","pred_values", "X1" , "X2" )
names(plot_data_any_NONECON10) <- c("ecosystem_values","pred_values", "X1" , "X2" )
plot_data_any_ECON9$WB <- "Economic"
plot_data_any_NONECON10$WB <- "Non-economic"
plot_data_all_WB5 <- rbind(plot_data_any_ECON7, plot_data_any_NONECON8)
str(plot_data_all_WB5)


#########################################################################################
###LINEPLOTS


#FIRST SET COMBINED

# Plot the results with confidence intervals
p2<-ggplot(plot_data_all_WB, aes(x = ecosystem_values, colour = WB, fill = WB)) +
  geom_line(aes(y = pred_values), col = "#238a8dFF", linewidth = 1) +
  geom_point(data = mydat1, aes(x = All_services_average_percent_transf, y = Econ_wellbeing_average, colour = "Economic", fill = "Economic"), size = 1, shape = 21, stroke = 1) +
  geom_point(data = mydat2, aes(x = All_services_average_percent_transf, y = Nonecon_wellbeing_average, colour = "Non-economic", fill = "Non-economic"), size = 1, shape = 21, stroke = 1) +
  geom_ribbon(aes(ymin = X1, ymax = X2,fill=WB), alpha = 0.3) +
  labs(title = "(a) Wellbeing Outcomes from All Ecosystem Services",
       x = "All Ecosystem Services Average % Change",
       y = "Predicted Well-being (WB) % Change") +
  scale_fill_manual(values = c("Non-economic" = "#44015470", "Economic" = "#FDE72580")) +
  scale_color_manual(values = c("Non-economic" = "#44015470", "Economic" = "#FDE72580")) +
  theme_minimal() + ylim(0, 600)

p2

p2_a<-p2 +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        legend.position="bottom", axis.title = element_text(size = 8), axis.text = element_text(size = 6))
        #axis.title.x = element_blank(),
        #axis.title.y = element_blank())
p2_a

#SECOND SET COMBINED


# Plot the results with confidence intervals
p3<-ggplot(plot_data_all_WB2, aes(x = ecosystem_values, colour = WB, fill = WB)) +
  geom_line(aes(y = pred_values), col = "#238a8dFF", linewidth = 1) +
  geom_point(data = mydat3, aes(x = Prov_services_average_percent_transf, y = Econ_wellbeing_average, colour = "Economic", fill = "Economic"), size = 1, shape = 21, stroke = 1) +
  geom_point(data = mydat4, aes(x = Prov_services_average_percent_transf, y = Nonecon_wellbeing_average, colour = "Non-economic", fill = "Non-economic"), size = 1, shape = 21, stroke = 1) +
  geom_ribbon(aes(ymin = X1, ymax = X2,fill=WB), alpha = 0.3) +
  labs(title = "(b) Wellbeing Outcomes from Provisioning Services",
       x = "Provisioning Services Average % Change",
       y = "Predicted Well-being (WB) % Change") +
  scale_fill_manual(values = c("Non-economic" = "#44015470", "Economic" = "#FDE72580")) +
  scale_color_manual(values = c("Non-economic" = "#44015470", "Economic" = "#FDE72580")) +
  theme_minimal() + ylim(0, 600)
p3

p3_b<-p3 +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        legend.position="bottom", axis.title = element_text(size = 8), axis.text = element_text(size = 6))
#axis.title.x = element_blank(),
#axis.title.y = element_blank())
p3_b



#THIRD SET COMBINED

# Plot the results with confidence intervals
# Filter rows where WB is "Economic"
plot_data_all_WB3_economic <- plot_data_all_WB3 %>% filter(WB == "Economic")
p5<-ggplot(plot_data_all_WB3_economic, aes(x = ecosystem_values, colour = WB, fill = WB)) +
  geom_line(aes(y = pred_values),color = "#238a8dFF", linewidth = 1) +
  geom_point(data = mydat5, aes(x = Biocarb_services_average_percent_transf, y = Econ_wellbeing_average, colour = "Economic", fill = "Economic"), size = 1, shape = 21, stroke = 1) +
  geom_point(data = mydat6, aes(x = Biocarb_services_average_percent_transf, y = Nonecon_wellbeing_average, colour = "Non-economic", fill = "Non-economic"), size = 1, shape = 21, stroke = 1) +
  geom_ribbon(aes(ymin = X1, ymax = X2,fill=WB), alpha = 0.3, fill = "#FDE72580") +
  labs(title = "(c) Economic Wellbeing Outcomes from Biodiversity & Soil Organic Carbon",
       x = "Biodiversity & Soil Organic Carbon Average % Change",
       y = "Predicted Well-being (WB) % Change") + 
  scale_fill_manual(values = c("Non-economic" = "#44015470", "Economic" = "#FDE72580")) +
  scale_color_manual(values = c("Non-economic" = "#44015470", "Economic" = "#FDE72580")) + 
  theme_minimal()+ ylim(0,600) +
  xlim(-40,620)
p5

p5_a<-p5 +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        legend.position="bottom", axis.title = element_text(size = 8), axis.text = element_text(size = 6))
#axis.title.x = element_blank(),
#axis.title.y = element_blank())
p5_a

##FOURTH SET COMBINED (removing the geomlines)

# Plot the results with confidence intervals
# Filter rows 
plot_data_all_WB4_economic <- plot_data_all_WB4 %>% filter(WB == "Economic")
plot_data_all_WB4_economic <- plot_data_all_WB4 %>% filter(WB == "Non-Economic")
p6<-ggplot(plot_data_all_WB4_economic, aes(x = ecosystem_values, colour = WB, fill = WB)) +
  geom_line(aes(y = pred_values), col = "#238a8dFF", linewidth = 1) +
  geom_point(data = mydat7, aes(x = Supp_services_average_percent_transf, y = Econ_wellbeing_average, colour = "Economic", fill = "Economic"), size = 1, shape = 21, stroke = 1) +
  geom_point(data = mydat8, aes(x = Supp_services_average_percent_transf, y = Nonecon_wellbeing_average, colour = "Non-economic", fill = "Non-economic"), size = 1, shape = 21, stroke = 1) +
  geom_ribbon(aes(ymin = X1, ymax = X2,fill=WB), alpha = 0.3, fill = "#FDE72580") +
  labs(title = "(b) Wellbeing Outcomes from Supporting Services",
       x = "Supporting Services Average % Change",
       y = "Predicted Well-being (WB) % Change") +
  scale_fill_manual(values = c("Non-economic" = "#44015470", "Economic" = "#FDE72580")) +
  scale_color_manual(values = c("Non-economic" = "#44015470", "Economic" = "#FDE72580")) +
  theme_minimal() + ylim(0, 600)
p6

p6_b<-p6 +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        legend.position="bottom", axis.title = element_text(size = 8), axis.text = element_text(size = 6))
#axis.title.x = element_blank(),
#axis.title.y = element_blank())
p6_b


##FIFTH SET COMBINED (removing the geomlines)

# Plot the results with confidence intervals
# Filter rows where WB is "Economic"
plot_data_all_WB5_economic <- plot_data_all_WB5 %>% filter(WB == "Economic")
plot_data_all_WB5_economic <- plot_data_all_WB5 %>% filter(WB == "Non-Economic")
p7<-ggplot(plot_data_all_WB5_economic, aes(x = ecosystem_values, colour = WB, fill = WB)) +
  geom_line(aes(y = pred_values), col = "#238a8dFF", linewidth = 1) +
  geom_point(data = mydat9, aes(x = Regul_services_average_percent_transf, y = Econ_wellbeing_average, colour = "Economic", fill = "Economic"), size = 1, shape = 21, stroke = 1) +
  geom_point(data = mydat10, aes(x = Regul_services_average_percent_transf, y = Nonecon_wellbeing_average, colour = "Non-economic", fill = "Non-economic"), size = 1, shape = 21, stroke = 1) +
  geom_ribbon(aes(ymin = X1, ymax = X2,fill=WB), alpha = 0.3, fill = "#FDE72580") +
  labs(title = "(b) Wellbeing Outcomes from Regulating Services",
       x = "Regulating Services Average % Change",
       y = "Predicted Well-being (WB) % Change") +
  scale_fill_manual(values = c("Non-economic" = "#44015470", "Economic" = "#FDE72580")) +
  scale_color_manual(values = c("Non-economic" = "#44015470", "Economic" = "#FDE72580")) +
  theme_minimal() + ylim(0, 600) + xlim(0,200)+
  theme(axis.text.y = element_text(size = 8))  # Adjust the size as needed
p7

p7_a<-p7 +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        legend.position="bottom", axis.title = element_text(size = 8), axis.text = element_text(size = 6))
#axis.title.x = element_blank(),
#axis.title.y = element_blank())
p7_a



##########################################################################
#############Joining plots


library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend_line <- get_legend(p2_a)

library(ggplot2)
library(gridExtra)


# Define plots with titles and without legends
p2_a <- p2_a + ggtitle("(a)") + theme(legend.position="none") + labs(y = "Well-being (WB) % Change")
p3_b <- p3_b + ggtitle("(b)") + theme(legend.position="none") + labs(y = "Well-being (WB) % Change")
p5_a <- p5_a + ggtitle("(c)") + theme(legend.position="none") + labs(y = "Well-being (WB) % Change")
p6_b <- p6_b + ggtitle("(d)") + theme(legend.position="none") + labs(y = "Well-being (WB) % Change")
p7_a <- p7_a + ggtitle("(e)") + theme(legend.position="none") + labs(y = "Well-being (WB) % Change")

# Create a layout matrix to arrange plots
layout_matrix <- rbind(
  c(NA, NA),  # legend_line spanning both columns
  c(2, 3),  # p2_a and p6_b
  c(4, 5),  # p3_b and p7_a
  c(6, 1)  # p5_a and empty space
)

# Arrange the plots in the grid using the layout matrix
grid.arrange(
  legend_line,
  p2_a,
  p6_b,
  p3_b,
  p7_a,
  p5_a,
  layout_matrix = layout_matrix,
  heights = c(1, 4, 4, 4)  # Adjust heights as necessary
)

