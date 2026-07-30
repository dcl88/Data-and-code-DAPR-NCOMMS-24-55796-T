##############################################################################
#Date: 2026
##Description: Social-ecological aggregated models legumes
##############################################################################


library(ggplot2)
library(scales)
library(viridis)
library(gridExtra) 
library(grid)
library(lattice)
library(DescTools) 
library(lme4)
library(jtools) 
library(coefplot) 
library(forestplot) 
library(performance)
library(ggpubr) 
library(dplyr) 
library(car)
library(MASS)


###Load mydat###
##set working directory
workingdir<-"insert pathway here"
setwd(workingdir)
getwd()

##read in first dataset (csv file)
mydat1<- read.csv("Econ_wellbeing_allservices_average_transf_3.1.csv", header=T,
                  fileEncoding = "Latin1")
head(mydat1)
names(mydat1) #gives the headings of every column
summary(mydat1)

mydat2<- read.csv("Nonecon_wellbeing_Allservices_average_transf_3.1.csv", header=T,
                  fileEncoding = "Latin1")
head(mydat2)
names(mydat2) #gives the headings of every column

mydat3<- read.csv("Econ_wellbeing_prov_services_average_transf_3.1.csv", header=T,
                  fileEncoding = "Latin1")
head(mydat3)
names(mydat3) #gives the headings of every column

mydat4<- read.csv("Nonecon_wellbeing_prov_services_average_transf_3.1.csv", header=T,
                  fileEncoding = "Latin1")
head(mydat4)
names(mydat4) #gives the headings of every column


mydat7<- read.csv("Econ_wellbeing_suppreg_services_average_transf_3.1.csv", header=T,
                  fileEncoding = "Latin1")
head(mydat7)
names(mydat7) #gives the headings of every column

mydat8<- read.csv("Nonecon_wellbeing_suppreg_services_average_transf_3.1.csv", header=T,
                  fileEncoding = "Latin1")
head(mydat8)
names(mydat8) #gives the headings of every column


#########################################################################################
###MODELS ALL SERVICES

###1. Econ_wellbeing_average ~ All_services_average
###2. Nonecon_wellbeing_average ~ All_services_average 
# Fit GLM model 1 & 2
model_any_ECON1 <- glm(Econ_wellbeing_average_percent_transf ~ All_services_average_percent_transf, data = mydat1)
plot(Econ_wellbeing_average_percent_transf ~ All_services_average_percent_transf, data = mydat1)
summary(model_any_ECON1)

model_any_NONECON2 <- glm(Nonecon_wellbeing_average_percent_transf ~ All_services_average_percent_transf, data = mydat2)
plot(Nonecon_wellbeing_average_percent_transf ~ All_services_average_percent_transf, data = mydat2)
summary(model_any_NONECON2)

# Create sequences of values for prediction based on respective datasets
seq_ECON1 <- data.frame(All_services_average_percent_transf = seq(
  min(mydat1$All_services_average_percent_transf, na.rm = TRUE), 
  max(mydat1$All_services_average_percent_transf, na.rm = TRUE), 
  length.out = 100))

seq_NONECON2 <- data.frame(All_services_average_percent_transf = seq(
  min(mydat2$All_services_average_percent_transf, na.rm = TRUE), 
  max(mydat2$All_services_average_percent_transf, na.rm = TRUE), 
  length.out = 100))

# Predict values and confidence intervals for each model using the correct sequence
pred_ECON1 <- predict(model_any_ECON1, newdata = seq_ECON1, se.fit = TRUE)
pred_NONECON2 <- predict(model_any_NONECON2, newdata = seq_NONECON2, se.fit = TRUE)

# Create data frames for visualization
plot_data_any_ECON1 <- data.frame(
  All_services_average_percent_transf = seq_ECON1$All_services_average_percent_transf,
  pred_values = pred_ECON1$fit,
  X1 = pred_ECON1$fit - 1.96 * pred_ECON1$se.fit,  # Lower bound of CI
  X2 = pred_ECON1$fit + 1.96 * pred_ECON1$se.fit,  # Upper bound of CI
  WB = "Economic"
)

plot_data_any_NONECON2 <- data.frame(
  All_services_average_percent_transf = seq_NONECON2$All_services_average_percent_transf,
  pred_values = pred_NONECON2$fit,
  X1 = pred_NONECON2$fit - 1.96 * pred_NONECON2$se.fit,  # Lower bound of CI
  X2 = pred_NONECON2$fit + 1.96 * pred_NONECON2$se.fit,  # Upper bound of CI
  WB = "Non-economic"
)

# Combine both datasets
plot_data_all_WB <- rbind(plot_data_any_ECON1, plot_data_any_NONECON2)


#########################################################################################
###MODELS PROVISIONING SERVICES
###3. Econ_wellbeing_average ~ Prov_services_average
###4. Nonecon_wellbeing_average ~ Prov_services_average 
# Fit GLM model 3 & 4
model_any_ECON3 <- glm(Econ_wellbeing_average_percent_transf ~ Prov_services_average_percent_transf, data = mydat3)
plot(Econ_wellbeing_average_percent_transf ~ Prov_services_average_percent_transf, data = mydat3)
summary(model_any_ECON3)

model_any_NONECON4 <- glm(Nonecon_wellbeing_average_percent_transf ~ Prov_services_average_percent_transf, data = mydat4)
plot(Nonecon_wellbeing_average_percent_transf ~ Prov_services_average_percent_transf, data = mydat4)
summary(model_any_NONECON4)

# Create sequences of values for prediction based on respective datasets
seq_ECON3 <- data.frame(Prov_services_average_percent_transf = seq(
  min(mydat3$Prov_services_average_percent_transf, na.rm = TRUE), 
  max(mydat3$Prov_services_average_percent_transf, na.rm = TRUE), 
  length.out = 100))

seq_NONECON4 <- data.frame(Prov_services_average_percent_transf = seq(
  min(mydat4$Prov_services_average_percent_transf, na.rm = TRUE), 
  max(mydat4$Prov_services_average_percent_transf, na.rm = TRUE), 
  length.out = 100))

# Predict values and confidence intervals for each model using the correct sequence
pred_ECON3 <- predict(model_any_ECON3, newdata = seq_ECON3, se.fit = TRUE)
pred_NONECON4 <- predict(model_any_NONECON4, newdata = seq_NONECON4, se.fit = TRUE)

# Create data frames for visualization
plot_data_any_ECON3 <- data.frame(
  Prov_services_average_percent_transf = seq_ECON3$Prov_services_average_percent_transf,
  pred_values = pred_ECON3$fit,
  X1 = pred_ECON3$fit - 1.96 * pred_ECON3$se.fit,  # Lower bound of CI
  X2 = pred_ECON3$fit + 1.96 * pred_ECON3$se.fit,  # Upper bound of CI
  WB = "Economic"
)

plot_data_any_NONECON4 <- data.frame(
  Prov_services_average_percent_transf = seq_NONECON4$Prov_services_average_percent_transf,
  pred_values = pred_NONECON4$fit,
  X1 = pred_NONECON4$fit - 1.96 * pred_NONECON4$se.fit,  # Lower bound of CI
  X2 = pred_NONECON4$fit + 1.96 * pred_NONECON4$se.fit,  # Upper bound of CI
  WB = "Non-economic"
)

# Combine both datasets
plot_data_all_WB_3_4 <- rbind(plot_data_any_ECON3, plot_data_any_NONECON4)


#########################################################################################
###MODELS SUPPORTING & REGULATING SERVICES

###7. Econ_wellbeing_average ~ Suppreg_services_average
###8. Nonecon_wellbeing_average ~ Suppreg_services_average_services_average 
# Fit GLM model 7 & 8
model_any_ECON7 <- glm(Econ_wellbeing_average_percent_transf ~ Suppreg_services_average_percent_transf, data = mydat7)
plot(Econ_wellbeing_average_percent_transf ~ Suppreg_services_average_percent_transf, data = mydat7)
summary(model_any_ECON7)

model_any_NONECON8 <- glm(Nonecon_wellbeing_average_percent_transf ~ Suppreg_services_average_percent_transf, data = mydat8)
plot(Nonecon_wellbeing_average_percent_transf ~ Suppreg_services_average_percent_transf, data = mydat8)
summary(model_any_NONECON8)

# Create sequences of values for prediction based on respective datasets
seq_ECON7 <- data.frame(Suppreg_services_average_percent_transf = seq(
  min(mydat7$Suppreg_services_average_percent_transf, na.rm = TRUE), 
  max(mydat7$Suppreg_services_average_percent_transf, na.rm = TRUE), 
  length.out = 100))

seq_NONECON8 <- data.frame(Suppreg_services_average_percent_transf = seq(
  min(mydat8$Suppreg_services_average_percent_transf, na.rm = TRUE), 
  max(mydat8$Suppreg_services_average_percent_transf, na.rm = TRUE), 
  length.out = 100))

# Predict values and confidence intervals for each model using the correct sequence
pred_ECON7 <- predict(model_any_ECON7, newdata = seq_ECON7, se.fit = TRUE)
pred_NONECON8 <- predict(model_any_NONECON8, newdata = seq_NONECON8, se.fit = TRUE)

# Create data frames for visualization
plot_data_any_ECON7 <- data.frame(
  Suppreg_services_average_percent_transf = seq_ECON7$Suppreg_services_average_percent_transf,
  pred_values = pred_ECON7$fit,
  X1 = pred_ECON7$fit - 1.96 * pred_ECON7$se.fit,  # Lower bound of CI
  X2 = pred_ECON7$fit + 1.96 * pred_ECON7$se.fit,  # Upper bound of CI
  WB = "Economic"
)

plot_data_any_NONECON8 <- data.frame(
  Suppreg_services_average_percent_transf = seq_NONECON8$Suppreg_services_average_percent_transf,
  pred_values = pred_NONECON8$fit,
  X1 = pred_NONECON8$fit - 1.96 * pred_NONECON8$se.fit,  # Lower bound of CI
  X2 = pred_NONECON8$fit + 1.96 * pred_NONECON8$se.fit,  # Upper bound of CI
  WB = "Non-economic"
)

# Combine both datasets
plot_data_all_WB_7_8 <- rbind(plot_data_any_ECON7, plot_data_any_NONECON8)


#LINEPLOT:###################
#####################################################################

#fIRST SET COMBINED

# Plot the results with confidence intervals
p2 <- ggplot(plot_data_all_WB, aes(x = All_services_average_percent_transf, colour = WB, fill = WB)) +
  geom_line(aes(y = pred_values), col = "#CC79A7", linewidth = 1) +
  geom_point(data = mydat1, aes(x = All_services_average_percent_transf, y = Econ_wellbeing_average_percent_transf, colour = "Economic", fill = "Economic"), size = 1, shape = 21, stroke = 1) +
  geom_point(data = mydat2, aes(x = All_services_average_percent_transf, y = Nonecon_wellbeing_average_percent_transf, colour = "Non-economic", fill = "Non-economic"), size = 1, shape = 21, stroke = 1) +
  geom_ribbon(aes(ymin = X1, ymax = X2, fill = WB), alpha = 0.3) +
  labs(title = "(a) Wellbeing Outcomes from All Ecosystem Services",
       x = "All Ecosystem Services % Change",
       y = "Well-being (WB) % Change") +
  scale_fill_manual(values = c("Non-economic" = scales::alpha("#56B4E9", 0.4), "Economic" = scales::alpha("#E69F00", 0.4))) +
  scale_color_manual(values = c("Non-economic" = "#56B4E9", "Economic" = "#E69F00")) +
  theme_minimal() + ylim(-84, 600)

# Refine plot aesthetics
p2_a <- p2 +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        legend.position = "bottom", 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 6))

# Display the plot
p2_a


#SECOND SET COMBINED
# Plot the results with confidence intervals
p3 <- ggplot(plot_data_all_WB_3_4, aes(x = Prov_services_average_percent_transf, colour = WB, fill = WB)) +
  geom_line(aes(y = pred_values), col = "#CC79A7",linewidth = 1) +
  geom_point(data = mydat3, aes(x = Prov_services_average_percent_transf, y = Econ_wellbeing_average_percent_transf, colour = "Economic", fill = "Economic"), size = 1, shape = 21, stroke = 1) +
  geom_point(data = mydat4, aes(x = Prov_services_average_percent_transf, y = Nonecon_wellbeing_average_percent_transf, colour = "Non-economic", fill = "Non-economic"), size = 1, shape = 21, stroke = 1) +
  geom_ribbon(aes(ymin = X1, ymax = X2, fill = WB), alpha = 0.3) +
  labs(title = "(a) Wellbeing Outcomes from Provisioning Ecosystem Services",
       x = "Provisioning Ecosystem Services % Change",
       y = "Well-being (WB) % Change") +
  scale_fill_manual(values = c("Non-economic" = scales::alpha("#56B4E9", 0.4), "Economic" = scales::alpha("#E69F00", 0.4))) +
  scale_color_manual(values = c("Non-economic" = "#56B4E9", "Economic" = "#E69F00")) +
  theme_minimal()+ ylim(-84, 600)

# Refine plot aesthetics
p3_b <- p3 +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        legend.position = "bottom", 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 6))

# Display the plot
p3_b


##Fourth ###

# Plot the results with confidence intervals
p6 <- ggplot(plot_data_all_WB_7_8, aes(x = Suppreg_services_average_percent_transf, colour = WB, fill = WB)) +
  geom_line(aes(y = pred_values),  col = "#CC79A7",linewidth = 1) +
  geom_point(data = mydat7, aes(x = Suppreg_services_average_percent_transf, y = Econ_wellbeing_average_percent_transf, colour = "Economic", fill = "Economic"), size = 1, shape = 21, stroke = 1) +
  geom_point(data = mydat8, aes(x = Suppreg_services_average_percent_transf, y = Nonecon_wellbeing_average_percent_transf, colour = "Non-economic", fill = "Non-economic"), size = 1, shape = 21, stroke = 1) +
  geom_ribbon(aes(ymin = X1, ymax = X2, fill = WB), alpha = 0.3) +
  labs(title = "(a) Wellbeing Outcomes from Supporting & Regulating Services",
       x = "Supporting & Regulating Services % Change",
       y = "Well-being (WB) % Change") +
  scale_fill_manual(values = c("Non-economic" = scales::alpha("#56B4E9", 0.4), "Economic" = scales::alpha("#E69F00", 0.4))) +
  scale_color_manual(values = c("Non-economic" = "#56B4E9", "Economic" = "#E69F00")) +
  theme_minimal()+ ylim(-84, 600)

# Refine plot aesthetics
p6_b <- p6 +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        legend.position = "bottom", 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 6))

# Display the plot
p6_b



##########################################################################
#############Join plots together###########################
#########################################################################

# Recreate plots with legends removed from all
p2_a <- p2_a + theme(legend.position = "none", plot.title = element_blank()) + labs(y = NULL)
p3_b <- p3_b + theme(legend.position = "none", plot.title = element_blank()) + labs(y = NULL)
p6_b <- p6_b + theme(legend.position = "none", plot.title = element_blank()) + labs(y = NULL)

# Open PNG graphics device
#png("Figure_aggregated_models_UPDATED_FINAL2.png", width = 3, height = 6, units = "in", res = 300)

grid.arrange(
  arrangeGrob(
    p2_a, p3_b, p6_b,
    ncol = 1,
    heights = c(4, 4, 4),
    left = textGrob("Well-being % Change", rot = 90, gp = gpar(fontsize = 8))
  )
)

# Close PNG graphics device
#dev.off()

### with multiple Y  axislabels

library(grid)  # Load grid package for text annotation

# Recreate plots with legends removed and title adjustments
p2_a <- p2_a + theme(legend.position = "none", plot.title = element_blank()) + labs(y = "Well-being % Change")
p3_b <- p3_b + theme(legend.position = "none", plot.title = element_blank()) + labs(y = "Well-being % Change")
p6_b <- p6_b + theme(legend.position = "none", plot.title = element_blank()) + labs(y = "Well-being % Change")

# Open PNG graphics device
png("Figure_aggregated_models_UPDATED_FINAL_3.png", width = 3, height = 6, units = "in", res = 300)

# Create layout matrix with a common Y-axis label
grid.arrange(
  arrangeGrob(
    p2_a, p3_b, p6_b, 
    ncol = 1, 
    heights = c(4, 4, 4)
    )
  )


# Close PNG graphics device
dev.off()
