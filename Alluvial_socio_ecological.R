############################################################################################
#    Generating alluvial plots for Social Ecological Data on Legume Introduction Food Systems
#    By: Anonymysed for double-blind peer review
#    Date: 2023-2024
############################################################################################

# Study are map plus some inset graphs to provide more information 
# Sub plots of the % of studies that were win win within each country
# Similar to https://www.nature.com/articles/s41893-023-01246-x

# 1. install libraries and get data directory#####
# Install only the required libraries
install.packages(c("dplyr", "scales", "tidyverse", "ggplot2", "ggpubr", "ggalluvial"))

# Load only the used libraries
library(dplyr)      # Load dplyr for data manipulation
library(scales)     # Load scales for data transformation
library(tidyverse)  # Load tidyverse for data manipulation and visualization
library(ggplot2)    # Load ggplot2 for data visualization
library(ggpubr)     # Load ggpubr for combining ggplot2 plots
library(ggalluvial) # Load ggalluvial for creating alluvial plots


# 2. set directories#####

wd <- "Insert filepath here"

# Read CSV file into a dataframe
df <- read.csv("Data_maps_and_alluvial_socio_ecological.csv")

# Summarize the count of studies grouped by combined ecological and social outcomes
df_summary <- df %>%
  group_by(Description.combined.ecological.social.outcome) %>%  # Group data by combined outcome description
  summarize(StudiesCount = n())  # Count the number of studies in each group

# Create a stacked bar plot to visualize the counts of different outcomes
ggplot(df_summary, aes(x = Description.combined.ecological.social.outcome, y = StudiesCount, fill = Description.combined.ecological.social.outcome)) +
  geom_bar(stat = "identity") +  # Use bars to represent the counts
  theme_minimal() +  # Apply a minimal theme to the plot
  labs(x = "Combined Ecological and Social Outcome", y = "Count of Studies", title = "Stacked Bar Plot of Outcome Counts") +  # Add labels and title
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_fill_manual(values = c("Negative Ecological Outcome" = "#404788FF",  # Define custom colors for the different outcomes
                               "Negative Social Outcomes" = "#482677FF",          
                               "Negative Socio-ecological Outcomes" = "#440154FF",
                               "Opposing Socio-ecological Outcome" = "#238a8dFF", 
                               "Mixed Socio-ecological Outcomes" = "#33638DFF",
                               "Positive Ecological Outcome" = "#29af7f",      
                               "Positive Social Outcome" = "#73d055FF",          
                               "Positive Socio-ecological Outcomes" = "#FDE725FF"))  # Customize the fill colors

# Summarize the count of studies grouped by both ecological and social outcomes
df_summary <- df %>%
  group_by(Ecological.outcome, Social.outcome) %>%  # Group by both ecological and social outcomes
  summarize(n = n())  # Count the number of studies in each combination of outcomes

# Create an alluvial plot to visualize the relationship between ecological and social outcomes
ggplot(as.data.frame(df_summary),
       aes(y = n, axis1 = Ecological.outcome, axis2 = Social.outcome)) +  # Map y to count, and define two axes
  geom_alluvium(aes(fill = Ecological.outcome), width = 1/8, knot.pos = 0) +  # Add alluvium flows colored by ecological outcome
  geom_stratum(width = 1/8, fill = "white", color = "#7f7f7f", alpha = .5) +  # Add strata (segments) with a defined appearance
  #geom_stratum(aes(fill = Ecological.outcome), fill = "#7f7f7f", alpha = .25, width = 1/8) +  # Optional: Uncomment to recolor ecological strata
  #geom_stratum(aes(fill = Social.outcome), fill = "#7f7f7f", alpha = .25, width = 1/8) +  # Optional: Uncomment to recolor social strata
  guides(fill = "none") +  # Disable legend for fill
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +  # Add labels to strata
  scale_fill_manual(values = c("Win" = "#FDE725FF", "Mix" = "#238a8dFF", "Lose" = "#440154FF")) +  # Define custom colors for alluvial flows
  scale_x_continuous(breaks = 1:2, labels = c("Ecological Outcome", "Social Outcome")) +  # Set x-axis labels for the two axes
  labs(y = "Number of Studies") +  # Set y-axis label
  theme_minimal()  # Apply a minimal theme to the plot

# Specify the legumes you want to include in the analysis
selected_grain_legumes <- c("Chickpea", "Common Bean", "Cowpea", "Groundnut", "Lentil", "Mung and Faba Bean",
                            "Pea", "Pigeonpea", "Soybean", "Multiple Legumes")

# Summarize the dataset for the selected legumes by counting the occurrences of each combination of ecological and social outcomes
df_legume_summary <- df %>%
  filter(Legume %in% selected_grain_legumes) %>%  # Filter the dataframe to include only the selected legumes
  group_by(Legume, Ecological.outcome, Social.outcome) %>%  # Group the data by legume, ecological outcome, and social outcome
  summarize(n = n())  # Count the number of studies in each group and create a summary dataframe

# Create an alluvial plot to visualize the relationship between ecological and social outcomes for the selected legumes
ggplot(as.data.frame(df_legume_summary),
       aes(y = n, axis1 = Ecological.outcome, axis2 = Social.outcome)) +  # Map y to the count and define the two axes
  geom_alluvium(aes(fill = `Ecological.outcome`), width = 1/8, knot.pos = 0) +  # Add alluvial flows colored by ecological outcome
  geom_stratum(width = 1/8, fill = "white", color = "#7f7f7f", alpha = .5) +  # Add strata (segments) with a defined appearance
  guides(fill = "none") +  # Disable legend for fill
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +  # Add labels to the strata showing their names
  scale_fill_manual(values = c("Win" = "#FDE725FF", "Mix" = "#238a8dFF", "Lose" = "#440154FF")) +  # Define custom colors for the alluvial flows
  scale_x_continuous(breaks = 1:2, labels = c("Ecological", "Social")) +  # Set x-axis labels for the two axes
  labs(y = "Number of Studies") +  # Set y-axis label
  theme_minimal() +  # Apply a minimal theme to the plot
  facet_wrap(~ Legume, scales = "free", nrow = 2)  # Create separate plots for each legume, allowing y-axis scales to vary


