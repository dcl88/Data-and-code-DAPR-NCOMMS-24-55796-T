############################################################################################
#    Mapping for Social Ecological Data on Legume Introduction Food Systems
#    By: Anonymysed for double-blind peer review
#    Date: 07/12/23
############################################################################################

# Study are map plus some inset graphs to provide more information 
# Sub plots of the % of studies that were win win within each country
# Similar to https://www.nature.com/articles/s41893-023-01246-x

# 1. install libraries and get data directory#####
install.packages(c("sp", "sf", "tmap", "stars", "raster", "tidyverse", "ggplot2", "ggpubr", "viridis", "cowplot","ggspatial"))

library(sp)         # Load sp for spatial data classes and methods
library(sf)         # Load sf for spatial data classes and methods
library(tmap)       # Load tmap for thematic maps
library(stars)      # Load stars for raster data manipulation
library(raster)     # Load raster for raster data analysis
library(tidyverse)  # Load tidyverse for data manipulation and visualization
library(ggplot2)    # Load ggplot2 for data visualization
library(ggpubr)     # Load ggpubr for combining ggplot2 plots
library(viridis)    # Load viridis for color scales
library(cowplot)    # Load cowplot for combining plots
library(ggspatial)


# 2. set directories#####

wd <- "Insert filepath here"

# Read CSV file into a dataframe
df <- read.csv("Data_maps_and_alluvial_socio_ecological.csv")



# Maps ####
#Read in data

countries_sdf <- countries_sdf <- st_read(dsn = "Insert filepath here") %>%
   dplyr::select(NAME,ISO_A3,INCOME_GRP,geometry)
countries_sdf_robin <- st_zm(st_transform(countries_sdf, crs = "+proj=robin +datum=WGS84"))


study_info_df <-  read.csv("Data_maps_and_alluvial_socio_ecological.csv")
study_points_sdf <- st_as_sf(study_info_df, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84")
study_points_sdf_robin <- st_zm(st_transform(study_points_sdf, crs = "+proj=robin +datum=WGS84"))

# study locations

# Plotting basic map of study locations with study points
ggplot() +
  geom_sf(data = countries_sdf, fill = "#7f7f7f", color = "white") +  # Basic country map with grey fill
  geom_sf(data = study_points_sdf, color = "#20A387FF", size = 2) +  # Highlighting study points in teal
  theme_minimal() +  # Minimal theme for clean aesthetics
  ggspatial::annotation_north_arrow(location = "tl",  # Adding north arrow to the top-left
                                    pad_x = unit(0.3, "in"), pad_y = unit(0, "in"),  
                                    height = unit(1, "cm"), width = unit(1, "cm")) +
  coord_sf(crs = 4326)  # Coordinate reference system set to WGS 84

# Summarizing study points by country
study_points_summary <- study_info_df %>%
  group_by(Case.countries) %>%
  summarize(StudiesCount = n())

# Correcting country names for accurate labeling
study_points_summary$Case.countries <- str_replace(study_points_summary$Case.countries, "CÃ´te dâ€™Ivoire", "Côte d'Ivoire")
study_points_summary$Case.countries <- str_replace(study_points_summary$Case.countries, "USA", "United States of America")
study_points_summary$Case.countries <- str_replace(study_points_summary$Case.countries, "TÃ¼rkiye", "Turkey")

# Merging study points summary with country shapefile data
countries_sdf <- left_join(countries_sdf, study_points_summary, by = c("NAME" = "Case.countries"))

# Plotting countries with the number of studies using a color gradient
ggplot(countries_sdf) +
  geom_sf(aes(fill = StudiesCount), color = "white") +
  theme_minimal()+
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0, "in"),  height = unit(1, "cm"), width = unit(1, "cm"), style =north_arrow_orienteering)+
  scale_fill_viridis_c(name = "Site Count")  # Color gradient based on site count

# Another map plot to show study points over countries with studies count
ggplot(countries_sdf) +
  geom_sf(aes(fill = StudiesCount), color = "white") +
  geom_sf(data = study_points_sdf, color = "#20A387FF", size = 1) +  # Highlighting study points
  theme_minimal()+
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0, "in"),  height = unit(1, "cm"), width = unit(1, "cm"), style =north_arrow_orienteering)+
  coord_sf(crs = 4326) +
  scale_fill_viridis_c(name = "Site Count")  # Color gradient for visual clarity


# Calculate summary statistics for joint ecological and social outcomes by country
joint_outcome_summary <- study_info_df %>% 
  group_by(Case.countries) %>%  # Group data by case countries
  summarise(total_studies = n(),  # Count total number of studies per country
            win_win_studies = sum(Combined.ecological.social.outcome == "Win-Win"),  # Count "Win-Win" outcomes
            win_win_percentage = (win_win_studies / total_studies) * 100,  # Calculate win-win percentage
            mix_mix_studies = sum(Combined.ecological.social.outcome %in% c("Win-Mix", "Mix-Win", "Lose-Mix", "Mix-Lose", "Mix-Mix")),  # Count mixed outcomes
            mix_mix_percentage = (mix_mix_studies / total_studies) * 100,  # Calculate mixed outcome percentage
            lose_lose_studies = sum(Combined.ecological.social.outcome == "Lose-Lose"),  # Count "Lose-Lose" outcomes
            lose_lose_percentage = (lose_lose_studies / total_studies) * 100) %>%  # Calculate lose-lose percentage
  mutate(quantile = ntile(total_studies, 4))  # Divide countries into 4 quantiles based on total studies


# Join the joint outcome summary with the countries spatial dataframe
joint_outcome_summary$Case.countries <- str_replace(joint_outcome_summary$Case.countries, "CÃ´te dâ€™Ivoire", "Côte d'Ivoire")
joint_outcome_summary$Case.countries <- str_replace(joint_outcome_summary$Case.countries, "India (Jammu and Kashmir)", "India")  
joint_outcome_sdf <- left_join(countries_sdf, joint_outcome_summary, by = c("NAME" = "Case.countries"))


# Create a ggplot map for visualizing win-win percentage
ggplot(joint_outcome_sdf) +
  geom_sf(aes(fill = win_win_percentage), color = "black") +  # Fill map by win-win percentage
  theme_minimal() +  # Apply minimal theme
  annotation_north_arrow(location = "tl",  # Add north arrow at the top left
                         pad_x = unit(0.3, "in"), pad_y = unit(0, "in"),  # Adjust padding of the arrow
                         height = unit(1, "cm"), width = unit(1, "cm")) +  # Set size of the arrow
  scale_fill_gradient(low = "white", high = "#FDE725FF", name = "Win Percentage (%)",  # Color gradient for win percentage
                      limits = c(0, 100))  # Set limits from 0 to 100%

# Create a ggplot map for visualizing mixed outcome studies
ggplot(joint_outcome_sdf) +
  geom_sf(aes(fill = mix_mix_studies), color = "black") +  # Fill map by number of mixed outcome studies
  theme_minimal() +
  annotation_north_arrow(location = "tl",  # Add north arrow at the top left
                         pad_x = unit(0.3, "in"), pad_y = unit(0, "in"),  
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  scale_fill_gradient(low = "white", high = "#238a8dFF", name = "Mixed Percentage (%)",  # Color gradient for mixed percentage
                      limits = c(0, 100))  # Set limits from 0 to 100%

# Create a ggplot map for visualizing lose-lose studies
ggplot(joint_outcome_sdf) +
  geom_sf(aes(fill = lose_lose_studies), color = "black") +  # Fill map by number of lose-lose studies
  theme_minimal() +
  annotation_north_arrow(location = "tl",  # Add north arrow at the top left
                         pad_x = unit(0.3, "in"), pad_y = unit(0, "in"),  
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  scale_fill_gradient(low = "white", high = "#440154FF", name = "Lose Percentage (%)",  # Color gradient for lose percentage
                      limits = c(0, 100))  # Set limits from 0 to 100%

