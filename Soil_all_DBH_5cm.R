# Date: 09/09/2025 (created, latest update)
# Author: Mako Shibata (s2471259@ed.ac.uk)
# Objective: Data cleaning, ANOVA analysis, Visualisation of 1. Soil O horizon depth 2. Soil moisture (% v/v)
# across different distance from tree trunk. **BETULA PENDULA**

# Libraries ----
library(tidyr) #formatting
library(dplyr) #manipulation 
library(ggplot2) #visualisation
library(readr) #manipulation
library(ggpubr)

# Defining functions ----
theme.LPI <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle=0, vjust=1, hjust=1), 
          axis.text.y=element_text(size=12), 
          axis.title.x=element_text(size=14, face="plain"), 
          axis.title.y=element_text(size=14, face="plain"), 
          panel.grid.major.x=element_blank(), 
          panel.grid.minor.x=element_blank(), 
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_blank(), 
          plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), units= , "cm"), 
          plot.title = element_text(size=20, vjust=1, hjust=0.5), 
          legend.text = element_text(size=12, face="italic"), 
          legend.title = element_blank(), 
          legend.position=c(0.9,0.9))
}


# Set the WD ----
setwd("/Users/Owner/Library/CloudStorage/OneDrive-UniversityofEdinburgh/#00_EM/Project_Soil Microclimate")

# Import data ----
soil_all <- read.csv("soil_all.csv")
str(soil_all)
soil_all$Distance <- as.factor(soil_all$Distance)
str(soil_all)
View(soil_all)

soil_raw <- read.csv("RAW_ALL_soil_microclimate.csv")

# Plot DBH (cm) vs. Difference in thickness between 0.5 m and 7.0 m ----
## Import data 
soil <- read.csv("RAW_ALL_soil_microclimate.csv")
View(soil)
summary(soil)
colnames(soil) 

soil <- soil %>% 
  mutate(Tree.species = if_else(
    Tree.species == "other", 
    Other..specify....Tree.species, 
    Tree.species
  )) %>%
  select(-Other..specify....Tree.species)

soil <- soil %>% 
  mutate(Ground.Cover.Types = if_else(
    Ground.Cover.Types == "other", 
    Mixed.Other..specify....Ground.Cover.Types, 
    Ground.Cover.Types
  )) %>%
  select(-Mixed.Other..specify....Ground.Cover.Types)

View(soil_all)
str(soil_all)
## Scatterplot
Dif <- soil_all$Depth_cm[soil_all$Distance == 7.0] - soil_all$Depth_cm[soil_all$Distance == 0.5]
TreeID <- soil_all$TreeID[soil_all$Distance == 7.0]
Difference <- data.frame(TreeID = TreeID, Dif_depth = Dif, DBH = soil_all$DBH_cm[soil_all$Distance == 0.5])
str(Difference)
ggscatter(Difference, x = "DBH", y = "Dif_depth", size = 3.5, color = "brown")

mean(soil_all$DBH_cm)
# Mean DBH (cm) 8.1 cm 
sd(soil_all$DBH_cm)
# standard deviation DBH (cm) 5.3 cm 

### Description 
# When 7.0 m is thicker, it has the opposite relation between DBH. 
# Larger the DBH (Older the tree), thicker the organic depth beneath the canopy. 
# The Depth difference is not correlating with DBH. *With DBH possible underestimation*


# What about depth vs. tree age straighforwardly? 
Difference$Depth_0.5 <- soil_all$Depth_cm[soil_all$Distance == 0.5]
Difference$Species <- soil_all$Species[soil_all$Distance == 0.5]
color <- Difference$Species
View(Difference)
ggscatter(Difference, 
          x = "DBH",
          y = "Depth_0.5", 
          color = color)


# Rather a positive relation between DBH (cm) and depth underneath the canopy. 
# Counteracts the overall result....
# OK so not the age. Age is not a big influencing factor. 
