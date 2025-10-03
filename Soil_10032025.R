# Author: Mako Shibata (s2471259@ed.ac.uk)
# Created: 03/10/2025 
# Aim: Calculating the DBH (cm), Girth (cm), and Age of trees

# load library ----
library(tidyr)
library(dplyr)
library(readr)

# set working directory ----
setwd("/Users/Owner/Library/CloudStorage/OneDrive-UniversityofEdinburgh/#00_EM/Project_Soil Microclimate/EM_SoilMicroclimate")

# load data ----
soil <- read.csv("soil_all.csv")

# clean data ----
## no pine 
soil <- soil %>% 
  relocate(Genus, .after = Species) %>%
  filter(Genus != "Pinus") %>%
  select(-c(1:7))
  
unique(soil$Genus)

# get average DBH (cm) +- std for the following groups: total, alder, birch, sorbus
DBH_mean <- c(mean(soil$DBH_cm), 
              mean(soil$DBH_cm[soil$Genus == "Alnus"]), 
              mean(soil$DBH_cm[soil$Genus == "Betula"]), 
              mean(soil$DBH_cm[soil$Genus == "Sorbus"])) 
DBH_sd <- c(sd(soil$DBH_cm), 
            sd(soil$DBH_cm[soil$Genus == "Alnus"]), 
            sd(soil$DBH_cm[soil$Genus == "Betula"]), 
            sd(soil$DBH_cm[soil$Genus == "Sorbus"])) 

DBH_max <- c(max(soil$DBH_cm), 
             max(soil$DBH_cm[soil$Genus == "Alnus"]), 
             max(soil$DBH_cm[soil$Genus == "Betula"]), 
             max(soil$DBH_cm[soil$Genus == "Sorbus"])) 

DBH_min <- c(min(soil$DBH_cm), 
             min(soil$DBH_cm[soil$Genus == "Alnus"]), 
             min(soil$DBH_cm[soil$Genus == "Betula"]), 
             min(soil$DBH_cm[soil$Genus == "Sorbus"])) 

Genus <- c("Total", "Alnus", "Betula", "Sorbus")

DBH <- data.frame(Genus, DBH_mean, DBH_sd, DBH_max, DBH_min)
names(DBH) <- c("Genus", "meanDBH", "stdDBH", "maxDBH", "minDBH")

# converting this to girth 
DBH <- DBH %>%
  mutate(girth_cm = meanDBH*3.14, 
         sd_girth_cm = stdDBH*3.14, 
         girth_max = DBH_max*3.14, 
         girth_min = DBH_min*3.14)

# applying the growth rate = 2.5 cm to estimate age
DBH <- DBH %>% 
  mutate(age = girth_cm/2.5, 
         age_sd = sd_girth_cm/2.5, 
         maxage = girth_max/2.5, 
         minage = girth_min/2.5) 


View(DBH)

write.csv(DBH, "DBH_Age.csv")