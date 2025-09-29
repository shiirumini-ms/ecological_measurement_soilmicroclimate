# Date: 17/09/2025 (created, latest update 14/09/2025)
# Author: Mako Shibata (s2471259@ed.ac.uk)
# Objective: Depth and moisture analyses - Management specific 

# Libraries ----
library(tidyr) #formatting
library(dplyr) #manipulation 
library(ggplot2) #visualisation
library(readr) #manipulation
library(ggpubr)
install.packages("colourpicker")

# Defining functions ----
theme.LPI <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle=0, vjust=1, hjust=1, color="black"), 
          axis.text.y=element_text(size=12, color="black"), 
          axis.title.x=element_text(size=14, face="plain", color="black"), 
          axis.title.y=element_text(size=14, face="plain", color="black"), 
          panel.grid.major.x=element_blank(), 
          panel.grid.minor.x=element_blank(), 
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_blank(), 
          plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), units= , "cm"), 
          plot.title = element_text(size=20, vjust=1, hjust=0.5), 
          legend.text = element_text(size=12, face="italic", color="black"), 
          legend.title = element_blank(), 
          legend.position=c(0.9,0.9))
}


# Set the WD ----
setwd("/Users/Owner/Library/CloudStorage/OneDrive-UniversityofEdinburgh/#00_EM/Project_Soil Microclimate")

# Import data ----
soil_all <- read.csv("soil_all.csv")
View(soil_all)

# Management-specific ----
## A) Visualisation
### Classifying tree ID based on management 
soil_all<- soil_all %>% 
  mutate(soil_all, Management = case_when(
    TreeID %in% c(1, 2, 3, 4, 5, 6, 7, 21, 22, 23, 24, 25) ~ "No mound", 
    TRUE ~ "mound"))

colnames(soil_all)
str(soil_all)
length(soil_all[soil_all$Management == "mound",])
length(soil_all[soil_all$Management == "No mound", ])

### Distance vs O Horizon depth (cm)
p8 <- ggbarplot(
  soil_all, x = "Distance", y = "Depth_cm", 
  add = c("mean_se", "jitter"), 
  add.params = list(shape = "Management"),
  fill= "Management", palette = c(
    "No mound" = "#D9C2C1", 
    "mound" = "gray"),
  position = position_dodge(0.8)
) + 
  labs(x = "Distance from a tree trunk (m)", y = "O Horizon Depth (cm)")

p8

### Distance vs Soil moisture (cm)
p9 <- ggbarplot(
  soil_all, x = "Distance", y = "Moisture_vv", 
  add = c("mean_se", "jitter"), 
  add.params = list(shape = "Management"),
  fill= "Management", palette = c(
    "No mound" = "#A7B9E8", 
    "mound" = "gray"),
  position = position_dodge(0.8)
) + 
  labs(x = "Distance from a tree trunk (m)", y = "Soil moisture (%, v/v)")

p9

combined_plot <- ggarrange(
  p8, p9,
  labels = c("a)", "b)"),
  ncol = 2, nrow = 1,
  common.legend = FALSE
)
print(combined_plot)

### Calculating SEs
#### calculating the standard error of depth. <MOUND> 
mound <- soil_all[soil_all$Management == "mound", ]
mound$count <- row_number(mound)
View(mound)

sd_depth_mound_0.5 = sd(mound$Depth_cm[mound$Distance == 0.5])
se_depth_mound_0.5 <- sd_depth_mound_0.5/sqrt(13)
print(se_depth_mound_0.5)
# 2.321145 cm for 0.5 m 

sd_depth_mound_7.0 = sd(mound$Depth_cm[mound$Distance == 7.0])
se_depth_mound_7.0 <- sd_depth_mound_7.0/sqrt(13)
print(se_depth_mound_7.0)
# 3.490801 cm for 7.0 m

#### calculating the standard error of moisture. <MOUND> 
sd_moisutre_mound_0.5 = sd(mound$Moisture_vv[mound$Distance == 0.5])
sd_moisutre_mound_0.5
se_moisture_mound_0.5 <- sd_moisutre_mound_0.5/sqrt(13)
print(se_moisture_mound_0.5)
# 8.409505 cm for 0.5 m 

sd_moisutre_mound_7.0 = sd(mound$Moisture_vv[mound$Distance == 7.0])
se_moisutre_mound_7.0 <- sd_moisutre_mound_7.0/sqrt(13)
print(se_moisutre_mound_7.0)
# 7.245946 cm for 7.0 m

#### calculating the standard error of depth. <NO MOUND> 

### calculating the standard error of moisture. <NO MOUND>

## B) ANOVA 
depth_lm = lm(Depth_cm ~ Distance, data = mound)
depth_anov = aov(depth_lm, data = soil_birch)
summary(depth_anov)

tapply(soil_birch$Depth_cm, soil_birch$Distance, mean, na.rm=TRUE)
# No significant relation, p = 0.928. 
# Mean 0.5 m: 14.54706 cm, 0.7 m: 18.02941 

plot(depth_lm) 
# Residuals vs. fitted: linearity met. 
# Q-Q plot generally follows the same line, residuals normally distributed. 
# Residuals variances are equal. 
# No significant outliers outside the Cook's distance. 

moisture_lm <- lm(Moisture_vv ~ Distance, data = soil_birch)
moisture_anov <- aov(moisture_lm)
summary(moisture_anov)

tapply(soil_birch$Moisture_vv, soil_birch$Distance, mean, na.rm=TRUE)
# No significant relation, p = 0.822. 
# Mean 0.5 m: 49.35882%, 0.7 m: 61.09412%

plot(moisture_lm)
# Linearity met.
# Q-Q plot generally follows the same line. 
# Residual variances are generally equal.
# No significant outliers outside the Cook's distance. 