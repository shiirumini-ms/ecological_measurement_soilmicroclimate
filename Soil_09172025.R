# Date: 17/09/2025 (created, latest update 29/09/2025)
# Author: Mako Shibata (s2471259@ed.ac.uk)
# Objective: Depth and moisture analyses - Management specific 

# Libraries ----
library(tidyr) #formatting
library(dplyr) #manipulation 
library(ggplot2) #visualisation
library(readr) #manipulation
library(ggpubr)
install.packages("colourpicker")
library(car)
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
setwd("/Users/Owner/Library/CloudStorage/OneDrive-UniversityofEdinburgh/#00_EM/Project_Soil Microclimate/EM_SoilMicroclimate")

# Import data ----
soil_all <- read.csv("soil_all.csv")
View(soil_all)

# Management-specific
## A) Visualisation ----
### Classifying tree ID based on management 
soil_all<- soil_all %>% 
  mutate(soil_all, Management = case_when(
    TreeID %in% c(1, 2, 3, 4, 5, 6, 7, 21, 22, 23, 24, 25) ~ "No mound", 
    TRUE ~ "mound"))

colnames(soil_all)
str(soil_all)
View(soil_all[soil_all$Management == "mound", ])
View(soil_all[soil_all$Management == "No mound", ])
nrow(soil_all[as.character(soil_all$Management) == "mound", ])
nrow(soil_all[as.character(soil_all$Management) == "No mound", ])

### mound = 13, No mound = 12. 

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

dev.off()

### Calculating SEs ----
#### calculating the standard error of depth. <MOUND> 
mound <- soil_all[soil_all$Management == "mound", ]
mound$count <- row_number(mound)
nrow(mound)

sd_depth_mound_0.5 = sd(mound$Depth_cm[mound$Distance == 0.5])
se_depth_mound_0.5 <- sd_depth_mound_0.5/sqrt(13)
print(se_depth_mound_0.5)
# 2.32 cm for 0.5 m 

sd_depth_mound_7.0 = sd(mound$Depth_cm[mound$Distance == 7.0])
se_depth_mound_7.0 <- sd_depth_mound_7.0/sqrt(13)
print(se_depth_mound_7.0)
# 3.49 cm for 7.0 m

#### calculating the standard error of moisture. <MOUND> 
sd_moisutre_mound_0.5 = sd(mound$Moisture_vv[mound$Distance == 0.5])
sd_moisutre_mound_0.5
se_moisture_mound_0.5 <- sd_moisutre_mound_0.5/sqrt(13)
print(se_moisture_mound_0.5)
# 7.38 % for 0.5 m 

sd_moisutre_mound_7.0 = sd(mound$Moisture_vv[mound$Distance == 7.0])
se_moisutre_mound_7.0 <- sd_moisutre_mound_7.0/sqrt(13)
print(se_moisutre_mound_7.0)
# 6.36 % for 7.0 m

#### calculating the standard error of depth. <NO MOUND> 
no_mound <- soil_all[soil_all$Management == "No mound", ]
nrow(no_mound)

sd_depth_nomound_0.5 = sd(no_mound$Depth_cm[no_mound$Distance == 0.5])
se_depth_nomound_0.5 <- sd_depth_nomound_0.5/sqrt(12)
print(se_depth_nomound_0.5)
# 2.38 cm for 0.5 m 

sd_depth_nomound_7.0 = sd(no_mound$Depth_cm[no_mound$Distance == 7.0])
se_depth_nomound_7.0 <- sd_depth_nomound_7.0/sqrt(12)
print(se_depth_nomound_7.0)
# 3.69 cm for 0.5 m 

#### calculating the standard error of moisture. <NO MOUND>
sd_moisutre_nomound_0.5 = sd(no_mound$Moisture_vv[no_mound$Distance == 0.5])
se_moisutre_nomound_0.5 <- sd_moisutre_nomound_0.5/sqrt(12)
print(se_moisutre_nomound_0.5)
# 6.61 % for 0.5 m

sd_moisutre_nomound_7.0 = sd(no_mound$Moisture_vv[no_mound$Distance == 7.0])
se_moisutre_nomound_7.0 <- sd_moisutre_nomound_7.0/sqrt(12)
print(se_moisutre_nomound_7.0)
# 5.60 % for 7.0 m

## B) Two-way ANOVA ----
#### Depth vs. management & tree trunk distance 
mod <- aov(Depth_cm ~ Distance * Management, data = soil_all)
Anova(mod, type = 3)
summary(mod)
# Significant for distance
# Non-significant for management
# Non-significant interaction: management difference does NOT make a difference to 
# the effect of distance

# just test 
test <- aov(Depth_cm ~ Distance, data = mound)
test2 <- aov(Depth_cm ~ Distance, data = no_mound)
summary(test)
summary(test2)

# two-way anova: p = 0.4758, df = 46. 
# p = 0.05 for mound, p = 0.34 for no-mound.
# df = 24, and df = 22, respectively. 

tapply(mound$Depth_cm, mound$Distance, mean, na.rm=TRUE)
tapply(no_mound$Depth_cm, no_mound$Distance, mean, na.rm=TRUE)
# mound: Mean 0.5 m: 15.18 cm, 7.0 m: 23.8 cm  
# no_mound: Mean 0.5 m: 13.93 cm, 7.0 m: 18.2 cm 


plot(mod) 
# Residuals vs. fitted: linearity met. 
# Q-Q plot generally follows the same line, residuals normally distributed. 
# Residuals variances are equal. 
# No significant outliers outside the Cook's distance. 

#### Moisture vs. Depth and management 
mod2 <- aov(Moisture_vv ~ Distance * Management, data = soil_all)
Anova(mod2, type = 3)
summary(mod2)

test3 <- aov(Moisture_vv ~ Distance, data = no_mound)
summary(test3)
test4 <- aov(Moisture_vv ~ Distance, data = mound)
summary(test4)

# two-way anova: p = 0.60, df = 46. 
# p = 0.04 for no-mound, p = 0.22 for mound. 
# df = 22 and df = 24 respectively.

tapply(no_mound$Moisture_vv, no_mound$Distance, mean, na.rm=TRUE)
tapply(mound$Moisture_vv, mound$Distance, mean, na.rm=TRUE)
# None Mean 0.5 m: 46.2%, 0.7 m: 65.3%
# Mound Mean 0.5 m: 52.3%, 7.0 m: 64.6%

plot(mod2)
# Linearity met.
# Q-Q plot generally follows the same line. 
# Residual variances are generally equal.
# No significant outliers outside the Cook's distance. 