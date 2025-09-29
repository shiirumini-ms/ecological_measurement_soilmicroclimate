# Date: 09/09/2025 (created, latest update 14/09/2025)
# Author: Mako Shibata (s2471259@ed.ac.uk)
# Objective: Data cleaning, ANOVA analysis, Visualisation of 1. Soil O horizon depth 2. Soil moisture (% v/v)
# across different distance from tree trunk. **BETULA (downy & silver)**

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
soil_birch <- read.csv("soil_birch.csv")
str(soil_birch)
soil_birch$Distance <- as.factor(soil_birch$Distance)
str(soil_birch)
View(soil_birch)

# Linear model and ANOVA Depth vs. Distance----
depth_lm = lm(Depth_cm ~ Distance, data = soil_birch)
depth_anov = aov(depth_lm, data = soil_birch)
summary(depth_anov)

tapply(soil_birch$Depth_cm, soil_birch$Distance, mean, na.rm=TRUE)

plot(depth_lm) 
# Residuals vs. fitted: linearity met. 
# Q-Q plot generally follows the same line, residuals normally distributed. 
# Residuals variances are equal. 
# No significant outliers outside the Cook's distance. 


moisture_lm <- lm(Moisture_vv ~ Distance, data = soil_birch)
moisture_anov <- aov(moisture_lm)
summary(moisture_anov)

tapply(soil_birch$Moisture_vv, soil_birch$Distance, mean, na.rm=TRUE)

plot(moisture_lm)
# Linearity met.
# Q-Q plot generally follows the same line. 
# Residual variances are generally equal.
# No significant outliers outside the Cook's distance. 


# Barplot visualisation ----
#### Depth 
p1 <- ggbarplot(
  soil_birch, x = "Distance", y = "Depth_cm", 
  add = c("mean_se", "jitter"),
  fill = "#BF504D", 
  xlab = "Distance from tree trunk (m)",
  ylab = "O layer depth (cm) "
  
) + geom_text(
  aes(x = Distance, y = max(soil_birch$Depth_cm) + 1, label = "n = 16"),
  position = position_nudge(x = -0.1, y = -5),
  inherit.aes = FALSE,
  size = 4
)


print(p1)

#### calculating the standard error of depth. 
sd_depth_birch_0.5 = sd(soil_birch$Depth_cm[soil_birch$Distance == 0.5])
se_depth_birch_0.5 <- sd_depth_birch_0.5/sqrt(25)
print(se_depth_birch_0.5)
# 1.666361 cm for 0.5 m 

sd_depth_birch_7.0 = sd(soil_birch$Depth_cm[soil_birch$Distance == 7.0])
se_depth_birch_7.0 <- sd_depth_birch_7.0/sqrt(25)
print(se_depth_birch_7.0)
# 2.063437 cm for 7.0 m

# Soil moisture
p2 <- ggbarplot(
  soil_birch, x = "Distance", y = "Moisture_vv", 
  add = c("mean_se", "jitter"),
  fill = "skyblue", 
  xlab = "Distance from tree trunk (m)",
  ylab = "Soil moisture (% v/v) "
  
) + geom_text(
  aes(x = Distance, y = max(soil_birch$Depth_cm) + 1, label = "n = 16"),
  position = position_nudge(x = -0.2, y = 45),
  inherit.aes = FALSE,
  size = 4
)

print(p2)

# calculating standard errors for soil moisture.
sd_moisture_birch_0.5 = sd(soil_birch$Moisture_vv[soil_birch$Distance == 0.5])
se_moisture_birch_0.5 <- sd_moisture_birch_0.5/sqrt(25)
print(se_moisture_birch_0.5)
# 4.962292 %  for 0.5 m 

sd_moisture_birch_7.0 = sd(soil_birch$Moisture_vv[soil_birch$Distance == 7.0])
se_moisture_birch_7.0 <- sd_moisture_birch_7.0/sqrt(25)
print(se_moisture_birch_7.0)
#  4.224644 % for 7.0 m

combined_plot <- ggarrange(
  p1, p2,
  labels = c("a)", "b)"),
  ncol = 2, nrow = 1,
  common.legend = FALSE
)
print(combined_plot)

str(soil_birch)
# Linearity between O layer depth and Soil moisture
cor.test(soil_birch$Depth_cm, soil_birch$Moisture_vv, method = "spearman")

# Spearman correlation test: 
# p-value = 0.3927, rho = 0.1563878 
# Not significant relation between depth and soil moisture. 

# Visualisation
p3 <- ggscatter(soil_birch, 
                x = "Moisture_vv", 
                y = "Depth_cm", 
                combine=FALSE)

print(p3)
# No pattern, if anything, 2nd degree polynomial but that is not scientifically supported. 
