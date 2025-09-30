# Date: 14/09/2025 (created, latest update 14/09/2025)
# Author: Mako Shibata (s2471259@ed.ac.uk)
# Objective: Additional Species-specific Analyses

# Libraries ----
library(tidyr) #formatting
library(dplyr) #manipulation 
library(ggplot2) #visualisation
library(readr) #manipulation
library(ggpubr)

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

# 1. Cleaning (Oak to Silver Birch) ----
unique(soil_all$Species)
soil_all <- soil_all %>% 
  mutate(Species = recode(Species,
                          "English Oak (Quercus robur)" = "Silver Birch (Betula pendula)"))
unique(soil_all$Species)

soil_all <- soil_all %>%
  mutate(Genus = case_when(
    Species == "Alder" ~ "Alnus", 
    Species == "Silver Birch (Betula pendula)" ~ "Betula", 
    Species == "Downy Birch (Betula pubescens)" ~ "Betula",
    Species == "Scots pine (Pinus sylvestris)" ~ "Pinus",
    Species == "Rowan (Sorbus aucuparia)" ~ "Sorbus"
  ))
  
unique(soil_all$Genus)

write.csv(soil_all, "soil_all.csv")
soil_all <- soil_all %>% 
  dplyr::select("Date", "TreeID","Species","DBH_cm","Cardinal",
         "Depth_cm","Moisture_vv","Distance",      
         "Notes","x","y","running_order","Genus")
View(soil_all)

# 2. ALL TREES (skipped, refer to the Soil_microclimate_2025EM.R) ----

# 3. Only silver birch : A) Barplot B) ANOVA ----
### Linear model and ANOVA Depth vs. Distance
soil_pendula <- soil_all[soil_all$Species == "Silver Birch (Betula pendula)", ]
View(soil_pendula)

depth_lm = lm(Depth_cm ~ Distance, data = soil_pendula)
depth_anov = aov(depth_lm, data = soil_pendula)
summary(depth_anov)

tapply(soil_pendula$Depth_cm, soil_pendula$Distance, mean, na.rm=TRUE)
# No significant relation, p = 0.928. 
# Mean 0.5 m: 16.19167 cm, 0.7 m: 16.52500

plot(depth_lm) 
# Residuals vs. fitted: linearity met. 
# Q-Q plot generally follows the same line, residuals normally distributed. 
# Residuals variances are equal. 
# No significant outliers outside the Cook's distance. 

moisture_lm <- lm(Moisture_vv ~ Distance, data = soil_pendula)
moisture_anov <- aov(moisture_lm)
summary(moisture_anov)

tapply(soil_pendula$Moisture_vv, soil_pendula$Distance, mean, na.rm=TRUE)
# No significant relation, p = 0.822. 
# Mean 0.5 m: 56.25455, 0.7 m: 59.64545

plot(moisture_lm)
# Linearity met.
# Q-Q plot generally follows the same line. 
# Residual variances are generally equal.
# No significant outliers outside the Cook's distance. 


### Barplot visualisation 
#### Depth 
p1 <- ggbarplot(
  soil_pendula, x = "Distance", y = "Depth_cm", 
  add = c("mean_se", "jitter"),
  fill = "#BF504D", 
  xlab = "Distance from tree trunk (m)",
  ylab = "O layer depth (cm) "
  
) + geom_text(
  aes(x = Distance, y = max(soil_pendula$Depth_cm) + 1, label = "n = 12"),
  position = position_nudge(x = -0.1, y = -5),
  inherit.aes = FALSE,
  size = 4
)


print(p1)

#### calculating the standard error of depth. 
sd_depth_pendula_0.5 = sd(soil_pendula$Depth_cm[soil_pendula$Distance == 0.5])
se_depth_pendula_0.5 <- sd_depth_pendula_0.5/sqrt(25)
print(se_depth_pendula_0.5)
# 1.761042 cm for 0.5 m 

sd_depth_pendula_7.0 = sd(soil_pendula$Depth_cm[soil_pendula$Distance == 7.0])
se_depth_pendula_7.0 <- sd_depth_pendula_7.0/sqrt(25)
print(se_depth_pendula_7.0)
# 1.831624 cm for 7.0 m

# Soil moisture
p2 <- ggbarplot(
  soil_pendula, x = "Distance", y = "Moisture_vv", 
  add = c("mean_se", "jitter"),
  fill = "skyblue", 
  xlab = "Distance from tree trunk (m)",
  ylab = "Soil moisture (% v/v) "
  
) + geom_text(
  aes(x = Distance, y = max(soil_birch$Depth_cm) + 1, label = "n = 12"),
  position = position_nudge(x = -0.27, y = 45),
  inherit.aes = FALSE,
  size = 4
)

print(p2)

# calculating standard errors for soil moisture.
sd_moisture_pendula_0.5 = sd(soil_pendula$Moisture_vv[soil_pendula$Distance == 0.5])
se_moisture_pendula_0.5 <- sd_moisture_pendula_0.5/sqrt(25)
print(se_moisture_pendula_0.5)
# 5.150288 %  for 0.5 m 

sd_moisture_pendula_7.0 = sd(soil_pendula$Moisture_vv[soil_pendula$Distance == 7.0])
se_moisture_pendula_7.0 <- sd_moisture_pendula_7.0/sqrt(25)
print(se_moisture_pendula_7.0)
#  4.519148 % for 7.0 m

combined_plot_pendula <- ggarrange(
  p1, p2,
  labels = c("a)", "b)"),
  ncol = 2, nrow = 1,
  common.legend = FALSE
)
print(combined_plot_pendula)


# 4. Only Birch : A) Barplot B) ANOVA ### Likely to use this one ----
soil_birch <- soil_all[soil_all$Genus == "Betula", ]
View(soil_birch)

depth_lm = lm(Depth_cm ~ Distance, data = soil_birch)
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


### Barplot visualisation 
#### Depth 
p3 <- ggbarplot(
  soil_birch, x = "Distance", y = "Depth_cm", 
  add = c("mean_se", "jitter"),
  fill = "#BF504D", 
  xlab = "Distance from tree trunk (m)",
  ylab = "O layer depth (cm) "
  
) + geom_text(
  aes(x = Distance, y = max(soil_birch$Depth_cm) + 1, label = "n = 17"),
  position = position_nudge(x = -0.2, y = -5),
  inherit.aes = FALSE,
  size = 4
)


print(p3)

#### calculating the standard error of depth. 
sd_depth_birch_0.5 = sd(soil_birch$Depth_cm[soil_birch$Distance == 0.5])
se_depth_birch_0.5 <- sd_depth_birch_0.5/sqrt(25)
print(se_depth_birch_0.5)
# 1.6725 cm for 0.5 m 

sd_depth_birch_7.0 = sd(soil_birch$Depth_cm[soil_birch$Distance == 7.0])
se_depth_birch_7.0 <- sd_depth_birch_7.0/sqrt(25)
print(se_depth_birch_7.0)
# 2.077207 cm for 7.0 m

# Soil moisture
p4 <- ggbarplot(
  soil_birch, x = "Distance", y = "Moisture_vv", 
  add = c("mean_se", "jitter"),
  fill = "skyblue", 
  xlab = "Distance from tree trunk (m)",
  ylab = "Soil moisture (% v/v) "
  
) + geom_text(
  aes(x = Distance, y = max(soil_birch$Depth_cm) + 1, label = "n = 17"),
  position = position_nudge(x = -0.27, y = 45),
  inherit.aes = FALSE,
  size = 4
)

print(p4)

# calculating standard errors for soil moisture.
sd_moisture_birch_0.5 = sd(soil_birch$Moisture_vv[soil_birch$Distance == 0.5])
se_moisture_birch_0.5 <- sd_moisture_birch_0.5/sqrt(25)
print(se_moisture_birch_0.5)
# 4.80973 %  for 0.5 m 

sd_moisture_birch_7.0 = sd(soil_birch$Moisture_vv[soil_birch$Distance == 7.0])
se_moisture_birch_7.0 <- sd_moisture_birch_7.0/sqrt(25)
print(se_moisture_birch_7.0)
#  4.309133 % for 7.0 m

combined_plot_birch <- ggarrange(
  p3, p4,
  labels = c("a)", "b)"),
  ncol = 2, nrow = 1,
  common.legend = FALSE
)
print(combined_plot_birch)
# 5. Species-specific: DBH (cm) vs. Depth_0.5 ----
soil_0.5 <- soil_all[soil_all$Distance == 0.5, ]
View(soil_0.5)
p5 <- ggscatter(soil_0.5, x = "DBH_cm", y = "Depth_cm", color= "Genus") + 
  theme.LPI() +
  theme(axis.text = element_text(size = 12, angle = 0, color = 'black'),
        axis.text.x = element_text(hjust = 0.5, color = 'black'), 
        legend.position = c(0.85, 0.85)) + 
  labs(x = "DBH (cm)", y = "O horizon Depth (cm)")

p5

### For alder, 3 are very young, one is > 20 cm. There seems to be a tendency 
### for depth to get thinner as DBH gets larger. 
### For birch, mostly it is < 10 cm diameter. There seems to be no effect < 10 cm. 
### Progressively with over 10 cm diameter O horizon depth may get thinner, but more
### data points inbetween 10-20 cm range are needed to confirm this impact. 

### if alder shows a clear linear relationship despite 4 points and 
### betula likely does not, we can infer that alder has stronger influence on 
### these soils because they thrive in waterlogged conditions. 
### alder is better adapted to the soil preliminary present there before planting, 
### which likely led them to influence soil condition (particularly, organic layer thinning)
### depth should be thinning with more DBH if our hypothesis is correct


# 6. Species-specific: DBH (cm) vs. Difference ----
soil_7.0 <- soil_all[soil_all$Distance == 7.0, ]
View(soil_7.0)

Dif <- soil_7.0$Depth_cm - soil_0.5$Depth_cm
# larger the DBH (cm), larger Dif expected. 
difference <- data.frame(Dif, soil_0.5$TreeID, soil_0.5$Species, soil_0.5$Genus, soil_0.5$DBH_cm)
colnames(difference)
p6 <- ggscatter(difference, x = "soil_0.5.DBH_cm", y = "Dif", color = "soil_0.5.Genus") + 
  theme.LPI() + 
  theme(axis.text = element_text(size = 12, angle = 0, color = 'black'),
        axis.text.x = element_text(hjust = 0.5, color = 'black'), 
        legend.position = c(0.80, 0.20)) + 
  labs(x = "DBH (cm)", y = "O horizon Depth Loss (cm) ", color="Genus")
  
p6

### Difference 
# 7. Species-specific: A) Barplot  ----
### A) Barplot for Alder, Birch, and Rowan. 
soil_all_nonpine <- soil_all %>%
  filter(Genus != "Pinus")

p7 <- ggbarplot(
  soil_all_nonpine, x = "Distance", y = "Depth_cm", 
  add = c("mean_se", "jitter"), 
  add.params = list(shape = "Genus"),
  fill= "Genus", palette = c(
    "Alnus" = "#BF504D", 
    "Betula" = "#6E9E76", 
    "Sorbus" = "purple"),
  position = position_dodge(0.8)
) + 
  labs(x = "Distance from a tree trunk (m)", y = "O Horizon Depth (cm)")

p7

p10 <- ggbarplot(
  soil_all_nonpine, x = "Distance", y = "Moisture_vv", 
  add = c("mean_se", "jitter"), 
  add.params = list(shape = "Genus"),
  fill= "Genus", palette = c(
    "Alnus" = "#BF504D", 
    "Betula" = "#6E9E76", 
    "Sorbus" = "purple"),
  position = position_dodge(0.8)
) + 
  labs(x = "Distance from a tree trunk (m)", y = "Soil moisture (%, v/v)")

p10

# 7. Species-specific: B) ANOVA - all one way. ----
## 7-1. Depth --------
#### Subsetting data for One-way ANOVA 
View(soil_all_nonpine)
nrow(soil_all_nonpine[soil_all_nonpine$Genus == "Alnus", ]) # n = 4 
nrow(soil_all_nonpine[soil_all_nonpine$Genus == "Betula", ]) # n = 17 
nrow(soil_all_nonpine[soil_all_nonpine$Genus == "Sorbus", ]) # n = 3

Alnus <- subset(soil_all_nonpine,Genus == "Alnus")
Betula <- subset(soil_all_nonpine, Genus == "Betula")
Sorbus <- subset(soil_all_nonpine, Genus == "Sorbus")

alnus_mod <- aov(Depth_cm ~ Distance, data = Alnus)
summary(alnus_mod)
# p = 0.01, df = 6. Significant difference. 
betula_mod <- aov(Depth_cm ~ Distance, data = Betula)
summary(betula_mod)
# p = 0.29, df = 32, not significant. 
sorbus_mod <- aov(Depth_cm ~ Distance, data = Sorbus)
summary(sorbus_mod)
# p = 0.58, df = 4, not significant. 

#### Two-way ANOVA 
mod <- aov(Depth_cm ~ Distance * Genus, data = soil_all_nonpine)
Anova(mod, type = 3)
summary(mod)
# p = 0.20, Species is not a significant factor for determining difference in
# organic layer depth, but this lacks sample size and violates balanced sample size. 
# therefore, not valid. 

## 7-2. Soil moisture -------
alnus_mod <- aov(Moisture_vv ~ Distance, data = Alnus)
summary(alnus_mod)
# p = 0.08, df = 6. not significant. 
betula_mod <- aov(Moisture_vv ~ Distance, data = Betula)
summary(betula_mod)
# p = 0.14, df = 32, not significant. 
sorbus_mod <- aov(Moisture_vv ~ Distance, data = Sorbus)
summary(sorbus_mod)
# p = 0.38, df = 4, not significant. 

#### Two-way ANOVA 
mod <- aov(Moisture_vv ~ Distance * Genus, data = soil_all_nonpine)
Anova(mod, type = 3)
summary(mod)
# p = 0.44, Species is not a significant factor for determining difference in
# organic layer depth, but this lacks sample size and violates balanced sample size. 
# therefore, not valid.



# 8. Species-specific AND Total species: Barplot ----
## combining rows with Genus = Total
datavis <- soil_all_nonpine
datavis <- soil_all_nonpine %>% 
  mutate(Genus = "Total")
datavis <- bind_rows(
  soil_all_nonpine, datavis
)
str(datavis)

datavis$Genus <- factor(datavis$Genus, levels = c("Total", "Alnus", "Betula", "Sorbus"))

p11 <- ggbarplot(
  datavis, x = "Distance", y = "Depth_cm", 
  add = c("mean_se", "jitter"), 
  add.params = list(shape = "Genus"),
  fill= "Genus", palette = c(
    "Total" = "#8B4513",
    "Alnus" = "#E0EEEE", 
    "Betula" = "#C1CDCD", 
    "Sorbus" = "#838B8B"),
  position = position_dodge(0.8)
) + 
  labs(x = "Distance from a tree trunk (m)", y = "O Horizon Depth (cm)")

p11

p12 <- ggbarplot(
  datavis, x = "Distance", y = "Moisture_vv", 
  add = c("mean_se", "jitter"), 
  add.params = list(shape = "Genus"),
  fill= "Genus", palette = c(
    "Total" = "#6495ED",
    "Alnus" = "#E0EEEE", 
    "Betula" = "#C1CDCD", 
    "Sorbus" = "#838B8B"),
  position = position_dodge(0.8)
) + 
  labs(x = "Distance from a tree trunk (m)", y = "Soil moisture (%, v/v)")

p12

ggarrange(p11, p12,
          nrow = 1, ncol = 2,
          labels = c("a", "b"))

datavis$Distance <- as.factor(datavis$Distance)

dev.off()

# 8. Reporting -----
### The se and means to be calculated.
### What does this imply? 
