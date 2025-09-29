# Date: 07/09/2025 (last update: 08/09/2025) 
# Author: Mako Shibata (s2471259@ed.ac.uk)
# Objective: Data cleaning, ANOVA analysis, Visualisation of 1. Soil O horizon depth 2. Soil moisture (% v/v)
# across different distance from tree trunk. **ALL SPECIES**

# Mostly upland healthland habitat is what I would describe as the habitat far from the tree.

# Libraries ----
library(tidyr) #formatting
library(dplyr) #manipulation 
library(ggplot2) #visualisation
library(readr) #manipulation

# Defining functions --------
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
theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.position = "right")
}

# Set the WD ----
setwd("/Users/Owner/Library/CloudStorage/OneDrive-UniversityofEdinburgh/#00_EM/Project_Soil Microclimate")

# Import and Clean data ----
soil <- read.csv("RAW_ALL_soil_microclimate.csv")
View(soil)
summary(soil)
colnames(soil) 



soil <- soil %>%
  mutate(Tree.species = if_else(
    Tree.species == "other", 
    Other..specify....Tree.species, 
    Tree.species)) %>%
  select(-Other..specify....Tree.species)

soil <- soil %>% 
  mutate(Ground.Cover.Types = if_else(
    Ground.Cover.Types == "other", 
    Mixed.Other..specify....Ground.Cover.Types, 
    Ground.Cover.Types
  )) %>%
  select(-Mixed.Other..specify....Ground.Cover.Types)

soil0.5 <- soil %>% 
  select(Date = Date...Time, 
         TreeID = Tree.Plot.ID.., 
         Species = Tree.species, 
         DBH_cm = DBH..cm., 
         Cardinal = Cardinal.Direction, 
         Depth_cm = Soil.organic.layer.depth..cm., 
         Moisture_vv = Soil.moisture....V.V., 
         Distance = Distance.from.tree.trunk..m., 
         Notes = Notes...Comments, 
         x = x, 
         y = y
         )

View(soil0.5)

soil7.0 <- soil %>% 
  select(Date = Date...Time, 
         TreeID = Tree.Plot.ID.., 
         Species = Tree.species, 
         DBH_cm = DBH..cm., 
         Cardinal = Cardinal.Direction, 
         Depth_cm = Soil.organic.layer.depth..cm..1, 
         Moisture_vv = Soil.moisture....V.V..1, 
         Distance = Distance.from.tree.trunk..m..1, 
         Notes = Notes...Comments, 
         x = x, 
         y = y
  )

View(soil7.0)

soil_all <- bind_rows(soil0.5, soil7.0) %>%
  arrange(TreeID, Distance) %>%
  mutate(running_order = row_number())

View(soil_all)
summary(soil_all)
soil_all$Distance <- as.factor(soil_all$Distance)
summary(soil_all)
soil_all <- na.omit(soil_all)
write.csv(soil_all, "soil_all.csv")

# Linear model and ANOVA Depth vs. Distance----
depth_lm = lm(Depth_cm ~ Distance, data = soil_all)
depth_anov = aov(depth_lm, data = soil_all)
summary(depth_anov)

str(soil_all)
is.factor(soil_all$Distance)
levels(soil_all$Distance)

tapply(soil_all$Depth_cm, soil_all$Distance, mean, na.rm=TRUE)

plot(depth_lm) 
# Residuals vs. fitted: linearity met. 
# Q-Q plot generally follows the same line, residuals normally distributed. 
# Residuals variances are equal. 
# No significant outliers outside the Cook's distance. 

# Linear model and ANOVA Moisture vs. Distance ----
soil_all$Distance <- factor(soil_all$Distance)
moisture_lm <- lm(Moisture_vv ~ Distance, data = soil_all)
moisture_anov <- aov(moisture_lm)
summary(moisture_anov)
tapply(soil_all$Moisture_vv, soil_all$Distance, mean, na.rm=TRUE)

plot(moisture_lm)
# Linearity met.
# Q-Q plot generally follows the same line. 
# Residual variances are generally equal.
# No significant outliers outside the Cook's distance. 

# Barplot Visualisation ----
install.packages("ggpubr")
library(ggpubr)
library(ggplot2)

#### Depth 
p1 <- ggbarplot(
  soil_all, x = "Distance", y = "Depth_cm", 
  add = c("mean_se", "jitter"),
  fill = "#BF504D", 
  xlab = "Distance from tree trunk (m)",
  ylab = "O layer depth (cm) "
  
) + geom_text(
  aes(x = Distance, y = max(soil_all$Depth_cm) + 1, label = "n = 25"),
  position = position_nudge(x = -0.1, y = -5),
  inherit.aes = FALSE,
  size = 4
)

#### calculating the standard error of depth. 
sd_depth_all_0.5 = sd(soil_all$Depth_cm[soil_all$Distance == 0.5])
se_depth_all_0.5 <- sd_depth_all_0.5/sqrt(25)
print(se_depth_all_0.5)
# 1.632391 cm for 0.5 m 

sd_depth_all_7.0 = sd(soil_all$Depth_cm[soil_all$Distance == 7.0])
se_depth_all_7.0 <- sd_depth_all_7.0/sqrt(25)
print(se_depth_all_7.0)
# 2.546383 cm for 7.0 m

# Soil moisture
p2 <- ggbarplot(
  soil_all, x = "Distance", y = "Moisture_vv", 
  add = c("mean_se", "jitter"),
  fill = "skyblue", 
  xlab = "Distance from tree trunk (m)",
  ylab = "Soil moisture (% v/v) "
  
) + geom_text(
  aes(x = Distance, y = max(soil_all$Depth_cm) + 1, label = "n = 25"),
  position = position_nudge(x = 0, y = 45),
  inherit.aes = FALSE,
  size = 4
)


# calculating standard errors for soil moisture.
sd_moisture_all_0.5 = sd(soil_all$Moisture_vv[soil_all$Distance == 0.5])
se_moisture_all_0.5 <- sd_moisture_all_0.5/sqrt(25)
print(se_moisture_all_0.5)
# 4.913187 %  for 0.5 m 

sd_moisture_all_7.0 = sd(soil_all$Moisture_vv[soil_all$Distance == 7.0])
se_moisture_all_7.0 <- sd_moisture_all_7.0/sqrt(25)
print(se_moisture_all_7.0)
# 4.173028 % for 7.0 m

combined_plot <- ggarrange(
  p1, p2,
  labels = c("a)", "b)"),
  ncol = 2, nrow = 1,
  common.legend = FALSE
)

combined_plot 
# Boxplot visualisation Depth vs Distance ----
(Depth_distance_all.p <- ggplot(soil_all, aes(x = Distance, y = Depth_cm)) + 
    geom_boxplot(fill = "#CD3323", alpha = 0.8, colour = "#8B2323") + 
    theme.LPI() + 
    theme(axis.text = element_text(size = 12, angle = 0)) + 
    labs(x= "Distance from tree trunk (m)", y = "O layer depth (cm) "))


# barplot visualisation Soil moisture vs Distance 
(Moisture_distance_all <- ggplot(soil_all, aes(x = Distance, y = Moisture_vv)) + 
    geom_boxplot(fill = "lightskyblue", alpha = 0.8, colour = "royalblue") + 
    theme.LPI() + 
    theme(axis.text = element_text(size = 12, angle =0)) + 
    labs(x = "Distance from tree trunk (m)", y = "Soil moisture (%, v/v)"))


# Scatterplot Soil moisture vs. Depth ----
color_species <- soil_all$Species
(moisture_depth <- ggplot(soil_all, aes(x = Moisture_vv, y = Depth_cm, color= color_species)) + 
    geom_point(mapping = aes(x = Moisture_vv, y = Depth_cm)) + 
    geom_smooth(method=lm, color="black", fill = "gray", se=TRUE) +
    theme.LPI() + 
    theme(axis.text = element_text(size = 12, angle = 0, color = 'black'),
          axis.text.x = element_text(hjust = 0.5), color = 'black', 
          legend.position = "right") + 
    labs(x = "Soil moisture (% v/v)", y = "Organic layer depth (cm)")) 

# No significant relation between soil moisture and organic layer depth. 
# Species specific. Linear regression. 
depth_moist <- lm(Depth_cm ~ Moisture_vv, data = soil_all)
print(depth_moist)
plot(depth_moist)
# linearity is not met..? 
# residuals are not normally distributed. 
# variances of residuals are not equal. (arguable)
# No significant outliers. 
# => Pearson's correlation coefficient 

cor.test(soil_all$Depth_cm, soil_all$Moisture_vv, method = "pearman")
# Spearman correlation test: 
# p-value = 0.1173, rho = 0.2243633 
# Not significant relation between depth and soil moisture. 

# Histogram for Soil Depth (cm) to check for normality ----
(hist(soil_all$Depth_cm,
     main = "Histogram of Soil Depth",
     xlab = "Soil Depth (cm)",
     ylab = "Frequency",
     col = "#CD3323",
     border = "#8B2323",
     breaks = 15))  # number of bins

# Histogram for Soil moisture (% v/v) to check for normality -
(hist(soil_all$Moisture_vv, 
  main = "Histogram of Soil moisture", 
  xlab = "Soil moisture (% v/v)", 
  ylab = "Frequency", 
  col = "skyblue",
  border="royalblue",
  breaks=10))

(ggplot(soil_all, aes(x = Depth_cm, fill = Distance)) +
  geom_histogram(binwidth = 2, alpha = 0.5, position = "identity") +
  xlab("Soil Depth (cm)") +
  ylab("Frequency") +
  ggtitle("Histogram of Soil Depth by Distance") +
  theme.LPI())

# Frequency at both distances seem to maximise around 10-20 cm. 

(ggplot(soil_all, aes(x = Moisture_vv, fill = Distance)) +
    geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") +
    xlab("Soil moisture (% v/v)") +
    ylab("Frequency") +
    ggtitle("Histogram of soil moisture by distance") +
    theme.LPI())

# Frequency at 0.5 m seems to peak around 30-40% but frequency of 7.0 seems to peak around 75-80%.



# Only Birch (Betula) nwe data frame -----
soil_birch <- soil_all[soil_all$Species %in% c("Downy Birch (Betula pubescens)", "Silver Birch (Betula pendula)"), ]
# Only Silver birch (Betula pendula)
soil_silverbirch <- soil_all[soil_all$Species == "Silver Birch (Betula pendula)", ]

write.csv(soil_birch, "soil_birch.csv")


# DBH (cm) distribution ----
hist(soil_all$DBH_cm, 
     breaks = 15)

# What if I only look into DBH > 5.0 cm.

unique(soil_all$Species)
length(soil_all$Species[soil_all$Species == "Alder"]) #4
length(soil_all$Species[soil_all$Species == "Silver Birch (Betula pendula)" ]) #11
length(soil_all$Species[soil_all$Species == "Downy Birch (Betula pubescens)"]) #5
length(soil_all$Species[soil_all$Species == "Rowan (Sorbus aucuparia)"]) #3
length(soil_all$Species[soil_all$Species == "English Oak (Quercus robur)"]) #1
length(soil_all$Species[soil_all$Species == "Scots pine (Pinus sylvestris)"]) #1

# Species -- English Oak to Silver Birch (Betula pendula)
# Changed upon photo insepction. Mis-note taking. 


