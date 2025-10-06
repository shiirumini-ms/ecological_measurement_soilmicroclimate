---
author: Mako Shibata
title: "Appendix. A: Summary of R code used in statistical analyses and visualisation. R version 4.5.1."
output: html_document
---

# Preamble

## Packages


``` r
library(tidyr) #formatting
library(dplyr) #manipulation 
library(ggplot2) #visualisation
library(readr) #manipulation
library(ggpubr) #visualisation
```

## Loading Data


``` r
# set working directory 
setwd("/Users/Owner/Library/CloudStorage/OneDrive-UniversityofEdinburgh/#00_EM/Project_Soil Microclimate/EM_SoilMicroclimate")

# load raw data
soil <- read.csv("RAW_ALL_soil_microclimate.csv")
head(soil)
```

```
##   ObjectID                             GlobalID            CreationDate
## 1        1 7e7b4262-b2a5-4c48-8196-22e85ef89bad 09/04/2025 11:37:52.585
## 2        2 d3613c91-aa93-4ff9-a8c4-2c4e5d3d1e54 09/04/2025 11:38:01.833
## 3        3 7f23214e-d717-47f2-baba-28918c5f613c 09/04/2025 11:38:10.122
## 4        4 68033e36-5b7a-47b7-b185-6a549a57e2a4 09/04/2025 11:38:14.674
## 5        5 b97c64a3-918e-4551-877b-f8682d6d2dce 09/04/2025 11:38:22.796
## 6        6 ea15178a-36cb-4d1b-a710-45b129ca207f 09/04/2025 11:38:28.984
##      Creator                EditDate     Editor      Date...Time Tree.Plot.ID..
## 1 HDuncanAEG 09/04/2025 11:37:52.585 HDuncanAEG 04/09/2025 09:29              1
## 2 HDuncanAEG 09/04/2025 11:38:01.833 HDuncanAEG 04/09/2025 09:37              2
## 3 HDuncanAEG 09/04/2025 11:38:10.122 HDuncanAEG 04/09/2025 09:53              3
## 4 HDuncanAEG 09/04/2025 11:38:14.674 HDuncanAEG 04/09/2025 10:01              4
## 5 HDuncanAEG 09/04/2025 11:38:22.796 HDuncanAEG 04/09/2025 10:07              5
## 6 HDuncanAEG 09/04/2025 11:38:28.984 HDuncanAEG 04/09/2025 10:15              6
##                     Tree.species Other..specify....Tree.species DBH..cm.
## 1                          other                          Alder      3.7
## 2  Silver Birch (Betula pendula)                                    26.0
## 3                          other                          Alder     22.6
## 4 Downy Birch (Betula pubescens)                                     9.6
## 5 Downy Birch (Betula pubescens)                                     4.9
## 6  Silver Birch (Betula pendula)                                     9.6
##   Cardinal.Direction Soil.organic.layer.depth..cm. Soil.moisture....V.V.
## 1              North                          13.0                  17.6
## 2               East                          13.0                  48.9
## 3               West                           9.0                  88.4
## 4              North                           5.0                  48.6
## 5               East                          18.5                  36.8
## 6              South                          25.0                  41.7
##   Distance.from.tree.trunk..m. Soil.organic.layer.depth..cm..1
## 1                          0.5                            17.5
## 2                          0.5                            32.5
## 3                          0.5                            37.0
## 4                          0.5                            36.5
## 5                          0.5                             3.7
## 6                          0.5                             9.0
##   Soil.moisture....V.V..1 Distance.from.tree.trunk..m..1
## 1                    77.6                              7
## 2                    70.2                              7
## 3                    81.6                              7
## 4                    89.7                              7
## 5                    69.9                              7
## 6                    33.1                              7
##                Ground.Cover.Types Mixed.Other..specify....Ground.Cover.Types
## 1 Grassland/Herbs (grasses, forbs                                           
## 2 Grassland/Herbs (grasses, forbs                                           
## 3 Mosses/Lichens (bryophyte or li                                           
## 4 Heather/Moorland (Calluna vulga                                           
## 5 Heather/Moorland (Calluna vulga                                           
## 6 Heather/Moorland (Calluna vulga                                           
##                                                                                Notes...Comments
## 1 Moss present and heather surrounding near by.\nNorth was chosen randomly for first direction.
## 2                                           Near heath with 3m.\nBlueberry and heath near tree.
## 3                           Mixed vegetation lots of reeds and grass, right next to the stream.
## 4                                          Not a breast height for DBH because it is not mature
## 5                   Very immature tree, lots of moss and lichen as well but not the dominate.\n
## 6                                      Mixed in with some moss, near some saplings within 7 m. 
##           x        y
## 1 -4.263475 56.51058
## 2 -4.263690 56.51062
## 3 -4.263706 56.51080
## 4 -4.264063 56.51096
## 5 -4.264144 56.51114
## 6 -4.264439 56.51107
```

``` r
# combine tree species entry into one column
# remove unnecessary columns
soil <- soil %>%
  mutate(Tree.species = if_else(
    Tree.species == "other", 
    Other..specify....Tree.species, 
    Tree.species)) %>%
  select(-Other..specify....Tree.species, 
         -Mixed.Other..specify....Ground.Cover.Types, 
         -Ground.Cover.Types)

# create a long format data, gathered by Distance (0.5 and 7.0 m)
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

soil_all <- bind_rows(soil0.5, soil7.0) %>%
  arrange(TreeID, Distance) %>%
  mutate(running_order = row_number())

# set distance as a factor 
soil_all$Distance <- as.factor(soil_all$Distance)
# omit N/A rows 
soil_all <- na.omit(soil_all)

# save this to csv. 
write.csv(soil_all, "soil_all.csv")
```
