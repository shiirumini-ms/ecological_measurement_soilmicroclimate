# Date: 30/09/2025 (created, latest update 30/09/2025)
# Author: Mako Shibata (s2471259@ed.ac.uk)
# Objective: ANOVA power analysis

# What is the sample size required to meet the 
# condition we are supposed to meet? 

# One-Way
# alpha = 0.05 
# power = 0.80 
# factor level = 2 

library(stats)
power.anova.test(groups = 2, between.var = 110.5, within.var = 115.5, power = 0.8)
## n = 25 per group for between.var = 1, within.var = 3 
## actual: between.var = 537.9, within.var = 114.4 
## total species number is suffice. 
## actual: between.var = 486.9, within.var = 114.2 for mound. n = 4.  
## actual: between.var = 110.5, within.var = 115.5 for no-mound. n = 10. 


# Two-way: sample size is too small to detect the effect size 
#          as per G*Power analysis

# For management, mounded soil had thinner O-horizon (p = 0.05, df = 24) near the tree 
# but not for non-mounded soil (p = 0.34, df = 22). 
# There was no significant difference in depth due to management type (p = 0.48, df = 2, 46). 
# Soil moisture was lower near the tree in non-mounded (p = 0.04, df = 22) but not for 
# mounded soil (p = 0.22, df = 24). 
# There was no significant difference in moisture due to management type (p = 0.60, df = 2, 46). 
