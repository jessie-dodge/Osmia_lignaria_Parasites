# title: "01a_libraries.R"
# author: "Jessie Dodge"
# date: "2024-05-17"
# Purpose: These are packages needed to analyze/visualize landscape effects on  Osmia lignaria parasitism
#######################################################################################################
# for knitting
library(knitr)

# for data cleaning
library(reshape2) # for data melting
library(tidyverse) # contains ggplot2, dplyr, tidyr, readr, purr, tibble, stringr, and forcats pkgs
library(stringr)
library(broom) # for tidy function (summary stats as table)
library(kableExtra)
library(stargazer) # for making SUPER pretty tables
#library(modelsummary) # prints summary stats as table
#library(gtsummary)

# for graphs
library(effects) #for predictor effect plots
library(ggcorrplot) # for ggplot style correlation plots
library(ggrepel) # to keep geom_text from overlapping
library(patchwork) # for plot annotations 
library(Ternary) # base ternary plots

## for high quality graphs
library(cowplot) # arranges plots into a grid
library(ggpubr) # for "pub ready" graphs
library(grid) # grid adds an nx by ny rectangular grid to an existing plot
library(gridExtra) # arrange multiple grid-based plots and draw tables
library(gridGraphics) # convert plots drawn with 'graphics' pkg into plots drawn  with the 'grid' pkg. Visually the same, consists of "grid" grobs
# that can be manipulated with "grid" functions
library(plotly) # interactive, publication-quality graphs
library(rcompanion) # Functions to Support Extension Education Program Evaluation

# Analysis
#library(aod) # for logistic models - I don't know if I need these now
library(AER) # to test for over/under dispersion in poisson glms
library(BiodiversityR)
library(car) # equality of variance test
library(corrplot) # for correlation plots
library(devtools) # for Spearman's correlation test
# library(DHARMa) # better than qqplot when checking normality of zero-inflated data
library(emmeans)
# library(GLMMadaptive) #for mixed models
library(lattice)
library(lme4) # for glmms
library(lmtest)
library(MASS) # necessary for standard negative binomial regression (maybe?) and (definitely needed) to compare SNBR to ZINB
library(multcomp) # post hoc comparison tests
library(multcompView)
library(MuMIn) # dredge 
library(pscl) # GLM with zero-inflated data (like Zero-inflated Poisson regression or zero-inflated negative binomial model)
library(pwr) # t-test, internal Hmisc functions (?)
library(stats)
library(vegan) ## vegan: is an ecological analysis package 
library(visreg) # visualization of regression functions
