###################################################################
#     Dissertation Model - Decission Trees and Random Forest      #
###################################################################

# Erase everything

rm(list = ls())

# Libraries and working directory 

library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(GGally)
library(gplots)
library(RColorBrewer)
library(party)
library(microbenchmark)


options(digits = 2)

setwd("/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/ImmediateAccesstoHECOL/Data and Descriptives/")

# Read data base

data_ti = read.csv("Data_TI.csv")

# Estimation of Innequality of Opportunity Ex - ante | 

data_ti %<>% select(Sex,SL_Student,Fathers_education,Mothers_education,Rurality,School_sector, age, Immediate_Access)

# Conditional Inference Tree


ctree_model = ctree(factor(Immediate_Access) ~ factor(Sex) + factor(SL_Student) + 
                      factor(Fathers_education) + factor (Mothers_education) + factor(Rurality) + 
                      factor(School_sector) + factor(age), data = data_ti)

# Conditional Random Forest

result_2 <- microbenchmark(ctree_forest = cforest(factor(Immediate_Access) ~ factor(Sex) + factor(SL_Student) + 
                         factor(Fathers_education) + factor (Mothers_education) + factor(Rurality) + 
                         factor(School_sector) + factor(age), data = data_ti))

png(width )
plot(ctree_model)
