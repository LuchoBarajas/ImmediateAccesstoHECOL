###################################################################
#     Dissertation Model - Decission Trees and Random Forest      #
###################################################################

# Libraries and working directory 


library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(GGally)
library(gplots)
library(RColorBrewer)
library(partykit)
library(microbenchmark)
library(ggparty)
library(shapr)
library(shapviz)
library(iml)

options(digits = 2)

setwd("/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/ImmediateAccesstoHECOL/Data and Descriptives/")

# Read data base


# Estimation of Innequality of Opportunity Ex - ante | 

data_ti %<>% select(Sex,SL_Student,Fathers_education,Mothers_education,Rurality,School_sector, age, Immediate_Access)
data_ti %<>% mutate(Sex = if_else(Sex == "Female",1,0))
data_ti$Sex %<>% as.factor()
data_ti %<>% mutate(Fathers_education = if_else(Fathers_education == "None",1,
                                        if_else(Fathers_education == "Incomplete primary",2,
                                        if_else(Fathers_education == "Complete primary", 3,
                                        if_else(Fathers_education == "Incomplete secondary",4,
                                        if_else(Fathers_education == "Complete secondary", 5,
                                        if_else(Fathers_education == "Incomplete technical education", 6,
                                        if_else(Fathers_education == "Complete technical education", 7, 
                                        if_else(Fathers_education == "Incomplete undergraduated degree", 8,
                                        if_else(Fathers_education == "Complete undergraduate degree", 9, 10))))))))))

data_ti$Fathers_education %<>% as.factor()


data_ti %<>% mutate(Mothers_education = if_else(Mothers_education == "None",1,
                                        if_else(Mothers_education == "Incomplete primary",2,
                                        if_else(Mothers_education == "Complete primary", 3,
                                        if_else(Mothers_education == "Incomplete secondary",4,
                                        if_else(Mothers_education == "Complete secondary", 5,
                                        if_else(Mothers_education == "Incomplete technical education", 6,
                                        if_else(Mothers_education == "Complete technical education", 7, 
                                        if_else(Mothers_education == "Incomplete undergraduated degree", 8,
                                        if_else(Mothers_education == "Complete undergraduate degree", 9, 10))))))))))


data_ti$Mothers_education %<>% as.factor()

data_ti %<>% mutate(Rurality = if_else(Rurality == "Rural",1,0))
data_ti$Rurality %<>% as.factor()

data_ti %<>% mutate(School_sector = if_else(School_sector == "Public",1,0))
data_ti$School_sector %<>% as.factor()

data_ti %<>% mutate(age = if_else(age == "Theoretical Age",1,0))
data_ti$age %<>% as.factor()

data_ti %<>% mutate(age = if_else(age == "Non Theoretical Age",0,1))
data_ti$age %<>% as.factor()

data_ti %<>% mutate(Immediate_Access = if_else(Immediate_Access == "IAHE",1,0))
data_ti$Immediate_Access %<>% as.factor()



# Conditional Inference Tree---------------------------------------------------

Sys.time()

set.seed(1)
ctree_model = ctree(Immediate_Access ~ Sex + SL_Student + 
                      Fathers_education + Mothers_education + Rurality + 
                      School_sector + age, data = data_ti)



print(ctree_model)


plt <- (
  ggparty(
    ctree_model,
    terminal_space = 0,
    add_vars       = list(intercept = "$node$info$nobs",
                          beta      = "$data$wealth_destin")) +
    geom_edge(
      size = .5
    ) +
    geom_edge_label(
      colour = "black",
      size   = 3
    ) +
    geom_node_label(
      aes(col = splitvar),
      line_list = list(aes(label = splitvar),
                       aes(label = paste("p =",
                                         formatC(p.value,
                                                 format = "e",
                                                 digits = 2))),
                       aes(label = ""),
                       aes(label = id)
      ),
      line_gpar = list(list(size = 10),
                       list(size = 8),
                       list(size = 6),
                       list(size      = 5,
                            col       = "black",
                            fontface  = "bold",
                            alignment = "left")
      ),
      ids = "inner"
    ) +
    geom_node_label(
      line_list = list(aes(label = paste("N =", round(intercept, 2))),
                       aes(label = paste("Y =", round(beta, 2))),
                       aes(label = ""),
                       aes(label = id)
      ),
      line_gpar = list(list(size = 10),
                       list(size = 10),
                       list(size = 6),
                       list(size = 5,
                            col = "black",
                            fontface = "bold",
                            alignment = "left")),
      ids     = "terminal",
      nudge_y = -.05
    ) +
    theme(
      legend.position   = "none"
    ) +
    coord_cartesian(
      xlim = c(0, 1),
      ylim = c(-0.1, 1.1)
    ) + 
    labs(color = "Varible:")
)

ggsave(
  "DTIAHE_2.png",
  path    = "/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/ImmediateAccesstoHECOL/Estimations/",
  plot    = plt,
  width   = 250,
  height  = 50,
  units   = "cm",
  bg = "white",
  limitsize = F
)


var_importance = varimp(cforest_model)
print(var_importance)

  # Disimilarity Index

data_ti$y_predict_tree = predict(ctree_model, type = 'node') # Predicción de los nodos en los que está cada aobservación 
P_iahe =  mean(as.numeric(data_ti$Immediate_Access)) - 1
DIndex = data_ti %>% select(everything(), Node = y_predict_tree)
DIndex %<>% group_by(Node) %>% summarise(weight = n()/nrow(data_ti), Pi = mean(as.numeric(Immediate_Access))-1)
DIndex %<>% mutate(p_iahe = P_iahe)

DIndex = sum(abs(DIndex$Pi- DIndex$p_iahe)*DIndex$weight) * (1/(2*P_iahe)) *100


# Conditional Random Forest-----------------------------------------------------

cforest_model = cforest(Immediate_Access ~ Sex + SL_Student + 
                          Fathers_education + Mothers_education + Rurality + 
                          School_sector + age, data = data_ti, ntree = 100)


  # Dissimilarity Index

types = predict(cforest_model, type = "node")
types_Df = data.frame(types)
colnames(types_Df) = 1:100
p_iahe = data.frame(rep(P_iahe, 476740))
p_iahe %<>% select(p_iahe = rep.P_iahe..476740.)
iahe = D_Index_RF %>% select(Immediate_Access)

types = types_Df %>% select(1)

calculate_DIndex <- function(types,p_iahe,iahe){
  Tree_types = cbind(types,p_iahe,iahe)
  column_index = 1
  colnames(Tree_types)[column_index]<- "type"
  DIndexBase = Tree_types %>% group_by(type) %>%
    summarise(weight = n()/nrow(Tree_types),Pi = mean(as.numeric(Immediate_Access))-1) %>%
    mutate(p_iahe = P_iahe)
  DIndex = sum(abs(DIndexBase$Pi- DIndexBase$p_iahe)*DIndexBase$weight) * (1/(2*P_iahe)) *100
  
  return(DIndex)
}

# Creating  an empty vector to store the results for each column
result_vector <- vector("numeric", length = ncol(types_Df))

# Looping through each column of the "Types" data frame and calculate D-Index
for (i in 1:ncol(types_Df)) {
  result_vector[i] <- calculate_DIndex(types_Df[, i], p_iahe, iahe)
}

# Print the results for each column
Results_vector = as_tibble(result_vector)
mean(result_vector)


# Shappley decomposition--------------------------------------------------------

