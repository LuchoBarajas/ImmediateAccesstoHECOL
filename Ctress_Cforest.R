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
library(dineq)
library(ggplot2)
library(hrbrthemes)


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

data_ti %<>% select(Immediate_Access, everything())


# Conditional Inference Tree---------------------------------------------------



set.seed(1)
ctree_model = ctree(Immediate_Access ~ Sex + SL_Student + 
                      Fathers_education + Mothers_education + Rurality + 
                      School_sector + age, data = data_ti)

ctree_model_2 = ctree(Immediate_Access ~ Sex + SL_Student + 
                      Fathers_education + Mothers_education + Rurality + 
                      School_sector + age, data = data_ti, control = ctree_control(maxdepth = 4))



print(ctree_model)


plt <- (
  ggparty(
    ctree_model_2,
    terminal_space = 0,
    add_vars       = list(intercept = "$node$info$nobs",
                          beta      = "$data$Immediate_Access")) +
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
      line_gpar = list(list(size = 14),
                       list(size = 12),
                       list(size = 10),
                       list(size      = 9,
                            col       = "black",
                            fontface  = "bold",
                            alignment = "left")
      ),
      ids = "inner"
    ) +
    geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = Immediate_Access),
                                          position = position_fill()),
                                 xlab("")),
                   # draw only one label for each axis
                   shared_axis_labels = TRUE,
                   # draw line between tree and legend
                   legend_separator = TRUE
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
  "DTIAHE_3.png",
  path    = "/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/ImmediateAccesstoHECOL/Estimations/",
  plot    = plt,
  width   = 40,
  height  = 25,
  units   = "cm",
  bg = "white",
  limitsize = F
)


  # Disimilarity Index

data_ti$types = predict(ctree_model, type = 'node')

calculate_DIndex <- function(data,y){
  data$y = as.numeric(y)
  p = mean(data$y)-1
  p_k = data %>% group_by(types) %>% summarise(p_k = mean(y)-1, w_k = n()/nrow(data), suma = abs(p_k-p))
  DIndex = (1/(2*p)) * sum(p_k$w_k*p_k$suma)
  return(DIndex)
}


# Conditional Random Forest-----------------------------------------------------

cforest_model = cforest(Immediate_Access ~ Sex + SL_Student + 
                          Fathers_education + Mothers_education + Rurality + 
                          School_sector + age, data = data_ti, ntree = 100)

var_importance = varimp(cforest_model)
print(var_importance)


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
t.test(Results_vector$value)


# Plotting the Dissimilariry Index results

ggplot() + geom_histogram(data = Results_vector,aes(x=value), binwidth=0.05, fill="#01579b", color="#e9ecef", alpha=0.7) +
  ggtitle("Bin size = 0.1") + theme(plot.title = element_text(size = 13), 
                                                    panel.background = element_blank(),
                                    panel.grid = element_blank(),
                                    axis.line = element_blank()) +
  labs(x = "Disimilarity Index", y = "Frecuency")+
  scale_x_continuous(breaks = seq(15.5, 16.5, by = 0.2), labels = seq(15.5, 16.5, by = 0.2)) + 
  geom_density(data = Results_vector, aes(x=value), alpha =.4, fill="#99FFFF") +
  geom_vline(data = Results_vector,aes(xintercept = mean(value)),color="black", linetype="dashed", size= 0.5) +
  geom_text(data = Results_vector, aes(x = 16.09, y = 20, label = "Mean = 16.2"))


hist(Results_vector$value)

# Shappley decomposition--------------------------------------------------------

library(partykit)

shapley_exante_binary <- function(data, target) {
  formula <- as.formula(paste(target, "~ ."))
  
  set.seed(123)
  # Total IOp
  model <- ctree(formula, data = data)
  
  ## Get the total IOp value
  y    <- data[, target]
  x    <- data[, -which(names(data) == target)]
  types <- predict(model, type = "node")
  
  data = cbind(data, types)
  
  IOp <- calculate_DIndex(data,y)
  print(IOp)
  total_IOp <- IOp
  gc()
  
  # All possible combinations
  v <- names(x)
  print(v)
  combinations <- lapply(1:length(v), function(x) combn(v, x, simplify = FALSE))
  combinations <- unlist(combinations, recursive = FALSE)
  
  # Shapley Decomposition
  shap_values <- NULL
  m = 1
  for (n in v) { # For each circumstance n in v
    print(n)
    j = 1
    MC = NULL
    for (i in (length(combinations)):1) { # for all possible combinations starting from the last one
      if (n %in% combinations[[i]]) {   # If n is in that specific combination
        if (length(combinations[[i]]) == 1) { # If the combination is just one circumstance
          formula <- formula(paste(target, "~", paste(combinations[[i]], collapse = "+")))
          
          set.seed(123)
          model_1 <- ctree(formula, data = data)
          data_1 = data
          data_1$types <- predict(model_1)
          
          # Compute IOp
          IOp_1 <- calculate_DIndex(data_1,y)
          
          MC[j] <- IOp_1 - 0
          j = j + 1
          
        } else { # If the combination is more than one circumstance
          combination_vars <- which(!colnames(data) %in% c(combinations[[i]], target))
          
          formula <- formula(paste(target, "~", paste(combinations[[i]], collapse = "+"))) # All circumstances in combination
          
          # Put 1 (no variance) in all circumstances that are not part of the combination
          data_1 <- data
          for (col in combination_vars) {
            data_1[, col] <- 1  
          }
          data_1$y <- y
          set.seed(123)
          model_1 <- ctree(formula, data = data_1)
          data_1$types <- predict(model_1)
          
          IOp_1   <- calculate_DIndex(data_1, y)
          
          # Put 1 to our n circumstance
          data_2 <- data_1
          n_col <- which(colnames(data_2) == n)
          
          data_2[, n_col] <- 1
          
          set.seed(123)
          model_2 <- ctree(formula, data = data_2)
          data_2$types = predict(model_2)
          IOp_2   <- calculate_DIndex(data_2,y)
          
          MC[j] <- IOp_1 - IOp_2 # Marginal contribution
          j = j + 1
        }
      }
    }
    # Get the mean Marginal Contribution of circumstance n
    shap_values[m] <- mean(MC)
    m = m + 1
  }
  
  # Normalize the Shapley values
  max_v <- which(shap_values == max(shap_values))
  n = 0
  for (s in shap_values) {
    n = n + 1
    if (s < 0) {
      shap_values[max_v] <- shap_values[max_v] + s
      shap_values[n] <- 0
    }
  }
  
  normalized_shap_values <- shap_values / sum(shap_values, na.rm = T)
  shap_values <- normalized_shap_values * total_IOp
  # Return the Shapley values and normalized Shapley values
  names(shap_values) <- colnames(x)
  return(list(model                  = model,
              total                  = total_IOp,
              names                  = colnames(x),
              shap_values            = shap_values,
              normalized_shap_values = normalized_shap_values)
  )
}

data_ti %<>% select(-types)

shapley_exante_binary(data_ti, "Immediate_Access")
