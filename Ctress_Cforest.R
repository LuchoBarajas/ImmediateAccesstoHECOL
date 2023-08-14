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
library(mable)
library(ggpubr)


options(digits = 2)

setwd("/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/ImmediateAccesstoHECOL/Data and Descriptives/")

# Read data base


# Estimation of Innequality of Opportunity Ex - ante | 

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

data_ti %<>% select(Immediate_Access,Sex,SL_Student,Fathers_education,Mothers_education,Rurality,School_sector, age,
                    Global_percentile)

data_ti_ep = data_ti 

data_ti %<>% select(-Global_percentile)



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
    ctree_model,
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
                                 xlab(""),
                                 scale_y_reverse(),
                                 scale_fill_manual(values = c("#AFDCFF","#01579b"))),
                   
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
    labs(color = "Variable:") + scale_color_manual(values = c("#94334F",
                                                              "#707788",
                                                              "#00A7BA",
                                                              "#8AE582",
                                                              "orange",
                                                              "#01579b"))
)

plt

ggsave(
  "DTIAHE_Complete.png",
  path    = "/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/ImmediateAccesstoHECOL/Estimations/",
  plot    = plt,
  width   = 175,
  height  = 50,
  units   = "cm",
  bg = "white",
  limitsize = F
)


  # Disimilarity Index

data_ti$types = predict(ctree_model, type = 'node')

calculate_DIndex <- function(data, y){
  data$y = as.numeric(y)
  p = mean(data$y)-1
  p_k = data %>% group_by(types) %>% summarise(p_k = mean(y)-1, w_k = n()/nrow(data), suma = abs(p_k-p))
  DIndex = (1/(2*p)) * sum(p_k$w_k*p_k$suma)
  return(DIndex)
}


DI_IOP = calculate_DIndex(data_ti, data_ti$Immediate_Access)

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

# Printing the results for each column
Results_vector = as_tibble(result_vector)
t.test(Results_vector$value)


# Plotting the Dissimilariry Index results

ggplot() + geom_histogram(data = Results_vector,aes(x=value), binwidth=0.05, fill="#01579b", color="#e9ecef", alpha=0.7) +
  ggtitle("Bin size = 0.1") + theme(plot.title = element_text(hjust = 0.5, size = 13), 
                                                    panel.background = element_blank(),
                                    panel.grid = element_blank(),
                                    axis.line = element_blank(),
                                    plot.caption = element_text(hjust=c(0,1), size = 12),axis.text = element_text(size = 12)) +
  labs(title = "Dissimilarity Index Histogram",x = "Disimilarity Index", y = "Frecuency",
       caption = c("Source:Colombian Ministry of Education", "Elaboración: @luchobarajas_"))+
  scale_x_continuous(breaks = seq(15.5, 16.5, by = 0.2), labels = seq(15.5, 16.5, by = 0.2)) + 
  geom_density(data = Results_vector, aes(x=value), alpha =.4, fill="#99FFFF") +
  geom_vline(data = Results_vector,aes(xintercept = mean(value)),color="black", linetype="dashed", size= 0.5) +
  geom_text(data = Results_vector, aes(x = 16.09, y = 20, label = "Mean = 16.2")) 


hist(Results_vector$value)

####### Shappley decomposition--------------------------------------------------------


shapley_binary <- function(data, target) {
  formula <- as.formula(paste(target, "~ ."))
  
  set.seed(123)
  # Total IOp
  model <- ctree(formula, data = data)
  
  ## Get the total IOp value
  y    <- data[, target]
  x    <- data[, -which(names(data) == target)]
  types <- predict(model, type = "node")
  
  data = cbind(data, types)
  
  if (TRUE %in% c(class(y) == "tbl")) {
    y <- y$Immediate_Access
  }
  
  IOp <- calculate_DIndex(data, y)
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

shapley_results = shapley_binary(data_ti, "Immediate_Access")

Features = c("Sex", "Socioeconómic level", "Fathers Education", "Mother Education", "Rurality", "School sector", "age")
S_Values = shapley_results$shap_values *100
NS_Values =  shapley_results$normalized_shap_values *100

# Shapley Plot

shapley_plot = data.frame(Features = Features, 'Shapley Values' = S_Values, "Normalized" = NS_Values)
shapley_plot %<>% mutate(Type = "Feature")

legend_labels <- c("Age", "Sex","Rurality", "Socioeconomic Level", "School Sector", "Father's Education", "Mother's Education")

Final_result_NSP = ggplot(shapley_plot, aes(Type))+ geom_bar(aes(weight = Normalized, fill = factor(Shapley.Values)), color = "white") + 
  labs(y ="% of Share") + 
  scale_fill_manual(name = "Circumstances", values = brewer.pal(7, "Blues"), labels = legend_labels)+
  coord_flip()+
  theme(panel.background = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 15))+
  guides(fill = guide_legend(reverse = TRUE))

ggsave("Shappley Decomposition.png", Final_result_NSP, width = 12, height = 7)

####### Ex - Post quintile approach ---------------------------------------------

data_ti_ep$types = predict(ctree_model, type ="node")

quantile_bernstein <- function(x){
  x = as.numeric(x)
  params <- mable(x,
                  M        = c(2, 100),
                  interval = c(min(x), max(x)),
                  controls = mable.ctrl(sig.level = 0.05,
                                        eps   = 1.0e-9, 
                                        maxit = 100),
                  progress = T)
  
  # Estimate the quantiles
  quantiles <- qmixbeta(u = c(0, 0.2, 0.4, 0.6, 0.8, 0.99),
                        p = params$p,
                        interval = c(min(x), max(x)))
  
  # Assign each observation to its quantile
  qi <- cut(x, breaks = quantiles, labels = FALSE, include.lowest = T)
  qi <- replace(qi, is.na(qi), 5)
  return(qi)
}

data_ti_ep %<>% filter(!is.na(Global_percentile)) %>% group_by(types) %>% mutate(q = quantile_bernstein(Global_percentile)) %>% ungroup()

# List of quantile values

quantile_values <- unique(data_ti_ep$q)

# Initializing a list to store the ctree models
ctree_models <- list()

# Iterating through each quantile value
for (quantile_value in quantile_values) {
  # Subset the data for the current quantile
  subset_data <- data_ti_ep[data_ti_ep$q == quantile_value, ]
  
  # Fiting the ctree model for the current quantile
  ctree_model <- ctree(Immediate_Access ~ Sex + SL_Student + 
                         Fathers_education + Mothers_education + Rurality + 
                         School_sector + age + q, data = subset_data)
  
  # Storing the model in the list
  ctree_models[[as.character(quantile_value)]] <- ctree_model
  
  # Predicting the types for the entire data frame
  predictions <- predict(ctree_model, newdata = data_ti_ep, type = "node")
  
  # Creating a column with quantile-specific predictions
  prediction_column_name <- paste0("Predicted_Types_", quantile_value)
  data_ti_ep[prediction_column_name] <- ifelse(data_ti_ep$q == quantile_value, predictions, NA)
}

data_ti_ep %<>% mutate(types = if_else(!is.na(Predicted_Types_1),Predicted_Types_1,
                                       if_else(!is.na(Predicted_Types_2), Predicted_Types_2,
                                               if_else(!is.na(Predicted_Types_3),Predicted_Types_3,
                                                       if_else(!is.na(Predicted_Types_4), Predicted_Types_4, Predicted_Types_5)))))

data_ti_ep %<>% select(-Predicted_Types_1,-Predicted_Types_2,-Predicted_Types_3,-Predicted_Types_4,-Predicted_Types_5)

# Calculating Dissimilarity Index per quintil


a = data_ti_ep %>% filter(q == 1)
a_DI = calculate_DIndex(a,a$Immediate_Access)
b = data_ti_ep %>% filter(q == 2)
b_DI =calculate_DIndex(b,b$Immediate_Access)
c = data_ti_ep %>% filter(q == 3)
c_DI =calculate_DIndex(c,c$Immediate_Access)
d = data_ti_ep %>% filter(q == 4)
d_DI =calculate_DIndex(d,d$Immediate_Access)
e = data_ti_ep %>% filter(q == 5)
e_DI =calculate_DIndex(e,e$Immediate_Access)

# Ponderate mean

values = c(0.24, 0.2, 0.17, 0.15, 0.1)
weight = c(0.2003489887, 0.1976723662, 0.1966356777,0.2004160159,0.2049269514)
Ponderated_mean = sum(values*weight)/sum(weight)

# Shapley Decomposition

Features = c("Sex", "Socioeconómic level", "Fathers Education", "Mother Education", "Rurality", "School sector", "age")

a %<>% select(-Global_percentile,-types,-q)
a_shapley = shapley_binary(a, "Immediate_Access")
a_svalues = a_shapley$shap_values
a_Nsvalues = a_shapley$normalized_shap_values
a_plot = data.frame(Features = Features, 'Shapley Values' = a_svalues, "Normalized" = a_Nsvalues)
a_plot %<>% mutate(Quintile = "Q1")

b %<>% select(-Global_percentile,-types,-q)
b_shapley = shapley_binary(b, "Immediate_Access")
b_svalues = b_shapley$shap_values
b_Nsvalues = b_shapley$normalized_shap_values
b_plot = data.frame(Features = Features, 'Shapley Values' = b_svalues, "Normalized" = b_Nsvalues)
b_plot %<>% mutate(Quintile = "Q2")

c %<>% select(-Global_percentile,-types,-q)
c_shapley = shapley_binary(c, "Immediate_Access")
c_svalues = c_shapley$shap_values
c_Nsvalues = c_shapley$normalized_shap_values
c_plot = data.frame(Features = Features, 'Shapley Values' = c_svalues, "Normalized" = c_Nsvalues)
c_plot %<>% mutate(Quintile = "Q3")


d %<>% select(-Global_percentile,-types,-q)
d_shapley = shapley_binary(d, "Immediate_Access")
d_svalues = d_shapley$shap_values
d_Nsvalues = d_shapley$normalized_shap_values
d_plot = data.frame(Features = Features, 'Shapley Values' = d_svalues, "Normalized" = d_Nsvalues)
d_plot %<>% mutate(Quintile = "Q4")

e %<>% select(-Global_percentile,-types,-q)
e_shapley = shapley_binary(e, "Immediate_Access")
e_svalues = e_shapley$shap_values
e_Nsvalues = e_shapley$normalized_shap_values
e_plot = data.frame(Features = Features, 'Shapley Values' = e_svalues, "Normalized" = e_Nsvalues)
e_plot %<>% mutate(Quintile = "Q5")


# Plotting results

q_shapley_plot = rbind(a_plot, b_plot, c_plot, d_plot, e_plot)

SV = ggplot(q_shapley_plot) +
  aes(x = Quintile, fill = Features, weight = Shapley.Values) +
  geom_bar(color = "white") +
  labs(y ="IOp") + 
  scale_fill_manual(name = "Circumstances", values = brewer.pal(7, "Blues"))+
  theme(panel.background = element_blank(), 
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15), 
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  guides(fill = guide_legend(reverse = F))

ggsave("Shappley quintile.png", SV, width = 12, height = 7)

SVN = ggplot(q_shapley_plot) +
  aes(x = Quintile, fill = Features, weight = Shapley.Values) +
  geom_bar(position = "fill",color = "white") + 
  labs(y ="% of Share IOp")+
  scale_fill_manual(name = "Circumstances", values = brewer.pal(7, "Blues"))+
  theme(panel.background = element_blank(), 
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15), 
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  guides(fill = guide_legend(reverse = F))

ggsave("Shappley quintile normalized.png", SVN, width = 12, height = 7)


####### Ex - Post general approach ---------------------------------------------

data_ti_ep$types = predict(ctree_model, type ="node")

quantile_bernstein <- function(x){
  x = as.numeric(x)
  params <- mable(x,
                      M        = c(2, 30),
                      interval = c(min(x), max(x)),
                      controls = mable.ctrl(sig.level = 0.001,
                                            eps   = 1.0e-9, 
                                            maxit = 100),
                      progress = T)
  
  # Estimate the quantiles
  quantiles <- qmixbeta(u = c(0, 0.2, 0.4, 0.6, 0.8, 0.99),
                        p = params$p,
                        interval = c(min(x), max(x)))
  
  # Assign each observation to its quantile
  qi <- cut(x, breaks = quantiles, labels = FALSE, include.lowest = T)
  qi <- replace(qi, is.na(qi), 5)
  plot(params)
  return(qi)
}

data_ti_ep %<>% na.omit() %>% group_by(types) %>% mutate(q = quantile_bernstein(Global_percentile)) %>% ungroup()

expost_tree = ctree(Immediate_Access ~ types + q,data = data_ti_ep)

data_ti_ep$types_ep = predict(expost_tree, type = 'node')
data_ti_ep %<>% select(Immediate_Access,types_0 =types, q, types = types_ep)
calculate_DIndex(data_ti_ep, data_ti_ep$Immediate_Access)


# Shapley Decomposition

data_ti_ep %<>% select(-types)
data_ti_ep$types_0 = data_ti_ep$types
data_ti_ep$q <- as.factor(data_ti_ep$q)

shapley_binary(data_ti_ep, "Immediate_Access")

# Conditional Inference Trees per Quintile -------------------------------------

ctree_q1 = ctree_models[[1]]

plt_1 <- (
  ggparty(
    ctree_q1,
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
                                 xlab(""),
                                 scale_y_reverse(),
                                 scale_fill_manual(values = c("#AFDCFF","#01579b"))),
                   
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
    labs(color = "Variable:") + scale_color_manual(values = c("#94334F",
                                                              "#707788",
                                                              "#00A7BA",
                                                              "#8AE582",
                                                              "orange",
                                                              "#01579b"))
)

ggsave(
  "DTIAHE_Q1.png",
  path    = "/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/ImmediateAccesstoHECOL/Estimations/",
  plot    = plt_1,
  width   = 175,
  height  = 50,
  units   = "cm",
  bg = "white",
  limitsize = F
)

ctree_q2 = ctree_models[[2]]

plt_2 <- (
  ggparty(
    ctree_q2,
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
                                 xlab(""),
                                 scale_y_reverse(),
                                 scale_fill_manual(values = c("#AFDCFF","#01579b"))),
                   
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
    labs(color = "Variable:") + scale_color_manual(values = c("#94334F",
                                                              "#707788",
                                                              "#00A7BA",
                                                              "#8AE582",
                                                              "orange",
                                                              "#01579b"))
)

ggsave(
  "DTIAHE_Q2.png",
  path    = "/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/ImmediateAccesstoHECOL/Estimations/",
  plot    = plt_2,
  width   = 175,
  height  = 50,
  units   = "cm",
  bg = "white",
  limitsize = F
)


ctree_q3 = ctree_models[[3]]

plt_3 <- (
  ggparty(
    ctree_q3,
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
                                 xlab(""),
                                 scale_y_reverse(),
                                 scale_fill_manual(values = c("#AFDCFF","#01579b"))),
                   
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
    labs(color = "Variable:") + scale_color_manual(values = c("#94334F",
                                                              "#707788",
                                                              "#00A7BA",
                                                              "#8AE582",
                                                              "orange",
                                                              "#01579b"))
)

ggsave(
  "DTIAHE_Q3.png",
  path    = "/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/ImmediateAccesstoHECOL/Estimations/",
  plot    = plt_3,
  width   = 175,
  height  = 50,
  units   = "cm",
  bg = "white",
  limitsize = F
)

ctree_q4 = ctree_models[[4]]

plt_4 <- (
  ggparty(
    ctree_q4,
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
                                 xlab(""),
                                 scale_y_reverse(),
                                 scale_fill_manual(values = c("#AFDCFF","#01579b"))),
                   
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
    labs(color = "Variable:") + scale_color_manual(values = c("#94334F",
                                                              "#707788",
                                                              "#00A7BA",
                                                              "#8AE582",
                                                              "orange",
                                                              "#01579b"))
)

ggsave(
  "DTIAHE_Q4.png",
  path    = "/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/ImmediateAccesstoHECOL/Estimations/",
  plot    = plt_4,
  width   = 175,
  height  = 50,
  units   = "cm",
  bg = "white",
  limitsize = F
)

ctree_q5 = ctree_models[[5]]

plt_5 <- (
  ggparty(
    ctree_q5,
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
                                 xlab(""),
                                 scale_y_reverse(),
                                 scale_fill_manual(values = c("#AFDCFF","#01579b"))),
                   
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
    labs(color = "Variable:") + scale_color_manual(values = c("#94334F",
                                                              "#707788",
                                                              "#00A7BA",
                                                              "#8AE582",
                                                              "orange",
                                                              "#01579b"))
)

ggsave(
  "DTIAHE_Q5.png",
  path    = "/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/ImmediateAccesstoHECOL/Estimations/",
  plot    = plt_5,
  width   = 175,
  height  = 50,
  units   = "cm",
  bg = "white",
  limitsize = F
)
