########################################################
#   Dissertation Data Set - Descriptive Statistics     #
########################################################

# Erase everything

rm(list = ls())

# Libraries and working directory 

library(dplyr)
library(magrittr)
library(openxlsx)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(GGally)
library(gplots)
library(RColorBrewer)
library(heatmaply)

options(digits = 2)

setwd("/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/ImmediateAccesstoHECOL/Data and Descriptives/")

# Read data base

data_ti = read.csv("Data_TI.csv")

# Preparation of the data to make descriptive statistics
data_ti %<>% select(-X)

# Descriptive Statistics - Table Construction 
data_ti %<>% mutate(Sex = if_else(Sex == "Male", 1, 0))

data_ti %<>% mutate(SL_Student_1 = if_else(SL_Student == 1 , 1, 0))
data_ti %<>% mutate(SL_Student_2 = if_else(SL_Student == 2 , 1, 0))
data_ti %<>% mutate(SL_Student_3 = if_else(SL_Student == 3 , 1, 0))
data_ti %<>% mutate(SL_Student_4 = if_else(SL_Student == 4 , 1, 0))
data_ti %<>% select(-SL_Student)

# Fathers education
data_ti %<>% mutate(fathers_none = if_else(Fathers_education == "None",1, 0))
data_ti %<>% mutate(fathers_ip = if_else(Fathers_education == "Incomplete primary",1, 0))
data_ti %<>% mutate(fathers_cp = if_else(Fathers_education == "Complete primary",1, 0))
data_ti %<>% mutate(fathers_is = if_else(Fathers_education == "Incomplete secondary",1, 0))
data_ti %<>% mutate(fathers_cs = if_else(Fathers_education == "Complete secondary",1, 0))
data_ti %<>% mutate(fathers_ite = if_else(Fathers_education == "Incomplete technical education",1, 0))
data_ti %<>% mutate(fathers_cte = if_else(Fathers_education == "Complete technical education",1, 0))
data_ti %<>% mutate(fathers_iud = if_else(Fathers_education == "Incomplete undergraduated degree",1, 0))
data_ti %<>% mutate(fathers_cud = if_else(Fathers_education == "Complete undergraduate degree",1, 0))
data_ti %<>% mutate(fathers_pd = if_else(Fathers_education == "Graduate degree",1, 0))

data_ti %<>% select(-Fathers_education)

# Mothers education
data_ti %<>% mutate(mothers_none = if_else(Mothers_education == "None",1, 0))
data_ti %<>% mutate(mothers_ip = if_else(Mothers_education == "Incomplete primary",1, 0))
data_ti %<>% mutate(mothers_cp = if_else(Mothers_education == "Complete primary",1, 0))
data_ti %<>% mutate(mothers_is = if_else(Mothers_education == "Incomplete secondary",1, 0))
data_ti %<>% mutate(mothers_cs = if_else(Mothers_education == "Complete secondary",1, 0))
data_ti %<>% mutate(mothers_ite = if_else(Mothers_education == "Incomplete technical education",1, 0))
data_ti %<>% mutate(mothers_cte = if_else(Mothers_education == "Complete technical education",1, 0))
data_ti %<>% mutate(mothers_iud = if_else(Mothers_education == "Incomplete undergraduated degree",1, 0))
data_ti %<>% mutate(mothers_cud = if_else(Mothers_education == "Complete undergraduate degree",1, 0))
data_ti %<>% mutate(mothers_pd = if_else(Mothers_education == "Graduate degree",1, 0))

data_ti %<>% select(-Mothers_education)

data_ti %<>% mutate(Internet = if_else(Internet == "Yes", 1, 0))

data_ti %<>% mutate(Computer= if_else(Computer == "Yes", 1, 0))

# Reading Time

data_ti %<>% mutate(Reads30m = if_else(Reading_time == "30 min or less", 1, 0))
data_ti %<>% mutate(Reads30to60m = if_else(Reading_time == "30 to 60 minutes", 1, 0))
data_ti %<>% mutate(Reads1to2h = if_else(Reading_time == "1 to 2 hours", 1, 0))
data_ti %<>% mutate(Readsmore2= if_else(Reading_time == "More than 2 hours", 1, 0))
data_ti %<>% mutate(Doesnotread= if_else(Reading_time == "Does not read foe enternainment", 1, 0))

data_ti %<>% select(-Reading_time)


# Rurality

data_ti %<>% mutate(Rurality = if_else(Rurality == "Urban",0,1))

# Sector

data_ti %<>% mutate(School_sector = if_else(School_sector == "Public",1,0))

# Descriptive Statistics Table

# Inmmediate access to HE - iahe

iahe = data_ti %>% filter(Immediate_Access == 1) %>% select(-HEI_ID_,-HEI_GID, -HEI_Sector, -HEI_Character, -Program_level,
                                  -Cod_dpto_School, -Cod_mun_School, -Reads30m, -Reads30to60m, -Reads1to2h, 
                                  -Readsmore2, -Doesnotread, - Immediate_Access)

Feature = colnames(iahe)
complete_iahe = apply(iahe, 2, complete.cases)
Inmediate_access= colSums(complete_iahe)

mean_ia = apply(iahe, 2, mean, na.rm = TRUE)
sd_ia = apply(iahe,2,sd, na.rm = TRUE)

# Non Inmmediate access to HE - niahe

niahe = data_ti %>% filter(Immediate_Access == 0) %>% select(-HEI_ID_,-HEI_GID, -HEI_Sector, -HEI_Character, -Program_level,
                                                            -Cod_dpto_School, -Cod_mun_School, -Reads30m, -Reads30to60m, -Reads1to2h, 
                                                            -Readsmore2, -Doesnotread, - Immediate_Access)

complete_niahe = apply(niahe, 2, complete.cases)
Non_inmediate_access= colSums(complete_niahe)

mean_nia = apply(niahe, 2, mean, na.rm = TRUE)
sd_nia = apply(niahe,2,sd, na.rm = TRUE)


descriptives = tibble(Feature,Inmediate_access, mean_ia,sd_ia,Non_inmediate_access, mean_nia, sd_nia)
descriptives %<>% mutate(Diff = mean_ia - mean_nia)
descriptives %<>% mutate(Standard_error = sqrt((sd_ia^2/Inmediate_access) + (sd_nia^2/Non_inmediate_access)))
descriptives %<>% mutate(t_statistic = (mean_ia - mean_nia)/Standard_error)
descriptives %<>% mutate(dof = Inmediate_access+Non_inmediate_access-2)
descriptives %<>% mutate(P_value = 2 * pt(abs(t_statistic), df = dof, lower.tail = FALSE))

descriptives %<>% select(-t_statistic,-dof)
descriptives$P_value %<>% round(digits = 2)


write.xlsx(descriptives, "Descriptive Statistics.xlsx")
  
# Correlation Plot------------------------------------------------

cor_plot = data_ti %>% select('Sex (men)'=Sex, 'T. Age(17-21)' = age,
                              'Literacy PCT' = Reading_percentile,
                              'Math PCT' = Math_Percentile,
                              'N Sci. PCT' = NaturalSci_percentile,
                              'S Sci. PCT' = SocialSci_percentile,
                              'English PCT' = English_percentile,
                              'School sector' = School_sector,
                              Rurality,
                              'SL Level 1' = SL_Student_1,
                              'SL Level 2' = SL_Student_2,
                              'SL Level 3' = SL_Student_3,
                              'SL Level 4' = SL_Student_4, 
                              'f None' = fathers_none,
                              'f I primary' = fathers_ip,
                              'f C primary' = fathers_cp,
                              'f I sec.' = fathers_is,
                              'f C sec.' = fathers_cs,
                              'f I techEd' = fathers_ite,
                              'f C techEd.' = fathers_cte,
                              'f I UG.' = fathers_iud,
                              'f C UG.' = fathers_cud,
                              'f Posgrad.' = fathers_pd,
                              'm I primary' = mothers_ip,
                              'm C primary' = mothers_cp,
                              'm I sec.' = mothers_is,
                              'm C sec.' = mothers_cs,
                              'm I techEd.' = mothers_ite,
                              'm C techEd.' = mothers_cte,
                              'm I UG.' = mothers_iud,
                              'm C UG.' = mothers_cud,
                              'm Posgrad.' = mothers_pd)

# Correlation Plot

  # Efficient way

cor_matrix <- cor(cor_plot, use = "pairwise.complete.obs")
coul <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(cor_matrix,Colv = NA, Rowv = NA, col= coul)

# Correlation Plot 2

  # Accurate way

complete_data <- na.omit(cor_plot)
cor_matrix_2 <- cor(complete_data)
coul <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(cor_matrix_2,Colv = NA, Rowv = NA, col= coul)

cor_matrix_2 %<>% as_tibble()

write.xlsx(cor_matrix_2, "Correlation Matrix.xlsx")

# Intermediate Access To higher Education plots

iahe_rates = read.xlsx("IAHE Rates.xlsx")


options(scipen = 999)
iahe_rates_plot = ggplot(iahe_rates, aes(x= Cohort, y= Total.Students, fill = Immediate.Access)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  coord_cartesian(ylim = c(0, max(iahe_rates$Total.Students)))+
  theme(legend.position = 'bottom', panel.background = element_blank(), axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank(),
        legend.text = element_text(size = 12), axis.text.y = element_text(size= 12), axis.title.y = element_text(size = 12),
        plot.caption = element_text(hjust=c(0,1), size = 12),
        plot.caption.position = "plot", plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(title ="Immediate access to Higher Education 2018 -2021", y = "Total Students", caption = c("Source: Colombian Ministry of National Education", "Elaborated by: @luchobarajas_"))+
  scale_fill_manual(values = c("High school graduates that access inmediately to HE" = "#01579b" ,
                               "High school graduates that do not access" = "#2196f3" ))+
  geom_text(aes(x= 2017, label="\nCohort 2017", y= 0), colour="black", angle=0, size = 4) +
  geom_text(aes(x= 2018, label="\nCohort 2018", y= 0), colour="black", angle=0, size = 4) +
  geom_text(aes(x= 2019, label="\nCohort 2019", y= 0), colour="black", angle=0, size = 4) +
  geom_text(aes(x= 2020, label="\nCohort 2020", y= 0), colour="black", angle=0, size = 4) +
  geom_text(aes(x= 2017, label="\n38.7 %", y= 150000), colour="white", angle=0, size = 6, hjust = 1.2)+
  geom_text(aes(x= 2017, label="\n61.3 %", y= 150000), colour="white", angle=0, size = 6, hjust = -0.4)+
  geom_text(aes(x= 2018, label="\n39.7 %", y= 150000), colour="white", angle=0, size = 6, hjust = 1.2)+
  geom_text(aes(x= 2018, label="\n60.3 %", y= 150000), colour="white", angle=0, size = 6, hjust = -0.4)+
  geom_text(aes(x= 2019, label="\n40 %", y= 150000), colour="white", angle=0, size = 6, hjust = 1.4)+
  geom_text(aes(x= 2019, label="\n60 %", y= 150000), colour="white", angle=0, size = 6, hjust = -0.8)+
  geom_text(aes(x= 2020, label="\n39.7 %", y= 150000), colour="white", angle=0, size = 6, hjust = 1.2)+
  geom_text(aes(x= 2020, label="\n60.3 %", y= 150000), colour="white", angle=0, size = 6, hjust = -0.4)


ggsave("Iahe_rates_2018-2021.png", iahe_rates_plot, width = 12, height = 7) 


