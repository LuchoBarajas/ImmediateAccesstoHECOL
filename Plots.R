# Dissertation plots ############

library(broom)
library(dplyr)
library(ggplot2)
library(mapview)
library(magrittr)
library(RColorBrewer)
library(readr)
library(rgdal)
library(openxlsx)
library(colmaps)
library(stringr)


setwd("/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project")


# Carga de Bases de Datos Requeridas ##############

data_ia = read.xlsx("/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/HE_statistical_brief.xlsx", 
                      sheet = 7, startRow = 5)

data_ia %<>% select(COD..MUNICIPIO, COD..DEPARTAMENTO, MUNICIPIO, DEPARTAMENTO,Year_2021 = '2021')
data_ia$Year_2021 %<>% str_replace("NO APLICA", "0")
data_ia$Year_2021 %<>% str_replace("No aplica ", "0")


data_ia$Year_2021 %<>% as.numeric()
data_ia %<>% filter(!is.na(Year_2021))
data_ia %<>% mutate(Year_2021 = Year_2021 * 100)
data_ia %<>% filter(COD..DEPARTAMENTO != "COLOMBIA")

data_ia %<>% select(id = COD..MUNICIPIO, id_depto = COD..DEPARTAMENTO,
                    municipio = MUNICIPIO, depto =DEPARTAMENTO, IAHE = Year_2021)


colmap(municipios, subset(data_ia), var = "IAHE") + 
  scale_fill_continuous(low = "#FAF884", high = "#006A4E", na.value = "wheat") + 
  labs(caption = "Source: Ministry of National Education - Colombia")+
  theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot") 

data_ia %<>% mutate(lower = IAHE < 40)

gg

