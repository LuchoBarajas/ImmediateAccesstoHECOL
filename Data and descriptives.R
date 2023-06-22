########################################################
#   Dissertation Data Set - Exploratory Data Analysis  #
########################################################

# Erase everything

rm(list = ls())

# Libraries and working directory 

library(dplyr)
library(magrittr)
library(openxlsx)
library(stringr)
library(lubridate)

setwd("/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/Data and Descriptives")

# Data base loading

data_ti <- read.csv2("prueba_2.csv", encoding = "UTF-8")

# Feature Selection 

data_ti %<>% select(Strata_ICFES = fami_estratovivienda, Fathers_education = fami_educacionpadre,
                    Mothers_education = fami_educacionmadre, Internet = fami_tieneinternet,
                    Computer = fami_tienecomputador, Reading_time = estu_dedicacionlecturadiaria,
                    Reading_percentile = percentil_lectura_critica, 
                    Reading_performance = desemp_lectura_critica,
                    Math_score = punt_matematicas, Math_Percentile = percentil_matematicas, 
                    Math_performance = desemp_matematicas, 
                    NaturalSci_score = punt_c_naturales, NaturalSci_percentile = percentil_c_naturales,
                    NaturalSci_performance = desemp_c_naturales,
                    SocialSci_score = punt_sociales_ciudadanas, SocialSci_percentile = percentil_sociales_ciudadanas,
                    SocialSci_performance = desemp_sociales_ciudadanas, English_score = punt_ingles,
                    English_percentile = percentil_ingles, English_performance = desemp_ingles,
                    Global_score = punt_global,Global_percentile = percentil_global,
                    Strata_SIMAT = t_ESTRATO, Date_of_Birth = t_FECHA_NACIMIENTO, 
                    Sex = t_GENERO, Rurality = t_ZON_ALU,Dpto_School = t_DPTO_CARGA,
                    School_sector = t_CTE_ID_SECTOR,Cod_mun_School =t_Divipola_MUNICIPIO,
                    ID_HEI = t_ID_IES, GID_HEI = t_IES_PADRE, Sector_HEI = t_ORIGEN_IES, 
                    HEI_Character = t_CARACTER_IES,Program_ID = t_CODIGO_PROGRAMA, Program = t_PROGRAMA,
                    Program_level = t_MODALIDAD_PROGRAMA,
                    Program_type = t_METODOLOGIA_PROGRAMA, Program_Dpto = t_DEPARTAMENTO_PROGRAMA, 
                    Program_Cod_Dpto = t_CODIGO_MUNICIPIO_PROGRAMA,
                    Program_mun = t_MUNICIPIO_PROGRAMA, Program_Area = t_Ã.rea, Sisben_Score = puntaje_sisben_3,
                    SEI_Student = estu_inse_individual, SL_Student = estu_nse_individual)


# Age Calculation

data_ti$Date_of_Birth = as.Date(data_ti$Date_of_Birth, format = "%d-%B-%y")
data_ti %<>% mutate(age = difftime("2021-01-01", data_ti$Date_of_Birth, units = "weeks")/ 52.1775)
data_ti$age %<>% as.integer()

# Data cleaning 

## Encoding characters

data_ti$Fathers_education %<>% str_replace_all("EducaciÃƒÂ³n", "Educacion")
data_ti$Fathers_education %<>% str_replace_all("TÃƒÂ©cnica o tecnolÃƒÂ³gica", "Tecnica o tecnologica")
data_ti$Mothers_education %<>% str_replace_all("EducaciÃƒÂ³n", "Educacion")
data_ti$Mothers_education %<>% str_replace_all("TÃƒÂ©cnica o tecnolÃƒÂ³gica", "Tecnica o tecnologica")
data_ti$Program_Dpto %<>% str_replace_all("Ã©", "e")
data_ti$Program_Dpto %<>% str_replace_all("Ã¡", "a")
data_ti$Program_Dpto %<>% str_replace_all("Ã", "i")
data_ti$Program_Dpto %<>% str_replace_all("i³", "o")
data_ti$Program_level %<>% str_replace_all("Ã³", "o")
data_ti$Program_level %<>% str_replace_all("Ã©", "e")
data_ti$HEI_Character %<>% str_replace_all("Ã³","o")
data_ti$HEI_Character %<>% str_replace_all("Ã©","e")
data_ti$Program_Area %<>% str_replace_all("Ã³","o")
data_ti$Program_Area %<>% str_replace_all("Ã¡","a")
data_ti$Program_Area %<>% str_replace_all("Ã³","o")
data_ti$Program_Area %<>% str_replace_all("Ã","i")
data_ti$Program_mun %<>% str_replace_all("Ã©", "e")
data_ti$Program_mun %<>% str_replace_all("Ã¡", "a")
data_ti$Program_mun %<>% str_replace_all("Ã±", "ñ")
data_ti$Program_mun %<>% str_replace_all("Ã", "i")
data_ti$Program_mun %<>% str_replace_all("i³", "o")
data_ti$Program_mun %<>% str_replace_all("iº", "u")
data_ti$Program_mun %<>% str_replace_all("i¼", "ü")
data_ti$Reading_time %<>% str_replace_all("ÃƒÂ¡", "a")

# Re-coding Variables

data_ti %<>% select(-Strata_ICFES) # Se elimina el Estrato del ICFES por mayor completitud en SIMAT
data_ti$Fathers_education %<>% str_replace("No sabe", "") # se remplaza el No sabe por missing
data_ti$Mothers_education %<>% str_replace("No sabe", "") # se remplaza el No sabe por missing
data_ti %<>% mutate(Internet = ifelse(Internet == "Si", 1, 0)) # 1 si tiene internet, 0 si no
data_ti %<>% mutate(Computer = ifelse(Computer == "Si", 1, 0)) # 0 si tiene internet, 0 si no
data_ti$Reading_time %<>% str_replace_all("Igual", "") # Se elimina la categoria igual. Es error y se cambia por missing
data_ti %<>% mutate(Reading_performance = if_else(Reading_performance > 4, NA_real_, Reading_performance)) # Se elimina la categoria 14. Es un error
data_ti %<>% mutate(Math_performance = if_else(Math_performance > 4, NA_real_, Math_performance))
data_ti %<>% mutate(NaturalSci_performance = if_else(NaturalSci_performance  > 4, NA_real_, NaturalSci_performance))
data_ti$English_performance %<>% str_replace_all("34", "")
data_ti %<>% mutate(Sex = if_else(Sex == "F", "Female","Male"))


