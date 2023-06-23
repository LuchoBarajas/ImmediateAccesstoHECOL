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

setwd("/Users/luchobarajas/Documents/OneDrive - London School of Economics/Capstone Project/ImmediateAccesstoHECOL/Data and Descriptives/")

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
                    SL_Student = estu_nse_individual)


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

# Re-coding Variables: correcting errors in database and creating factors when needed

data_ti %<>% select(-Strata_ICFES) # Eliminating ICFES Strata due to a mayor availability of information in SIMAT Strata

# Fathers Education: replacing not information and errir categories for missing values/translating

table(data_ti$Fathers_education)
data_ti$Fathers_education %<>% str_replace("No sabe", "") 
data_ti$Fathers_education %<>% str_replace("No Aplica", "")

data_ti$Fathers_education %<>% str_replace("Ninguno", "None")
data_ti$Fathers_education %<>% str_replace("Primaria incompleta", "Incomplete primary")
data_ti$Fathers_education %<>% str_replace("Primaria completa", "Complete primary")
data_ti$Fathers_education %<>% str_replace("Secundaria \\(Bachillerato\\) incompleta", "Incomplete secondary")
data_ti$Fathers_education %<>% str_replace("Secundaria \\(Bachillerato\\) completa", "Complete secondary")
data_ti$Fathers_education %<>% str_replace("Tecnica o tecnologica incompleta", "Incomplete technical education")
data_ti$Fathers_education %<>% str_replace("Tecnica o tecnologica completa", "Complete technical education")
data_ti$Fathers_education %<>% str_replace("Educacion profesional incompleta", "Incomplete undergraduated degree")
data_ti$Fathers_education %<>% str_replace("Educacion profesional completa", "Complete undergraduate degree")
data_ti$Fathers_education %<>% str_replace("Postgrado", "Graduate degree")

data_ti$Fathers_education %<>% factor(levels = c("None","Incomplete primary","Complete primary","Incomplete secondary",
                                                 "Complete secondary","Incomplete technical education",
                                                 "Complete technical education", "Incomplete undergraduated degree",
                                                 "Complete undergraduate degree", "Graduate degree"))


# Mothers Education: replacing not information and errir categories for missing values/translating

table(data_ti$Mothers_education)
data_ti$Mothers_education %<>% str_replace("No sabe", "") 
data_ti$Mothers_education %<>% str_replace("No Aplica", "")

data_ti$Mothers_education %<>% str_replace("Ninguno", "None")
data_ti$Mothers_education %<>% str_replace("Primaria incompleta", "Incomplete primary")
data_ti$Mothers_education %<>% str_replace("Primaria completa", "Complete primary")
data_ti$Mothers_education %<>% str_replace("Secundaria \\(Bachillerato\\) incompleta", "Incomplete secondary")
data_ti$Mothers_education %<>% str_replace("Secundaria \\(Bachillerato\\) completa", "Complete secondary")
data_ti$Mothers_education %<>% str_replace("Tecnica o tecnologica incompleta", "Incomplete technical education")
data_ti$Mothers_education %<>% str_replace("Tecnica o tecnologica completa", "Complete technical education")
data_ti$Mothers_education %<>% str_replace("Educacion profesional incompleta", "Incomplete undergraduated degree")
data_ti$Mothers_education %<>% str_replace("Educacion profesional completa", "Complete undergraduate degree")
data_ti$Mothers_education %<>% str_replace("Postgrado", "Graduate degree")

data_ti$Mothers_education %<>% factor(levels = c("None","Incomplete primary","Complete primary","Incomplete secondary",
                                                 "Complete secondary","Incomplete technical education",
                                                 "Complete technical education", "Incomplete undergraduated degree",
                                                 "Complete undergraduate degree", "Graduate degree"))

# Internet: Transforming the variable into a factor, correcting mistakes of the data

table(data_ti$Internet)
data_ti %<>% mutate(Internet = ifelse(Internet == "Si", 1, 0)) # 1 si tiene internet, 0 si no
data_ti$Internet %<>% factor(labels= c("No", "Yes"))

# Computer: Transforming the variable into a factor, correcting mistakes of the data

table(data_ti$Computer)
data_ti %<>% mutate(Computer = ifelse(Computer == "Si", 1, 0)) # 0 si tiene internet, 0 si no
data_ti$Computer %<>% factor(labels= c("No", "Yes"))

# Reading Time: Transforming the variable into a factor

table(data_ti$Reading_time)
data_ti$Reading_time %<>% str_replace_all("Igual", "") 
data_ti$Reading_time %<>% str_replace_all("30 minutos o menos", "30 min or less") 
data_ti$Reading_time %<>% str_replace_all("Entre 30 y 60 minutos", "30 to 60 minutes") 
data_ti$Reading_time %<>% str_replace_all("Entre 1 y 2 horas", "1 to 2 hours") 
data_ti$Reading_time %<>% str_replace_all("Mas de 2 horas", "More than 2 hours") 
data_ti$Reading_time %<>% str_replace_all("No leo por entretenimiento", "Does not read foe enternainment") 

data_ti$Reading_time %<>% factor(levels = c("More than 2 hours", "1 to 2 hours", "30 to 60 minutes", "30 min or less",
                                            "Does not read foe enternainment"))

# Standardized test results

table(data_ti$Reading_performance)
data_ti %<>% mutate(Reading_performance = if_else(Reading_performance > 4, NA_real_, Reading_performance)) 

table(data_ti$Math_performance)
data_ti %<>% mutate(Math_performance = if_else(Math_performance > 4, NA_real_, Math_performance))

table(data_ti$NaturalSci_performance)
data_ti %<>% mutate(NaturalSci_performance = if_else(NaturalSci_performance  > 4, NA_real_, NaturalSci_performance))

table(data_ti$English_performance)
data_ti$English_performance %<>% str_replace_all("34", "")

table(data_ti$English_performance)
data_ti$English_performance %<>% factor(levels = c("B+","B1","A2","A1","A-"))

# Strata SIMAT: Replacing Strata 0 and 9 for missing considering these strata do nor exist.

table(data_ti$Strata_SIMAT)
data_ti %<>% mutate(SEStrata = if_else(Strata_SIMAT == 0|Strata_SIMAT == 9, NA_real_, Strata_SIMAT))
data_ti %<>% select(-Strata_SIMAT)

# Sex: 

data_ti %<>% mutate(Sex = if_else(Sex == "F", "Female","Male"))
data_ti$Sex %<>% factor()

# Rurality

table(data_ti$Rurality)
data_ti$Rurality %<>% factor(labels= c("Urban", "Rural"))

# School Sector

table(data_ti$School_sector)
data_ti$School_sector %<>% factor(labels= c("Public", "Private"))

# Other Variables

data_ti$Dpto_School %<>% factor()
data_ti$Sisben_Score %<>% as.numeric()

# Response Variable (s): the response variable indicates weather the student accessed intermediate or not to higher education

data_ti %<>% mutate(Immediate_Access = if_else(is.na(GID_HEI), 0,1))

# HEI Level

table(data_ti$Sector_HEI)
data_ti %<>% mutate(Sector_HEI = if_else(Sector_HEI == "OFICIAL", "Public", 
                                         if_else(Sector_HEI == "PRIVADA", "Private", Sector_HEI)))
data_ti$Sector_HEI %<>% factor()

table(data_ti$HEI_Character)

data_ti$HEI_Character %<>% str_replace_all("Universidad", "University")
data_ti$HEI_Character %<>% str_replace_all("Institucion Universitaria/Escuela Tecnologica", "University Institution/Technology School")
data_ti$HEI_Character %<>% str_replace_all("Institucion Tecnologica", "Technological Institution")
data_ti$HEI_Character %<>% str_replace_all("Institucion Tecnica Profesional", "Technical Institution")

data_ti$HEI_Character %<>% factor(levels = c("University", "University Institution/Technology School",
                                                "Technological Institution", "Technical Institution"))

# Academic Program 

table(data_ti$Program_level)
data_ti$Program_level %<>% str_replace_all("Especializacion tecnologica", "Technological")
data_ti$Program_level %<>% str_replace_all("Tecnologico", "Technological")
data_ti$Program_level %<>% str_replace_all("Formacion tecnica profesional", "Technical")
data_ti$Program_level %<>% str_replace_all("Universitario", "Universitary")

data_ti$Program_level %<>% factor(levels = c("Universitary", "Technological", "Technical"))


