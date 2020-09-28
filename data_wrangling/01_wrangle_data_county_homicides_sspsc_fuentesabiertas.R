################################################################################
#                                                                             ##
#              Homicide data 2019-2020 at municipal level (fuentes abiertas)  ##
#                                                                             ##
#    Goal: Add population data and estimate mortality rate                    ##
#    Authors: Regina Isabel Medina and Mariana Consuelo Fernandez Espinosa    ##
#    Date:               2020-09-07                                           ##
#    Last update:        2020-09-27                                           ##
#                                                                             ##
################################################################################


# 00. Initial set up -----------------------------------------------------------

# Load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)

# Clean the workspace
rm(list = ls()) 



# 01. Load Data ----------------------------------------------------------------

# Population data at county level 
load("~/GitHub/homicides-mx-data/data_raw/df_pop_county_2019_2020.Rdata")

# Homicide data from open sources (newspapers)
df_homicides_open <- read.csv("data_raw/fuentes_abiertas/2019_2020/df_homicides_daily_2019_2020_sspc_fuentesabiertas.csv", 
        encoding = "UTF-8")



# 02. Data wrangling  ----------------------------------------------------------

# 02.1 Relabel, filter,  group and select  -------------------------------------

# Change variable names for compatibility, eliminate non-identified counties 
df_pop <- df_pop_county_2019_2020                               %>%
        rename(entidad    = entidad,    
                municipio = county_name_esp,    
                county    = county_name_eng,    
                año       = year)                               %>%
        mutate(entidad = case_when(entidad =="México" ~ "Estado de México", 
                entidad == entidad ~ entidad))                  %>%
        filter(entidad != is.na(entidad))                       %>% 
        mutate(population = as.numeric(population))     

        
df_homicides_county <- df_homicides_open                        %>% 
        rename("entidad"          = Entidad, 
                "homicidios"      = Homicidios,
                "fecha"           = Fecha, 
                "municipio"       = Municipio, 
                "hombre"          = Hombre, 
                "mujer"           = Mujer, 
                "no_identificado" = No.Identificado)            %>% 
        mutate(año = year(as.Date(fecha)))                     

# 02.2 Add population for open source  -----------------------------------------

# Join data frames
df_homicides_pop <- df_homicides_county                         %>%  
        full_join(df_pop, by = c("entidad", "municipio", "año"))

# 02.2 Find mistake  -----------------------------------------
df_prev_mistake <- df_homicides_county                          %>% 
        rename("antes_total" = homicidios, 
                "antes_hombre" = hombre, 
                "antes_mujer" = mujer, 
                "antes_no_iden" = no_identificado)              %>%
        select(entidad, municipio, fecha, antes_total, antes_hombre, antes_mujer,
                antes_no_iden)

df_afte_mistake <- df_homicides_pop                             %>% 
        rename("desp_total" = homicidios, 
                "desp_hombre" = hombre, 
                "desp_mujer" = mujer, 
                "desp_no_iden" = no_identificado)               %>% 
        select(entidad, municipio, fecha, desp_total, desp_hombre, desp_mujer,
                desp_no_iden)

df_mistake_total <- df_prev_mistake                                   %>% 
        full_join(df_afte_mistake, by = c("entidad", "municipio", "fecha")) %>% 
        mutate(diff_total = antes_total - desp_total, 
                equal_total = case_when(diff_total==0 ~ T, diff_total !=0 ~ F)) 

df_mistake_hombre <- df_prev_mistake                                   %>% 
        full_join(df_afte_mistake, by = c("entidad", "municipio", "fecha")) %>% 
        mutate(diff_hombre = antes_hombre - desp_hombre, 
                equal_hombre = case_when(diff_hombre==0 ~ T, diff_hombre !=0 ~ F))
        
df_mistake_mujer <- df_prev_mistake                                   %>% 
        full_join(df_afte_mistake, by = c("entidad", "municipio", "fecha")) %>%         
        mutate(diff_mujer = antes_mujer - desp_mujer, 
                equal_mujer= case_when(diff_mujer==0 ~ T, diff_mujer !=0 ~ F))

df_mistake_no_iden <- df_prev_mistake                                   %>% 
        full_join(df_afte_mistake, by = c("entidad", "municipio", "fecha")) %>%
        mutate(diff_no_iden = antes_no_iden - desp_no_iden, 
                equal_no_iden = case_when(diff_no_iden==0 ~ T, diff_no_iden !=0 ~ F)) 
        
sum(df_mistake$diff[df_mistake$equal==F], na.rm = T)        


# 02.3 Estimate mortality rate -------------------------------------------------

# Estimate number of homicides per 100,000 people, convert implicit missing 
# values to explicit missing values:we will have data for every date.

df_homicides_pop_mort <- df_homicides_pop                       %>%
        mutate(mort_rate = (homicidios*100000/population))      %>%  
        mutate(fecha = as.Date(fecha))                          %>% 
        select(entidad, municipio, county, county_id, fecha, homicidios, hombre, 
                mujer, no_identificado, population, mort_rate)
        #View(df_homicides_county_daily)

df_final <- df_homicides_pop_mort

# 02.4 Fill missing dates ------------------------------------------------------
df_final <- df_homicides_pop_mort %>% 
        mutate(mort_rate = (homicidios*100000/population))      %>% 
        complete(fecha, nesting(entidad, municipio), 
                fill = list(homicidios=0, hombres=0, mujeres=0, no_identificado=0))



# 03. Check consistency of data ------------------------------------------------

# Total homicides
sum(df_homicides_open$Homicidios)
        sum(df_homicides_county$homicidios)
        sum(df_homicides_pop$homicidios, na.rm = T)
        sum(df_homicides_pop_mort$homicidios, na.rm = T)
sum(df_final$homicidios, na.rm = T)
        # Difference: -28
# Men 
sum(df_homicides_open$Hombre, na.rm = T)
sum(df_final$hombre, na.rm = T)
        # Difference: -26

# Women 
sum(df_homicides_open$Mujer, na.rm = T)
sum(df_final$mujer, na.rm = T)
        # Difference: -1

# Non-identified 
sum(df_homicides_open$No.Identificado, na.rm = T)
sum(df_final$no_identificado, na.rm = T)
        # Difference: 0 


# 04. Save final data sets -----------------------------------------------------

# Rename df
df_homicides_county_daily_sspc_fuentesabiertas <- df_final

# Save df
save(df_homicides_county_daily_sspc_fuentesabiertas, file = "data/fuentes_abiertas/df_homicides_county_daily_sspc_fuentesabiertas.RData")
