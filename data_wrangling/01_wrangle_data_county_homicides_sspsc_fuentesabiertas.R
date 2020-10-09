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
load("./data_raw/df_pop_county_2019_2020.Rdata")

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

df_homicides_pop <- df_homicides_county                         %>%  
        full_join(df_pop, by = c("entidad", 
                "municipio", "año"))  


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
                fill = list(homicidios=0, hombres=0, 
                        mujeres=0, no_identificado=0))



# 03. Check consistency of data ------------------------------------------------

# Total homicides
sum(df_homicides_open$Homicidios)
sum(df_final$homicidios, na.rm = T)
        # Difference: 2
# Men 
sum(df_homicides_open$Hombre, na.rm = T)
sum(df_final$hombre, na.rm = T)
        # Difference: 1

# Women 
sum(df_homicides_open$Mujer, na.rm = T)
sum(df_final$mujer, na.rm = T)
        # Difference: 1

# Non-identified 
sum(df_homicides_open$No.Identificado, na.rm = T)
sum(df_final$no_identificado, na.rm = T)
         # Difference: 0 


# 04. Save final data sets -----------------------------------------------------

# Rename df
df_homicides_county_daily_sspc_fuentesabiertas <- df_final

# Save df
save(df_homicides_county_daily_sspc_fuentesabiertas, 
        file = "data/fuentes_abiertas/df_homicides_county_daily_sspc_fuentesabiertas.RData")

