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
df_homicides_daily_fuentesabiertas <- read.csv("data_raw/fuentes_abiertas/2019_2020/df_homicides_daily_2019_2020_sspc_fuentesabiertas.csv",
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

        
df_homicides_county <- df_homicides_daily_fuentesabiertas                        %>% 
        rename( "entidad"         = Entidad, 
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
                "municipio", "año"))                            %>% 
        filter(is.na(fecha) == FALSE)

# 02.3 Estimate mortality rate -------------------------------------------------

# Estimate number of homicides per 100,000 people, convert implicit missing 
# values to explicit missing values:we will have data for every date.

df_homicides_pop_mort <- df_homicides_pop                       %>%
        mutate(mort_rate = (homicidios*100000/population))      %>%  
        mutate(fecha = as.character(fecha))                     %>%  
        mutate(fecha = case_when(fecha == "08/03/19" ~ "2019-03-08",
                fecha == fecha ~ fecha))                        %>% 
        mutate(fecha = as.Date(fecha))                          %>% 
        select(entidad, municipio, county, county_id, fecha, homicidios, hombre, 
                mujer, no_identificado, population, mort_rate)
        #View(df_homicides_county_daily)


# 02.4 Fill missing dates ------------------------------------------------------
df_final <- df_homicides_pop_mort %>% 
        mutate(mort_rate = (homicidios*100000/population))      %>% 
        complete(fecha, nesting(entidad, municipio), 
                fill = list(homicidios=0, hombres=0, 
                        mujeres=0, no_identificado=0))



# 03. Check consistency of data ------------------------------------------------

# 03.1 Totals ------------------------------------------------------------------
# All homicides
sum(df_homicides_daily_fuentesabiertas$Homicidios)              # Original df
sum(df_final$homicidios, na.rm = T)                             # Final df
        

# Men   
sum(df_homicides_daily_fuentesabiertas$Hombre, na.rm = T)       # Original df
sum(df_final$hombre, na.rm = T)                                 # Final df


# Women 
sum(df_homicides_daily_fuentesabiertas$Mujer, na.rm = T)        # Original df
sum(df_final$mujer, na.rm = T)                                  # Final df


# Non-identified 
sum(df_homicides_daily_fuentesabiertas$No.Identificado, na.rm = T) # Original df
sum(df_final$no_identificado, na.rm = T)                           # Final df

        # All the homicide categories add up to the same number as in the 
        # original data frame. 

# 03.2 Consistency by gender ---------------------------------------------------

# Estimate the number of homicides by adding up by gender. Then compare with 
# the total number of homicides originally reported. Create a variable for 
# differences. 
df_gender <- df_final %>% 
        mutate(suma = hombre + mujer + no_identificado, 
                diff = case_when(homicidios == suma ~ 0, 
                        homicidios == suma ~ 1))

# Compute the number of observations where reported and estimated total homicides 
# mismatch. 
sum(df_gender$diff, na.rm = T)

        # There are no mismatchs. 

# 03.3 Categories --------------------------------------------------------------
# Make sure NA is not a category on any variable
sum(is.na(df_final$entidad))
sum(is.na(df_final$municipio))
sum(is.na(df_final$population))
sum(is.na(df_final$fecha))
sum(is.na(df_final$homicidios))
        
        # The population variable has several NAs.

# 04. Save final data sets -----------------------------------------------------

# Rename df
df_homicides_county_daily_sspc_fuentesabiertas <- df_final

# Save df
save(df_homicides_county_daily_sspc_fuentesabiertas, 
        file = "data/fuentes_abiertas/df_homicides_county_daily_sspc_fuentesabiertas.RData")

