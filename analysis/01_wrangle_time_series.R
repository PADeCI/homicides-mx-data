################################################################################
#                                                                             ##
#              Homicide data 2019-2020 at municipal level                     ##
#                                                                             ##
#    Goal: Check consistency in data frames create weekly and monthly         ##
#    Authors: Regina Isabel Medina and Mariana Fernández                      ##
#    Date: September 7th, 2020                                                ##
#                                                                             ##
################################################################################

#-----------------------------------------------------------------------------#
##            0. Initial set up                                            ####
#-----------------------------------------------------------------------------#
# Load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)

# Clean the workspace
rm(list = ls()) 


#-----------------------------------------------------------------------------#
##            1. Load data                                                 ####
#-----------------------------------------------------------------------------#
# Homicide data with population 
load("~/GitHub/homicides-mx-data/data/df_homicides_fuentesabiertas_county_day.Rdata")

#-----------------------------------------------------------------------------#
##            2. Create new df on time frames                              ####
#-----------------------------------------------------------------------------#
# Create time variables
df_homicides_fuentesabiertas_county_day <- df_homicides_fuentesabiertas_county_day %>% 
        mutate("Año" = year(Fecha), "Year" = year(Fecha),
                "Mes" = month(Fecha), "Month" = month(Fecha), 
                "Semana" = isoweek(Fecha), "Week"= isoweek(Fecha)) 

# Rename for simplicity 
df_dates <- df_homicides_fuentesabiertas_county_day        

## Monthly ##
df_month <- df_dates %>%
        group_by(Entidad, Municipio, County, Year, Año, Month, Mes) %>% 
        summarise("Homicidios" = sum(Homicidios, na.rm = T), 
                  "Hombre"     = sum(Hombre, na.rm = T), 
                  "Mujer"      = sum(Mujer, na.rm = T), 
                  "No.Identificado" = sum(No.Identificado, na.rm = T)) 
## Weekly ##
df_week <- df_dates %>% 
        group_by(Entidad, Municipio, Year, Año, Month, Mes, Week, Semana) %>% 
        summarise("Homicidios" = sum(Homicidios, na.rm = T), 
                  "Hombre"     = sum(Hombre, na.rm = T), 
                  "Mujer"      = sum(Mujer, na.rm = T),
                  "No.Identificado" = sum(No.Identificado, na.rm = T)) 

          

#-----------------------------------------------------------------------------#
##            3. Check consistency of data                                 ####
#-----------------------------------------------------------------------------#
# Total homicides
sum(df_homicides_county_daily$Homicidios)
sum(df_month$Homicidios)
sum(df_week$Homicidios)
        
# Men 
sum(df_homicides_county_daily$Hombre, na.rm = T)
sum(df_month$Hombre)
sum(df_week$Hombre)

# Women 
sum(df_homicides_county_daily$Mujer, na.rm = T)
sum(df_month$Mujer)
sum(df_week$Mujer)

# Non identified
sum(df_homicides_county_daily$No.Identificado, na.rm = T)
sum(df_month$No.Identificado)
sum(df_week$No.Identificado)


#-----------------------------------------------------------------------------#
##            4. Save data                                                 ####
#-----------------------------------------------------------------------------#
# Rename data 
df_homicides_fuentesabiertas_county_day <- df_dates
df_homicides_fuentesabiertas_county_week <- df_week
df_homicides_fuentesabiertas_county_month <- df_month

# Save data
save(df_homicides_fuentesabiertas_county_day, file = "data/df_homicides_fuentesabiertas_county_day.RData")
save(df_homicides_fuentesabiertas_county_week, file = "data/df_homicides_fuentesabiertas_county_week.RData")
save(df_homicides_fuentesabiertas_county_month, file = "data/df_homicides_fuentesabiertas_county_month.RData")

