################################################################################
#                                                                             ##
#              Homicide data 2019-2020 at municipal level                     ##
#                                                                             ##
#    Goal: Add population data                                                ##
#    Authors: Regina Isabel Medina and Mariana Consuelo Fernandez Espinosa    ##
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
# Population data
load("~/GitHub/homicides-mx-data/data_raw/df_pop_county_2019_2020.Rdata")

# Homicide data
df_homicides <- read.csv("data_raw/county/2019_2020/df_homicidios_dia_fuentesabiertas.csv", encoding = "UTF-8")


#-----------------------------------------------------------------------------#
##            2. Data wrangling                                            ####
#-----------------------------------------------------------------------------#
# Change variable names for compatibility 
df_pop <- df_pop_county_2019_2020 %>% 
        rename(Entidad    = entidad, 
                Municipio = county_name_esp, 
                County    = county_name_eng, 
                Año       = year)

# Join data frames
df_homicides_county <- df_homicides  %>% 
        mutate(Año = year(as.Date(Fecha))) %>% 
        left_join(df_pop, by = c("Entidad", "Municipio", "Año"))
# Estimate mortality rates 
df_homicides_county_daily <- df_homicides_county %>% 
        # Estimate number of homicides per 100,000 people
        mutate(mort_rate = (Homicidios*100000/population)) %>%  
        # Convert implicit missing values to explicit missing values 
        # We will have data for every date
        complete(Fecha, nesting(Entidad, Municipio),
                fill = list(Homicidios = 0)) %>% 
        # Select variables
        mutate(Fecha = as.Date(Fecha)) %>% 
        select(Entidad, Municipio, County, county_id, Fecha, Homicidios, Hombre, 
                Mujer, No.Identificado, population, mort_rate)
        
        #View(df_homicides_county_daily)

# Rename for consistency with county data #
# Convert into lower case
df_homicides_county_daily <- df_homicides_county_daily %>% 
        rename("entidad" = Entidad, "municipio" = Municipio, "county" = County, 
                "fecha" = Fecha, "homicidios" = Homicidios, "hombre" = Hombre, 
                "mujer" = Mujer, "no_identificado" = No.Identificado)

#-----------------------------------------------------------------------------#
##            3. Check consistency of data                                 ####
#-----------------------------------------------------------------------------#
# Homicides
# Total homicides
sum(df_homicides$Homicidios)
sum(df_homicides_county_daily$homicidios)

# Men 
sum(df_homicides$Hombre, na.rm = T)
sum(df_homicides_county_daily$hombre, na.rm = T)


# Women 
sum(df_homicides$Mujer, na.rm = T)
sum(df_homicides_county_daily$mujer, na.rm = T)

# Non-identified 
sum(df_homicides$No.Identificado, na.rm = T)
sum(df_homicides_county_daily$no_identificado, na.rm = T)


#-----------------------------------------------------------------------------#
##            4. Save data                                                 ####
#-----------------------------------------------------------------------------#
# Rename df
df_homicides_fuentesabiertas_county_day <- df_homicides_county_daily

# Save df
save(df_homicides_fuentesabiertas_county_day, file = "data/county/df_homicides_fuentesabiertas_county_day.RData")


