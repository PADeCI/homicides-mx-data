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

# Homicide data from interinstitutional group
df_homicides_gpo <- read.csv("data_raw/gpo_interinstitucional/2019_2020/df_homicides_daily_2019_2020_sspc_gpointerinstitucional.csv", 
        encoding = "UTF-8")

# Homicide data from open sources (newspapers)
df_homicides_open <- read.csv("data_raw/fuentes_abiertas/2019_2020/df_homicides_daily_2019_2020_sspc_fuentesabiertas.csv", 
        encoding = "UTF-8")

#-----------------------------------------------------------------------------#
##            2. Data wrangling                                            ####
#-----------------------------------------------------------------------------#
# Change variable names for compatibility, eliminate non-identified counties 
# and make Oaxaca Regions a single category named "Oaxaca"
df_pop <- df_pop_county_2019_2020                               %>%
        filter(entidad != is.na(entidad))                       %>% 
        rename(Entidad    = entidad, 
                Municipio = county_name_esp, 
                County    = county_name_eng, 
                Año       = year)                               %>% 
        mutate(Entidad = case_when(Entidad == "Oaxaca-Región Costa" ~ "Oaxaca",
                                Entidad == "Oaxaca-Región Mixteca" ~ "Oaxaca",
                                Entidad == "Oaxaca-Región Sierra Norte" ~ "Oaxaca",
                                Entidad == "Oaxaca-Región Valles Centrales" ~ "Oaxaca",
                                Entidad == "Oaxaca-Región Cañada" ~ "Oaxaca",
                                Entidad == "Oaxaca-Región Istmo" ~ "Oaxaca",
                                Entidad == "Oaxaca-Región Papaloapam" ~ "Oaxaca", 
                                Entidad == "Oaxaca-Región Sierra Sur" ~ "Oaxaca", 
                                Entidad != "Oaxaca" ~ Entidad))


df_pop_state <- df_pop                                          %>% 
        group_by(Entidad, Año)                                  %>%
        summarise(population = sum(population))
        
        

#### INTERINSTITUTIONAL GROUP ####
# Join data frames
df_homicides_state <- df_homicides_gpo                          %>% 
        mutate(Año = year(as.Date(Fecha)))                      %>% 
        left_join(df_pop_state, by = c("Entidad", "Año"))


# Estimate mortality rates 
df_homicide_state_daily <- df_homicides_state                   %>% 
        # Estimate number of homicides per 100,000 people
        # Convert implicit missing values to explicit missing values 
        # We will have data for every date
        mutate(mort_rate = (Homicidios*100000/population))      %>% 
        complete(Fecha, nesting(Entidad), 
                 fill = list(Homicidios =0))                    %>%
        # Select variables
        mutate(Fecha = as.Date(Fecha))                          %>% 
        select(Entidad, Año, Fecha, Homicidios, population, mort_rate)
        
#### OPEN SOURCE ####
# Join data frames
df_homicides_county_open <- df_homicides_open        %>% 
        mutate(Año = year(as.Date(Fecha)))      %>% 
        left_join(df_pop, by = c("Entidad", "Municipio", "Año"))

# Estimate mortality rates 
df_homicides_county_open_daily <- df_homicides_county_open      %>% 
        mutate(mort_rate = (Homicidios*100000/population))      %>%  
        complete(Fecha, nesting(Entidad, Municipio),
                fill = list(Homicidios = 0))                    %>% 
        mutate(Fecha = as.Date(Fecha))                          %>% 
        select(Entidad, Municipio, County, county_id, Fecha, Homicidios, Hombre, 
                Mujer, No.Identificado, population, mort_rate)
        
        #View(df_homicides_county_daily)

# Rename for consistency with county data #
# Convert into lower case
df_homicides_county_open_daily <- df_homicides_county_open_daily %>% 
        rename("entidad" = Entidad, "municipio" = Municipio, "county" = County, 
                "fecha" = Fecha, "homicidios" = Homicidios, "hombre" = Hombre, 
                "mujer" = Mujer, "no_identificado" = No.Identificado)

#-----------------------------------------------------------------------------#
##            3. Check consistency of data                                 ####
#-----------------------------------------------------------------------------#
# Homicides
# Total homicides
sum(df_homicides_open$Homicidios)
sum(df_homicides_county_open_daily$homicidios)

# Men 
sum(df_homicides_open$Hombre, na.rm = T)
sum(df_homicides_county_open_daily$hombre, na.rm = T)


# Women 
sum(df_homicides_open$Mujer, na.rm = T)
sum(df_homicides_county_open_daily$mujer, na.rm = T)

# Non-identified 
sum(df_homicides_open$No.Identificado, na.rm = T)
sum(df_homicides_county_open_daily$no_identificado, na.rm = T)


#-----------------------------------------------------------------------------#
##            4. Save data                                                 ####
#-----------------------------------------------------------------------------#
# Rename df
df_homicides_county_daily_sspc_fuentesabiertas <- df_homicides_county_open_daily

# Save df
save(df_homicides_county_daily_sspc_fuentesabiertas, file = "data/fuentes_abiertas/df_homicides_county_daily_sspc_fuentesabiertas.RData")


