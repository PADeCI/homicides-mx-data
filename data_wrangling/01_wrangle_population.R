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
#### 2.1 POPULATION ####
# Change variable names for compatibility, eliminate non-identified counties 
# and make Oaxaca Regions a single category named "Oaxaca"
df_pop <- df_pop_county_2019_2020                               %>%
        rename(entidad    = entidad, 
                municipio = county_name_esp, 
                county    = county_name_eng, 
                año       = year)                               %>%                  
        mutate(population = as.numeric(population))

df_pop_state <- df_pop                                          %>% 
        filter(entidad != is.na(entidad))                       %>%                  
        group_by(entidad, año)                                  %>%
        summarise(population = sum(population))

#### 2.2 INTERINSTITUTIONAL GROUP ####
# Join data frames
df_homicides_state <- df_homicides_gpo                          %>% 
        rename("entidad" = Entidad, 
                "homicidios" = Homicidios,
                "fecha" = Fecha)                                %>% 
        mutate(año = year(as.Date(fecha)))                      %>% 
        left_join(df_pop_state, by = c("entidad", "año"))

# Estimate mortality rates 
df_homicides_state_daily <- df_homicides_state                  %>% 
        # Estimate number of homicides per 100,000 people
        # Convert implicit missing values to explicit missing values 
        # We will have data for every date
        mutate(mort_rate = (homicidios*100000/population))      %>% 
        complete(fecha, nesting(entidad), 
                 fill = list(homicidios =0))                    %>%
        # Select variables
        mutate(fecha = as.Date(fecha))                          %>% 
        select(entidad, año, fecha, homicidios, population, mort_rate)
        
#### 2.3 OPEN SOURCE ####
# Join data frames
df_homicides_county_open <- df_homicides_open                   %>% 
        rename("entidad"          = Entidad, 
                "homicidios"      = Homicidios,
                "fecha"           = Fecha, 
                "municipio"       = Municipio, 
                "hombre"          = Hombre, 
                "mujer"           = Mujer, 
                "no_identificado" = No.Identificado)            %>% 
        mutate(año = year(as.Date(fecha)))                      %>% 
        left_join(df_pop, by = c("entidad", "municipio", "año"))

# Estimate mortality rates 
df_homicides_county_open_daily <- df_homicides_county_open      %>%
        mutate(mort_rate = (homicidios*100000/population))      %>%  
        complete(fecha, nesting(entidad, municipio),
                fill = list(homicidios = 0))                    %>% 
        mutate(fecha = as.Date(fecha))                          %>% 
        select(entidad, municipio, county, county_id, fecha, homicidios, hombre, 
                mujer, no_identificado, population, mort_rate)
        #View(df_homicides_county_daily)

#-----------------------------------------------------------------------------#
##            3. Check consistency of data                                 ####
#-----------------------------------------------------------------------------#
#### Homicides according to interinstitutional group
sum(df_homicides_gpo$Homicidios)
sum(df_homicides_state_daily$homicidios)

#### Homicides according to open sources
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
df_homicides_state_daily_sspc_gpointerinstitucional <- df_homicides_state_daily
df_homicides_county_daily_sspc_fuentesabiertas <- df_homicides_county_open_daily

# Save df
save(df_homicides_state_daily_sspc_gpointerinstitucional, file = "data/gpo_interinstitucional/df_homicides_state_daily_sspc_gpointerinstitucional.RData")
save(df_homicides_county_daily_sspc_fuentesabiertas, file = "data/fuentes_abiertas/df_homicides_county_daily_sspc_fuentesabiertas.RData")


