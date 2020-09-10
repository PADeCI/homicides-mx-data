################################################################################
#                                                                             ##
#              Homicide data 2019-2020 at municipal level                     ##
#                                                                             ##
#    Goal: Add population data                                                ##
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
# Population data
load("data_raw/df_pop_county_2019_2020.Rdata")

# Homicide data
df_homicides <- read_csv("data_raw/county/2019_2020/df_homicides_daily_fuentesabiertas.csv")


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
df_homicides_county_daily <- df_homicides %>% 
        mutate(Año = year(Fecha)) %>% 
        left_join(df_pop, by = c("Entidad", "Municipio", "Año"))


save(df_homicides_county_daily, file = "data/df_homicides_county_daily.RData")




