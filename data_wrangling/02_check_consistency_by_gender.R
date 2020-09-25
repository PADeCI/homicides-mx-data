################################################################################
#                                                                             ##
#              Homicide data 2019-2020 at municipal level                     ##
#                                                                             ##
#    Goal: Data validation for gendered data                                  ##
#    Authors: Regina Isabel Medina and Mariana Fernández                      ##
#    Date: September 18th, 2020                                                ##
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
# State level 
load("~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_state_daily_sspc_fuentesabiertas.RData")
load("~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_state_weekly_sspc_fuentesabiertas.RData")
load("~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_state_monthly_sspc_fuentesabiertas.RData")

# County level 
load("~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_county_daily_sspc_fuentesabiertas.RData")
load("~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_county_weekly_sspc_fuentesabiertas.RData")
load("~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_county_monthly_sspc_fuentesabiertas.RData")

#-----------------------------------------------------------------------------#
##            1. Check sum of genders                                      ####
#-----------------------------------------------------------------------------#
# When producing the descriptive statistics, it was obivous that there was a 
# problem with data, since the addition of homicides disagregated by gender 
# is not equal to the total homicides that are reported. 

## State by month ####
df_state_m <- df_homicides_state_monthly_sspc_fuentesabiertas  %>% 
        mutate(suma  = hombre + mujer + no_identificado)       %>% 
        mutate(equal = case_when(suma == homicidios ~ TRUE, 
                                 suma != homicidios ~ FALSE), 
                diff = homicidios - suma) 

df_state_NAs <- df_state_m %>% 
        mutate(hombre = case_when((hombre == 0 & mujer == 0 & no_identificado == 0) ~ NA))

df_NA            <- df_state_m
df_NA$hombre[df_NA$hombre==0 & df_NA&mujer == 0 & df_NA&no_identificado == 0]     <- NA

length(df_state_m$equal)-sum(df_state_m$equal) # Amount of FALSE
100*(length(df_state_m$equal)-sum(df_state_m$equal))/length(df_state_m$equal) # Percentage of FALSE
mean((df_state_m$diff)[df_state_m$diff != 0])  # Average difference

## State by week #### 
df_state_w <- df_homicides_state_weekly_sspc_fuentesabiertas   %>% 
        mutate(suma  = hombre + mujer + no_identificado)       %>% 
        mutate(equal = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                diff = homicidios - suma) 

length(df_state_w$equal)-sum(df_state_w$equal) # Amount of FALSE
100*(length(df_state_w$equal)-sum(df_state_w$equal))/length(df_state_w$equal) # Percentage of FALSE
mean((df_state_w$diff)[df_state_w$diff != 0])  # Average difference       

## State by day ####
df_state_d <- df_homicides_state_daily_sspc_fuentesabiertas    %>% 
        mutate(suma  = hombre + mujer + no_identificado)       %>% 
        mutate(equal = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                diff = homicidios - suma) 

length(df_state_d$equal)-sum(df_state_d$equal, na.rm = T) # Amount of FALSE
100*(length(df_state_d$equal)-sum(df_state_d$equal, na.rm = T))/length(df_state_d$equal) # Percentage of FALSE
mean((df_state_d$diff)[df_state_d$diff != 0],  na.rm = T)  # Average difference   


## County by month ####
df_county_m <- df_homicides_county_monthly_sspc_fuentesabiertas %>% 
        mutate(suma  = hombre + mujer + no_identificado)        %>% 
        mutate(equal = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                diff = homicidios - suma) 

length(df_county_m$equal)-sum(df_county_m$equal) # Amount of FALSE
100*(length(df_county_m$equal)-sum(df_county_m$equal))/length(df_county_m$equal) # Percentage of FALSE
mean((df_county_m$diff)[df_county_m$diff != 0])  # Average difference        

## County by week ####
df_county_w <- df_homicides_county_weekly_sspc_fuentesabiertas  %>% 
        mutate(suma  = hombre + mujer + no_identificado)        %>% 
        mutate(equal = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                diff = homicidios - suma) 

length(df_county_w$equal)-sum(df_county_w$equal) # Amount of FALSE
100*(length(df_county_w$equal)-sum(df_county_w$equal))/length(df_county_w$equal) # Percentage of FALSE
mean((df_county_w$diff)[df_county_w$diff != 0])  # Average difference    

 ## County by day ####
df_county_d <- df_homicides_county_daily_sspc_fuentesabiertas %>% 
        mutate(suma  = hombre + mujer + no_identificado)        %>% 
        mutate(equal = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                diff = homicidios - suma) 

length(df_county_d$equal)-sum(df_county_d$equal, na.rm = T) # Amount of FALSE
100*(length(df_county_d$equal)-sum(df_county_d$equal, na.rm = T))/length(df_county_d$equal) # Percentage of FALSE
mean((df_county_d$diff)[df_county_d$diff != 0], na.rm = T)  # Average difference    

