################################################################################
#                                                                             ##
#              Homicide data 2019-2020 at municipal level                     ##
#                                                                             ##
#    Goal: Data validation for gendered data                                  ##
#    Authors: Regina Isabel Medina and Mariana Fernández                      ##
#    Date: September 18th, 2020                                               ##
#                                                                             ##
################################################################################

# 00. Initial set up -----------------------------------------------------------

# Load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(magrittr)

# Clean the workspace
rm(list = ls()) 


# 01. Load Data ----------------------------------------------------------------

# State level 
load("./data/fuentes_abiertas/df_homicides_state_daily_sspc_fuentesabiertas.RData")
load("./data/fuentes_abiertas/df_homicides_state_weekly_sspc_fuentesabiertas.RData")
load("./data/fuentes_abiertas/df_homicides_state_monthly_sspc_fuentesabiertas.RData")

# County level 
load("./data/fuentes_abiertas/df_homicides_county_daily_sspc_fuentesabiertas.RData")
load("./data/fuentes_abiertas/df_homicides_county_weekly_sspc_fuentesabiertas.RData")
load("./data/fuentes_abiertas/df_homicides_county_monthly_sspc_fuentesabiertas.RData")


# 02. Check sum of genders -----------------------------------------------------

# When producing the descriptive statistics, it was obivous that there was a 
# problem with data, since the addition of homicides disagregated by gender 
# is not equal to the total homicides that are reported. 

# 02.1 State level -------------------------------------------------------------

## State by day ####
df_state_d <- df_homicides_state_daily_sspc_fuentesabiertas    %>% 
        mutate(suma  = hombre + mujer + no_identificado, na.rm = T)       %>% 
        mutate(equal = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                diff = homicidios - suma) 

length(df_state_d$equal)-sum(df_state_d$equal, na.rm = T) # Amount of FALSE
100*(length(df_state_d$equal)-sum(df_state_d$equal, na.rm = T))/length(df_state_d$equal) # Percentage of FALSE
mean((df_state_d$diff)[df_state_d$diff != 0],  na.rm = T)  # Average difference   
sum(df_state_d$diff, na.rm = T)



 ## State by week #### 
df_state_w <- df_homicides_state_weekly_sspc_fuentesabiertas   %>% 
        mutate(suma  = hombre + mujer + no_identificado, na.rm = T)       %>% 
        mutate(equal = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                diff = homicidios - suma) 

length(df_state_w$equal)-sum(df_state_w$equal, na.rm = T) # Amount of FALSE
100*(length(df_state_w$equal)-sum(df_state_w$equal, na.rm = T))/length(df_state_w$equal) # Percentage of FALSE
mean((df_state_w$diff)[df_state_w$diff != 0], na.rm = T)  # Average difference       
sum(df_state_w$diff, na.rm = T)

## State by month ####
df_state_m <- df_homicides_state_monthly_sspc_fuentesabiertas  %>% 
        mutate(suma  = sum(hombre, mujer, no_identificado, na.rm = T)) %>% 
        mutate(equal = case_when(suma == homicidios ~ TRUE, 
                                 suma != homicidios ~ FALSE), 
                diff = homicidios - suma) 

length(df_state_m$equal)-sum(df_state_m$equal, na.rm = T) # Amount of FALSE
100*(length(df_state_m$equal)-sum(df_state_m$equal, na.rm = T))/length(df_state_m$equal) # Percentage of FALSE
mean((df_state_m$diff)[df_state_m$diff != 0], na.rm = T)  # Average difference

df_state_m_mistakes <-  df_state_m %>% 
        filter(equal == FALSE) %>% 
        filter(is.na(hombre) == FALSE)



# 02.2 County level -------------------------------------------------------------

 ## County by day ####
df_county_d <- df_homicides_county_daily_sspc_fuentesabiertas   %>% 
        mutate(suma  = hombre + mujer + no_identificado, na.rm = T)        %>% 
        mutate(equal = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                diff = homicidios - suma) 

length(df_county_d$equal)-sum(df_county_d$equal, na.rm = T, na.rm = T) # Amount of FALSE
100*(length(df_county_d$equal)-sum(df_county_d$equal, na.rm = T, na.rm = T))/length(df_county_d$equal) # Percentage of FALSE
mean((df_county_d$diff)[df_county_d$diff != 0], na.rm = T)  # Average difference    

## County by week ####
df_county_w <- df_homicides_county_weekly_sspc_fuentesabiertas  %>% 
        mutate(suma  = hombre + mujer + no_identificado)        %>% 
        mutate(equal = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                diff = homicidios - suma) 

length(df_county_w$equal)-sum(df_county_w$equal, na.rm = T) # Amount of FALSE
100*(length(df_county_w$equal)-sum(df_county_w$equal, na.rm = T))/length(df_county_w$equal) # Percentage of FALSE
mean((df_county_w$diff)[df_county_w$diff != 0], na.rm = T)  # Average difference    

## County by month ####
df_county_m <- df_homicides_county_monthly_sspc_fuentesabiertas %>% 
        mutate(suma  = hombre + mujer + no_identificado, na.rm = T)        %>% 
        mutate(equal = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                diff = homicidios - suma) 

length(df_county_m$equal)-sum(df_county_m$equal, na.rm = T) # Amount of FALSE
100*(length(df_county_m$equal)-sum(df_county_m$equal, na.rm = T))/length(df_county_m$equal) # Percentage of FALSE
mean((df_county_m$diff)[df_county_m$diff != 0], na.rm = T)  # Average difference        


# 03. Identify inconsistencies  ------------------------------------------------

# Get all the mistakes in a single df
df_county_d_mistakes <- df_county_d %>% 
        filter(equal == FALSE) %>% 
        filter(is.na(hombre) == FALSE)

# Total number of mistakes: 33 
length(df_county_d_mistakes$fecha)

# Number of dates with mistakes: 20
length(unique(df_county_d_mistakes$fecha))

# Dates with mistakes
unique(df_county_d_mistakes$fecha)

View(df_county_d_mistakes)
