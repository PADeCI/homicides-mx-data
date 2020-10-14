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
# is not diff to the total homicides that are reported. 

# 02.1 State level -------------------------------------------------------------

## State by day ####
df_state_d <- df_homicides_state_daily_sspc_fuentesabiertas    %>% 
        mutate(suma  = hombre + mujer + no_identificado)       %>% 
        mutate(diff = case_when(suma == homicidios ~ FALSE, 
                suma != homicidios ~ TRUE), 
                scale = homicidios - suma) 

length(df_state_d$diff)-sum(df_state_d$diff, na.rm = T) # Amount of FALSE
100*(length(df_state_d$diff)-sum(df_state_d$diff, na.rm = T))/length(df_state_d$diff) # Percentage of FALSE
mean((df_state_d$scale)[df_state_d$scale != 0],  na.rm = T)  # Average scaleerence   
sum(df_state_d$scale, na.rm = T)



 ## State by week #### 
df_state_w <- df_homicides_state_weekly_sspc_fuentesabiertas   %>% 
        mutate(suma  = hombre + mujer + no_identificado, na.rm = T)       %>% 
        mutate(diff = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                scale = homicidios - suma) 

length(df_state_w$diff)-sum(df_state_w$diff, na.rm = T) # Amount of FALSE
100*(length(df_state_w$diff)-sum(df_state_w$diff, na.rm = T))/length(df_state_w$diff) # Percentage of FALSE
mean((df_state_w$scale)[df_state_w$scale != 0], na.rm = T)  # Average scaleerence       
sum(df_state_w$scale, na.rm = T)

## State by month ####
df_state_m <- df_homicides_state_monthly_sspc_fuentesabiertas  %>% 
        mutate(suma  = sum(hombre, mujer, no_identificado, na.rm = T)) %>% 
        mutate(diff = case_when(suma == homicidios ~ TRUE, 
                                 suma != homicidios ~ FALSE), 
                scale = homicidios - suma) 

length(df_state_m$diff)-sum(df_state_m$diff, na.rm = T) # Amount of FALSE
100*(length(df_state_m$diff)-sum(df_state_m$diff, na.rm = T))/length(df_state_m$diff) # Percentage of FALSE
mean((df_state_m$scale)[df_state_m$scale != 0], na.rm = T)  # Average scaleerence
sum(df_state_m$diff, na.rm = T)

df_state_m_mistakes <-  df_state_m %>% 
        filter(diff == TRUE) %>% 
        filter(is.na(hombre) == FALSE)



# 02.2 County level -------------------------------------------------------------

 ## County by day ####
df_county_d <- df_homicides_county_daily_sspc_fuentesabiertas   %>% 
        mutate(suma  = hombre + mujer + no_identificado, na.rm = T)        %>% 
        mutate(diff = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                scale = homicidios - suma) 

length(df_county_d$diff)-sum(df_county_d$diff, na.rm = T, na.rm = T) # Amount of FALSE
100*(length(df_county_d$diff)-sum(df_county_d$diff, na.rm = T, na.rm = T))/length(df_county_d$diff) # Percentage of FALSE
mean((df_county_d$scale)[df_county_d$scale != 0], na.rm = T)  # Average scaleerence    

## County by week ####
df_county_w <- df_homicides_county_weekly_sspc_fuentesabiertas  %>% 
        mutate(suma  = hombre + mujer + no_identificado)        %>% 
        mutate(diff = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                scale = homicidios - suma) 

length(df_county_w$diff)-sum(df_county_w$diff, na.rm = T) # Amount of FALSE
100*(length(df_county_w$diff)-sum(df_county_w$diff, na.rm = T))/length(df_county_w$diff) # Percentage of FALSE
mean((df_county_w$scale)[df_county_w$scale != 0], na.rm = T)  # Average scaleerence    

## County by month ####
df_county_m <- df_homicides_county_monthly_sspc_fuentesabiertas %>% 
        mutate(suma  = hombre + mujer + no_identificado, na.rm = T)        %>% 
        mutate(diff = case_when(suma == homicidios ~ TRUE, 
                suma != homicidios ~ FALSE), 
                scale = homicidios - suma) 

length(df_county_m$diff)-sum(df_county_m$diff, na.rm = T) # Amount of FALSE
100*(length(df_county_m$diff)-sum(df_county_m$diff, na.rm = T))/length(df_county_m$diff) # Percentage of FALSE
mean((df_county_m$scale)[df_county_m$scale != 0], na.rm = T)  # Average scaleerence        


# 03. Identify inconsistencies  ------------------------------------------------

# Get all the mistakes in a single df
df_county_d_mistakes <- df_county_d %>% 
        filter(diff == FALSE) %>% 
        filter(is.na(hombre) == FALSE)

# Total number of mistakes: 33 
length(df_county_d_mistakes$fecha)

# Number of dates with mistakes: 20
length(unique(df_county_d_mistakes$fecha))

# Dates with mistakes
unique(df_county_d_mistakes$fecha)

View(df_county_d_mistakes)


# 02.1 State level -------------------------------------------------------------
df_state_d <- df_homicides_state_daily_sspc_fuentesabiertas     %>% 
        mutate(suma  = hombre + mujer + no_identificado)        %>% 
        mutate(diff = case_when(suma == homicidios ~ FALSE, 
                suma != homicidios ~ TRUE), 
                scale = homicidios - suma) 

sum(df_state_d$diff, na.rm = T)


df_state_w <- df_homicides_state_weekly_sspc_fuentesabiertas    %>% 
        mutate(suma  = hombre + mujer + no_identificado)        %>% 
        mutate(diff = case_when(suma == homicidios ~ FALSE, 
                suma != homicidios ~ TRUE), 
                scale = homicidios - suma) 

sum(df_state_w$diff, na.rm = T)

df_state_m <- df_homicides_state_monthly_sspc_fuentesabiertas   %>% 
        mutate(suma  = hombre + mujer + no_identificado)        %>% 
        mutate(diff = case_when(suma == homicidios ~ FALSE, 
                suma != homicidios ~ TRUE), 
                scale = homicidios - suma) 

sum(df_state_m$diff, na.rm = T)



