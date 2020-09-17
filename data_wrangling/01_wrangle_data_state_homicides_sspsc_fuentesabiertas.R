##************************************************************************
## Script Name: 
## Purpose:         
## 
##
## Created: 2020-09-07                               
## Authors: Mariana Consuelo Fernandez Espinosa, Regina Isabel Medina
##          
## GitHub: marianafdz465  
##
##
##************************************************************************


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readr)
library(lubridate)
library(httr)
library(repmis)
library(dplyr)
library(magrittr)
library(gapminder)


# Load Data ---------------------------------------------------------------

#States
source_data("https://github.com/PADeCI/demog-mx/blob/master/data/Estatal/df_pop_state.Rdata?raw=true")

df_homicides_daily_fuentesabiertas <- read.csv("data_raw/county/2019_2020/df_homicidios_dia_fuentesabiertas.csv")


# State Level -------------------------------------------------------------

#Select columns of interest and rename columns

df_homicides_daily_state <- df_homicides_daily_fuentesabiertas %>% 
        select(-Municipio) %>%
        rename(entidad = "Entidad", 
               homicidios = "Homicidios",
               hombre = "Hombre",
               mujer = "Mujer", no_identificado = "No.Identificado",
               fecha = "Fecha")
# Group by state

df_homicides_daily_state <- df_homicides_daily_state %>% 
        group_by(entidad,fecha) %>%
        summarise_all(.funs = sum,na.rm=F)

save(df_homicides_daily_state, file = "data/df_homicidios_estado_dia_fuentesabiertas.RData")



# Add population and estimate homicide mortality rate -----------------------------------


load("data/df_homicidios_estado_dia_fuentesabiertas.RData")

df_homicides_state_daily_fuentesabiertas <- df_homicides_daily_state


# # Vectors and relabeling that will be needed 
df_pop_state <- df_pop_state %>%
        rename(state_code = "CVE_GEO")


# Join data frames

#Filter year 2019 and 2020
df_pop_2019_2020 <- df_pop_state %>% 
        filter(year == 2019 || year == 2020)

df_homicides_state_daily_fuentesabiertas <- df_homicides_state_daily_fuentesabiertas 
                                              
        
df_homicides_daily <- df_homicides_state_daily_fuentesabiertas %>% 
        
        mutate(state_code = case_when(
                entidad == "Aguascalientes" ~ 1,
                entidad == "Baja California" ~ 2,
                entidad == "Baja California Sur" ~ 3,
                entidad == "Campeche" ~ 4,
                entidad == "Coahuila" ~ 5,
                entidad == "Colima" ~ 6,
                entidad == "Chiapas" ~ 7,      
                entidad == "Chihuahua" ~ 8,
                entidad == "Ciudad de México" ~ 9,
                entidad == "Durango" ~ 10,
                entidad == "Guanajuato" ~ 11,
                entidad == "Guerrero" ~ 12,
                entidad == "Hidalgo" ~ 13,
                entidad == "Jalisco" ~ 14,
                entidad == "Estado de México" ~ 15,
                entidad == "Michoacán" ~ 16,
                entidad == "Morelos" ~ 17,
                entidad == "Nacional" ~ 0, 
                entidad == "Nayarit" ~ 18,
                entidad == "Nuevo León"  ~ 19,
                entidad == "Oaxaca" ~ 20,
                entidad == "Puebla" ~ 21,
                entidad == "Querétaro" ~ 22,
                entidad == "Quintana Roo" ~ 23,
                entidad == "San Luis Potosí" ~ 24,
                entidad == "Sinaloa" ~ 25,
                entidad == "Sonora" ~ 26,
                entidad == "Tabasco" ~ 27,
                entidad == "Tamaulipas" ~ 28,
                entidad == "Tlaxcala" ~ 29,
                entidad == "Veracruz" ~ 30,
                entidad == "Yucatán" ~ 31, 
                entidad == "Zacatecas" ~ 32)) 

df_homicides_daily$fecha <- as.Date(df_homicides_daily$fecha) 
df_homicides_daily$year <- format(as.Date(df_homicides_daily$fecha, format="%Y/%m/%d"),"%Y")
df_homicides_daily$year <- as.integer(df_homicides_daily$year)

df_homicides_daily$fecha<- as.Date(df_homicides_daily$fecha , format = "%Y/%m/%d")



# Add population and estimate homicide mortality rate


df_homicides_daily <- df_homicides_daily %>% 
        left_join(df_pop_2019_2020, by = c("state_code", "year")) 
        
#Select columns

df_homicides_daily <- df_homicides_daily %>% 
        select(entidad,state,
               fecha, homicidios,
               hombre, mujer,
               no_identificado, population)

df_homicidios_estado_fuentesabiertas_poblacion <- df_homicides_daily

#Save with population column
        
save(df_homicidios_estado_fuentesabiertas_poblacion, file = "data/df_homicidios_estado_fuentesabiertas_poblacion.RData")


# Mort Rate ---------------------------------------------------------------

df_homicidios_estado_fuentesabiertas_mort_rate <-  df_homicidios_estado_fuentesabiertas_poblacion%>% 
        mutate(mort_rate = round((homicidios/population)*100000, 2)) 

save(df_homicidios_estado_fuentesabiertas_mort_rate, file = "data/df_homicidios_estado_fuentesabiertas_mort_rate.RData")





        
        