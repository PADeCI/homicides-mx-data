##************************************************************************
## Script Name: 
## Purpose:         
## 
##
## Created:             2020-09-07 
## Last update:         2020-09-27
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

# Clean the workspace
rm(list = ls()) 


# Load Data --------------------------------------------------------------------

# Population at state level
source_data("https://github.com/PADeCI/demog-mx/blob/master/data/Estatal/df_pop_state.Rdata?raw=true")

# Homicides at state level (from open sources)
df_homicides_daily_fuentesabiertas <- read.csv("data_raw/fuentes_abiertas/2019_2020/df_homicides_daily_2019_2020_sspc_fuentesabiertas.csv")


# Data relabeling and grouping -------------------------------------------------

# Select columns of interest and rename

df_homicides_daily_state <- df_homicides_daily_fuentesabiertas  %>% 
        select(-Municipio)                                      %>%
        rename(entidad = "Entidad", 
               homicidios = "Homicidios",
               hombre = "Hombre",
               mujer = "Mujer", 
               no_identificado = "No.Identificado",
               fecha = "Fecha")

# Group by state

df_homicides_daily_state <- df_homicides_daily_state            %>% 
        group_by(entidad, fecha)                                %>%
        summarise_all(.funs = sum, na.rm=F)



# # Vectors and relabeling that will be needed 
df_pop_state <- df_pop_state                                    %>%
        rename(state_code = "CVE_GEO")


# Fill omitted dates with 0s  --------------------------------------------------
df_homicides_state_daily_fuentesabiertas <-  df_homicides_daily_state %>% 
        complete(fecha, nesting(entidad),
                fill = list(homicidios=0, hombre=0, mujer=0, no_identificado=0))


# Add population  --------------------------------------------------------------

# Join data frames

# Filter year 2019 and 2020
df_pop_2019_2020 <- df_pop_state %>% 
        filter(year == 2019 || year == 2020)

df_homicides_state_daily_fuentesabiertas <- df_homicides_state_daily_fuentesabiertas 
                                              
# Create code variable for states
df_homicides_daily <- df_homicides_state_daily_fuentesabiertas  %>% 
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

# Give date format to date variables
df_homicides_daily$fecha <- as.Date(df_homicides_daily$fecha) 
df_homicides_daily$year <- format(as.Date(df_homicides_daily$fecha, format="%Y/%m/%d"),"%Y")
df_homicides_daily$year <- as.integer(df_homicides_daily$year)

df_homicides_daily$fecha<- as.Date(df_homicides_daily$fecha , format = "%Y/%m/%d")

# Join data frames
df_homicides_daily <- df_homicides_daily                        %>% 
        left_join(df_pop_2019_2020, by = c("state_code", "year")) 
        
# Select columns
df_homicides_daily <- df_homicides_daily                        %>% 
        select(entidad,state,
               fecha, homicidios,
               hombre, mujer,
               no_identificado, population)

df_homicidios_estado_fuentesabiertas_poblacion <- df_homicides_daily



# Mort Rate --------------------------------------------------------------------

df_homicides_state_daily_sspc_fuentesabiertas <- df_homicidios_estado_fuentesabiertas_poblacion%>% 
        mutate(mort_rate = round((homicidios/population)*100000, 2)) 


# Save final data set ----------------------------------------------------------

save(df_homicides_state_daily_sspc_fuentesabiertas, file = "data/df_homicides_state_daily_sspc_fuentesabiertas.RData")

        
        