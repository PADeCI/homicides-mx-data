##******************************************************************************
## Script Name: Wrangle homicide data from sspc fuentes abiertas at state-level
## Purpose:         
## 
##
## Created:             2020-10-15 
## Last update:         2020-10-15
## Authors: }Regina Isabel Medina Rosales
##          
##******************************************************************************

# 00. Initial set up -----------------------------------------------------------

# Load libraries
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

# 01. Load Data ----------------------------------------------------------------

# Population data at state level
source_data("https://github.com/PADeCI/demog-mx/blob/master/data/Estatal/df_pop_state.Rdata?raw=true")

# Homicides at state level (from open sources)
read_excel("data_raw/inegi/mes_registro/df_homicides_01-2019_inegi.xls")
View(df_homicides_01_2019_inegi)

# 02. Data wrangling  ----------------------------------------------------------
# 03. Check consistency of data ------------------------------------------------
# 04. Save final data set ------------------------------------------------------
