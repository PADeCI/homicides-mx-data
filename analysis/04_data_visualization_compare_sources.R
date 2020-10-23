################################################################################
#                                                                             ##
#              Homicide data (2019-2020) visualizations comparing sources     ##
#                                                                             ##
#    Goal: Create visualizations for homicide data                            ##
#    Authors: Regina Isabel Medina Rosales                                    ##
#    Date: October 23th, 2020                                                 ##
#                                                                             ##
################################################################################

# 00. Initial set up -----------------------------------------------------------
# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)          # For reshaping wide to long
library(stringr)        # For string manipulation
library(forcats)        # For category handling

# Clean workspace 
rm(list=ls())

# 01. Load Data ----------------------------------------------------------------
# Long format 
load("~/GitHub/homicides-mx-data/data/df_homicides_state_month_all_sources_long.RData")

# Wiede format
load("~/GitHub/homicides-mx-data/data/df_homicides_state_month_all_sources_wide.RData")


df_wide <- df_homicides_state_month_all_sources_wide

# 02. Data wrangling  ----------------------------------------------------------
df_long <- df_homicides_state_month_all_sources_long                    %>% 
        mutate(month_year = as.factor(paste0(str_sub(month, 1, 3), "-",
                str_sub(year, 3, 4))), 
                mes_year = as.factor(paste0(str_sub(mes, 1, 3), "-",
                        str_sub(year, 3, 4)))) %>% 
        mutate(mort_rate = round(homicidios*100000/population, 2)) %>% 
        mutate(source = case_when(source == "INEGI_REGISTER" ~ "INEGI", 
                                source == "SSCP_GPO_INTER"~ "SSCP (Inter. Group)", 
                                source == "SSCP_OPEN_SOURCE"~ "SSCP (Newspapers)")) %>% 
        mutate(source = factor(source, levels=c("SSCP (Inter. Group)", "SSCP (Newspapers)", "INEGI")))
        
df_national <- df_long %>% 
        filter(month != "Total", state == "National")

df_national_2019 <- df_long %>% 
        filter(month != "Total", state == "National", 
                #source != "SSCP (Inter. Group)", 
                year == "2019")
df_cdmx_2019 <- df_long %>% 
        filter(month != "Total", state == "Mexico City", 
                #source != "SSCP (Inter. Group)", 
                year == "2019")
df_gto_2019 <- df_long %>% 
        filter(month != "Total", state == "Mexico City", 
                #source != "SSCP (Inter. Group)", 
                year == "2019")

df_national_2020 <- df_long %>% 
        filter(month != "Total", state == "National", 
                #source != "SSCP (Inter. Group)", 
                year == "2020")
df_cdmx_2020 <- df_long %>% 
        filter(month != "Total", state == "Mexico City", 
                #source != "SSCP (Inter. Group)", 
                year == "2020")
df_gto_2020 <- df_long %>% 
        filter(month != "Total", state == "Mexico City", 
                #source != "SSCP (Inter. Group)", 
                year == "2020")



# 03. Create graphs  -----------------------------------------------------------
v_caption_SSPC <- "Source: Own elaboration with data from INEGI and SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/"

# Cases from 2019
ggplot(df_national_2019, 
        aes(x = month, y = homicidios, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Total homicides comparison between sources (2019)", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Homicides",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) 

ggsave(file = "figs/graphs/all_sources/g_compare_national_2019.png")

ggplot(df_cdmx_2019, 
        aes(x = month, y = homicidios, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Total homicides comparison between sources (2019)", 
                subtitle = "Mexico City",
                hjust = 0, 
                x = "Month",
                y = "Homicides",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_cdmx_2019.png")


ggplot(df_gto_2019, 
        aes(x = month, y = homicidios, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Total homicides comparison between sources (2019)", 
                subtitle = "Guanajuato",
                hjust = 0, 
                x = "Month",
                y = "Homicides",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_gto_2019.png")


# Cases from 2020
ggplot(df_national_2020, 
        aes(x = month, y = homicidios, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Total homicides comparison between sources (2020)", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Homicides",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_national_2020.png")

ggplot(df_cdmx_2020, 
        aes(x = month, y = homicidios, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Total homicides comparison between sources (2020)", 
                subtitle = "Mexico City",
                hjust = 0, 
                x = "Month",
                y = "Homicides",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_cdmx_2020.png")


ggplot(df_gto_2020, 
        aes(x = month, y = homicidios, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Total homicides comparison between sources (2020)", 
                subtitle = "Guanajuato",
                hjust = 0, 
                x = "Month",
                y = "Homicides",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_gto_2020.png")


# Mortality rate from 2019 
ggplot(df_national_2019, 
        aes(x = month, y = mort_rate, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Homicide rate comparison between sources (2019)", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Homicides",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_mortrate_national_2019.png")


ggplot(df_national_2020, 
        aes(x = month, y = mort_rate, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Total homicides comparison between sources (2020)", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Homicides",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_mortrate_national_2020.png")
