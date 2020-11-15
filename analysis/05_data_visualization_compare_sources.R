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
        mutate(source = case_when(source == "INEGI_REGISTER" ~ "INEGI (Register)",
                                source == "INEGI_OCURRENCE" ~ "INEGI (Ocurrence)",
                                source == "SSCP_GPO_INTER"~ "SSCP (Inter. Group)", 
                                source == "SSCP_OPEN_SOURCE"~ "SSCP (Newspapers)")) %>% 
        mutate(source = factor(source, levels=c("SSCP (Inter. Group)", "SSCP (Newspapers)", "INEGI (Register)", "INEGI (Ocurrence)")))
        
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


# Create numbers by year 
df_national_2019_total <- df_national_2019 %>% 
        group_by(source) %>% 
        summarise(homicides = sum(homicidios), 
                  population = population) %>% 
        distinct(source, .keep_all = TRUE) %>% 
        mutate(year = 2019, 
                mort_rate = homicides*100000/population, 
                total = "total")
        

# 03. Create graphs  -----------------------------------------------------------
# 03.1 Define useful vectors  --------------------------------------------------

v_caption_SSPC  <- "Source: Own elaboration with data from INEGI and SSPC, retrieved from: http://www.informeseguridad.cns.gob.mx/"

v_colors        <- c("#D77A61", "#FFC857", "#9DB4C0", "#650D1B", "#223843") 

# 03.2 Cases from 2019 ---------------------------------------------------------

# Total homicides in 2019 by source 
ggplot(df_national_2019_total, 
        aes(x = total, y = homicides, fill = source)) +
        geom_col(position = "dodge") +
        geom_text(aes(label=df_national_2019_total$homicides), position_dodge(0.9)) +
        theme_minimal() +
        scale_fill_manual(values = v_colors) 

 # National level
g_national_2019 <- ggplot(df_national_2019, 
        aes(x = month, y = homicidios)) +
        labs(title = "Total homicides comparison between sources (2019)", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Homicides",
                fill = "Source", 
                color = "Source",
                caption = v_caption_SSPC) +
        theme_minimal() +
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 30)) 

g_national_2019 + geom_col(position = "dodge", aes(fill = source))
ggsave(file = "figs/graphs/all_sources/g_compare_national_2019_bars.png", 
        width = 7, height = 4)


g_national_2019 + geom_line(aes(group = source, color = source), size=1) +
        geom_point(aes(color=source), size = 1.5)
ggsave(file = "figs/graphs/all_sources/g_compare_national_2019_lines.pdf", 
        width = 7, height = 4)


# Mexico city 
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
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_cdmx_2019.png")


# Guanajuato 
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
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_gto_2019.png")


# 03.3 Cases from 2020 ---------------------------------------------------------
# National level
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
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_national_2020.pdf")

# Mexico City 
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
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_cdmx_2020.png")


# Guanajuato 
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
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_gto_2020.png")


# 03.4 Homicide rate from 2019 -------------------------------------------------
# National level 
ggplot(df_national_2019, 
        aes(x = month, y = mort_rate, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Homicide rate comparison between sources (2019)", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Homicide rate\n(number of homicides per 100,000 inhabitans)",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_mortrate_national_2019.pdf")


# 03.5 Homicide rate from 2020 -------------------------------------------------
# National level 
ggplot(df_national_2020, 
        aes(x = month, y = mort_rate, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Homicide rate comparison between sources (2020)", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Homicide rate\n(number of homicides per 100,000 inhabitans)",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_minimal() +
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 30)) 
ggsave(file = "figs/graphs/all_sources/g_compare_mortrate_national_2020.pdf")

