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
# Set vectors 
v_month_abv <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
        "Sep", "Oct", "Nov", "Dec", "Unk")
# Define factors 
df_long <- df_homicides_state_month_all_sources_long                    %>% 
        mutate(month_year = as.factor(paste0(str_sub(month, 1, 3), "-",
                str_sub(year, 3, 4))), 
                mes_year = as.factor(paste0(str_sub(mes, 1, 3), "-",
                        str_sub(year, 3, 4)))) %>% 
        mutate(mort_rate = round(homicidios*100000/population, 2))      %>% 
        mutate(source = case_when(source == "INEGI_REGISTER" ~ "INEGI_S1 (Register)",
                                source == "INEGI_OCURRENCE" ~ "INEGI_S1 (Ocurrence)",
                                source == "INEGI_OCURRENCE_S2" ~ "INEGI (Ocurrence)", 
                                source == "SSCP_GPO_INTER"~ "SSCP (Inter. Group)", 
                                source == "SSCP_OPEN_SOURCE"~ "SSCP (Newspapers)")) %>% 
        mutate(source = factor(source, levels=c("SSCP (Inter. Group)", "SSCP (Newspapers)", 
                "INEGI (Register)", "INEGI (Ocurrence)", "INEGI S2 (Ocurrence)"))) %>% 
        mutate(month_abv = str_sub(month, 1, 3), 
                month_abv = factor(month_abv, levels = c(v_month_abv)))


save(df_long, file = "~/GitHub/homicides-mx-data/data/df_homicides_long.RData")

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
df_national_2019_total <- df_national_2019      %>% 
        group_by(source)                        %>% 
        summarise(homicides = sum(homicidios), 
                  population = population)      %>% 
        distinct(source, .keep_all = TRUE)      %>% 
        mutate(year = 2019, 
                mort_rate = homicides*100000/population, 
                total = "total") 
        
        
df_long_short_2019 <- df_long                   %>% 
        filter(source != "INEGI_S1 (Register)", source != "INEGI_S1 (Ocurrence)") %>% 
        filter(month != "Total", state == "National", year == "2019")       %>% 
        mutate(month_abv = str_sub(month, 1, 3), 
                month_abv = factor(month_abv, levels = c(v_month_abv)))

df_long_short_2019_total <- df_long_short_2019 %>% 
        group_by(source) %>% 
        summarise(homicides = sum(homicidios), 
                population = population) %>% 
        distinct(source, .keep_all = TRUE)      %>% 
        mutate(year = 2019, 
                mort_rate = homicides*100000/population, 
                total = "total")

# 03. Create graphs  -----------------------------------------------------------
# 03.1 Define useful vectors  --------------------------------------------------

v_caption_SSPC  <- "Source: Own elaboration with data from INEGI and SSPC, retrieved from:\nhttp://www.informeseguridad.cns.gob.mx/ and https://www.inegi.org.mx/programas/mortalidad/?ps=microdatos"

v_colors        <- c("#D77A61", "#FFC857", "#9DB4C0", "#650D1B", "#223843") 
v_colors <- c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")
v_colors <- c("#219EBC", "#023047", "#FFB703", "#FB8500", "#8ECAE6")

# 03.2 National level (2019) ---------------------------------------------------

# Total homicides in 2019 by source (5 sources) 
ggplot(df_national_2019_total, 
        aes(x = total, y = homicides, fill = source)) +
        geom_col(position = "dodge") +
        theme_classic() +
        theme(axis.title.x = element_blank(), 
                axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(), 
                plot.caption = element_text(hjust = 0)) +
        scale_fill_manual(values = v_colors) +
        labs(title = "Total homicides according to different sources (2019)", 
                y = "Homicides", 
                x = "", 
                fill = "Source",
                caption = v_caption_SSPC) 

ggsave(file = "figs/graphs/all_sources/g_compare_national_total_2019_bars.pdf", 
        width = 7, height = 4)

# Total homicides in 2019 by source (3 sources) 
ggplot(df_long_short_2019_total, 
        aes(x = total, y = homicides, fill = source)) +
        geom_col(position = "dodge") +
        theme_classic() +
        theme(axis.title.x = element_blank(), 
                axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(), 
                plot.caption = element_text(hjust = 0)) +
        scale_fill_manual(values = v_colors) +
        labs(title = "Total homicides according to different sources (2019)", 
                y = "Homicides", 
                x = "", 
                fill = "Source",
                caption = v_caption_SSPC) 

ggsave(file = "figs/graphs/all_sources/g_compare_national_total_2019_bars_3s.pdf", 
        width = 7, height = 4)


# National level by month (5 sources)
g_national_2019 <- ggplot(df_national_2019, 
        aes(x = month_abv, y = homicidios)) +
        labs(title = "Total monthly homicides comparison by different sources (2019)", 
                subtitle = "National level",
                hjust = 0.5, 
                x = "Month",
                y = "Homicides",
                fill = "Source", 
                color = "Source",
                caption = v_caption_SSPC) +
        theme_classic() +
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 0), 
                plot.caption = element_text(hjust = 0.5)) 

g_national_2019 + geom_col(position = "dodge", aes(fill = source))
ggsave(file = "figs/graphs/all_sources/g_compare_national_2019_bars.pdf", 
        width = 7, height = 4)


g_national_2019 + geom_line(aes(group = source, color = source), size=1) +
        geom_point(aes(color=source), size = 1.5)
ggsave(file = "figs/graphs/all_sources/g_compare_national_2019_lines.pdf", 
        width = 7, height = 4)


# National level by month (3 sources)
g_national_2019_3s <- ggplot(df_long_short_2019, 
        aes(x = month_abv, y = homicidios)) +
        labs(title = "Total monthly homicides comparison by different sources (2019)", 
                subtitle = "National level",
                hjust = 0.5, 
                x = "Month",
                y = "Homicides",
                fill = "Source", 
                color = "Source",
                caption = v_caption_SSPC) +
        theme_classic() +
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 0), 
                plot.caption = element_text(hjust = 0.5)) 

g_national_2019_3s + geom_col(position = "dodge", aes(fill = source))

ggsave(file = "figs/graphs/all_sources/g_compare_national_2019_bars_3s.pdf", 
        width = 7, height = 4)


g_national_2019 + geom_line(aes(group = source, color = source), size=1) +
        geom_point(aes(color=source), size = 1.5)

ggsave(file = "figs/graphs/all_sources/g_compare_national_2019_lines_3s.pdf", 
        width = 7, height = 4)



# 03.2 State level (2019) ------------------------------------------------------

# Mexico city 
        # ggplot(df_cdmx_2019, 
        #         aes(x = month, y = homicidios, fill = source)) +
        #         geom_col(position = "dodge") +
        #         labs(title = "Total homicides comparison between sources (2019)", 
        #                 subtitle = "Mexico City",
        #                 hjust = 0, 
        #                 x = "Month",
        #                 y = "Homicides",
        #                 fill = "Source", 
        #                 caption = v_caption_SSPC) +
        #         theme_classic() +
        #         scale_fill_manual(values = v_colors) +
        #         scale_color_manual(values=v_colors) +
        #         theme(axis.text.x = element_text(angle = 30), 
        #                 plot.caption = element_text(hjust = 0.5)) 
        # ggsave(file = "figs/graphs/all_sources/g_compare_cdmx_2019.pdf", 
        #         width = 7, height = 4)


# Guanajuato 
        # ggplot(df_gto_2019, 
        #         aes(x = month, y = homicidios, fill = source)) +
        #         geom_col(position = "dodge") +
        #         labs(title = "Total homicides comparison between sources (2019)", 
        #                 subtitle = "Guanajuato",
        #                 hjust = 0, 
        #                 x = "Month",
        #                 y = "Homicides",
        #                 fill = "Source", 
        #                 caption = v_caption_SSPC) +
        #         theme_classic() +
        #         scale_fill_manual(values = v_colors) +
        #         scale_color_manual(values=v_colors) +
        #         theme(axis.text.x = element_text(angle = 30),
        #                 plot.caption = element_text(hjust = 0.5)) 
        # ggsave(file = "figs/graphs/all_sources/g_compare_gto_2019.pdf", 
        #         width = 7, height = 4)


# 03.3 Cases from 2020 ---------------------------------------------------------
# National level
ggplot(df_national_2020, 
        aes(x = month_abv, y = homicidios, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Total homicides comparison between sources (2020)", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Homicides",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_classic() +
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 0), 
                plot.caption = element_text(hjust = 0.5)) 

ggsave(file = "figs/graphs/all_sources/g_compare_national_2020.pdf", 
        width = 7, height = 4)

# Mexico City 
        # ggplot(df_cdmx_2020, 
        #         aes(x = month, y = homicidios, fill = source)) +
        #         geom_col(position = "dodge") +
        #         labs(title = "Total homicides comparison between sources (2020)", 
        #                 subtitle = "Mexico City",
        #                 hjust = 0, 
        #                 x = "Month",
        #                 y = "Homicides",
        #                 fill = "Source", 
        #                 caption = v_caption_SSPC) +
        #         theme_classic() +
        #         scale_fill_manual(values = v_colors) +
        #         scale_color_manual(values=v_colors) +
        #         theme(axis.text.x = element_text(angle = 30), 
        #                 plot.caption = element_text(hjust = 0.5)) 
        # ggsave(file = "figs/graphs/all_sources/g_compare_cdmx_2020.pdf", 
        #         width = 7, height = 4)


# Guanajuato 
        # ggplot(df_gto_2020, 
        #         aes(x = month, y = homicidios, fill = source)) +
        #         geom_col(position = "dodge") +
        #         labs(title = "Total homicides comparison between sources (2020)", 
        #                 subtitle = "Guanajuato",
        #                 hjust = 0, 
        #                 x = "Month",
        #                 y = "Homicides",
        #                 fill = "Source", 
        #                 caption = v_caption_SSPC) +
        #         theme_classic() +
        #         scale_fill_manual(values = v_colors) +
        #         scale_color_manual(values=v_colors) +
        #         theme(axis.text.x = element_text(angle = 30), 
        #                 plot.caption = element_text(hjust = 0.5)) 
        # ggsave(file = "figs/graphs/all_sources/g_compare_gto_2020.pdf", 
        #         width = 7, height = 4)


# 03.4 Homicide rate from 2019 -------------------------------------------------
# National level 
ggplot(df_national_2019, 
        aes(x = month_abv, y = mort_rate, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Homicide rate comparison between sources (2019)", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Homicide rate\n(number of homicides per 100,000 inhabitans)",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_classic() +
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 0), 
                plot.caption = element_text(hjust = 0.5)) 
ggsave(file = "figs/graphs/all_sources/g_compare_mortrate_national_2019.pdf", 
        width = 7, height = 4)



ggplot(df_long_short_2019, 
        aes(x = month_abv, y = mort_rate, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Homicide rate comparison between sources (2019)", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Homicide rate\n(number of homicides per 100,000 inhabitans)",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_classic() +
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 0), 
                plot.caption = element_text(hjust = 0.5)) 
ggsave(file = "figs/graphs/all_sources/g_compare_mortrate_national_2019_3s.pdf", 
        width = 7, height = 4)


# 03.5 Homicide rate from 2020 -------------------------------------------------
# National level 
ggplot(df_national_2020, 
        aes(x = month_abv, y = mort_rate, fill = source)) +
        geom_col(position = "dodge") +
        labs(title = "Homicide rate comparison between sources (2020)", 
                subtitle = "National level",
                hjust = 0, 
                x = "Month",
                y = "Homicide rate\n(number of homicides per 100,000 inhabitans)",
                fill = "Source", 
                caption = v_caption_SSPC) +
        theme_classic() +
        scale_fill_manual(values = v_colors) +
        scale_color_manual(values=v_colors) +
        theme(axis.text.x = element_text(angle = 0), 
                plot.caption = element_text(hjust = 0.5)) 
ggsave(file = "figs/graphs/all_sources/g_compare_mortrate_national_2020.pdf", 
        width = 7, height = 4)

