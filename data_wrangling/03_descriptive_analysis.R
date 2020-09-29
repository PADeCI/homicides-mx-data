################################################################################
#                                                                             ##
#              Homicide data 2019-2020 at municipal level                     ##
#                                                                             ##
#    Goal: Create descriptive analysis for homicide data                      ##
#    Authors: Regina Isabel Medina Rosales                                    ##
#    Date: September 18th, 2020                                               ##
#                                                                             ##
################################################################################

# 00. Initial set up -----------------------------------------------------------

# Libraries 
library(dplyr)          # For data cleaning
library(lubridate)      # For dates handling
library(forcats)        # For factor handling 

library(grid)           # For table generation
library(gridExtra)      # For table manipulation
library(pander)         # For table reading 
library(captioner)      # For table and figure labels


# Clean the workspace
rm(list = ls()) 


# 01. Load data ----------------------------------------------------------------
# 01.1 Interinstitutional group (SSCP) -----------------------------------------
# By day 
load("~/GitHub/homicides-mx-data/data/gpo_interinstitucional/df_homicides_state_daily_sspc_gpointerinstitucional.RData")
df_state_d_gpo <- df_homicides_state_daily_sspc_gpointerinstitucional # Rename

# By month
load("~/GitHub/homicides-mx-data/data/gpo_interinstitucional/df_homicides_state_monthly_sspc_gpointerinstitucional.Rdata")
df_state_m_gpo <- df_homicides_state_monthly# Rename

# By year 
load("~/GitHub/homicides-mx-data/data/gpo_interinstitucional/df_homicides_state_year_sspc_gpointerinstitucional.RData")
df_state_y_gpo <- df_homicides_state_year


# 01.2 Open sorces (SSCP) ------------------------------------------------------
# By day 
load("~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_state_daily_sspc_fuentesabiertas.Rdata")

# By week 
load("~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_state_weekly_sspc_fuentesabiertas.Rdata")

# By month 
load("~/GitHub/homicides-mx-data/data/fuentes_abiertas/df_homicides_state_monthly_sspc_fuentesabiertas.Rdata")
df_state_m_os <-  df_homicides_state_monthly_sspc_fuentesabiertas

# 01.3 INEGI (SSCP) ------------------------------------------------------------




# 02. Descriptive statistics from the Interinstitutional Group data ------------

# 02.1 National level homicides (gpo. interinst.) ------------------------------

# 02.1.1 National homicides by month 
# Generate 
df_nation_m_gpo <- df_state_m_gpo                               %>% 
        filter(state == "National")                             %>% 
        mutate(month = as.factor(month))                        %>%  
        mutate(month = fct_relevel(month, c("January", "February", "March", 
                "April","May", "June", "July", "August", "September", "October",
                "November", "December")))                       %>% 
        arrange(year, month)

# Labels for final table 
df_nation_names_gpo <- df_nation_m_gpo                          %>% 
        ungroup() %>% 
        select(year, month, cases, mort_rate)                   %>% 
        rename("Year" = year, "Month" = month, "Homicides" = cases,       
                "Monthly homicide rate" = mort_rate)

# Render table
pander(df_nation_names_gpo)


# 02.1.2 National homicides by year (descriptive statistics with total homicides)
df_nation_m_gpo <- slice(df_nation_m_gpo, 1:16) # Drop August 2020
df_nation_stats_gpo <- df_nation_m_gpo                          %>%
        group_by(year)                                          %>% 
        summarise("Lowest number of homicides in a single month" = min(cases), 
                 "Highest number of homicides in a single month" = max(cases), 
                "Average number of homicides" = mean(cases), 
                "Std. Deviation" = sd(cases))                   %>% 
        rename("Year" = year) 

# Render table
pander(df_nation_stats_gpo, split.table = Inf)

# 02.1.3 National homicides by year (descriptive statistics with homicide rate)
df_nation_stats_rate_gpo <- df_nation_m_gpo                     %>%
        group_by(year)                                          %>% 
        summarise("Lowest homicide rate in a single month" = min(mort_rate),
                "Highest homicide rate in a single month" = max(mort_rate), 
                "Average mortality rate" =       mean(mort_rate), 
                "Std. Deviation" = sd(mort_rate))               %>% 
        rename("Year" = year)

# Render of table
pander(df_nation_stats_rate_gpo, split.table = Inf)

# 02.2 State level homicides (gpo. interinst.)----------------------------------
# 02.2.1 State level homicides by year 
df_st_y_gpo <- df_state_y_gpo                                   %>% 
        select(state, year, cases, mort_rate)                   %>% 
        arrange(state, year)                                    %>% 
        rename("State" = state, 
                "Year" = year, 
                "Homicides" = cases, 
                "Homicide rate" = mort_rate)  

# Render table 
pander(df_st_y_gpo, split.table = Inf)

# Change headers (make 2019 and 2020 headers)
tab       <- tableGrob(d = df_st_y_gpo, rows = NULL)
header    <- tableGrob(d = df_st_y_gpo[1, 3:4], rows = NULL, cols=c("2019", "2020"))
jn        <- gtable_combine(header[1,], tab, along=2)
jn$widths <- rep(max(jn$widths), length(jn$widths))

# change the relevant rows of gtable
jn$layout[1:4 , c("l", "r")] <- list(c(1, 3), c(2, 4))
grid.newpage()
grid.draw(jn)


# 02.2.2 State homicides by year (descriptive statistics with total homicides)
df_states_stats_cases_gpo <- df_state_m_gpo                     %>% 
        group_by(state, year)                                   %>% 
        summarise("Lowest homicide rate in a single month" = min(mort_rate), 
                "Highest homicide rate in a single month" = max(mort_rate), 
                "Average mortality rate" = mean(mort_rate), 
                "Std. Deviation" = sd(mort_rate))               %>% 
        rename("State" = state, "Year" = year)

# Render table 
pander(df_states_stats_cases_gpo, split.table = Inf)


# 03. Descriptive statistics from the Open Sources data ------------------------

# 03.1 National level homicides (open sources) ---------------------------------
df_nation_y_os <- df_state_m_os                                         %>% 
        group_by(year)                                                  %>% 
        summarise("Total homicides" = sum(homicidios, na.rm = T), 
                  "Male" = sum(hombre, na.rm = T), 
                  "Female" = sum(mujer, na.rm = T), 
                  "Non identified" = sum(no_identificado, na.rm = T))   %>% 
        rename("Year" = year)

# Render table 
pander(df_nation_y_os)


# 03.2 State level homicides (open sources) ------------------------------------
# 03.2.1 State homicides by year (disaggregated by gender)
df_state_year_os <- df_state_m_os                                       %>% 
        group_by(state, year)                                           %>% 
        summarise("Total homicides" = sum(homicidios, na.rm = T), 
                "Male" = sum(hombre, na.rm = T), 
                "Female" = sum(mujer, na.rm = T), 
                "Non identified" = sum(no_identificado, na.rm = T))     %>% 
        rename("State" = state, "Year" = year)

# Render table 
pander(df_state_year_os, split.table = Inf)

# 03.2.2 State homicides by year (descriptive statistics with total homicides)
df_state_year_stats_os <- df_state_m_os                                 %>% 
        group_by(state, year)                                           %>% 
        group_by(year)                                                  %>% 
        summarise("Lowest number of homicides in a single month" = min(homicidios), 
                "Highest number of homicides in a single month" = max(homicidios), 
                "Average number of homicides" =  mean(homicidios), 
                "Std. Deviation" = sd(homicidios))                      %>% 
        rename("Year" = year)

# Render table 
pander(df_state_year_stats, split.table = Inf)

# 04. Comparison between SSCP sources ------------------------------------------
df_gpo_state_m <- df_state_m_gpo                                        %>% 
        ungroup()                                                       %>% 
        select(state, year, month, cases, population)                   %>% 
        mutate(month = as.character(month))                             %>% 
        rename(homicides_gpo = cases)

df_gpo_national_m <- df_gpo_state_m                                     %>% 
        filter(state == "National")                                     %>% 
        mutate(month_n = case_when(month == "January"  ~ 1, 
                month == "February" ~ 2, 
                month == "March" ~ 3, 
                month == "April" ~ 4, 
                month == "May" ~ 5,
                month == "June" ~ 6,
                month == "July" ~  7,
                month == "August" ~ 8, 
                month == "September" ~ 9,
                month == "October" ~ 10,
                month == "November" ~  11,
                month == "December" ~  12))                             %>% 
        mutate(month_n = as.character(month_n))

df_gpo_national_y <- df_gpo_national_m                                  %>% 
        group_by(year, state)                                           %>% 
        summarise("homicides_gpo" = sum(homicides_gpo, na.rm = T))

# Open sources (newspapers)
df_fuentes_state_m <- df_homicides_state_monthly_sspc_fuentesabiertas   %>%
        ungroup()                                                       %>% 
        mutate(month = as.character(month))                             %>% 
        select(state, year, month, homicidios)                          %>% 
        rename("homicides_fuentes" = homicidios, "month_n" = month)

df_fuentes_national_m <- df_fuentes_state_m                             %>% 
        group_by(year, month_n)                                         %>% 
        summarise("homicides_fuentes" = sum(homicides_fuentes, na.rm = T))

df_fuentes_national_y <- df_fuentes_state_m                             %>% 
        group_by(year)                                                  %>% 
        summarise("homicides_fuentes" = sum(homicides_fuentes, na.rm = T))

# Combined sources
df_combined_sources_national_y <- df_gpo_national_y                     %>%
        left_join(df_fuentes_national_y, by = "year")                   %>% 
        select(year, homicides_gpo, homicides_fuentes)                  %>% 
        rename("Year" = year, 
                "Homicides reported by Gpo. Inter." = homicides_gpo, 
                "Homicides reported by newspapers" = homicides_fuentes)

df_combined_sources_national_m <- df_gpo_national_m                     %>% 
        left_join(df_fuentes_national_m, by = c("year", "month_n"))     %>% 
        arrange(year, month_n)                                          %>% 
        select(year, month, homicides_gpo, homicides_fuentes)           %>% 
        rename("Year" = year, 
                "Month" = month, 
                "Homicides reported by Gpo. Inter." = homicides_gpo, 
                "Homicides reported by newspapers" = homicides_fuentes)

df_combined_sources_national_m  %>% 
        filter(is.na(Month) == F) 

# Render tables 
pander(df_combined_sources_national_y)
pander(df_combined_sources_national_m)



# 05. Comparison with INEGI data -----------------------------------------------




#### EXAMPLE OF TABLES WITH GRIDEXTRA ####
library(grid)           # For table generation
library(gridExtra)

# example data & header row
tab <- tableGrob(mtcars[1:3, 1:4], rows=NULL)
header <- tableGrob(mtcars[1, 1:2], rows=NULL, cols=c("head1", "head2")) 
jn <- gtable_combine(header[1,], tab, along=2)
jn$widths <- rep(max(jn$widths), length(jn$widths)) # make column widths equal

#grid.newpage()
#grid.draw(jn) # see what it looks like before altering gtable
# change the relevant rows of gtable
jn$layout[1:4 , c("l", "r")] <- list(c(1, 3), c(2, 4))
grid.newpage()
grid.draw(jn)

