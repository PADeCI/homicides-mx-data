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
library(tibble)
library(ggplot2)

# Clean the workspace
rm(list = ls()) 

# Create table theme (tth) for gridExtra, tables 
tth <- ttheme_default()

# 01. Load data ----------------------------------------------------------------
# 01.1 Interinstitutional group (SSCP) -----------------------------------------
# By day 
load("~/GitHub/homicides-mx-data/data/gpo_interinstitucional/df_homicides_state_daily_sspc_gpointerinstitucional.RData")
df_state_d_gpo <- df_homicides_state_daily_sspc_gpointerinstitucional # Rename

# By month
load("~/GitHub/homicides-mx-data/data/gpo_interinstitucional/df_homicides_state_monthly_sspc_gpointerinstitucional.Rdata")
df_state_m_gpo <- df_homicides_state_monthly_sspc_gpointerinstitucional # Rename

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
df_nation_m_gpo <- df_state_m_gpo                                       %>% 
        filter(state == "National")                                     %>% 
        arrange(year, month)                                            %>% 
        filter(is.na(homicidios)==F)

# Labels for final table 
df_nation_names_gpo <- df_nation_m_gpo                                  %>%
        ungroup()                                                       %>%
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
        arrange(year, month_n) %>% 
        select(year, month, homicidios, mort_rate)                      %>% 
        rename("Year" = year, "Month" = month, "Homicides" = homicidios,       
                "Monthly homicide rate" = mort_rate)

# Convert tables into grid graphics (gridExtra's format)
tab_nation_gpo <- tableGrob(df_nation_names_gpo, 
        rows = NULL, 
        theme = tth)

t <- grid.arrange(tab_nation_gpo)               # Render table
ggsave("figs/tab_nation_year_totals_gpo.jpg", 
        plot = t, width = 5, height = 5)      # Save table




# 02.1.2 National homicides by year (descriptive statistics with total homicides)
df_nation_stats_gpo_short <- df_nation_m_gpo                    %>%
        group_by(year)                                          %>% 
        summarise(min_h = min(homicidios), 
                  max_h = max(homicidios), 
                  mean_h = round(mean(homicidios), 1), 
                  sd_h = round(sd(homicidios), 1))                   

# Get months with highest and lowest homicides
df_2019 <- df_nation_m_gpo %>% filter(year==2019) %>% mutate(month = as.character(month))
df_2020 <- df_nation_m_gpo %>% filter(year==2020) %>% mutate(month = as.character(month))

# Get months with lowest and highest homicides in a year
low_m_19 <- df_2019$month[df_2019$homicidios == min(df_2019$homicidios)]
low_m_20 <- df_2020$month[df_2020$homicidios == min(df_2020$homicidios)]
high_m_19 <- df_2019$month[df_2019$homicidios == max(df_2019$homicidios)]
high_m_20 <- df_2020$month[df_2020$homicidios == max(df_2020$homicidios)]

# Build vectors
year  <- c(2019, 2020)
low_m   <- c(low_m_19, low_m_20)
high_m <- c(high_m_19, high_m_20)

# Create data frame for months
df_months <- data.frame(year, low_m, high_m) 

# Create final data set for table
df_nation_stats_gpo <- df_nation_stats_gpo_short              %>% 
        left_join(df_months, by = "year")               %>% 
        select(year, low_m, min_h, high_m, max_h, mean_h, sd_h) %>%
        rename("Year" = year,
                "Month with\nlowest\nhomicides" = low_m, 
                "Lowest number\nof homicides in\na single month" =  min_h, 
                "Month with\nhighest\nhomicides" = high_m,
                "Highest number\nof homicides in\na single month" = max_h, 
                "Average\nnumber of\nhomicides" = mean_h, 
                "Standard\nDeviation" = sd_h)

# Render table
tab_nation_stats_gpo <- tableGrob(df_nation_stats_gpo, rows = NULL, theme = tth)
t <- grid.arrange(tab_nation_stats_gpo) # Render table
ggsave("figs/tab_nation_stats_gpo.jpg", plot = t, width = 8, height = 1.5) # Save table

# 02.1.3 National homicides by year (descriptive statistics with homicide rate)
df_nation_stats_rate_gpo_short <- df_nation_m_gpo                     %>%
        group_by(year)                                          %>% 
        summarise(min_mr = min(mort_rate),
                  max_mr = max(mort_rate), 
                  mean_mr = round(mean(mort_rate), 1), 
                  sd_mr = round(sd(mort_rate), 1))      

# Get months with lowest and highest mortality rate in a year
low_m_19 <- df_2019$month[df_2019$mort_rate == min(df_2019$mort_rate)]
low_m_20 <- df_2020$month[df_2020$mort_rate == min(df_2020$mort_rate)]
high_m_19 <- df_2019$month[df_2019$mort_rate == max(df_2019$mort_rate)]
high_m_20 <- df_2020$month[df_2020$mort_rate == max(df_2020$mort_rate)]

# Build vectors
year  <- c(2019, 2020)
low_m   <- c(low_m_19, low_m_20)
high_m <- c(high_m_19, high_m_20)

# Create data frame for months
df_months <- data.frame(year, low_m, high_m) 

# Create final data set for table
df_nation_stats_rate_gpo <- df_nation_stats_rate_gpo_short %>% 
        left_join(df_months, by = "year") %>% 
        select(year, low_m, min_mr, high_m, max_mr, mean_mr, sd_mr) %>% 
        rename("Year" = year, 
                "Month with\nlowest\nmortality rate" = low_m, 
                "Lowest homicide\nrate in a single\nmonth" = min_mr, 
                "Month with\nhighest\nmortality rate" = high_m,
                "Highest homicide\nrate in a single\nmonth" = max_mr, 
                "Average\nmortality rate" = mean_mr, 
                "Standard\nDeviation" = sd_mr)


# Render of table
tab_nation_stats_rate_gpo <- tableGrob(df_nation_stats_rate_gpo, rows = NULL, theme =tth)
t <- grid.arrange(tab_nation_stats_rate_gpo)
ggsave("figs/tab_nation_stats_rate_gpo.jpg", plot = t, width = 9, height = 1.5)

# 02.2 State level homicides (gpo. interinst.)----------------------------------
# 02.2.1 State level homicides by year 
df_st_y_gpo <- df_state_y_gpo                                   %>% 
        select(state, year, cases, mort_rate)                   %>% 
        arrange(state, year)                                    

 
# 02.2.1.1 Format table for report ---------------------------------------------

# Filter by year and change columns' headers
df_st_y_gpo_2019 <- df_state_y_gpo                              %>% 
        filter(year == 2019)                                    %>% 
        select(state, cases, mort_rate)                         %>% 
        column_to_rownames("state")                             %>% 
        rename("Homicides" = cases, 
               "Homicide rate" = mort_rate) 


df_st_y_gpo_2020 <- df_state_y_gpo                              %>% 
        filter(year == 2020)                                    %>% 
        select(state, cases, mort_rate)                         %>% 
        column_to_rownames("state")                             %>% 
        rename("Homicides" = cases, 
                "Homicide rate" = mort_rate) 


# Convert tables into grid graphics (gridExtra's format)
tab_2019 <- tableGrob(df_st_y_gpo_2019, 
        rows = rownames(df_st_y_gpo_2019), 
        theme = tth)

tab_2020 <- tableGrob(df_st_y_gpo_2020, 
        rows = NULL, 
        theme = tth) # we don't want to repeat the states names

# Create headers
header_2019 <- tableGrob(df_st_y_gpo_2019[1, 1:2], 
        rows = NULL, 
        cols = c("2019", "2019"), 
        theme = tth)

header_2020 <- tableGrob(df_st_y_gpo_2019[1, 1:2], 
        rows = NULL, 
        cols = c("2020", "2020"), 
        theme = tth)

# Paste headers tables 
tab_head_2019 <- gtable_combine(header_2019[1,], tab_2019, along = 2)
tab_head_2020 <- gtable_combine(header_2020[1,], tab_2020, along = 2)

# Make columns widths equal 
tab_head_2019$widths <- rep(max(tab_head_2019$widths), length(tab_head_2019$widths))
tab_head_2020$widths <- rep(max(tab_head_2020$widths), length(tab_head_2020$widths))

# Correct positioning of the headers
tab_head_2019$layout[1:4, c("l", "r")] <- list(c(2), c(3))
tab_head_2020$layout[1:4, c("l", "r")] <- list(c(1), c(2)) # Number are different since we don't have a rowname column

# Combine table
tab_head_2019_2020 <- gtable_cbind(tab_head_2019, tab_head_2020)

# Render table
t <- grid.arrange(tab_head_2019_2020)

# Save table
ggsave("figs/tab_state_year_totals_os.jpg", plot = t, width = 8, height = 11)



# 02.2.2 State homicides by year (descriptive statistics with total homicides)
df_states_stats_cases_gpo <- df_state_m_gpo                     %>% 
        group_by(state, year)                                   %>% 
        summarise("Lowest homicide\nrate in a single\nmonth" = min(mort_rate),
                  "Highest homicide\nrate in a single\nmonth" = max(mort_rate), 
                  "Average\nmortality rate" = round(mean(mort_rate), 1), 
                  "Standard\nDeviation" = round(sd(mort_rate), 1)) %>% 
        rename("State" = state, "Year" = year)

# 02.2.2.1 Format table for report ---------------------------------------------

# Filter by year and change columns' headers
df_states_stats_2019 <- df_states_stats_cases_gpo               %>% 
        filter(Year == 2019)                                    %>% 
        column_to_rownames("State")                             %>% 
        select(-Year)

df_states_stats_2020 <- df_states_stats_cases_gpo               %>% 
        filter(Year == 2020)                                    %>% 
        column_to_rownames("State")                             %>% 
        select(-Year)


# Convert tables into grid graphics (gridExtra's format)
tab_2019 <- tableGrob(df_states_stats_2019, 
        rows = rownames(df_states_stats_2019), 
        theme = tth)

tab_2020 <- tableGrob(df_states_stats_2020, 
        rows = NULL, 
        theme = tth) # we don't want to repeat the states names

# Create headers
header_2019 <- tableGrob(df_states_stats_2019[1, 1:2], 
        rows = NULL, 
        cols = c("2019", "2019"), 
        theme = tth)

header_2020 <- tableGrob(df_states_stats_2020[1, 1:2], 
        rows = NULL, 
        cols = c("2020", "2020"), 
        theme = tth)

# Paste headers tables 
tab_head_2019 <- gtable_combine(header_2019[1,], tab_2019, along = 2)
tab_head_2020 <- gtable_combine(header_2020[1,], tab_2020, along = 2)

# Make columns widths equal 
tab_head_2019$widths <- rep(max(tab_head_2019$widths), length(tab_head_2019$widths))
tab_head_2020$widths <- rep(max(tab_head_2020$widths), length(tab_head_2020$widths))

# Correct positioning of the headers
tab_head_2019$layout[1:4, c("l", "r")] <- list(c(2), c(5))
tab_head_2020$layout[1:4, c("l", "r")] <- list(c(1), c(4)) # Number are different since we don't have a rowname column

# Combine table
tab_head_2019_2020 <- gtable_cbind(tab_head_2019, tab_head_2020)


# Render table 
t <- grid.arrange(tab_head_2019_2020)        
ggsave("figs/tab_state_year_stats_cases_gpo.jpg", plot = t, width = 14, height = 12) 


# 03. Descriptive statistics from the Open Sources data ------------------------

# 03.1 National level homicides (open sources) ---------------------------------
df_nation_y_os <- df_state_m_os                                         %>% 
        group_by(year)                                                  %>% 
        summarise("Total\nhomicides" = sum(homicidios, na.rm = T), 
                  "Male" = sum(hombre, na.rm = T), 
                  "Female" = sum(mujer, na.rm = T), 
                  "Non\nidentified" = sum(no_identificado, na.rm = T))   %>% 
        rename("Year" = year)

# Render table 
tab_nation_y_os <- tableGrob(df_nation_y_os, row = NULL)
t <- grid.arrange(tab_nation_y_os)
ggsave("figs/tab_nation_year_os.jpg", plot = t, width = 4, height = 1.5)

# 03.2 State level homicides (open sources) ------------------------------------
# 03.2.1 State homicides by year (disaggregated by gender)
df_state_year_os <- df_state_m_os                                       %>% 
        group_by(state, year)                                           %>% 
        summarise("Total\nhomicides" = sum(homicidios, na.rm = T), 
                "Male" = sum(hombre, na.rm = T), 
                "Female" = sum(mujer, na.rm = T), 
                "No identificado" = sum(no_identificado, na.rm = T))     %>% 
        rename("State" = state, "Year" = year)

# 02.2.2.1 Format table for report ---------------------------------------------

# Filter by year and change columns' headers
df_state_year_2019 <- df_state_year_os %>% 
        filter(Year == 2019) %>% 
        select(-Year) %>% 
        column_to_rownames("State")
        

df_state_year_2020 <- df_state_year_os %>% 
        filter(Year == 2020) %>% 
        select(-Year) %>% 
        column_to_rownames("State")
        

# Convert tables into grid graphics (gridExtra's format)
tab_2019 <- tableGrob(df_state_year_2019, 
        rows = rownames(df_state_year_2019), 
        theme = tth)

tab_2020 <- tableGrob(df_state_year_2020, 
        rows = NULL, 
        theme = tth) # we don't want to repeat the states names

# Create headers
header_2019 <- tableGrob(df_state_year_2019[1, 1:2], 
        rows = NULL, 
        cols = c("2019", "2019"), 
        theme = tth)

header_2020 <- tableGrob(df_state_year_2020[1, 1:2], 
        rows = NULL, 
        cols = c("2020", "2020"), 
        theme = tth)

# Paste headers tables 
tab_head_2019 <- gtable_combine(header_2019[1,], tab_2019, along = 2)
tab_head_2020 <- gtable_combine(header_2020[1,], tab_2020, along = 2)

# Make columns widths equal 
tab_head_2019$widths <- rep(max(tab_head_2019$widths), length(tab_head_2019$widths))
tab_head_2020$widths <- rep(max(tab_head_2020$widths), length(tab_head_2020$widths))

# Correct positioning of the headers
tab_head_2019$layout[1:4, c("l", "r")] <- list(c(2), c(5))
tab_head_2020$layout[1:4, c("l", "r")] <- list(c(1), c(4)) # Number are different since we don't have a rowname column

# Combine table
tab_head_2019_2020 <- gtable_cbind(tab_head_2019, tab_head_2020)


# Render table 
t <- grid.arrange(tab_head_2019_2020)
ggsave("figs/tab_state_year_os.jpg", plot = t, width = 15, height = 11)



# 03.2.2 State homicides by year (descriptive statistics with total homicides)
df_state_year_stats_os_short <- df_state_m_os                                 %>% 
        group_by(state, year)                                           %>% 
        group_by(year)                                                  %>% 
        summarise(min_h = min(homicidios), 
                  max_h = max(homicidios), 
                  mean_h =  round(mean(homicidios), 1), 
                  sd_h = round(sd(homicidios), 1))              

# Get df for each year
df_2019 <- df_state_m_os %>% filter(year==2019) %>% mutate(month = as.character(month))
df_2020 <- df_state_m_os %>% filter(year==2020) %>% mutate(month = as.character(month))

# Get months with lowest and highest homicides in a state 
low_m_19 <- df_2019$month[df_2019$homicidios == min(df_2019$homicidios)]
low_m_20 <- df_2020$month[df_2020$homicidios == min(df_2020$homicidios)]
high_m_19 <- df_2019$month[df_2019$homicidios == max(df_2019$homicidios)]
high_m_20 <- df_2020$month[df_2020$homicidios == max(df_2020$homicidios)]

# Get state with lowest and highest homicides in a single month 
low_s_19 <- df_2019$state[df_2019$homicidios == min(df_2019$homicidios)]
low_s_20 <- df_2020$state[df_2020$homicidios == min(df_2020$homicidios)]
high_s_19 <- df_2019$state[df_2019$homicidios == max(df_2019$homicidios)]
high_s_20 <- df_2020$state[df_2020$homicidios == max(df_2020$homicidios)]

# Build vectors
year    <- c(2019, 2020)
low_m   <- c(low_m_19, low_m_20)
high_m  <- c(high_m_19, high_m_20)
low_s   <- c(low_s_19, low_s_20)
high_s  <- c(high_s_19, high_s_20)

# Create data frame for months
df_months_states <- data.frame(year, low_s, low_m, high_s, high_m) 

# Create final data set for table
df_state_year_stats_os <- df_state_year_stats_os_short %>% 
        left_join(df_months_states, by = "year") %>% 
        select(year, low_s, low_m, min_h, high_s, high_m, max_h, mean_h, sd_h) %>% 
        rename("Year" = year,
                "State with\nlowest\nhomicides\nin a month" = low_s, 
                "Month with\nlowest\nhomicides" = low_m, 
                "Lowest number\nof homicides in\na single month" =  min_h, 
                "State with\nhighest\nhomicides\nin a month" = high_s,
                "Month with\nhighest\nhomicides" = high_m,
                "Highest number\nof homicides in\na single month" = max_h, 
                "Average\nnumber of\nhomicides" = mean_h, 
                "Standard\nDeviation" = sd_h)

# Render table 
tab_state_year_stats_os <- tableGrob(df_state_year_stats_os, row = NULL)
t <- grid.arrange(tab_state_year_stats_os)
ggsave("figs/tab_state_year_stats_os.jpg", plot = t, width = 11, height = 4)

# 04. Comparison between SSCP sources ------------------------------------------
df_gpo_state_m <- df_state_m_gpo                                        %>% 
        ungroup()                                                       %>% 
        mutate(cases = homicidios)                                      %>% 
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
        mutate(month_n = as.numeric(month_n))

df_gpo_national_y <- df_gpo_national_m                                  %>% 
        group_by(year, state)                                           %>% 
        summarise("homicides_gpo" = sum(homicides_gpo, na.rm = T))

# Open sources (newspapers)
df_fuentes_state_m <- df_homicides_state_monthly_sspc_fuentesabiertas   %>%
        ungroup()                                                       %>% 
        mutate(month = as.character(month))                             %>% 
        select(state, year, month, homicidios)                          %>%
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
                                   month == "December" ~  12)) %>% 
        mutate(month_n = as.numeric(month_n)) %>% 
        rename("homicides_fuentes" = homicidios)

df_fuentes_national_m <- df_fuentes_state_m                             %>% 
        group_by(year, month, month_n)                                  %>% 
        summarise("homicides_fuentes" = sum(homicides_fuentes, na.rm = T))

df_fuentes_national_y <- df_fuentes_state_m                             %>% 
        group_by(year)                                                  %>% 
        summarise("homicides_fuentes" = sum(homicides_fuentes, na.rm = T))

# Combined sources
df_combined_sources_national_y <- df_gpo_national_y                     %>%
        left_join(df_fuentes_national_y, by = "year")                   %>% 
        select(year, homicides_gpo, homicides_fuentes)                  %>% 
        rename("Year" = year, 
                "Homicides reported\nby Inter. Group" = homicides_gpo, 
                "Homicides reported\nby newspapers" = homicides_fuentes)

df_combined_sources_national_m <- df_gpo_national_m                     %>% 
        left_join(df_fuentes_national_m, by = c("year", "month_n", "month")) %>% 
        arrange(year, month_n)                                          %>% 
        select(year, month, homicides_gpo, homicides_fuentes)           %>%
        mutate(diff = homicides_gpo - homicides_fuentes)                %>% 
        rename("Year" = year, 
                "Month" = month, 
                "Homicides reported\nby Inter. Group" = homicides_gpo, 
                "Homicides reported\nby newspapers" = homicides_fuentes, 
                "Difference between\nsources" = diff)

df_combined_sources_national_m  %>% 
        filter(is.na(Month) == F) 

# Render tables with pander
pander(df_combined_sources_national_y)
pander(df_combined_sources_national_m)

# Render table 
tab_nation_year_combined <- tableGrob(df_combined_sources_national_y, rows = NULL)
t <- grid.arrange(tab_nation_year_combined)
ggsave("figs/tab_nation_year_combined.jpg", plot = t, width = 4.5, height = 1)

# Render table
tab_nation_month_combined <- tableGrob(df_combined_sources_national_m, rows = NULL)
t <- grid.arrange(tab_nation_month_combined)
ggsave("figs/tab_nation_month_combined.jpg", plot = t, width = 7, height = 6)


# 05. Comparison with INEGI data -----------------------------------------------


