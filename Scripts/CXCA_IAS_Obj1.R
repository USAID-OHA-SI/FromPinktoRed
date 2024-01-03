# PROJECT:  FromPinktoRed
# AUTHOR:   J. Stephens | USAID
# PURPOSE:  Descriptive statistics of Cervical Cancer for IAS
#           Objective 1
# LICENSE:  MIT
# DATE:     2023-12-08
# UPDATED:  2023-12-21


# CALCULATED VARIABLES DATAFRAMES-----------------------------------------------

# • Cervical Cancer Screening of ART Treatment
# % CXCA_SCRN Cumulative / TX_CURR Cumulative

# collapse_scrn_txcurr_tbl  <- function(df_clean, ...) {

scrn_txcurr_indics <- c("CXCA_SCRN", "TX_CURR")

# scrn_txcurr_df <-  df_clean %>% 
#   dplyr::filter(indicator %in% scrn_txcurr_indics) %>% 
#   dplyr::select(country, indicator, fiscal_year,ageasentered, cumulative) %>% 
#   dplyr::group_by(country, indicator, fiscal_year,ageasentered) %>% 
#   dplyr::summarise(cumulative= sum(cumulative, na.rm = TRUE))

scrn_txcurr_df <-  df_clean %>% 
  dplyr::filter(indicator %in% scrn_txcurr_indics) %>% 
  dplyr::select(country, indicator, fiscal_year,ageasentered, cumulative) %>%
  dplyr::group_by(across(where(~!is.double(.)))) %>% 
  dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>%
  dplyr::ungroup() 


#   return(scrn_txcurr_df)
# }
# view(scrn_txcurr_df)

# • Cervical Cancer Screening Achievement
# % CXCA_SCRN Cumulative / Targets

# collapse_scrn_ach_tbl  <- function(df_other, ...) {
#   
#   scrn_ach_indics <- c("CXCA_SCRN")
#   
#   scrn_ach_df <-  df_other %>% 
#     dplyr::filter(indicator %in% scrn_ach_indics,
#                   standardizeddisaggregate %in%  c("Age/Sex/HIVStatus/ScreenResult/ScreenVisitType", "Age/Sex/HIVStatus"),
#                   # otherdisaggregate %in% c("Negative", "Positive", "Suspected"),
#     ) %>% 
#     dplyr::group_by(across()) %>% 
#     dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
#     dplyr::ungroup() 
#   
#   return(scrn_ach_df)
# }




# ANALYTICS --------------------------------------------------------------------------

####### OVERALL - no fy ###############################################################

scrn_txcurr_df_fy <- scrn_txcurr_df %>%
  group_by(indicator) %>%
  select(-fiscal_year) %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 

view(scrn_txcurr_df_fy)

#pivot wider to create calculation
scrn_txcurr_df_fy_wide <- scrn_txcurr_df_fy %>% 
  group_by(indicator) %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(values_from = cumulative, names_from = indicator) %>% 
  dplyr::mutate(scrn_ach_curr=(CXCA_SCRN/TX_CURR)*100) 

view(scrn_txcurr_df_fy_wide) 


################################################################################
################################### COUNTRY

# compare percentages for different countries at a single point in time

scrn_txcurr_df_ou <- scrn_txcurr_df %>%
  group_by(country, indicator) %>%
  select(-fiscal_year) %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 

view(scrn_txcurr_df_ou)

#pivot wider to create calculation
scrn_txcurr_df_ou_wide <- scrn_txcurr_df_ou %>% 
  group_by(country, indicator) %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(values_from = cumulative, names_from = indicator) %>% 
  dplyr::mutate(scrn_ach_curr=(CXCA_SCRN/TX_CURR)*100) 

view(scrn_txcurr_df_ou_wide) 


################################################################################
################################### AGE

#### by ages
scrn_txcurr_viz_age <- scrn_txcurr_df %>%
  select(-fiscal_year) %>% 
  group_by(indicator, ageasentered) %>%
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 

view(scrn_txcurr_viz_age)

scrn_txcurr_viz_wide_age <- scrn_txcurr_viz_age %>% 
  pivot_wider(values_from = cumulative, names_from = indicator) %>% 
  dplyr::mutate(scrn_ach_curr=(CXCA_SCRN/TX_CURR)*100) 

view(scrn_txcurr_viz_wide_age) 


# scrn_txcurr_viz_wide <- scrn_txcurr_viz %>% 
#   pivot_wider(values_from = cumulative, names_from = indicator) %>% 
#   dplyr::mutate(scrn_ach_curr=CXCA_SCRN/TX_CURR)  %>% 
#   group_by(ageasentered) %>% 
#   dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 
# 
# view(scrn_txcurr_viz_wide) 






####### OVERALL - by fiscal year# #####################################################################

scrn_txcurr_df_fy <- scrn_txcurr_df %>%
  group_by(indicator, fiscal_year) %>%
  dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 

view(scrn_txcurr_df_fy)

#pivot wider to create calculation
scrn_txcurr_df_fy_wide <- scrn_txcurr_df_fy %>% 
  group_by(indicator, fiscal_year) %>% 
  dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(values_from = cumulative, names_from = indicator) %>% 
  dplyr::mutate(scrn_ach_curr=(CXCA_SCRN/TX_CURR)*100) 


view(scrn_txcurr_df_fy_wide) 

# Run the regression for fiscal year
# lm_model <- lm(scrn_ach_curr ~ fiscal_year, data = scrn_txcurr_df_fy_wide)
# summary(lm_model)



################################################################################
################################### COUNTRY

# compare percentages for different countries at a single point in time

scrn_txcurr_df_ou <- scrn_txcurr_df %>%
  group_by(country,fiscal_year, indicator) %>%
  dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 

view(scrn_txcurr_df_ou)

#pivot wider to create calculation
scrn_txcurr_df_ou_wide <- scrn_txcurr_df_ou %>% 
  group_by(country, indicator, fiscal_year) %>% 
  dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(values_from = cumulative, names_from = indicator) %>% 
  dplyr::mutate(scrn_ach_curr=(CXCA_SCRN/TX_CURR)*100) 


view(scrn_txcurr_df_ou_wide) 

# # Perform ANOVA
# result_anova <-   aov(scrn_ach_curr~country , scrn_txcurr_df_ou_wide)
# summary(result_anova)
# 
# 
# # Perform Kruskal-Wallis test (non-parametric)
# result_kw <- kruskal.test(list(country_A, country_B, country_C))
# print(result_kw$p.value)

# result_kw <- kruskal.test(scrn_ach_curr ~ country, scrn_txcurr_df_ou_wide)
# print(result_kw)
# 


################################################################################
################################### AGE

#### by ages
scrn_txcurr_viz_age <- scrn_txcurr_df %>%
  group_by(indicator,fiscal_year, ageasentered) %>%
  dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 

view(scrn_txcurr_viz_age)

scrn_txcurr_viz_wide_age <- scrn_txcurr_viz_age %>% 
  pivot_wider(values_from = cumulative, names_from = indicator) %>% 
  dplyr::mutate(scrn_ach_curr=(CXCA_SCRN/TX_CURR)*100) 

view(scrn_txcurr_viz_wide_age) 


# scrn_txcurr_viz_wide <- scrn_txcurr_viz %>% 
#   pivot_wider(values_from = cumulative, names_from = indicator) %>% 
#   dplyr::mutate(scrn_ach_curr=CXCA_SCRN/TX_CURR)  %>% 
#   group_by(ageasentered) %>% 
#   dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 
# 
# view(scrn_txcurr_viz_wide) 
# 
# lm_model_age <- lm(scrn_ach_curr ~ ageasentered, data = scrn_txcurr_viz_wide_age)
# 
# summary(lm_model_age)







# VIZ --------------------------------------------------------------------------

# scrn_txcurr_viz_wide %>%
#   ggplot(aes(fiscal_year, scrn_ach_curr)) +
#   geom_col()








