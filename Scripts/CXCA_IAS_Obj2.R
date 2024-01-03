# PROJECT:  FromPinktoRed
# AUTHOR:   J. Stephens | USAID
# PURPOSE:  Descriptive statistics of Cervical Cancer for IAS
#           Objective 2
# LICENSE:  MIT
# DATE:     2023-12-08
# UPDATED:  202-12-21


# CALCULATED VARIABLES DATAFRAMES-----------------------------------------------

# • Cervical Cancer % Positive
# % CXCA_SCRN_POS results / CXCA_SCRN results
 names(df_other)
# collapse_scrn_pos_tbl  <- function(df_other, ...) {
  
  scrn_pos_indics <- c("CXCA_SCRN", "CXCA_SCRN_POS")
  
  scrn_pos_df <-  df_other %>% 
    dplyr::select(-cumulative, -cxca_tx, -cxca_status) %>% 
    dplyr::filter(indicator %in% scrn_pos_indics,
                  standardizeddisaggregate %in%  c("Age/Sex/HIVStatus/ScreenResult/ScreenVisitType")
                  )  %>% 
    dplyr::group_by(across(where(~!is.double(.)))) %>% 
    dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup() 
  
   view(scrn_pos_df)
  
#   return(scrn_pos_df)
# }

  # unique_combinations <- scrn_pos_df %>%
  #   filter(indicator=="CXCA_SCRN") %>%
  #   distinct(indicator, standardizeddisaggregate, ageasentered)
  # print(unique_combinations)
  
# RESHAPE  --------------------------------------------------------------------------



# df_semi<- df %>% 
#   reshape_msd(direction="semi_wide")
# 
# view(df_semi)
# 
df_long<- scrn_pos_df %>%
  pivot_longer(
    cols = qtr2:qtr4,
    names_to = c("qtr"),
    values_to = "results"
  )

  view(df_long)
  
  
  # ANALYTICS --------------------------------------------------------------------------
  
  ####### OVERALL - no fy ###############################################################
  
  scrn_pos_df_global <- df_long %>%
    group_by(indicator) %>%
    select(-fiscal_year) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 
  
  view(scrn_pos_df_global)
  
  #pivot wider to create calculation
  scrn_pos_df_global_wide <- scrn_pos_df_global %>% 
    pivot_wider(values_from = results, names_from = indicator) %>% 
    dplyr::mutate(scrn_pos_perc=(CXCA_SCRN_POS/CXCA_SCRN)*100) 
  
  view(scrn_pos_df_global_wide) 
  
  
  ################################################################################
  ################################### COUNTRY
  
  # compare percentages for different countries at a single point in time
  
  scrn_pos_df_ou <- df_long %>%
    group_by(country, indicator) %>%
    select(-fiscal_year) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 
  
  view(scrn_pos_df_ou)
  
  #pivot wider to create calculation
  scrn_pos_df_ou_wide <- scrn_pos_df_ou %>% 
    group_by(country, indicator) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(values_from = results, names_from = indicator) %>% 
    dplyr::mutate(scrn_pos_perc=(CXCA_SCRN_POS/CXCA_SCRN)*100) 
  
  view(scrn_pos_df_ou_wide) 
  
  ################################################################################
  ################################### AGE
  
  scrn_pos_df_age <- df_long %>%
    group_by(ageasentered, indicator) %>%
    select(-fiscal_year) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 
  
  view(scrn_pos_df_age)
  
#pivot wider to create calculation
  scrn_pos_df_age_wide <- scrn_pos_df_age %>% 
    pivot_wider(values_from = results, names_from = indicator) %>% 
    dplyr::mutate(scrn_pos_perc=(CXCA_SCRN_POS/CXCA_SCRN)*100) 
  
  view(scrn_pos_df_age_wide) 
  
  
  ####### OVERALL  ###############################################################
  ### BY FISCAL YEAR and qtr
  
  scrn_pos_df_global <- df_long %>%
    group_by(indicator, fiscal_year, qtr) %>%
    dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 
  
  view(scrn_pos_df_global)
  
  #pivot wider to create calculation
  scrn_pos_df_global_wide <- scrn_pos_df_global %>% 
    # group_by(indicator) %>% 
    # dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(values_from = results, names_from = indicator) %>% 
    dplyr::mutate(scrn_pos_perc=(CXCA_SCRN_POS/CXCA_SCRN)*100) 
  
  view(scrn_pos_df_global_wide) 

  
  ################################################################################
  ################################### COUNTRY
  
  # compare percentages for different countries at a single point in time
  
  scrn_pos_df_ou <- df_long %>%
    group_by(country, indicator, fiscal_year, qtr) %>%
    dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 
  
  view(scrn_pos_df_ou)
  
  #pivot wider to create calculation
  scrn_pos_df_ou_wide <- scrn_pos_df_ou %>% 
    pivot_wider(values_from = results, names_from = indicator) %>% 
    dplyr::mutate(scrn_pos_perc=(CXCA_SCRN_POS/CXCA_SCRN)*100) 
  
  view(scrn_pos_df_ou_wide) 
  
  
  ################################################################################
  ################################### AGE
  

  scrn_pos_df_age <- df_long %>%
    group_by(ageasentered, indicator, fiscal_year, qtr) %>%
    dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 
  
  view(scrn_pos_df_age)
  
  #pivot wider to create calculation
  scrn_pos_df_age_wide <- scrn_pos_df_age %>% 
    pivot_wider(values_from = results, names_from = indicator) %>% 
    dplyr::mutate(scrn_pos_perc=(CXCA_SCRN_POS/CXCA_SCRN)*100) 
  
  view(scrn_pos_df_age_wide) 
  