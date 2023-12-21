# PROJECT:  FromPinktoRed
# AUTHOR:   J. Stephens | USAID
# PURPOSE:  Descriptive statistics of Cervical Cancer for IAS
#           Objective 3
# LICENSE:  MIT
# DATE:     2023-12-08
# UPDATED:  202-12-21


# • Cervical Cancer % Positive on Treatment
# % CXCA_TX results / CXCA_SCRN_POS results

# collapse_pos_tx_tbl  <- function(df_other, ...) {
  
  pos_tx_indics <- c("CXCA_TX", "CXCA_SCRN_POS")
  
  pos_tx_df <-  df_clean %>% 
    dplyr::filter(indicator %in% pos_tx_indics) %>% 
                  # standardizeddisaggregate %in%  c("Age/Sex/HIVStatus/ScreenResult/ScreenVisitType",
                  #                                  "Age/Sex/HIVStatus/TreatmentType/ScreenVisitType") |
                  #   cxca_tx %in% c("Thermocoagulation", "Cryotherapy", "LEEP")) %>% 
    dplyr::group_by(across()) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup() 
  
  view(pos_tx_df)
  
#   return(pos_tx_df)
# }



# RESHAPE  --------------------------------------------------------------------------
  
  df_long<- pos_tx_df %>%
    pivot_longer(
      cols = qtr2:qtr4,
      names_to = c("qtr"),
      values_to = "results"
    )    %>% 
    select( -cumulative)

  
  view(df_long)
  
  
  # ANALYTICS --------------------------------------------------------------------------
  
  ####### OVERALL - no fy ###############################################################
  
  pos_tx_df_global <- df_long %>%
    group_by(indicator) %>%
    select(-fiscal_year) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 
  
  view(pos_tx_df_global)
  
  #pivot wider to create calculation
  pos_tx_df_global_wide <- pos_tx_df_global %>% 
    pivot_wider(values_from = results, names_from = indicator) %>% 
    dplyr::mutate(pos_tx_perc=(CXCA_TX/CXCA_SCRN_POS)*100) 
  
  view(pos_tx_df_global_wide) 
  
  
  ################################################################################
  ################################### COUNTRY
  
  # compare percentages for different countries at a single point in time
  
  pos_tx_df_ou <- df_long %>%
    group_by(country, indicator) %>%
    select(-fiscal_year) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 
  
  view(pos_tx_df_ou)
  
  #pivot wider to create calculation
  pos_tx_df_ou_wide <- pos_tx_df_ou %>% 
    group_by(country, indicator) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(values_from = results, names_from = indicator) %>% 
    dplyr::mutate(pos_tx_perc=(CXCA_TX/CXCA_SCRN_POS)*100) 
  
  view(pos_tx_df_ou_wide) 
  
  ################################################################################
  ################################### AGE
  
  pos_tx_df_age <- df_long %>%
    group_by(ageasentered, indicator) %>%
    select(-fiscal_year) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 
  
  view(pos_tx_df_age)
  
  #pivot wider to create calculation
  pos_tx_df_age_wide <- pos_tx_df_age %>% 
    pivot_wider(values_from = results, names_from = indicator) %>% 
    dplyr::mutate(pos_tx_perc=(CXCA_TX/CXCA_SCRN_POS)*100) 
  
  view(pos_tx_df_age_wide) 
  
  
  ####### OVERALL  ###############################################################
  ### BY FISCAL YEAR and qtr
  
  pos_tx_df_global <- df_long %>%
    group_by(indicator, fiscal_year, qtr) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 
  
  view(pos_tx_df_global)
  
  #pivot wider to create calculation
  pos_tx_df_global_wide <- pos_tx_df_global %>% 
    pivot_wider(values_from = results, names_from = indicator) %>% 
    dplyr::mutate(pos_tx_perc=(CXCA_TX/CXCA_SCRN_POS)*100) 
  
  view(pos_tx_df_global_wide) 
  
  
  ################################################################################
  ################################### COUNTRY
  
  # compare percentages for different countries at a single point in time
  
  pos_tx_df_ou <- df_long %>%
    group_by(country, indicator, fiscal_year, qtr) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 
  
  view(pos_tx_df_ou)
  
  #pivot wider to create calculation
  pos_tx_df_ou_wide <- pos_tx_df_ou %>% 
    pivot_wider(values_from = results, names_from = indicator) %>% 
    dplyr::mutate(pos_tx_perc=(CXCA_TX/CXCA_SCRN_POS)*100) 
  
  view(pos_tx_df_ou_wide) 
  
  
  ################################################################################
  ################################### AGE
  
  
  pos_tx_df_age <- df_long %>%
    group_by(ageasentered, indicator, fiscal_year, qtr) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 
  
  view(pos_tx_df_age)
  
  #pivot wider to create calculation
  pos_tx_df_age_wide <- pos_tx_df_age %>% 
    pivot_wider(values_from = results, names_from = indicator) %>% 
    dplyr::mutate(pos_tx_perc=(CXCA_TX/CXCA_SCRN_POS)*100) 
  
  view(pos_tx_df_age_wide) 
  

