# PROJECT:  FromPinktoRed
# AUTHOR:   J. Stephens | USAID
# PURPOSE:  Descriptive statistics of Cervical Cancer for IAS
#           Objective 4
# LICENSE:  MIT
# DATE:     2023-12-08
# UPDATED:  202-12-21


# • Cervical Cancer % Positive on Treatment
# % CXCA_TX results / CXCA_SCRN_POS results

# collapse_tx_type_tbl  <- function(df_other, ...) {

tx_type_indics <- c("CXCA_TX")

tx_type_df <-  df_clean %>% 
  dplyr::filter(indicator %in% tx_type_indics) %>% 
  # standardizeddisaggregate %in%  c("Age/Sex/HIVStatus/ScreenResult/ScreenVisitType",
  #                                  "Age/Sex/HIVStatus/TreatmentType/ScreenVisitType") |
  #   cxca_tx %in% c("Thermocoagulation", "Cryotherapy", "LEEP")) %>% 
  dplyr::group_by(across(where(~!is.double(.)))) %>% 
  dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>%
  dplyr::ungroup() 

# view(tx_type_df)

#   return(tx_type_df)
# }



# RESHAPE  --------------------------------------------------------------------------

df_long<- tx_type_df %>%
  pivot_longer(
    cols = qtr2:qtr4,
    names_to = c("qtr"),
    values_to = "results"
  )    %>% 
  select( -cumulative)


# view(df_long)


# ANALYTICS --------------------------------------------------------------------------

####### OVERALL - no fy ###############################################################

tx_type_df_global <- df_long %>%
  group_by(indicator, cxca_tx) %>%
  select(-fiscal_year) %>% 
  dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 

 view(tx_type_df_global)


################################################################################
################################### COUNTRY


tx_type_df_ou <- df_long %>%
  group_by(country, indicator,cxca_tx) %>%
  select(-fiscal_year) %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 

view(tx_type_df_ou)


################################################################################
################################### AGE

tx_type_df_age <- df_long %>%
  group_by(ageasentered, indicator,cxca_tx) %>%
  select(-fiscal_year) %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 

view(tx_type_df_age)


####### OVERALL  ###############################################################
### BY FISCAL YEAR and qtr

pos_tx_df_global <- df_long %>%
  group_by(indicator, fiscal_year, qtr,cxca_tx) %>%
  dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 

view(pos_tx_df_global)



################################################################################
################################### COUNTRY

# compare percentages for different countries at a single point in time

pos_tx_df_ou <- df_long %>%
  group_by(country, indicator, fiscal_year, qtr,cxca_tx) %>%
  dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 

view(pos_tx_df_ou)


################################################################################
################################### AGE


pos_tx_df_age <- df_long %>%
  group_by(ageasentered, indicator, fiscal_year, qtr,cxca_tx) %>%
  dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 

view(pos_tx_df_age)

