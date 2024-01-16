# PROJECT:  FromPinktoRed
# AUTHOR:   J. Stephens | USAID
# PURPOSE:  Descriptive statistics of Cervical Cancer for IAS
# LICENSE:  MIT
# DATE:     2023-12-08
# UPDATED:  202-12-11



# CUSTOMIZATIONS ----------------------------------------------------------

# Define the country name variable, leave blank for ALL OUs
cntry <- c("Zimbabwe", "Botswana", "Eswatini","Ethiopia","Haiti", "Kenya", "Lesotho",
           "Malawi", "Mozambique", "Namibia", "Uganda", "Zambia")

#Define the period
# FYQ<- "FY23Q4"

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)

library(gagglr)

library(grabr)

library(glue)
# ls("package:gophr")

# library(vroom)

# GLOBAL VARIABLES --------------------------------------------------------

file.path(si_path())


# DATA ---------------------------------------------------------------------

df <- si_path() %>%
  return_latest("OU_IM") %>%
  read_psd()

# view(df)

 # get_metadata()
 # metadata$source
 # metadata$type
 # metadata$curr_fy
 # metadata$curr_pd


# CLEAN DISAGS -----------------------------------------------------------------

df_filter <- df %>% 
  #optional country, primarily used for smaller data set QC
  filter(operatingunit %in% cntry) %>% 
  #filtering TX_CURR & CXCA to the specific age / sex disags 
   filter(
    (indicator == "TX_CURR" & 
       standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus") &
           ageasentered %in% c("15-19", "20-24", "25-29", "30-34","35-39", "40-44", 
              "45-49","50+", "50-54", "55-59","60-64","65+","15+")&
           sex=="Female") | 
    (str_detect(indicator, "CXCA") & 
        str_detect(standardizeddisaggregate, "Age/Sex/HIVStatus") &
       #consider changing above to otherdisaggregate!=null 
       ageasentered %in% c("15-19", "20-24", "25-29", "30-34","35-39", "40-44", 
                           "45-49","50+", "50-54", "55-59","60-64","65+","15+")&
       !(ageasentered %in% c("Unknown Age")))) %>% 
  #filtering necessary variables for ou / global analysis
  select(operatingunit, country, funding_agency, indicator, standardizeddisaggregate,
         otherdisaggregate, ageasentered, fiscal_year:cumulative)

#QC cxca_scrn
df_filter_cxca <- df_filter %>% 
filter(operatingunit %in% cntry) %>% 
  #filtering TX_CURR & CXCA to the specific age / sex disags 
  filter(
(str_detect(indicator, "CXCA")))
 
df_check_1 <- df_filter_cxca %>%
  dplyr::group_by(operatingunit, indicator, fiscal_year) %>%
  dplyr::summarize(total=sum(cumulative, na.rm=TRUE))

view(df_check_1)
#cxca_scrn matches tableau by OU 

#QC cxca_scrn by age 
df_check_age_1 <- df_filter_cxca %>%
  dplyr::group_by(indicator, fiscal_year, ageasentered) %>%
  dplyr::summarize(total=sum(cumulative, na.rm=TRUE))
view(df_check_age_1)
#cxca_scrn matches by age

#QC tx_curr

df_curr_age_check <-df_filter %>% 
  mutate(ageasentered=case_when(
    ageasentered %in% c("50-54", "55-59", "60-64", "65+")
    ~ "50+",
    TRUE~ ageasentered))

df_filter_curr1 <- df_curr_age_check %>% 
  filter(operatingunit %in% cntry) %>% 
  #filtering TX_CURR 
  filter(
    (str_detect(indicator, "TX_CURR")))

df_check_curr_2 <- df_filter_curr1 %>%
  dplyr::group_by(operatingunit, indicator, fiscal_year) %>%
  dplyr::summarize(total=sum(cumulative, na.rm=TRUE))

view(df_check_curr_2)
# names(df_filter)
 # view(df_filter)


# Clean otherdisaggregate

df_other <- df_filter %>%
  mutate(otherdisaggregate = sub("^Cervical Cancer Screened - ", 
                                 "", otherdisaggregate)) %>%
  separate(otherdisaggregate, into = c("scrn_type", 
                                       "otherdisaggregate"), sep = ", ") %>% 
  mutate(otherdisaggregate = sub("Cervical Cancer - |Eligible for ", 
                                 "", otherdisaggregate)) 
# %>% 
#   mutate(standardizeddisaggregate = str_replace(standardizeddisaggregate, 
#                                            ".*ScreenResult.*", "ScreenResult"), 
#          standardizeddisaggregate = str_replace(standardizeddisaggregate, 
#                                                 ".*TreatmentType.*", "TreatmentType")) 
    
# unique(df_other$standardizeddisaggregate)
# unique(df_other$otherdisaggregate)
# 
# unique(df_other$scrn_type)
# 
# # Print or inspect the updated data frame
# view(df_other)

# Print or inspect the unique combinations
# unique_combinations <- df_other %>%
#   distinct(indicator, standardizeddisaggregate, otherdisaggregate, scrn_type)
# print(unique_combinations, n=24, sort=indicator)


#TX_CURR age/sex/hiv status age 15-49 women (currently is all above 15+)
# unique_combinations <- df_other %>%
#   # filter(indicator=="TX_CURR") %>%
#   distinct(indicator, standardizeddisaggregate, otherdisaggregate)
# print(unique_combinations)
# A tibble: 24 × 4

# indicator     standardizeddisaggregate                        otherdisaggregate scrn_type 
# <chr>         <chr>                                           <chr>             <chr>     
#   1 TX_CURR       Age/Sex/HIVStatus                               NA                NA        
# 2 TX_CURR       Age Aggregated/Sex/HIVStatus                    NA                NA        
# 3 CXCA_SCRN     Age/Sex/HIVStatus                               NA                NA        
# 4 CXCA_SCRN     Age/Sex/HIVStatus/ScreenResult/ScreenVisitType  Negative          Rescreened
# 5 CXCA_SCRN     Age/Sex/HIVStatus/ScreenResult/ScreenVisitType  Negative          First Time
# 6 CXCA_SCRN     Age/Sex/HIVStatus/ScreenResult/ScreenVisitType  Positive          Rescreened
# 7 CXCA_SCRN     Age/Sex/HIVStatus/ScreenResult/ScreenVisitType  Positive          Follow Up 
# 8 CXCA_SCRN_POS Age/Sex/HIVStatus/ScreenResult/ScreenVisitType  Positive          Rescreened
# 9 CXCA_SCRN     Age/Sex/HIVStatus/ScreenResult/ScreenVisitType  Negative          Follow Up 
# 10 CXCA_SCRN     Age/Sex/HIVStatus/ScreenResult/ScreenVisitType  Positive          First Time
# 11 CXCA_SCRN_POS Age/Sex/HIVStatus/ScreenResult/ScreenVisitType  Positive          First Time
# 12 CXCA_SCRN     Age/Sex/HIVStatus/ScreenResult/ScreenVisitType  Suspected         First Time
# 13 CXCA_TX       Age/Sex/HIVStatus/TreatmentType/ScreenVisitType Cryotherapy       First Time
# 14 CXCA_TX       Age/Sex/HIVStatus/TreatmentType/ScreenVisitType LEEP              Rescreened
# 15 CXCA_TX       Age/Sex/HIVStatus/TreatmentType/ScreenVisitType Cryotherapy       Rescreened
# 16 CXCA_TX       Age/Sex/HIVStatus/TreatmentType/ScreenVisitType LEEP              First Time
# 17 CXCA_SCRN_POS Age/Sex/HIVStatus/ScreenResult/ScreenVisitType  Positive          Follow Up 
# 18 CXCA_SCRN     Age/Sex/HIVStatus/ScreenResult/ScreenVisitType  Suspected         Rescreened
# 19 CXCA_TX       Age/Sex/HIVStatus/TreatmentType/ScreenVisitType Thermocoagulation First Time
# 20 CXCA_SCRN     Age/Sex/HIVStatus/ScreenResult/ScreenVisitType  Suspected         Follow Up 
# 21 CXCA_TX       Age/Sex/HIVStatus/TreatmentType/ScreenVisitType Thermocoagulation Follow Up 
# 22 CXCA_TX       Age/Sex/HIVStatus/TreatmentType/ScreenVisitType Cryotherapy       Follow Up 
# 23 CXCA_TX       Age/Sex/HIVStatus/TreatmentType/ScreenVisitType Thermocoagulation Rescreened
# 24 CXCA_TX       Age/Sex/HIVStatus/TreatmentType/ScreenVisitType LEEP              Follow Up 


# FORMAT VARIABLES ---------------------------------------------------------


# str(df_other)

# change fiscal year ?
# df_vars<-df_other %>% 
#   fiscal_year

#### need to aggregate 50+ of tx_curr (50-54 ad 55-59)
df_clean<-df_other %>% 
  mutate(ageasentered=case_when(
             ageasentered %in% c("50-54", "55-59", "60-64", "65+")
             ~ "50+",
              TRUE~ ageasentered))

#QC cxca_scrn
df_clean_cxca <- df_clean %>% 
  filter(operatingunit %in% cntry) %>% 
  #filtering TX_CURR & CXCA to the specific age / sex disags 
  filter(
    (str_detect(indicator, "CXCA")))

df_clean_check_cxca <- df_clean_cxca %>%
  dplyr::group_by(operatingunit, indicator, fiscal_year) %>%
  dplyr::summarize(total=sum(cumulative, na.rm=TRUE))

view(df_clean_check_cxca) #cxca_scrn matches

#QC cxca_scrn by age 
df_check_age_2 <- df_clean_cxca %>%
  dplyr::group_by(indicator, fiscal_year, ageasentered) %>%
  dplyr::summarize(total=sum(cumulative, na.rm=TRUE))
view(df_check_age_2)
#cxca_scrn matches by age

# CALCULATED VARIABLES DATAFRAMES-----------------------------------------------

# • Cervical Cancer Screening of ART Treatment
# % CXCA_SCRN Cumulative / TX_CURR Cumulative

collapse_scrn_txcurr_tbl  <- function(df_clean, ...) {
  
  scrn_txcurr_indics <- c("CXCA_SCRN", "TX_CURR")
  
  scrn_txcurr_df <- df_clean %>% 
    dplyr::filter(indicator %in% scrn_txcurr_indics,
                  standardizeddisaggregate %in% c("Age/Sex/HIVStatus/ScreenResult/ScreenVisitType",
                                                   "Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus")) %>% 
    dplyr::group_by(across()) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup() 
  
  return(scrn_txcurr_df)
}
view(scrn_txcurr_df)

#QC cxca_scrn
df_txcurr_cxca_1 <- scrn_txcurr_df %>% 
  filter(operatingunit %in% cntry) %>% 
  #filtering TX_CURR & CXCA to the specific age / sex disags 
  filter(
    (str_detect(indicator, "CXCA")))

df_txcurr_cxca_1_check <- df_txcurr_cxca_1 %>%
  dplyr::group_by(operatingunit, indicator, fiscal_year) %>%
  dplyr::summarize(total=sum(cumulative, na.rm=TRUE))

view(df_txcurr_cxca_1_check)

df_check_age_3 <- scrn_txcurr_df %>%
  dplyr::group_by(indicator, fiscal_year, ageasentered) %>%
  dplyr::summarize(total=sum(cumulative, na.rm=TRUE))
view(df_check_age_3)
#cxca_scrn by age doesn't match



# • Cervical Cancer Screening Achievement
# % CXCA_SCRN Cumulative / Targets

collapse_scrn_ach_tbl  <- function(df_other, ...) {
  
scrn_ach_indics <- c("CXCA_SCRN")

scrn_ach_df <-  df_other %>% 
  dplyr::filter(indicator %in% scrn_ach_indics,
                standardizeddisaggregate %in%  c("Age/Sex/HIVStatus/ScreenResult/ScreenVisitType", "Age/Sex/HIVStatus"),
                  # otherdisaggregate %in% c("Negative", "Positive", "Suspected"),
                funding_agency != "Dedup") %>% 
  dplyr::group_by(across()) %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  dplyr::ungroup() 

return(scrn_ach_df)
}

# • Cervical Cancer % Positive
# % CXCA_SCRN_POS results / CXCA_SCRN results

collapse_scrn_pos_tbl  <- function(df_other, ...) {
  
  scrn_pos_indics <- c("CXCA_SCRN", "CXCA_SCRN_POS")
 
  scrn_pos_df <-  df_other %>% 
    dplyr::filter(indicator %in% scrn_pos_indics,
                  standardizeddisaggregate %in%  c("Age/Sex/HIVStatus/ScreenResult/ScreenVisitType") ,
                  funding_agency != "Dedup") 
  %>% 
    dplyr::group_by(across()) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup() 
  
  # view(scrn_pos_df)
  
  return(scrn_pos_df)
}

# • Cervical Cancer % Positive on Treatment
# % CXCA_TX results / CXCA_SCRN_POS results

collapse_pos_tx_tbl  <- function(df_other, ...) {
    
  pos_tx_indics <- c("CXCA_TX", "CXCA_SCRN_POS")
  
  pos_tx_df <-  df_other %>% 
    dplyr::filter(indicator %in% pos_tx_indics,
                  standardizeddisaggregate %in%  c("Age/Sex/HIVStatus/ScreenResult/ScreenVisitType",
                                                   "Age/Sex/HIVStatus/TreatmentType/ScreenVisitType") |
                    otherdisaggregate %in% c( "Positive","Thermocoagulation", "Cryotherapy", "LEEP"),
                  funding_agency != "Dedup") %>% 
    dplyr::group_by(across()) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup() 
  
  # view(pos_tx_df)
  
  return(pos_tx_df)
}
  
  
# RESHAPE  --------------------------------------------------------------------------



# df_semi<- df %>% 
#   reshape_msd(direction="semi_wide")
# 
# view(df_semi)
# 
# df_long<- df %>% 
#   reshape_msd()





# ANALYTICS --------------------------------------------------------------------------

#leaves out age and country for trends
scrn_txcurr_df_fy <- scrn_txcurr_df %>%
  group_by(operatingunit, country, indicator, fiscal_year) %>%
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
  select( indicator:cumulative) %>% 
  select(indicator, fiscal_year,cumulative) %>% 
  filter(fiscal_year!=2024)

# view(scrn_txcurr_df_fy)

#pivot wider to create calculation
scrn_txcurr_df_fy_wide <- scrn_txcurr_df_fy %>% 
  group_by(indicator, fiscal_year) %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(values_from = cumulative, names_from = indicator) %>% 
  dplyr::mutate(scrn_ach_curr=CXCA_SCRN/TX_CURR) 


# view(scrn_txcurr_df_fy_wide) 

# Run the regression for fiscal year
lm_model <- lm(scrn_ach_curr ~ fiscal_year, data = scrn_txcurr_df_fy_wide)
summary(lm_model)

# Call:
#   lm(formula = scrn_ach_curr ~ fiscal_year, data = scrn_txcurr_viz_wide)
# 
# Residuals:
#   1        2        3 
# -0.01297  0.02594 -0.01297 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -14.471689  45.418298  -0.319    0.804
# fiscal_year   0.007303   0.022462   0.325    0.800
# 
# Residual standard error: 0.03177 on 1 degrees of freedom
# Multiple R-squared:  0.09561,	Adjusted R-squared:  -0.8088 
# F-statistic: 0.1057 on 1 and 1 DF,  p-value: 0.7999

################################################################################
################################### COUNTRY
#leaves out age and fiscal year, for trends by age group
scrn_txcurr_df_ou <- scrn_txcurr_df %>%
    filter(fiscal_year!=2024) %>% 
  group_by(operatingunit, country, indicator) %>%
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
  select( country:cumulative) %>% 
  select(indicator, country, cumulative)  

view(scrn_txcurr_df_ou)

#pivot wider to create calculation
scrn_txcurr_df_ou_wide <- scrn_txcurr_df_ou %>% 
  group_by(country, indicator) %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(values_from = cumulative, names_from = indicator) %>% 
  dplyr::mutate(scrn_ach_curr=CXCA_SCRN/TX_CURR) 


 view(scrn_txcurr_df_ou_wide) 

# Run the regression for fiscal year
lm_model <- lm(scrn_ach_curr ~ country, data = scrn_txcurr_df_ou_wide)
summary(lm_model)

  
# Call:
#   lm(formula = scrn_ach_curr ~ country, data = scrn_txcurr_df_ou_wide)
# 
# Residuals:
#   ALL 12 residuals are 0: no residual degrees of freedom!
#   
#   Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)        0.17272        NaN     NaN      NaN
# countryEswatini    0.08857        NaN     NaN      NaN
# countryEthiopia    0.07565        NaN     NaN      NaN
# countryHaiti      -0.17170        NaN     NaN      NaN
# countryKenya       0.18210        NaN     NaN      NaN
# countryLesotho     0.05055        NaN     NaN      NaN
# countryMalawi      0.07239        NaN     NaN      NaN
# countryMozambique  0.15320        NaN     NaN      NaN
# countryNamibia    -0.01612        NaN     NaN      NaN
# countryUganda      0.09104        NaN     NaN      NaN
# countryZambia      0.15726        NaN     NaN      NaN
# countryZimbabwe    0.16285        NaN     NaN      NaN
# 
# Residual standard error: NaN on 0 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:    NaN 
# F-statistic:   NaN on 11 and 0 DF,  p-value: NA



################################################################################
################################### AGE

#### by ages
scrn_txcurr_viz_age <- scrn_txcurr_df %>%
  group_by(operatingunit, country, indicator, ageasentered, fiscal_year) %>%
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
  select( indicator:cumulative) %>% 
  select(indicator, ageasentered, fiscal_year, cumulative) 


view(scrn_txcurr_viz_age)
#this is where the scrn_cxca are slightly off....

scrn_txcurr_viz_wide_age <- scrn_txcurr_viz_age %>% 
  pivot_wider(values_from = cumulative, names_from = indicator) %>% 
  dplyr::mutate(scrn_ach_curr=CXCA_SCRN/TX_CURR) 

view(scrn_txcurr_viz_wide_age) 


scrn_txcurr_viz_wide <- scrn_txcurr_viz %>% 
  pivot_wider(values_from = cumulative, names_from = indicator) %>% 
  dplyr::mutate(scrn_ach_curr=CXCA_SCRN/TX_CURR)  %>% 
  group_by(ageasentered) %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") 

view(scrn_txcurr_viz_wide) 

lm_model_age <- lm(scrn_ach_curr ~ ageasentered, data = scrn_txcurr_viz_wide_age)

summary(lm_model_age)



# VIZ --------------------------------------------------------------------------

# scrn_txcurr_viz_wide %>%
#   ggplot(aes(fiscal_year, scrn_ach_curr)) +
#   geom_col()








