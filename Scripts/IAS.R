# PROJECT:  FromPinktoRed
# AUTHOR:   J. Stephens | USAID
# PURPOSE:  Descriptive statistics of Cervical Cancer for IAS
# LICENSE:  MIT
# DATE:     2023-12-08
# UPDATED:  202-12-11



# CUSTOMIZATIONS ----------------------------------------------------------

# Define the country name variable, leave blank for ALL OUs
cntry <- "Zimbabwe"

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
  #filtering TX_CURR & CXCA to the specific age / sex disags 
   filter(
    (indicator == "TX_CURR" & 
       standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus")) | 
      
    (str_detect(indicator, "CXCA") & 
        str_detect(standardizeddisaggregate, "Age/Sex/HIVStatus")),
    sex=="Female", 
    ageasentered %in% c("35-39","50-54", "45-49","20-24", "25-34", "15-24", "30-34", "40-44", 
             "25-29", "15-19", "50+", "35-49", "15+", "65+")
    )  %>% 
  #filtering necessary variables for ou / global analysis
  select(operatingunit, country, funding_agency, indicator, standardizeddisaggregate,
         otherdisaggregate, ageasentered, fiscal_year:cumulative)

names(df_filter)


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
#   filter(indicator=="TX_CURR") %>% 
#   distinct(indicator, standardizeddisaggregate, ageasentered)
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


str(df_other)

# change fiscal year ?
# df_vars<-df_other %>% 
#   fiscal_year



# CALCULATED VARIABLES ---------------------------------------------------------

# • Cervical Cancer Screening of ART Treatment
# % CXCA_SCRN Cumulative / TX_CURR Cumulative

collapse_scrn_txcurr_tbl  <- function(df_other, ...) {
  
  scrn_txcurr_indics <- c("CXCA_SCRN", "TX_CURR")
  
  scrn_txcurr_df <-  df_other %>% 
    dplyr::filter(indicator %in% scrn_txcurr_indics,
                  standardizeddisaggregate %in%  c("Age/Sex/HIVStatus/ScreenResult/ScreenVisitType",
                                                   "Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus"),
                  funding_agency != "Dedup") %>% 
    dplyr::group_by(across()) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup() 
  
  return(scrn_txcurr_df)
}


# • Cervical Cancer Screening Achievement
# % CXCA_SCRN Cumulative / Targets

collapse_scrn_ach_tbl  <- function(df_other, ...) {
  
scrn_ach_indics <- c("CXCA_SCRN")

scrn_ach_df <-  df_other %>% 
  dplyr::filter(indicator %in% scrn_ach_indics,
                standardizeddisaggregate %in%  c("Age/Sex/HIVStatus/ScreenResult/ScreenVisitType", "Age/Sex/HIVStatus") |
                  otherdisaggregate %in% c("Negative", "Positive", "Suspected"),
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
                  funding_agency != "Dedup") %>% 
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

df_long<- df %>% 
  reshape_msd()


zim<-df_long %>% 
  filter(operatingunit==cntry)

# view(zim)



# VIZ --------------------------------------------------------------------------

#leaves out age and fiscal year, for trends by age group
scrn_ach_viz <- scrn_ach_df %>%
  group_by(across(operatingunit:otherdisaggregate) ) %>%
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(scrn_ach=cumulative/targets)
  
view(scrn_ach_viz)


















# df_viz <- reshape_msd(df_viz, "quarters")

df_viz %>%
  filter(period_type=="results") %>% 
  ggplot(aes(period, value)) +
  geom_col()+
  facet_wrap(vars(indicator) )

















+
  geom_text(data = . %>% filter(., period == metadata$curr_pd),
            aes(label = results),
            vjust = -.5) +
  labs(title = glue("Upward trend in TX_NEW results thru {metadata$curr_qtr} quarters") %>% toupper,
       subtitle = glue("{cntry} | {metadata$curr_fy_lab} cumulative mechanism results"),
       x = NULL, y = NULL,
       caption = glue("{metadata$caption}")) 


#examples

df_ach_usaid <- df_ach %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>% 
  adorn_achievement(curr_qtr)


df_tx <- df %>% 
  filter(funding_agency == "USAID",
         indicator == "TX_CURR",
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(snu1, indicator, fiscal_year) %>% 
  summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd("quarters") %>% 
  select(-results_cumulative)












# EXPORT -------------------------------------------------------------------

# Set location to export file
setwd(file.path(si_path(), "SITE_SHIFT"))

#reading and combining local files (replaced map_dfr w/ vroom)
df_full<- list.files(file.path(si_path(), "SITE_SHIFT")) %>%  
  vroom

#export
# readr::write_csv(df_full, file.path(si_path(), "SITE_SHIFT_full_FY23Q2.csv"))
# readr::write_csv(df_full, file.path(si_path(), "SITE_SHIFT_zim_FY23Q3.csv"))
readr::write_csv(df_full, file.path(si_path(), paste0("SITE_SHIFT_",
                                                      operating_unit,
                                                      FYQ,".csv")))

#delete folder
unlink((file.path(si_path(), "SITE_SHIFT")))

