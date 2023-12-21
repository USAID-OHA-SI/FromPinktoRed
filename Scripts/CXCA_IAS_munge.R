# PROJECT:  FromPinktoRed
# AUTHOR:   J. Stephens | USAID
# PURPOSE:  Descriptive statistics of Cervical Cancer for IAS
# LICENSE:  MIT
# DATE:     2023-12-08
# UPDATED:  202-12-21



# CUSTOMIZATIONS ----------------------------------------------------------

# Define the country name variable, leave blank for ALL OUs
# cntry <- c("Zimbabwe", "Botswana", "Eswatini","Ethiopia","Haiti", "Kenya", "Lesotho",
#            "Malawi", "Mozambique", "Namibia", "Uganda", "Zambia")
cntry_gofurther <- c("Zimbabwe", "Botswana", "Eswatini", "Lesotho",
           "Malawi", "Mozambique", "Namibia", "Zambia")


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
  return_latest("OU_IM_FY21") %>%
  read_psd()   
df_arch <- si_path() %>% 
  return_latest("OU_IM_FY15") %>% 
  read_msd()


# view(df)

 # get_metadata()
 # metadata$source
 # metadata$type
 # metadata$curr_fy
 # metadata$curr_pd


# MUNGE -----------------------------------------------------------------

#bind archived + current MSD and filter 
df_bind <- df %>%
  bind_rows(df_arch) %>% 
  dplyr::filter(!(fiscal_year %in% c("2024", "2015", "2016", "2017")))%>% 
  dplyr::filter(operatingunit %in% cntry_gofurther)


#####################CLEAN DISAGS 

df_filter <- df_bind %>% 
  #filtering TX_CURR & CXCA to the specific age / sex disags 
   filter(
    (indicator == "TX_CURR" & 
       standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus") &
           ageasentered %in% c("15-19", "20-24", "25-29", "30-34","35-39", "40-44", 
              "45-49","50+", "50-54", "55-59","60-64","65+","15+")&
           sex=="Female") | 
    (str_detect(indicator, "CXCA") & 
        str_detect(standardizeddisaggregate, "Age/Sex/HIVStatus/") &
       #consider changing above to otherdisaggregate!=null 
       ageasentered %in% c("15-19", "20-24", "25-29", "30-34","35-39", "40-44", 
                           "45-49","50+", "50-54", "55-59","60-64","65+","15+")))%>% 
  #filtering necessary variables for ou / global analysis
  select(country, indicator, standardizeddisaggregate,
         otherdisaggregate, ageasentered, fiscal_year, qtr2, qtr4, cumulative)

# names(df_filter)
# view(df_filter)


# Clean otherdisaggregate

df_other <- df_filter %>%
  mutate(otherdisaggregate = sub("^Cervical Cancer Screened - ", 
                                 "", otherdisaggregate)) %>%
  separate(otherdisaggregate, into = c("scrn_type", 
                                       "otherdisaggregate"), sep = ", ") %>% 
  mutate(otherdisaggregate = sub("Cervical Cancer - |Eligible for ", 
                                 "", otherdisaggregate)) %>% 
  mutate(cxca_status = case_when(
    otherdisaggregate %in% c("Positive", "Negative", "Suspected") ~ otherdisaggregate,
    TRUE ~ NA_character_
  ),
  cxca_tx = case_when(
    otherdisaggregate %in% c("LEEP", "Cryotherapy", "Thermocoagulation") ~ otherdisaggregate,
    TRUE ~ NA_character_
  )) %>% 
  select(-otherdisaggregate)


#TX_CURR age/sex/hiv status age 15-49 women (currently is all above 15+)
# unique_combinations <- df_other %>%
#   filter(indicator=="CXCA_SCRN") %>%
#   distinct(indicator, standardizeddisaggregate, ageasentered)
# print(unique_combinations)

# FORMAT VARIABLES ---------------------------------------------------------

#### need to aggregate 50+ of tx_curr (50-54 ad 55-59)
df_clean<-df_other %>% 
  mutate(ageasentered=case_when(
             ageasentered %in% c("50-54", "55-59", "60-64", "65+")
             ~ "50+",
              TRUE~ ageasentered))

# names(df_clean)










