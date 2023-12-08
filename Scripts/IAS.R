# PROJECT:  FromPinktoRed
# AUTHOR:   J. Stephens | USAID
# PURPOSE:   
# LICENSE:  MIT
# DATE:     2022-12-08
# UPDATED:  



# CUSTOMIZATIONS ----------------------------------------------------------

# Define the country name variable, leave blank for ALL OUs
operating_unit <- ""

#Define the period
FYQ<- "FY23Q4"

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)

library(gagglr)
pak::pak("USAID-OHA-SI/glitr")

library(grabr)
pak::pak("USAID-OHA-SI/grabr")

library(vroom)

# GLOBAL VARIABLES --------------------------------------------------------

file.path(si_path())

cxca_inds <- c(
  'TX_CURR',
  'CXCA_SCRN',
  'CXCA_SCRN_POS',
  'CXCA_TX')

# DATA -------------------------------------------------------------------

df <- si_path() %>%
  return_latest("OU_IM") %>%
  read_psd()

view(df)

# MUNGE -------------------------------------------------------------------

df <- df %>% 
  filter(indicator == cxca_inds,  
  fiscal_year>=2018)


# df_semi<- df %>% 
#   reshape_msd(direction="semi_wide")
# 
# view(df_semi)

df_long<- df %>% 
  reshape_msd()

view(df_long)



df_filter <- df_long %>% 
  filter(
    (indicator == "TX_CURR" & 
       standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus")) | 
      (str_detect(indicator, "CXCA") & 
         str_detect(standardizeddisaggregate, "Age/Sex/HIVStatus/")),
      sex=="Female", 
      ageasentered %in% c("35-39","50-54", "45-49","20-24", "25-34", "15-24", "30-34", "40-44", 
             "25-29", "15-19", "50+", "35-49", "15+", "65+")
      ) %>% 
  select(operatingunit, country, funding_agency, indicator, numeratordenom, standardizeddisaggregate,
         otherdisaggregate, ageasentered, period, period_type, value)

view(df_filter)

# unique(df_filter$otherdisaggregate)
# [2] "Cervical Cancer Screened - Rescreened, Cervical Cancer - Positive"    
# [3] "Cervical Cancer Screened - Follow Up, Cervical Cancer - Negative"     
# [4] "Cervical Cancer Screened - First Time, Eligible for Cryotherapy"      
# [5] "Cervical Cancer Screened - Rescreened, Cervical Cancer - Negative"    
# [6] "Cervical Cancer Screened - Follow Up, Cervical Cancer - Suspected"    
# [7] "Cervical Cancer Screened - First Time, Cervical Cancer - Suspected"   
# [8] "Cervical Cancer Screened - First Time, Cervical Cancer - Positive"    
# [9] "Cervical Cancer Screened - Rescreened, Cervical Cancer - Suspected"   
# [10] "Cervical Cancer Screened - First Time, Cervical Cancer - Negative"    
# [11] "Cervical Cancer Screened - Rescreened, Eligible for Thermocoagulation"
# [12] "Cervical Cancer Screened - Follow Up, Eligible for LEEP"              
# [13] "Cervical Cancer Screened - Rescreened, Eligible for LEEP"             
# [14] "Cervical Cancer Screened - Rescreened, Eligible for Cryotherapy"      
# [15] "Cervical Cancer Screened - Follow Up, Cervical Cancer - Positive"     
# [16] "Cervical Cancer Screened - Follow Up, Eligible for Cryotherapy"       
# [17] "Cervical Cancer Screened - First Time, Eligible for LEEP"             
# [18] "Cervical Cancer Screened - First Time, Eligible for Thermocoagulation"
# [19] "Cervical Cancer Screened - Follow Up, Eligible for Thermocoagulation" 

# Clean otherdisaggregate

df_other <- df_filter %>%
  mutate(otherdisaggregate = sub("^Cervical Cancer Screened - ", "", otherdisaggregate))

df_other <- df_filter %>%
  mutate(otherdisaggregate = sub("^Cervical Cancer Screened - |Cervical Cancer - |Eligible for ", "", otherdisaggregate)) %>%
  separate(otherdisaggregate, into = c("scrn_type", "otherdisaggregate"), sep = ", ") %>% 
  mutate(otherdisaggregate = sub("^Cervical Cancer Screened - |Cervical Cancer - |Eligible for ", "", otherdisaggregate))

unique(df_other$scrn_type)
unique(df_other$otherdisaggregate)

# Print or inspect the updated data frame
print(df_filter)


#TX_CURR age/sex/hiv status age 15-49 women (currently is all above 15+)


























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

