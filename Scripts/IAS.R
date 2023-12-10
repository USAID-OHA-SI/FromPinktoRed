# PROJECT:  FromPinktoRed
# AUTHOR:   J. Stephens | USAID
# PURPOSE:   
# LICENSE:  MIT
# DATE:     2022-12-08
# UPDATED:  



# CUSTOMIZATIONS ----------------------------------------------------------

# Define the country name variable, leave blank for ALL OUs
cntry <- "Zimbabwe"

#Define the period
# FYQ<- "FY23Q4"

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)

library(gagglr)

library(grabr)
ls("package:gophr")

# library(vroom)

# GLOBAL VARIABLES --------------------------------------------------------

file.path(si_path())

cxca_inds <- c(
  'TX_CURR',
  'CXCA_SCRN',
  'CXCA_SCRN_POS',
  'CXCA_TX')

# DATA -------------------------------------------------------------------

df <- si_path() %>%
  return_latest("PSNU_IM") %>%
  read_psd()

view(df)

 get_metadata()
 metadata$source
 metadata$type
 # metadata$curr_fy
# metadata$curr_pd

# MUNGE -------------------------------------------------------------------

#minimises data source to years and indicators for CXCA
df <- df %>% 
  filter(indicator == cxca_inds)

#########################   QC    #####################################
 
 
 df_viz <- df %>%
   filter(operatingunit == cntry,
          fiscal_year == metadata$curr_fy,
          indicator == "TX_CURR",
          standardizeddisaggregate == "Total Numerator")

 df_viz2 <- df_viz %>%
   group_by(fiscal_year, indicator, mech_code) %>%
   summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
             .groups = "drop")
 
 view(df_viz2)
 
 df_test <- df %>% 
     filter(indicator == "CXCA_SCRN") %>% 
  filter(operatingunit=="Zimbabwe", fiscal_year=="2023") %>% 
   filter(funding_agency!="Dudup")
 
df_view <- df_test %>%
  group_by(fiscal_year, indicator, operatingunit, numeratordenom, standardizeddisaggregate, mech_code) %>%
  summarise(across(c(targets,cumulative, starts_with("qtr")), sum, na.rm = TRUE),
            .groups = "drop") 

view(df_view)


#reshape

# df_semi<- df %>% 
#   reshape_msd(direction="semi_wide")
# 
# view(df_semi)

df_long<- df %>% 
  reshape_msd()

view(df_long)


#filtering CXCA to the specific age / sex disags 
#filtering necessary variables for ou / global analysis
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

# Clean otherdisaggregate

# df_other <- df_filter %>%
#   mutate(otherdisaggregate = sub("^Cervical Cancer Screened - ", "", otherdisaggregate))

df_other <- df_filter %>%
  mutate(otherdisaggregate = sub("^Cervical Cancer Screened - ", 
                                 "", otherdisaggregate)) %>%
  separate(otherdisaggregate, into = c("scrn_type", 
                                       "otherdisaggregate"), sep = ", ") %>% 
  mutate(otherdisaggregate = sub("Cervical Cancer - |Eligible for ", 
                                 "", otherdisaggregate)) %>% 
  mutate(standardizeddisaggregate = str_replace(standardizeddisaggregate, 
                                           ".*ScreenResult.*", "ScreenResult"), 
         standardizeddisaggregate = str_replace(standardizeddisaggregate, 
                                                ".*TreatmentType.*", "TreatmentType")) 
    

unique(df_other$scrn_type)
unique(df_other$otherdisaggregate)
unique(df_other$standardizeddisaggregate)

# Print or inspect the updated data frame
view(df_other)

# Print or inspect the unique combinations
unique_combinations <- df_other %>%
  distinct(indicator, standardizeddisaggregate, otherdisaggregate)
print(unique_combinations)

# A tibble: 9 × 3
# indicator     standardizeddisaggregate     otherdisaggregate
# <chr>         <chr>                        <chr>            
#   1 TX_CURR       Age/Sex/HIVStatus            NA               
# 2 TX_CURR       Age Aggregated/Sex/HIVStatus NA               
# 3 CXCA_SCRN     ScreenResult                 Negative         
# 4 CXCA_SCRN     ScreenResult                 Positive         
# 5 CXCA_SCRN_POS ScreenResult                 Positive         
# 6 CXCA_TX       TreatmentType                Cryotherapy      
# 7 CXCA_TX       TreatmentType                LEEP             
# 8 CXCA_SCRN     ScreenResult                 Suspected        
# 9 CXCA_TX       TreatmentType                Thermocoagulation

#TX_CURR age/sex/hiv status age 15-49 women (currently is all above 15+)
unique_combinations <- df_other %>%
  filter(indicator=="TX_CURR") %>% 
  distinct(indicator, standardizeddisaggregate, ageasentered)
print(unique_combinations)

# A tibble: 14 × 3
# indicator standardizeddisaggregate     ageasentered
# <chr>     <chr>                        <chr>       
#   1 TX_CURR   Age/Sex/HIVStatus            35-39       
# 2 TX_CURR   Age/Sex/HIVStatus            50-54       
# 3 TX_CURR   Age/Sex/HIVStatus            45-49       
# 4 TX_CURR   Age/Sex/HIVStatus            20-24       
# 5 TX_CURR   Age/Sex/HIVStatus            25-34       
# 6 TX_CURR   Age/Sex/HIVStatus            15-24       
# 7 TX_CURR   Age/Sex/HIVStatus            30-34       
# 8 TX_CURR   Age/Sex/HIVStatus            40-44       
# 9 TX_CURR   Age/Sex/HIVStatus            25-29       
# 10 TX_CURR   Age/Sex/HIVStatus            15-19       
# 11 TX_CURR   Age/Sex/HIVStatus            50+         
#   12 TX_CURR   Age/Sex/HIVStatus            65+         
#   13 TX_CURR   Age/Sex/HIVStatus            35-49       
# 14 TX_CURR   Age Aggregated/Sex/HIVStatus 15+   





# VIZ -------------------------------------------------------------------

df_viz <- df_long %>%
   filter(indicator == "CXCA_SCRN") %>% 
  filter(operatingunit=="Botswana", period=="FY23")


df_viz <- df_viz %>%
  group_by(period, period_type, indicator ) %>%
  summarise(value = sum(value, na.rm = FALSE),
            .groups = "drop")

view(df_viz)
# 
# df_viz <- reshape_msd(df_viz, "quarters")

df_viz %>%
  ggplot(aes(period, results_cumulative)) +
  geom_col() +
  geom_text(data = . %>% filter(., period == metadata$curr_pd),
            aes(label = results_cumulative),
            vjust = -.5) +
  facet_wrap(~fct_reorder2(mech_code, period, targets)) +
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

