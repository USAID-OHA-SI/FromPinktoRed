
# cxca_inds <- c(
#   'TX_CURR',
#   'CXCA_SCRN',
#   'CXCA_SCRN_POS',
#   'CXCA_TX')

library(selfdestructin5)


#########################   QC    #####################################

mdb_df   <- make_mdb_df(df)
mdb_df_small<-mdb_df %>% 
  filter(operatingunit == cntry,
         indicator == "TX_CURR")

view(mdb_df_small)
mdb_tbl  <- reshape_mdb_df(df, metadata$curr_pd) 
#mutate(agency = "PEPFAR") 
mdb_df_tx    <- make_mdb_tx_df(df_msd)
mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd)
#mutate(agency = "PEPFAR") 







df_viz <- df %>%
  filter(operatingunit == cntry,
         fiscal_year == metadata$curr_fy,
         indicator == "TX_CURR",
         standardizeddisaggregate == "Total Numerator")

df_viz2 <- df_viz %>%
  group_by(fiscal_year, indicator) %>%
  summarise(across(c(targets, starts_with("qtr"), cumulative), sum, na.rm = TRUE),
            .groups = "drop")

view(df_viz2)

df_test <- df %>%
  filter(operatingunit == cntry,
         fiscal_year == metadata$curr_fy,
         indicator == "CXCA_SCRN")

df_test2 <- df_test %>%
  group_by(fiscal_year, indicator, standardizeddisaggregate) %>%
  summarise(across(c(targets, starts_with("qtr"), cumulative), sum, na.rm = TRUE),
            .groups = "drop")

view(df_test2)

# MUNGE -------------------------------------------------------------------

df_testy <- df %>%
  filter(operatingunit == cntry,
         fiscal_year == metadata$curr_fy,
         indicator == "CXCA_SCRN")

df_testy2 <- df_testy %>%
  group_by(fiscal_year, indicator, standardizeddisaggregate) %>%
  summarise(across(c(targets, starts_with("qtr"), cumulative), sum, na.rm = TRUE),
            .groups = "drop")

view(df_testy2)
