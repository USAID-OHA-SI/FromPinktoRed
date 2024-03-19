# PROJECT:  FromPinktoRed
# AUTHOR:   J. Stephens | USAID
# PURPOSE:  Calculate statistical relationship between categorical independent 
# variables: country, ageasentered, scrn_type
# and continuous independent variable: percent treated
# LICENSE:  MIT
# DATE:     2024-03-19
# UPDATED:  2024-03-19


# DEPENDENCIES ------------------------------------------------------------

# install.packages("car") # Uncomment and run if 'car' package is not installed
 library(car)
#install.packages("modelr")
 library(modelr)


# RESHAPE  ----------------------------------------------------------------

# Beginning with df_long from obj3
# Note: Errors when attempted with each variable alone 
# (ie. collapsed data set to furthest level)
# thus, completed with 3 variables in data set
pos_tx_df_ou_widefirst <- df_long %>%
  pivot_wider(values_from = results, names_from = indicator) %>% 
  select(-c(cxca_status,cxca_tx, standardizeddisaggregate, fiscal_year, qtr)) %>% 
  group_by(across(country:ageasentered)) %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
  dplyr::mutate(pos_tx_perc=(CXCA_TX/CXCA_SCRN_POS)*100)   %>% 
  filter(!is.nan(pos_tx_perc) & is.finite(pos_tx_perc) & pos_tx_perc != 0)

# view(pos_tx_df_ou_widefirst)


# INFERENTIAL STATISTICS - BIVARIATE -------------------------------------------
# One-way ANOVA test to compare means between categorical variables

anova_result <- aov(pos_tx_perc ~ country, data = pos_tx_df_ou_widefirst)
summary(anova_result)

########## country SIGNIFICANT ###############
# Df  Sum Sq Mean Sq F value  Pr(>F)   
# country       7  142044   20292   2.926 0.00628 **
#   Residuals   187 1297016    6936                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova_result <- aov(pos_tx_perc ~ ageasentered, data = pos_tx_df_ou_widefirst)
summary(anova_result)

########## ageasentered SIGNIFICANT ###############
# Df  Sum Sq Mean Sq F value   Pr(>F)    
# ageasentered   8  261167   32646   5.155 8.07e-06 ***
#   Residuals    186 1177892    6333                  

anova_result <- aov(pos_tx_perc ~ scrn_type, data = pos_tx_df_ou_widefirst)
summary(anova_result)

########## scrn_type NOT SIGNIFICANT ###############
# Df  Sum Sq Mean Sq F value Pr(>F)
# scrn_type     2    3771    1886   0.252  0.777
# Residuals   192 1435288    7475               
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# INFERENTIAL STATISTICS - BIVARIATE -------------------------------------------
## Alternative method with model - QC'd ANOVA

mod_ou<- pos_tx_df_ou_widefirst %>%
  lm(pos_tx_perc~country, .)

mod_ou_sum<- pos_tx_df_ou_widefirst %>%
  lm(pos_tx_perc~country, .)%>%
  summary()

mod_ou_sum
# Call:
#   lm(formula = pos_tx_perc ~ country, data = .)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -98.80 -18.39 -10.20   2.75 736.38 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         87.188     17.000   5.129 7.26e-07 ***
#   countryEswatini      6.771     24.041   0.282  0.77855    
# countryLesotho      -2.686     24.885  -0.108  0.91418    
# countryMalawi       -8.344     23.575  -0.354  0.72377    
# countryMozambique   13.598     23.575   0.577  0.56475    
# countryNamibia      76.429     24.041   3.179  0.00173 ** 
#   countryZambia       -2.273     23.575  -0.096  0.92330    
# countryZimbabwe    -17.165     24.041  -0.714  0.47612    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 83.28 on 187 degrees of freedom
# Multiple R-squared:  0.09871,	Adjusted R-squared:  0.06497 
# F-statistic: 2.926 on 7 and 187 DF,  p-value: 0.006279



mod_age<- pos_tx_df_ou_widefirst %>%
  lm(pos_tx_perc~ageasentered, .)

mod_age_sum<- pos_tx_df_ou_widefirst %>%
  lm(pos_tx_perc~ageasentered, .) %>%
  summary()

mod_age_sum
# Call:
#   lm(formula = pos_tx_perc ~ ageasentered, data = .)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -191.71  -15.87   -4.36    7.93  683.29 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               73.070     19.301   3.786 0.000207 ***
#   ageasentered20-24          2.162     25.698   0.084 0.933029    
# ageasentered25-29          5.860     25.227   0.232 0.816563    
# ageasentered30-34          6.193     25.227   0.245 0.806348    
# ageasentered35-39          7.452     25.227   0.295 0.768008    
# ageasentered40-44          9.800     25.227   0.388 0.698114    
# ageasentered45-49         13.618     25.227   0.540 0.589975    
# ageasentered50+           64.903     25.227   2.573 0.010868 *  
#   ageasenteredUnknown Age  143.636     30.004   4.787 3.44e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 79.58 on 186 degrees of freedom
# Multiple R-squared:  0.1815,	Adjusted R-squared:  0.1463 
# F-statistic: 5.155 on 8 and 186 DF,  p-value: 8.068e-06


mod_scrn<- pos_tx_df_ou_widefirst %>%
  lm(pos_tx_perc~scrn_type, .)

mod_scrn_sum<- pos_tx_df_ou_widefirst %>%
  lm(pos_tx_perc~scrn_type, .) %>%
  summary()

mod_scrn_sum
# Call:
#   lm(formula = pos_tx_perc ~ scrn_type, data = .)
# 
# Residual standard error: 86.46 on 192 degrees of freedom
# Multiple R-squared:  0.002621,	Adjusted R-squared:  -0.007769 
# F-statistic: 0.2523 on 2 and 192 DF,  p-value: 0.7773




# INFERENTIAL STATISTICS - MULTIVARIATE ----------------------------------------
## Adding significant bivariates to model
### ageasentered & country

mod_ou_age<- pos_tx_df_ou_widefirst %>%
  lm(pos_tx_perc~country+ageasentered, .)

mod_ou_age_sum<- pos_tx_df_ou_widefirst %>%
  lm(pos_tx_perc~country+ageasentered, .) %>%
  summary()

mod_ou_age_sum
# SAME SIGNIFICANT VALUES AS BIVARIATE

# Call:
# lm(formula = pos_tx_perc ~ country + ageasentered, data = .)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -175.04  -25.32    1.62   14.82  617.55 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              59.0121    24.4092   2.418 0.016626 *  
#   countryEswatini          24.5021    22.2771   1.100 0.272863    
# countryLesotho           13.4364    22.9893   0.584 0.559645    
# countryMalawi            -6.0819    21.6246  -0.281 0.778843    
# countryMozambique        21.2850    21.6848   0.982 0.327640    
# countryNamibia           82.4082    22.0622   3.735 0.000252 ***
#   countryZambia             5.4139    21.6848   0.250 0.803135    
# countryZimbabwe           0.5662    22.2771   0.025 0.979752    
# ageasentered20-24        -2.4686    24.7194  -0.100 0.920564    
# ageasentered25-29         2.2266    24.3162   0.092 0.927144    
# ageasentered30-34         2.5594    24.3162   0.105 0.916291    
# ageasentered35-39         3.8188    24.3162   0.157 0.875384    
# ageasentered40-44         6.1663    24.3162   0.254 0.800106    
# ageasentered45-49         9.9841    24.3162   0.411 0.681862    
# ageasentered50+          61.2695    24.3162   2.520 0.012621 *  
#   ageasenteredUnknown Age 141.0296    29.3453   4.806 3.25e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 76.32 on 179 degrees of freedom
# Multiple R-squared:  0.2755,	Adjusted R-squared:  0.2148 
# F-statistic: 4.537 on 15 and 179 DF,  p-value: 2.837e-07

mod_ou__age<- pos_tx_df_ou_widefirst %>%
  lm(pos_tx_perc~country*ageasentered, .)

mod_ou__age_sum<- pos_tx_df_ou_widefirst %>%
  lm(pos_tx_perc~country*ageasentered, .)%>%
  summary()

mod_ou__age
# output too complicated to post


################################################################################
# DATA VISUALIZATIONS--------------------------------------------------------
# NOT ALL TESTED

ggplot(pos_tx_df_ou_widefirst) +
  geom_point(aes(country, pos_tx_perc))

ggplot(pos_tx_df_ou_widefirst) +
  geom_point(aes(ageasentered, pos_tx_perc))

barplot(pos_tx_df_ou_wide$pos_tx_perc, names.arg = pos_tx_df_ou_wide$country,
        xlab = "Country", ylab = "Achievement Percentage", main = "Achievement Percentage by Country",
        col = rainbow(length(pos_tx_df_ou_wide$country)))


grid <- pos_tx_df_ou_widefirst %>% 
  data_grid(country) %>% 
  add_predictions(mod_ou)
grid    

ggplot(pos_tx_df_ou_widefirst, aes(country)) + 
  geom_point(aes(y = pos_tx_perc)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

###########################################################################
#multiple categorical variables

ggplot(pos_tx_df_ou_widefirst, aes(country, pos_tx_perc)) +
  geom_point(aes(colour=ageasentered))


mod_ou_age<- pos_tx_df_ou_widefirst %>%
  lm(pos_tx_perc~country+ageasentered, .) 
# mod_ou_age

mod_ou__age<- pos_tx_df_ou_widefirst %>%
  lm(pos_tx_perc~country*ageasentered, .) 
# mod_ou__age

# models vis
grid <- pos_tx_df_ou_widefirst %>% 
  data_grid(country, ageasentered) %>% 
  gather_predictions(mod_ou_age, mod_ou__age)
# Warning message:
# In predict.lm(model, data) :
#   prediction from a rank-deficient fit may be misleading
# grid


ggplot(pos_tx_df_ou_widefirst, aes(country, pos_tx_perc, colour = ageasentered)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

ggplot(pos_tx_df_ou_widefirst, aes(country, pos_tx_perc, colour = ageasentered)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)


data <- pos_tx_df_ou_widefirst %>% 
  gather_residuals(mod_ou_age, mod_ou__age)

ggplot(data, aes(country, resid, colour = ageasentered)) + 
  geom_point() + 
  facet_grid(model ~ ageasentered)




################################################################################
# ALTERNATIVE WAY TO VIEW CORRELATION - UNTESTED


# 
# # Load required libraries
# library(dplyr) # for data manipulation
# library(purrr) # for functional programming
# # install.packages("vcd") #unable to install
# # library(vcd) # for Cramer's V calculation
# # install.packages("corrr") #unable to install
# # library(corrr) # for correlation analysis
# 
# # Example dataset (replace this with your own data)
# # Let's assume you have a dataframe called 'data' with categorical variables 'var1', 'var2', and 'var3'
# # and you want to assess collinearity among these variables
# 
# # Chi-square test of independence
# chi_square_test <- function(x, y) {
#   chisq.test(table(x, y))$p.value
# }
# 
# # Compute p-values for all pairwise combinations of categorical variables
# p_values <- combn(names(data), 2, FUN = function(names) {
#   chi_square_test(data[[names[1]]], data[[names[2]]])
# })
# 
# # Print p-values
# colnames(p_values) <- combn(names(data), 2, simplify = TRUE, FUN = paste, collapse = "_vs_")
# print(p_values)



