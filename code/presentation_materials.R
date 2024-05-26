# graduate thesis replication materials
# «The Impact of Economic Nationalism in the European Union on the 
# Economic Development of Member States: A Comparative Empirical Analysis»
# author: Vladislav Rubanov
# institution: HSE University
# year: 2024
# this file provides extra materials for presentation


######################## PACKAGES ########################
### install packages if you need
## directly from cran
# install.packages("pcse") 
## or from github
# library(devtools)  # to download from github you need to upload this package
# install_github("leeper/margins")

### use this packages
library(plm)       # work with panel data
library(stats)     # add some lags
library(ggplot2)   # graphs creation
library(haven)
library(dplyr)
library(lmtest)
library(sandwich)
library(margins)   # margin effect estimating
library(foreign) 
library(psych)
library(memisc)    # tabulate results
library(stargazer) # create beautiful output
library(car)
library(broom)     # work with influence observations
library(olsrr)     # visualisation of influence observations
library(interflex) # testing moderation effect on robustness
library(pcse)      # panel-corrected standard errors


###################### PREPARE DATA ######################
### read data
df <- read.csv('data.csv')
# drop service cols
df <- df[-c(1, 2)]  

# a subsample for each of the three models will be created below
# this is due to the data requirements of the pcse package


######################## MODELS ##########################
################ GDP Growth Rates Model  #################
### create panel data format
panel_gdp <- pdata.frame(df, index = c("country", "period"), row.names = TRUE)

### create lagged variables
panel_gdp$w_avg_tariff_lag_1 <- stats::lag(panel_gdp$w_avg_tariff, 1)
panel_gdp$duty_free_import_usd_log_lag_1 <- stats::lag(panel_gdp$duty_free_import_usd_log, 1)
panel_gdp$f_workers_num_log_lag_1 <- stats::lag(panel_gdp$f_workers_num_log, 1)

### drop rows with NA
panel_gdp <- na.omit(panel_gdp)


### FINAL SPECIFICATION
model_gdp <- lm(gdp ~ duty_free_import_usd_log
                + duty_free_import_usd_log_lag_1
                + w_avg_tariff 
                + w_avg_tariff_lag_1
                + w_avg_tariff_lag_1 * export_market_penetration_index_log
                + trade_bal_perc_of_gdp
                + export_market_penetration_index_log
                + f_workers_num_log_lag_1
                + f_workers_num_log_lag_1 * export_market_penetration_index_log
                # controls
                + world_trade_growth_percentage
                + country_trade_growth_percentage
                + population_log
                + fdi_in_perc_of_gdp
                + fdi_out_perc_of_gdp
                + inflation
                + unemployment_log
                , data = panel_gdp)

### check summary
summary(model_gdp)

# you can see these standard errors in the paper
coeftest(model_gdp, vcov = vcovHC, type = "HC3") 

# now let us give the significance of the estimates 
# taking into account panel-corrected standard errors (PCSE)
model_gdp.pcse <- pcse(model_gdp, 
                       groupN = panel_gdp$country, 
                       groupT = panel_gdp$period)

summary(model_gdp.pcse)


############ Unemployment Rates (log) Model  #############
### create panel data format
panel_unemployment <- pdata.frame(df, index = c("country", "period"), row.names = TRUE)

### create lagged variables
panel_unemployment$w_avg_tariff_lag_1 <- stats::lag(panel_unemployment$w_avg_tariff, 1)
panel_unemployment$f_workers_num_log_lag_1 <- stats::lag(panel_unemployment$f_workers_num_log, 1)
panel_unemployment$unemployment_log_lag_1 <- stats::lag(panel_unemployment$unemployment_log, 1)
panel_unemployment$unemployment_log_lag_2 <- stats::lag(panel_unemployment$unemployment_log, 2)

### drop rows with NA
panel_unemployment <- na.omit(panel_unemployment)


### FINAL SPECIFICATION
model_unemployment <- lm(unemployment_log ~ 
                         + unemployment_log_lag_1
                         + unemployment_log_lag_2
                         + w_avg_tariff
                         + w_avg_tariff_lag_1
                         + hh_market_concentration_index_log
                         + export_market_penetration_index_log
                         + f_workers_num_log_lag_1
                         + f_workers_num_log_lag_1 * export_market_penetration_index_log
                         + trade_bal_perc_of_gdp
                         # controls
                         + population_log
                         + world_trade_growth_percentage
                         + inflation
                         , data = panel_unemployment)

### check summary
summary(model_unemployment)

# you can see these standard errors in the paper
coeftest(model_unemployment, vcov = vcovHC, type = "HC3") 

# now let us give the significance of the estimates 
# taking into account panel-corrected standard errors (PCSE)
model_unemployment.pcse <- pcse(model_unemployment, 
                                groupN = panel_unemployment$country, 
                                groupT = panel_unemployment$period)

summary(model_unemployment.pcse)


################# Inflation Rates Model  #################
### create panel data format
panel_inflation <- pdata.frame(df, index = c("country", "period"), row.names = TRUE)

### create lagged variables
panel_inflation$w_avg_tariff_lag_1 <- stats::lag(panel_inflation$w_avg_tariff, 1)
panel_inflation$duty_free_import_usd_log_lag_1 <- stats::lag(panel_inflation$duty_free_import_usd_log, 1)
panel_inflation$f_workers_num_log_lag_1 <- stats::lag(panel_inflation$f_workers_num_log, 1)
panel_inflation$f_workers_num_log_lag_2 <- stats::lag(panel_inflation$f_workers_num_log, 2)
panel_inflation$inflation_lag_1 <- stats::lag(panel_inflation$inflation, 1)
panel_inflation$inflation_lag_2 <- stats::lag(panel_inflation$inflation, 2)

### drop rows with NA
panel_inflation <- na.omit(panel_inflation)


### FINAL SPECIFICATION (1-and-2-lags Model)
model_inflation <- lm(inflation ~ 
                      + inflation_lag_1
                      + inflation_lag_2
                      + duty_free_import_usd_log
                      + duty_free_import_usd_log_lag_1
                      + export_market_penetration_index_log
                      + w_avg_tariff
                      + w_avg_tariff_lag_1
                      + f_workers_num_log
                      + f_workers_num_log * export_market_penetration_index_log
                      + f_workers_num_log_lag_1
                      + f_workers_num_log_lag_1 * export_market_penetration_index_log
                      + f_workers_num_log_lag_2
                      + f_workers_num_log_lag_2 * export_market_penetration_index_log
                      # controls
                      + world_trade_growth_percentage
                      + population_log
                      + gdp
                      + unemployment_log
                      + trade_bal_perc_of_gdp
                      + fdi_in_perc_of_gdp
                      + fdi_out_perc_of_gdp
                      + country_trade_growth_percentage
                      , data = panel_inflation)

### check summary
summary(model_inflation)

# you can see these standard errors in the paper
coeftest(model_inflation, vcov = vcovHC, type = "HC3") 

# now let us give the significance of the estimates 
# taking into account panel-corrected standard errors (PCSE)
model_inflation.pcse <- pcse(model_inflation, 
                             groupN = panel_inflation$country, 
                             groupT = panel_inflation$period)

summary(model_inflation.pcse)
