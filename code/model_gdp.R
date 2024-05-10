# graduate thesis replication materials
# «The Impact of Economic Nationalism in the European Union on the 
# Economic Development of Member States: A Comparative Empirical Analysis»
# author: Vladislav Rubanov
# institution: HSE University
# year: 2024
# this file provides code for replication GDP Growth Rates Model


######################## PACKAGES ########################
### install packages if you need
## directly from cran
# install.packages("margins") 
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


###################### PREPARE DATA ######################
### read data
df <- read.csv('data.csv')
# drop service cols
df <- df[-c(1, 2)]  

### create panel data format
panel <- pdata.frame(df, index = c("country", "period"), row.names = TRUE)
### look at the data
head(panel[, c(1:8)], 10)

### create lagged variables
panel$w_avg_tariff_lag_1 <- stats::lag(panel$w_avg_tariff, 1)
panel$duty_free_import_usd_log_lag_1 <- stats::lag(panel$duty_free_import_usd_log, 1)
panel$f_workers_num_log_lag_1 <- stats::lag(panel$f_workers_num_log, 1)
panel$f_workers_num_log_lag_2 <- stats::lag(panel$f_workers_num_log, 2)
panel$inflation_lag_1 <- stats::lag(panel$inflation, 1)
panel$inflation_lag_2 <- stats::lag(panel$inflation, 2)
panel$inflation_lag_3 <- stats::lag(panel$inflation, 3)
panel$unemployment_log_lag_1 <- stats::lag(panel$unemployment_log, 1)
panel$unemployment_log_lag_2 <- stats::lag(panel$unemployment_log, 2)

# panel <- na.omit(panel)  # be accurate, don't do this until you are sure


######################### MODEL ##########################
################ GDP Growth Rates Model  #################

### FINAL SPECIFICATION
### Multiple R-squared:  0.4662,	Adjusted R-squared:  0.4449 
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
                , data = panel)

### check summary
summary(model_gdp)


### check ME
me_gdp <- margins(model_gdp, at = list(export_market_penetration_index_log = 0:4))
summary(me_gdp)  # large output

### ME of Average Tariffs Rate
ggplot(me_gdp, aes(x = export_market_penetration_index_log)) + 
  geom_line(aes(y = dydx_w_avg_tariff_lag_1)) +
  geom_line(aes(y = dydx_w_avg_tariff_lag_1+1.645*sqrt(Var_dydx_w_avg_tariff_lag_1)), color = 'darkgrey', linetype = 2) + 
  geom_line(aes(y = dydx_w_avg_tariff_lag_1-1.645*sqrt(Var_dydx_w_avg_tariff_lag_1)), color = 'darkgrey', linetype = 2) + 
  geom_hline(yintercept = 0) +
  ggtitle("Dependent Variable: GDP Growth Rate") +
  theme(plot.title = element_text(hjust = 0.5, size= 14)) + 
  xlab("log(Export Market Penetration Index)") + ylab("Marginal Effect of Average Tariffs Rate") +
  theme(axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12))
  
### ME of log(Export Market Penetration Index)
ggplot(me_gdp, aes(x = export_market_penetration_index_log)) + 
  geom_line(aes(y = dydx_f_workers_num_log_lag_1)) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_1+1.645*sqrt(Var_dydx_f_workers_num_log_lag_1)), color='darkgrey', linetype = 2) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_1-1.645*sqrt(Var_dydx_f_workers_num_log_lag_1)), color='darkgrey', linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Dependent Variable: GDP Growth Rate") +
  theme(plot.title = element_text (hjust = 0.5, size=14)) +
  xlab("log(Export Market Penetration Index)") + 
  ylab(expression(Marginal~Effect~of~log(Foreign~Workers~Number[t-1]))) +
  theme(axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 10.5))


### robustness tests
### heteroskedasticity adjustment 
bptest(model_gdp)  # there is heteroscedasticity
# you can see these standard errors in the paper
coeftest(model_gdp, vcov = vcovHC, type = "HC3")  # everything is ok


### outliers and leverages observations
# studentized residuals (outliers)
car::outlierTest(model_gdp)  
# diagnostic plots (Cook's distance, hat values, studentized residuals, p-value Bonferroni)
car::influenceIndexPlot(model_gdp)  
# hat-values VS Studentized residuals
car::influencePlot(model_gdp)  
# Cook's measure only
ols_plot_cooksd_bar(model_gdp)
# DFBETAS only
ols_plot_dfbetas(model_gdp) 


### data without outliers
panel_dgp <- panel %>% 
  filter(!((country == 'Ireland' & period == 2015) | (country == 'Luxembourg' & period == 2008) | (country == 'Greece' & period == 2011)))

model_gdp_small <- lm(gdp ~ duty_free_import_usd_log
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
                      , data = panel_dgp)

### check summary
summary(model_gdp_small)
### compare results
mtable(model_gdp, model_gdp_small)


### check ME (again)
me_gdp_small <- margins(model_gdp_small, at = list(export_market_penetration_index_log = 0:4))
summary(me_gdp_small)


### ME of Average Tariffs Rate (again)
ggplot(me_gdp_small, aes(x = export_market_penetration_index_log)) + 
  geom_line(aes(y = dydx_w_avg_tariff_lag_1)) +
  geom_line(aes(y = dydx_w_avg_tariff_lag_1+1.645*sqrt(Var_dydx_w_avg_tariff_lag_1)), color = 'darkgrey', linetype = 2) + 
  geom_line(aes(y = dydx_w_avg_tariff_lag_1-1.645*sqrt(Var_dydx_w_avg_tariff_lag_1)), color = 'darkgrey', linetype = 2) + 
  geom_hline(yintercept = 0) +
  ggtitle("Dependent Variable: GDP Growth Rate") +
  theme(plot.title = element_text(hjust = 0.5, size= 14)) + 
  xlab("log(Export Market Penetration Index)") + ylab("Marginal Effect of Average Tariffs Rate") +
  theme(axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12))

### ME of log(Export Market Penetration Index) (again)
ggplot(me_gdp_small, aes(x = export_market_penetration_index_log)) + 
  geom_line(aes(y = dydx_f_workers_num_log_lag_1)) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_1+1.645*sqrt(Var_dydx_f_workers_num_log_lag_1)), color='darkgrey', linetype = 2) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_1-1.645*sqrt(Var_dydx_f_workers_num_log_lag_1)), color='darkgrey', linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Dependent Variable: GDP Growth Rate") +
  theme(plot.title = element_text (hjust = 0.5, size=14)) +
  xlab("log(Export Market Penetration Index)") + 
  ylab(expression(Marginal~Effect~of~log(Foreign~Workers~Number[t-1]))) +
  theme(axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 10.5))


### multicollinearity assessment
# calculate GVIF
# remember that for models with interaction variables 
# it is more correct to use a df normalised GVIF
vif_gdp <- vif(model_gdp, type="predictor")
vif_gdp
