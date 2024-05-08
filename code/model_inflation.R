# graduate thesis replication materials
# «The Impact of Economic Nationalism in the European Union on the 
# Economic Development of Member States: A Comparative Empirical Analysis»
# author: Vladislav Rubanov
# institution: HSE University
# year: 2024
# this file provides code for replication Inflation Rates Model


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
################# Inflation Rates Model  #################
### 3-lags Model
### Multiple R-squared:  0.7057,	Adjusted R-squared:  0.6874
model_inflation_3_lags <- lm(inflation ~ 
                      + inflation_lag_1
                      + inflation_lag_2
                      + inflation_lag_3
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
                      , data = panel)

### FINAL SPECIFICATION (2-lags Model)
### Multiple R-squared:  0.6927,	Adjusted R-squared:  0.6754
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
              , data = panel)

### check summary
summary(model_inflation)
### compare with and without 3rd inflation lag
mtable(model_inflation_3_lags, model_inflation)


### check ME
me_inflation <- margins(model_inflation, at = list(export_market_penetration_index_log = 0:4))
summary(me_inflation)  # large output


### ME of log(Foreign Workers Number) t_0
ggplot(me_inflation, aes(x = export_market_penetration_index_log)) + 
  geom_line(aes(y = dydx_f_workers_num_log)) +
  geom_line(aes(y = dydx_f_workers_num_log+1.645*sqrt(Var_dydx_f_workers_num_log)), color = 'darkgrey', linetype = 2) +
  geom_line(aes(y = dydx_f_workers_num_log-1.645*sqrt(Var_dydx_f_workers_num_log)), color = 'darkgrey', linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Dependent Variable: Inflation Rate") +
  theme(plot.title = element_text (hjust = 0.5, size=14)) +
  xlab("log(Export Market Penetration Index)") + 
  ylab(expression(Marginal~Effect~of~log(Foreign~Workers~Number[t-0]))) +
  theme(axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 10.5))

### ME of log(Foreign Workers Number) t_1
ggplot(me_inflation, aes(x = export_market_penetration_index_log)) + 
  geom_line(aes(y = dydx_f_workers_num_log_lag_1)) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_1+1.645*sqrt(Var_dydx_f_workers_num_log_lag_1)), color = 'darkgrey', linetype = 2) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_1-1.645*sqrt(Var_dydx_f_workers_num_log_lag_1)), color = 'darkgrey', linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Dependent Variable: Inflation Rate") +
  xlab("log(Export Market Penetration Index)") + 
  ylab(expression(Marginal~Effect~of~log(Foreign~Workers~Number[t-1]))) +
  theme(plot.title = element_text (hjust = 0.5, size=14),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 10.5))

### ME of log(Foreign Workers Number) t_2
ggplot(me_inflation, aes(x = export_market_penetration_index_log)) + 
  geom_line(aes(y = dydx_f_workers_num_log_lag_2)) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_2+1.645*sqrt(Var_dydx_f_workers_num_log_lag_2)), color = 'darkgrey', linetype = 2) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_2-1.645*sqrt(Var_dydx_f_workers_num_log_lag_2)), color = 'darkgrey', linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Dependent Variable: Inflation Rate") +
  xlab("log(Export Market Penetration Index)") + 
  ylab(expression(Marginal~Effect~of~log(Foreign~Workers~Number[t-2]))) +
  theme(plot.title = element_text (hjust = 0.5, size=14),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 10.5))


### robustness tests
### heteroskedasticity adjustment 
bptest(model_inflation)  # there is heteroscedasticity
# you can see these standard errors in the paper
coeftest(model_inflation, vcov = vcovHC, type = "HC3")  # everything is ok


### outliers and leverages observations
# studentized residuals (outliers)
car::outlierTest(model_inflation)  
# diagnostic plots (Cook's distance, hat values, studentized residuals, p-value Bonferroni)
car::influenceIndexPlot(model_inflation)  
# hat-values VS Studentized residuals
car::influencePlot(model_inflation)  
# Cook's measure only
ols_plot_cooksd_bar(model_inflation)
# DFBETAS only
ols_plot_dfbetas(model_inflation) 


### data without outliers
panel_inflation <- panel %>% 
  filter(!(country == 'Lithuania' & period == 2008))

model_inflation_sm <- lm(inflation ~ 
                         + inflation_lag_1
                         + inflation_lag_2
                         + inflation_lag_3
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
                         , data = panel)

### check summary
summary(model_inflation_sm)
### compare results
mtable(model_inflation, model_inflation_sm)


### check ME (again)
me_inflation_small <- margins(model_inflation_sm, at = list(export_market_penetration_index_log = 0:4))
summary(me_inflation_small)  # large output


### ME of log(Foreign Workers Number) t_0 (again)
ggplot(me_inflation_small, aes(x = export_market_penetration_index_log)) + 
  geom_line(aes(y = dydx_f_workers_num_log)) +
  geom_line(aes(y = dydx_f_workers_num_log+1.645*sqrt(Var_dydx_f_workers_num_log)), color = 'darkgrey', linetype = 2) +
  geom_line(aes(y = dydx_f_workers_num_log-1.645*sqrt(Var_dydx_f_workers_num_log)), color = 'darkgrey', linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Dependent Variable: Inflation Rate") +
  theme(plot.title = element_text (hjust = 0.5, size=14)) +
  xlab("log(Export Market Penetration Index)") + 
  ylab(expression(Marginal~Effect~of~log(Foreign~Workers~Number[t-0]))) +
  theme(axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 10.5))

### ME of log(Foreign Workers Number) t_1 (again)
ggplot(me_inflation_small, aes(x = export_market_penetration_index_log)) + 
  geom_line(aes(y = dydx_f_workers_num_log_lag_1)) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_1+1.645*sqrt(Var_dydx_f_workers_num_log_lag_1)), color = 'darkgrey', linetype = 2) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_1-1.645*sqrt(Var_dydx_f_workers_num_log_lag_1)), color = 'darkgrey', linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Dependent Variable: Inflation Rate") +
  xlab("log(Export Market Penetration Index)") + 
  ylab(expression(Marginal~Effect~of~log(Foreign~Workers~Number[t-1]))) +
  theme(plot.title = element_text (hjust = 0.5, size=14),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 10.5))

### ME of log(Foreign Workers Number) t_2 (again)
ggplot(me_inflation_small, aes(x = export_market_penetration_index_log)) + 
  geom_line(aes(y = dydx_f_workers_num_log_lag_2)) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_2+1.645*sqrt(Var_dydx_f_workers_num_log_lag_2)), color = 'darkgrey', linetype = 2) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_2-1.645*sqrt(Var_dydx_f_workers_num_log_lag_2)), color = 'darkgrey', linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Dependent Variable: Inflation Rate") +
  xlab("log(Export Market Penetration Index)") + 
  ylab(expression(Marginal~Effect~of~log(Foreign~Workers~Number[t-2]))) +
  theme(plot.title = element_text (hjust = 0.5, size=14),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 10.5))


### multicollinearity assessment
# calculate GVIF
# remember that for models with interaction variables 
# it is more correct to use a df normalised GVIF
vif_inflation <- vif(model_inflation, type="predictor")
vif_inflation
