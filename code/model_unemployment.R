# graduate thesis replication materials
# «The Impact of Economic Nationalism in the European Union on the 
# Economic Development of Member States: A Comparative Empirical Analysis»
# author: Vladislav Rubanov
# institution: HSE University
# year: 2024
# this file provides code for replication Unemployment Rates (log) Model


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
############ Unemployment Rates (log) Model  #############

### FINAL SPECIFICATION
### Multiple R-squared:   0.94,	Adjusted R-squared:  0.9382
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
                         , data = panel)

### check summary
summary(model_unemployment)


### check ME
me_unemployment <- margins(model_unemployment, at = list(export_market_penetration_index_log = 0:4))
summary(me_unemployment)  # large output


### ME of log(Foreign Workers Number)
ggplot(me_unemployment, aes(x = export_market_penetration_index_log)) + 
  geom_line(aes(y = dydx_f_workers_num_log_lag_1)) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_1+1.645*sqrt(Var_dydx_f_workers_num_log_lag_1)), color='darkgrey', linetype = 2) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_1-1.645*sqrt(Var_dydx_f_workers_num_log_lag_1)), color='darkgrey', linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Dependent Variable: Unemployment Rate (log)") +
  theme(plot.title = element_text (hjust = 0.5, size=14)) +
  xlab("Export Market Penetration Index (log)") + 
  ylab(expression(ME~of~Foreign~Workers~Number[t-1]~(log))) + 
  theme(axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12))


### robustness tests
### heteroskedasticity adjustment
bptest(model_unemployment)  # there is heteroscedasticity
# you can see these standard errors in the paper
coeftest(model_unemployment, vcov = vcovHC, type = "HC3")  # everything is ok


### outliers and leverages observations
# studentized residuals (outliers)
car::outlierTest(model_unemployment)  
# diagnostic plots (Cook's distance, hat values, studentized residuals, p-value Bonferroni)
car::influenceIndexPlot(model_unemployment)  
# hat-values VS Studentized residuals
car::influencePlot(model_unemployment)  
# Cook's measure only
ols_plot_cooksd_bar(model_unemployment)
# DFBETAS only
ols_plot_dfbetas(model_unemployment) 


### data without outliers
panel_unemployment <- panel %>% 
  filter(!((country == 'Estonia' & period == 2009) | (country == 'Estonia' & period == 2020)))

model_unemployment_small <- lm(unemployment_log ~ 
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
summary(model_unemployment_small)
### compare results
mtable(model_unemployment, model_unemployment_small)  # equal


### check ME (again)
me_unemployment_sm <- margins(model_unemployment_small, at = list(export_market_penetration_index_log = 0:4))
summary(me_unemployment_sm)


### ME of log(Foreign Workers Number) (again)
ggplot(me_unemployment, aes(x = export_market_penetration_index_log)) + 
  geom_line(aes(y = dydx_f_workers_num_log_lag_1)) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_1+1.645*sqrt(Var_dydx_f_workers_num_log_lag_1)), color='darkgrey', linetype = 2) +
  geom_line(aes(y = dydx_f_workers_num_log_lag_1-1.645*sqrt(Var_dydx_f_workers_num_log_lag_1)), color='darkgrey', linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Dependent Variable: log(Unemployment Rate)") +
  theme(plot.title = element_text (hjust = 0.5, size=14)) +
  xlab("log(Export Market Penetration Index)") + 
  ylab(expression(Marginal~Effect~of~log(Foreign~Workers~Number[t-1]))) + 
  theme(axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 10.5))


### multicollinearity assessment
# calculate GVIF
# remember that for models with interaction variables 
# it is more correct to use a df normalised GVIF
vif_unemployment <- vif(model_unemployment, type="predictor")
vif_unemployment
