# 作業ディレクト
setwd("./MMM_01") 
# remotes　インストール
install.packages('remotes')
# install 
remotes::install_github("facebookexperimental/Robyn/R")
# reticulate
install.packages("reticulate")

#dplyr インストール
install.packages("dplyr")
library(dplyr)

library(reticulate)

virtualenv_create("r-reticulate")
py_install("nevergrad", pip = TRUE)
use_virtualenv("r-reticulate", required = TRUE)

use_python("~/.virtualenvs/r-reticulate/bin/python")

#DEMO
library(Robyn)

dt_simulated = read.csv(file='sample.csv')
#data("dt_simulated_weekly")
#change var names
dt_simulated = dplyr::select(dt_simulated,datetime=1, Net.Spend=2 ,Tv.Spend=3 ,temperature=4,
              rain=5, revenue=6, Weekend.FLG=7,dplyr::everything())
head(dt_simulated)

data("dt_prophet_holidays")
head(dt_prophet_holidays)

#create object path
robyn_object <- "/Users/mitsuru_urushibata/MMM_01//MyRobyn.RDS"

#input 
InputCollect <- robyn_inputs(
  dt_input = dt_simulated #入力する元データ
  ,dt_holidays = dt_prophet_holidays #祝日データ
  ,date_var = "datetime" # 以下のフォーマットで"2020-01-01"
  ,dep_var = "revenue" # 売上高やCVなどの従属変数
  ,dep_var_type = "revenue" # 売上高かCVフラグか
  ,prophet_vars = c("trend", "season") # "trend","season", "weekday" & "holiday"
  ,prophet_country = "US"# 国名 祝日 デフォルトで日本がないため一旦USとしておく
  ,context_vars = c("temperature", "rain","Weekend.FLG") # イベント情報
  ,paid_media_spends = c("Net.Spend","Tv.Spend") # メディア投下
  ,paid_media_vars = c("Net.Spend","Tv.Spend") # メディア
  # paid_media_vars must have same order as paid_media_spends. Use media exposure metrics like
  # impressions, GRP etc. If not applicable, use spend instead.
  #,organic_vars = c("newsletter") # PRなどの非広告メディア
  ,factor_vars = c("Weekend.FLG") # イベント因子
  ,window_start = "2019-04-01" # モデル構築に使用するデータの開始日
  ,window_end = "2020-03-31" # モデル構築に使用するデータの終了日
  ,adstock = "geometric" # adstockの形状
)
print(InputCollect)

plot_adstock(plot = FALSE)
plot_saturation(plot = FALSE)

#使用するハイパーパラメータ表示
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

#上記のハイパーパラメータの可変領域を設定
hyperparameters <- list(
  Net.Spend_alphas = c(0.5, 3)
  ,Net.Spend_gammas = c(0.3, 1)
  ,Net.Spend_thetas = c(0, 0.3)
  
  ,Tv.Spend_alphas = c(0.5, 3)
  ,Tv.Spend_gammas = c(0.3, 1)
  ,Tv.Spend_thetas = c(0.1, 0.4)
)

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)

# dt_calibration <- data.frame(
#   channel = c("facebook_S",  "tv_S", "facebook_S")
#   # channel name must in paid_media_vars
#   , liftStartDate = as.Date(c("2018-05-01", "2017-11-27", "2018-07-01"))
#   # liftStartDate must be within input data range
#   , liftEndDate = as.Date(c("2018-06-10", "2017-12-03", "2018-07-20"))
#   # liftEndDate must be within input data range
#   , liftAbs = c(400000, 300000, 200000) # Provided value must be
#   # tested on same campaign level in model and same metric as dep_var_type
# )

################################################################
#### Step 3: Build initial model

## Run all trials and iterations. Use ?robyn_run to check parameter definition
OutputModels <- robyn_run(
  InputCollect = InputCollect # feed in all model specification
  #, cores = NULL # default
  #, add_penalty_factor = FALSE # Untested feature. Use with caution.
  , iterations = 2000 # recommended for the dummy dataset
  , trials = 5 # recommended for the dummy dataset
  , outputs = FALSE # outputs = FALSE disables direct model output
)
print(OutputModels)

OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

## Calculate Pareto optimality, cluster and export results and plots. See ?robyn_outputs
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels
  , pareto_fronts = 1
  # , calibration_constraint = 0.1 # range c(0.01, 0.1) & default at 0.1
  , csv_out = "pareto" # "pareto" or "all"
  , clusters = TRUE # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  , plot_pareto = TRUE # Set to FALSE to deactivate plotting and saving model one-pagers
  , plot_folder = robyn_object # path for plots export
)
print(OutputCollect)

select_model <- "2_399_4" # select one from above
robyn_save(robyn_object = robyn_object # model object location and name
           , select_model = select_model # selected model ID
           , InputCollect = InputCollect # all model input
           , OutputCollect = OutputCollect # all model output
)

################################################################
#### Step 5: Get budget allocation based on the selected model above

## Budget allocation result requires further validation. Please use this recommendation with caution.
## Don't interpret budget allocation result if selected model above doesn't meet business expectation.

# Check media summary for selected model
OutputCollect$xDecompAgg[solID == select_model & !is.na(mean_spend)
                         , .(rn, coef,mean_spend, mean_response, roi_mean
                             , total_spend, 
                             total_response=xDecompAgg, roi_total, solID)]

# Run ?robyn_allocator to check parameter definition
# Run the "max_historical_response" scenario: "What's the revenue lift potential with the
# same historical spend level and what is the spend mix?"
AllocatorCollect <- robyn_allocator(
  InputCollect = InputCollect
  , OutputCollect = OutputCollect
  , select_model = select_model
  , scenario = "max_historical_response"
  , channel_constr_low = c(0.01, 0.01) #メディア数と同じ長さが必要
  , channel_constr_up = c(10, 10)
)
print(AllocatorCollect)
AllocatorCollect$dt_optimOut

# Run the "max_response_expected_spend" scenario: "What's the maximum response for a given
# total spend based on historical saturation and what is the spend mix?" "optmSpendShareUnit"
# is the optimum spend share.
AllocatorCollect <- robyn_allocator(
  InputCollect = InputCollect
  , OutputCollect = OutputCollect
  , select_model = select_model
  , scenario = "max_response_expected_spend"
  , channel_constr_low = c(0.01, 0.01) #メディア数と同じ長さが必要
  , channel_constr_up = c(10, 10)
  , expected_spend = 65600940 # Total spend to be simulated
  , expected_spend_days = 366 # Duration of expected_spend in days
)
print(AllocatorCollect)
AllocatorCollect$dt_optimOut

## A csv is exported into the folder for further usage. Check schema here:
## https://github.com/facebookexperimental/Robyn/blob/main/demo/schema.R

## QA optimal response
if (TRUE) {
  cat("QA if results from robyn_allocator and robyn_response agree: ")
  select_media <- "Net.Spend"
  optimal_spend <- AllocatorCollect$dt_optimOut[channels== select_media, optmSpendUnit]
  optimal_response_allocator <- AllocatorCollect$dt_optimOut[channels== select_media, optmResponseUnit]
  optimal_response <- robyn_response(
    robyn_object = robyn_object,
    select_build = 0,
    media_metric = select_media,
    metric_value = optimal_spend)
  cat(round(optimal_response_allocator) == round(optimal_response$response), "( ")
  plot(optimal_response$plot)
  cat(optimal_response$response, "==", optimal_response_allocator, ")\n")
}

################################################################
#### Step 6: Model refresh based on selected model and saved Robyn.RDS object - Alpha

## NOTE: must run robyn_save to select and save an initial model first, before refreshing below
## The robyn_refresh() function is suitable for updating within "reasonable periods"
## Two situations are considered better to rebuild model:
## 1, most data is new. If initial model has 100 weeks and 80 weeks new data is added in refresh,
## it might be better to rebuild the model
## 2, new variables are added

# Run ?robyn_refresh to check parameter definition
Robyn <- robyn_refresh(
  robyn_object = robyn_object
  , dt_input = dt_simulated
  , dt_holidays = dt_prophet_holidays
  , refresh_steps = 13
  , refresh_mode = "auto"
  , refresh_iters = 1000 # 1k is estimation. Use refresh_mode = "manual" to try out.
  , refresh_trials = 3
  , clusters = TRUE
)

## Besides plots: there're 4 csv output saved in the folder for further usage
# report_hyperparameters.csv, hyperparameters of all selected model for reporting
# report_aggregated.csv, aggregated decomposition per independent variable
# report_media_transform_matrix.csv, all media transformation vectors
# report_alldecomp_matrix.csv,all decomposition vectors of independent variables


################################################################
#### Step 7: Get budget allocation recommendation based on selected refresh runs

# Run ?robyn_allocator to check parameter definition
AllocatorCollect <- robyn_allocator(
  robyn_object = robyn_object
  #, select_build = 1 # Use third refresh model
  , scenario = "max_response_expected_spend"
  , channel_constr_low = c(0.7, 0.7)
  , channel_constr_up = c(1.2, 1.5)
  , expected_spend = 2000000 # Total spend to be simulated
  , expected_spend_days = 14 # Duration of expected_spend in days
)
print(AllocatorCollect)
AllocatorCollect$plots
AllocatorCollect$dt_optimOut

################################################################
#### Step 8: get marginal returns

## Example of how to get marginal ROI of next 1000$ from the 80k spend level for search channel

# Run ?robyn_response to check parameter definition

## -------------------------------- NOTE v3.6.0 CHANGE !!! ---------------------------------- ##
## The robyn_response() function can now output response for both spends and exposures (imps,
## GRP, newsletter sendings etc.) as well as plotting individual saturation curves. New
## argument names "media_metric" and "metric_value" instead of "paid_media_var" and "spend"
## are now used to accommodate this change. Also the returned output is a list now and
## contains also the plot.
## ------------------------------------------------------------------------------------------ ##

# Get response for 80k from result saved in robyn_object
Spend1 <- 60000
Response1 <- robyn_response(
  robyn_object = robyn_object
  #, select_build = 1 # 2 means the second refresh model. 0 means the initial model
  , media_metric = "Net.Spend"
  , metric_value = Spend1)
Response1$response/Spend1 # ROI for search 80k
Response1$plot

# Get response for 81k
Spend2 <- Spend1 + 1000
Response2 <- robyn_response(
  robyn_object = robyn_object
  #, select_build = 1
  , media_metric = "Net.Spend"
  , metric_value = Spend2)
Response2$response/Spend2 # ROI for search 81k
Response2$plot

# Marginal ROI of next 1000$ from 80k spend level for search
(Response2$response - Response1$response)/(Spend2 - Spend1)

## Example of getting paid media exposure response curves
imps <- 5000000
response_imps <- robyn_response(
  robyn_object = robyn_object
  #, select_build = 1
  , media_metric = "facebook_I"
  , metric_value = imps)
response_imps$response / imps * 1000
response_imps$plot

## Example of getting organic media exposure response curves
sendings <- 30000
response_sending <- robyn_response(
  robyn_object = robyn_object
  #, select_build = 1
  , media_metric = "newsletter"
  , metric_value = sendings)
response_sending$response / sendings * 1000
response_sending$plot

################################################################
#### Optional: get old model results

# # Get old hyperparameters and select model
# dt_hyper_fixed <- data.table::fread("~/Desktop/2022-02-21 11.29 rf11/pareto_hyperparameters.csv")
# select_model <- "1_51_11"
# dt_hyper_fixed <- dt_hyper_fixed[solID == select_model]
# 
# OutputCollectFixed <- robyn_run(
#   # InputCollect must be provided by robyn_inputs with same dataset and parameters as before
#   InputCollect = InputCollect
#   , plot_folder = robyn_object
#   , dt_hyper_fixed = dt_hyper_fixed)
# 
# # Save Robyn object for further refresh
# robyn_save(robyn_object = robyn_object
#            , select_model = select_model
#            , InputCollect = InputCollect
#            , OutputCollect = OutputCollectFixed)