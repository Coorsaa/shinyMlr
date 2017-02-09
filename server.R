library(tools)
library(httr)
library(mlr)
library(readr)
library(RWeka)
library(BBmisc)
library(checkmate)
library(ParamHelpers)
library(farff)
library(OpenML)
library(ggplot2)
library(DT)
library(parallelMap)
library(rmarkdown)

source("server_helpers.R")
source("server_helpers_import.R")
source("server_helpers_learners.R")
source("server_helpers_tuning.R")
source("server_helpers_train_and_predict.R")
source("server_helpers_visualisation.R")

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output, session) {
  
  source("server_import.R", local = TRUE)
  source("server_data_summary.R", local = TRUE)
  source("server_task.R", local = TRUE)
  source("server_learners.R", local = TRUE)
  source("server_tuning.R", local = TRUE)
  source("server_train_and_predict.R", local = TRUE)
  source("server_benchmark.R", local = TRUE)
  source("server_visualisation.R", local = TRUE)
  source("server_report.R", local = TRUE)

  session$onSessionEnded(stopApp)

})
