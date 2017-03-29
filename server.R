library(tools)
library(httr)
library(mlr)
library(stringi)
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
library(xtable)
library(plyr)
library(GGally)
library(plotly)


helper.files = list.files(path = "./helpers", pattern="*.R")
helper.files = paste0("helpers/", helper.files)

for (i in seq_along(helper.files)) {
  source(helper.files[i], local = TRUE)
}

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output, session) {
  shinyjs::addClass(id = "mlrlink", class = "navbar-right")

  server.files = list.files(path = "./server", pattern = "*.R")
  server.files = paste0("server/", server.files)
  for (i in seq_along(server.files)) {
    source(server.files[i], local = TRUE)
  }

  session$onSessionEnded(stopApp)
})
