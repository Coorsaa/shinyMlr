source("ui_tabpanel_import.R")
source("ui_tabpanel_summary.R")
source("ui_tabpanel_task.R")
source("ui_tabpanel_benchmark.R")
source("ui_tabpanel_bmrplots.R")
source("ui_tabpanel_predictionplot.R")
source("ui_tabpanel_partialdep.R")
source("ui_tabpanel_train_and_predict.R")


shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  fluidRow(
    column(2, img(src="mlrLogo_blue_141x64.png")),
    column(8, align = "center", 
      h2("Integration of the",  a("mlr", href = "https://github.com/mlr-org/mlr"), "package into shiny")
    ),
    column(2, img(src=("rabbit.png")))
  ),
  navbarPage("", 
    tabpanel.import, 
    tabpanel.summary, 
    tabpanel.task, 
    tabpanel.benchmark,
    navbarMenu("Visualisations",
      tabpanel.bmrplots,
      tabpanel.predictionplot,
      tabpanel.partialdep
    ),
    navbarMenu("Train and Predict",
      tabpanel.train,
      tabpanel.predict,
      tabpanel.performance
    )
  )
)
)