library(shiny)
library(shinydashboard)
library(shinyjs)

source("ui_tabpanel_import.R")
source("ui_tabpanel_browse_openml.R")
source("ui_tabpanel_summary.R")
source("ui_tabpanel_task.R")
source("ui_tabpanel_learners.R")
source("ui_tabpanel_benchmark.R")
source("ui_tabpanel_bmrplots.R")
source("ui_tabpanel_predictionplot.R")
source("ui_tabpanel_partialdep.R")
source("ui_tabpanel_train_and_predict.R")

shinyUI(
  dashboardPage(
    dashboardHeader(title = "shinyMlr",
      tags$li(class = "dropdown",
        tags$a(href = "https://github.com/mlr-org/mlr", target = "_blank", 
          tags$img(height = "19px", alt = "mlr Logo", src = "mlrLogo_blue_141x64.png")
        )
      )
    ),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Import", tabName = "import", icon = icon("folder-open"),
          menuSubItem("Import File", tabName = "import-file", icon = icon("file")),
          menuSubItem("Browse OpenML", tabName = "browse-openml", icon = icon("search"))),
        menuItem("Data Summary", tabName = "summary", icon = icon("database")),
        menuItem("Task", tabName = "task", icon = icon("flag")),
        menuItem("Learners", tabName = "learners", icon = icon("cog")),
        menuItem("Train and Predict", tabName = "modelling", icon = icon("graduation-cap")
          # menuSubItem("Train", tabName = "traintab"),
          # menuSubItem("Predict", tabName = "predtab"),
          # menuSubItem("Performance", tabName = "perftab")
        ),
        menuItem("Benchmark", tabName = "benchmark", icon = icon("hourglass-start")),
        menuItem("Visualisations", tabName = "visualisations", icon = icon("bar-chart"),
          menuSubItem("Benchmark Plots", tabName = "bmrplots"),
          menuSubItem("Prediction Plots", tabName = "predplots"),
          menuSubItem("Partial Dep. Plots", tabName = "partdepplots")
        )
      )
    ),
    
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabItems(
        tabItem(tabName = "import-file", tabpanel.import),
        tabItem(tabName = "browse-openml", tabpanel.browse.openml),
        tabItem(tabName = "summary", tabpanel.summary),
        tabItem(tabName = "task", tabpanel.task),
        tabItem(tabName = "learners", tabpanel.learners),
        tabItem(tabName = "benchmark", tabpanel.benchmark),
        tabItem(tabName = "bmrplots", tabpanel.bmrplots),
        tabItem(tabName = "predplots", tabpanel.predictionplot),
        tabItem(tabName = "partdepplots", tabpanel.partialdep),
        tabItem(tabName = "modelling", tabpanel.modelling)
        # tabItem(tabName = "predtab", tabpanel.predict),
        # tabItem(tabName = "perftab", tabpanel.performance)
      )
    )
  )
)

