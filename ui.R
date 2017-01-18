library(shiny)
library(shinydashboard)
library(shinyjs)


source("ui_tabpanel_import.R")
source("ui_tabpanel_browse_openml.R")
source("ui_tabpanel_summary.R")
source("ui_tabpanel_preprocessing.R")
source("ui_tabpanel_task.R")
source("ui_tabpanel_learners.R")
source("ui_tabpanel_train_and_predict.R")
source("ui_tabpanel_benchmark.R")

shinyUI(
  dashboardPage(
    dashboardHeader(title = span(img(src="logo.png", height = 40)),
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
        menuItem("Data", tabName = "data", icon = icon("database"),
          menuSubItem("Summary", tabName = "summary", icon = icon("bar-chart")),
          menuSubItem("Preprocessing", tabName = "preprocessing", icon = icon("magic"))),
        menuItem("Task", tabName = "task", icon = icon("flag")),
        menuItem("Learners", tabName = "learners", icon = icon("cog")),
        menuItem("Train and Predict", tabName = "modelling", icon = icon("graduation-cap")),
        menuItem("Benchmark", tabName = "benchmark", icon = icon("hourglass-start"))
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
        tabItem(tabName = "preprocessing", tabpanel.preprocessing),
        tabItem(tabName = "task", tabpanel.task),
        tabItem(tabName = "learners", tabpanel.learners),
        tabItem(tabName = "modelling", tabpanel.modelling),
        tabItem(tabName = "benchmark", tabpanel.benchmark)
      )
    )
  )
)

