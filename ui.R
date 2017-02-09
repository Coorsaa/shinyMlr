library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)

source("ui_helpers.R")
source("ui_tabpanel_import.R")
source("ui_tabpanel_summary.R")
source("ui_tabpanel_preprocessing.R")
source("ui_tabpanel_task.R")
source("ui_tabpanel_learners.R")
source("ui_tabpanel_tuning.R")
source("ui_tabpanel_train_and_predict.R")
source("ui_tabpanel_benchmark.R")
source("ui_tabpanel_report.R")

shinyUI(
  dashboardPage(
    dashboardHeader(title = span(img(src="logo.png", height = 40)),
      tags$li(class = "dropdown",
        tags$a(href = "https://github.com/mlr-org/mlr", target = "_blank",  
          tags$img(height = "19px", alt = "mlr Logo", src = "mlrLogo_blue_141x64.png")
        )
      ),
      tags$li(class = "dropdown",
        tags$a(href = "https://github.com/mlr-org/mlr_shiny", target = "_blank",  
          tags$img(icon("github"))
        )
      )
    ),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Import", tabName = "import", icon = icon("folder-open")),
        menuItem("Data", tabName = "data", icon = icon("database"),
          menuSubItem("Summary", tabName = "summary", icon = icon("bar-chart")),
          menuSubItem("Preprocessing", tabName = "preprocessing", icon = icon("magic"))),
        menuItem("Task", tabName = "task", icon = icon("flag")),
        menuItem("Learners", tabName = "learners", icon = icon("cog")),
        menuItem("Tuning", tabName = "tuning", icon = icon("wrench")),
        menuItem("Train and Predict", tabName = "modelling", icon = icon("graduation-cap")),
        menuItem("Benchmark", tabName = "benchmark", icon = icon("hourglass-start")),
        menuItem("Report", tabName = "report", icon = icon("book"))
      )
    ),
    
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabItems(
        tabItem(tabName = "import", tabpanel.import),
        tabItem(tabName = "summary", tabpanel.summary),
        tabItem(tabName = "preprocessing", tabpanel.preprocessing),
        tabItem(tabName = "task", tabpanel.task),
        tabItem(tabName = "learners", tabpanel.learners),
        tabItem(tabName = "tuning", tabpanel.tuning),
        tabItem(tabName = "modelling", tabpanel.modelling),
        tabItem(tabName = "benchmark", tabpanel.benchmark),
        tabItem(tabName = "report", tabpanel.report)
      )
    )
  )
)

