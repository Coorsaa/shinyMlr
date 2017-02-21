library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(DT)

source("./helpers/helpers_ui.R", local = TRUE)$value

ui.files = list.files(path = "./ui", pattern = "*.R")
ui.files = paste0("ui/", ui.files)

for (i in seq_along(ui.files)) {
  source(ui.files[i], local = TRUE)
}

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
          menuSubItem("Preprocessing", tabName = "preprocessing", icon = icon("magic"))
        ),
        menuItem("Task", tabName = "task", icon = icon("flag")),
        menuItem("Learners", tabName = "learners", icon = icon("cog")),
        menuItem("Tuning", tabName = "tuning", icon = icon("wrench")),
        menuItem("Train and Predict", tabName = "modelling", icon = icon("graduation-cap")),
        menuItem("Benchmark", tabName = "benchmark", icon = icon("hourglass-start")),
        menuItem("Report", tabName = "report", icon = icon("book")),
        br(),
        column(align = "center", width = 6,
        bsButton("show.help", "Show help", type = "toggle",
          icon = icon("question-circle"))
        )
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

