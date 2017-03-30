library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(DT)
library(plotly)
library(shinythemes)

source("./helpers/helpers_ui.R", local = TRUE)$value

ui.files = list.files(path = "./ui", pattern = "*.R")
ui.files = paste0("ui/", ui.files)

for (i in seq_along(ui.files)) {
  source(ui.files[i], local = TRUE)
}

shinyUI(
  tagList(
    useShinyjs(),
    # tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    # headerPanel(span(img(src = "new_shiny_logo.png", height = 40))),
    navbarPage(div(class = "navlogo", img(src = "new_shiny_logo.png", height = 45)), theme = shinytheme("united"), id = "top-nav",
      tabPanel("Import", tabpanel.import,
        icon = icon("folder-open")),
      navbarMenu("Data", icon = icon("database"),
        tabPanel("Summary", tabpanel.summary,
          icon = icon("bar-chart")),
        tabPanel("Preprocessing", tabpanel.preprocessing,
          icon = icon("magic"))
      ),
      tabPanel("Task", tabpanel.task, icon = icon("flag")),
      tabPanel("Learners", tabpanel.learners,
        icon = icon("cog")),
      tabPanel("Tuning", tabpanel.tuning,
        icon = icon("wrench")),
      navbarMenu("Train and Predict", icon = icon("graduation-cap"),
        tabPanel("Train", tabpanel.train),
        tabPanel("Predict", tabpanel.predict),
        tabPanel("Performance & Visualisation", tabpanel.performance)
      ),
      # tabPanel("Train and Predict", tabpanel.modelling,
      #   icon = icon("graduation-cap")),
      tabPanel("Benchmark", tabpanel.benchmark,
        icon = icon("hourglass-start")),
      tabPanel("Report", tabpanel.report,
        icon = icon("book")),
      tabPanel(title = "hide_me"),
      tabPanel(title = div(class = "navbarlink-container",
        tags$img(height = "20px", alt = "mlr Logo",
          src = "new_logo.png")
        ), value = "https://github.com/mlr-org/mlr"),
      tabPanel(title = "", icon = icon("github", "fa-lg"),
      value = "https://github.com/mlr-org/mlr"),
      tabPanel(title = "", icon = icon("question-circle", "fa-lg"), value = "javascript:$('.helptext').slideToggle(); $('.fa-code').parent().parent().toggleClass('active'); code_clicked();"),
      footer = tagList(includeScript("scripts/top-nav-links.js"),
        includeScript("scripts/app.js"),
        tags$link(rel = "stylesheet", type = "text/css",
          href = "custom.css"),
        tags$link(rel = "stylesheet", type = "text/css",
          href = "https://fonts.googleapis.com/css?family=Roboto"),
        tags$link(rel = "stylesheet", type = "text/css",
          href = "AdminLTE.css")),
      windowTitle = "shiny-mlr"
    )
    )
)

        # , icon = icon("book")),
        # br(),
        # column(align = "center", width = 6,
        # bsButton("show.help", "Show help", type = "toggle",
        #   icon = icon("question-circle"))
        # )
    # ),

    # dashboardBody(
    #   shinyjs::useShinyjs(),
    #   tags$head(
    #     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    #   ),
    #   tabItems(
    #     tabItem(id = "import", tabpanel.import),
    #     tabItem(id = "summary", tabpanel.summary),
    #     tabItem(id = "preprocessing", tabpanel.preprocessing),
    #     tabItem(id = "task", tabpanel.task),
    #     tabItem(id = "learners", tabpanel.learners),
    #     tabItem(id = "tuning", tabpanel.tuning),
    #     tabItem(id = "modelling", tabpanel.modelling),
    #     tabItem(id = "benchmark", tabpanel.benchmark),
    #     tabItem(id = "report", tabpanel.report)
    #   )
    # )
#   )
# )

