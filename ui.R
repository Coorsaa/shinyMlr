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
    navbarPage(title = div(img(src = "new_shiny_logo.png", height = 35)),
      theme = shinytheme("united"), id = "top-nav",
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
      tabPanel("Benchmark", tabpanel.benchmark,
        icon = icon("hourglass-start")),
      tabPanel("Report", tabpanel.report,
        icon = icon("book")),
      tabPanel(title = "hide_me"),
      tabPanel(title = div(class = "navbarlink-container",
        tags$img(height = "20px", alt = "mlr Logo",
          src = "new_mlr_logo.png")
        ), value = "https://github.com/mlr-org/mlr"),
      tabPanel(title = "", icon = icon("github", "fa-lg"),
      value = "https://github.com/mlr-org/mlr"),

      footer = tagList(
        includeScript("scripts/top-nav-links.js"),
        includeScript("scripts/app.js"),
        tags$link(rel = "stylesheet", type = "text/css",
          href = "custom.css"),
        tags$link(rel = "stylesheet", type = "text/css",
          href = "https://fonts.googleapis.com/css?family=Roboto"),
        tags$link(rel = "stylesheet", type = "text/css",
          href = "AdminLTE.css"),
        tags$footer(title = "", # align = "right",
          # tags$a(id = "show_help",
          # href = "https://github.com/mlr-org/mlr_shiny", target = "_blank",
          # div(id = "copyright-container",
          #   column(width = 6, align = "left",
          tags$p(id = "copyright",
            tags$img(icon("copyright")),
            2017,
            tags$a(href = "https://github.com/Coorsaa",
              target = "_blank", "Stefan Coors, "),
            tags$a(href = "https://github.com/florianfendt",
              target = "_blank", "Florian Fendt"),
            " (members of the mlr-organization)"
          ),
          tags$p(id = "help_toggler",
            bsButton(inputId = "show_help", label = "show help",
              type = "toggle", icon = icon("question-circle"))
          )
        )
      ), windowTitle = "shinyMlr"
    )
  )
)
