tabpanel.report = fluidPage(theme = shinytheme("united"),
  sidebarLayout(
    sidebarPanel(
      sidebarMenu(
        menuItem("Document Format"),
        selectInput("report.format", "",
          choices = c("PDF", "HTML"), selected = "PDF"),
        menuItem("title"),
        textInput("report.title", label = "", value = "shinyMlr report"),
        menuItem("subtitle"),
        textInput("report.subtitle", label = "",
          value = "Your data analysis and machine learning experiment summary"),
        menuItem("authors"),
        textInput("report.authors", label = "", value = "mlr organization"),
        div(align = "center", downloadButton("report", "Generate report"))
      )
    ),
    mainPanel(
      fluidRow(
        htmlOutput("report.text")
      ),
      fluidRow(
        makeReportConfigUI("Data summary", "data",
        # passage = "Summary of your data:"
        ),
        makeReportConfigUI("Task", "task",
          # passage = "Here's your task:"
        ),
        makeReportConfigUI("Learners", "learners",
          # passage = "You constructed these learners:"
        ),
        makeReportConfigUI("Tuning", "tuning",
          # passage = "Tuning result:"
        ),
        makeReportConfigUI("Train and Predict", "modelling",
          # passage = "Summary of modelling section:"
        ),
        makeReportConfigUI("Benchmark", "benchmark",
          # passage = "Look what bmr did:"
        )
      )
    )
  )
)
