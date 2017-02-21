tabpanel.report = fluidRow(
  htmlOutput("report.text"),
  column(width = 3, align = "center",
    makeSidebar(bar.height = 600,
      selectInput("report.format", "Document Format",
        choices = c("PDF", "HTML"), selected = "PDF"),
      textInput("report.title", label = "title", value = "shinyMlr report"),
      textInput("report.subtitle", label = "subtitle",
        value = "Your data analysis and machine learning experiment summary"),
      textInput("report.authors", label = "authors", value = "mlr organization"),
      downloadButton("report", "Generate report")
    )
  ),
  column(width = 9,
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
    ),
    fluidRow(verbatimTextOutput("testout"))
  )
)




