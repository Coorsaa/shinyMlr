tabpanel.modelling = tabBox(width = 12,
  tabPanel(title = "Train",
    fluidRow(
      column(width = 12, align = "center",
        uiOutput("train.learner.sel")
      )
    ),
    br(),
    br(),
    fluidRow(
      column(width = 12,
        verbatimTextOutput("model.overview")
      )
    )
  ),
  tabPanel(title = "Predict",
    fluidRow(
      column(width = 3, align = "center",
        box(background = "light-blue", width = NULL, height = 450,
          selectInput("newdatatype", "Predict on:", choices = c("task", "new data"),
            selected = "task"),
          conditionalPanel("input.newdatatype == 'new data'",
            selectInput("import.pred.type", "Type", selected = "mlr",
              choices = c("mlr", "OpenML", "CSV", "ARFF"))
          ),
          uiOutput("import.pred.ui"),
          actionButton("predict.run", label = "Predict"),
          br(),
          br(),
          conditionalPanel(condition = "output.predoverview",
            downloadButton("predict.download", "download predictions")
          )
        )
      ),
      column(width = 9,
        dataTableOutput("import.pred.preview"),
        dataTableOutput("predoverview")
      )
    )
  ),
  tabPanel(title = "Performance",
    fluidRow(
      column(width = 12, align = "center",
        uiOutput("perf.measures.sel"),
        actionButton("performance.run", label = "Measure Performance"),
        br(),
        br(),
        uiOutput("performance.overview", align = "left")
      )
    )
  )  
)
