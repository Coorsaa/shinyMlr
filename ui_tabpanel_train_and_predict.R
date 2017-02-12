tabpanel.modelling = fluidRow(
  tabBox(width = 12,
    tabPanel(title = "Train",
      # fluidRow(
      fluidRow(
        htmlOutput("train.text"),
        column(width = 4, NULL),
        column(width = 4, align = "center",
          fluidRow(uiOutput("train.learner.sel")),
          br(),
          br(),
          fluidRow(uiOutput("model.overview"))
        ),
        column(width = 4, NULL)
      )
    ),
    tabPanel(title = "Predict",
      htmlOutput("prediction.text"),
      fluidRow(
        column(width = 3, align = "center",
          box(background = "light-blue", width = NULL, height = 520,
            selectInput("newdatatype", "Predict on:", choices = c("task", "new data"),
              selected = "task"),
            conditionalPanel("input.newdatatype == 'new data'",
              selectInput("import.pred.type", "Type", selected = "mlr",
                choices = c("mlr", "OpenML", "CSV", "ARFF"))
            ),
            uiOutput("import.pred.ui"),
            actionButton("predict.run", label = "Predict"),
            br(),
            br()
          )
        ),
        column(width = 9, align = "center",
          tabBox(id = "predict.tab", selected = "test.set", side = "right", width = 12,
            tabPanel("Predictions", value = "pred.res",
              DT::dataTableOutput("predoverview"),
              br(),
              br(),
              downloadButton("predict.download", "download predictions")
            ),
            tabPanel("Test Set", value = "test.set",
              DT::dataTableOutput("import.pred.preview")
            )
          )
        )
      )
    ),
    tabPanel(title = "Performance",
      htmlOutput("performance.text"),
      fluidRow(
        column(width = 12, align = "center",
          uiOutput("perf.measures.sel"),
          actionButton("performance.run", label = "Measure Performance"),
          br(),
          br(),
          uiOutput("performance.overview", align = "left")
        )
      )
    ),
    tabPanel("Visualisations",
      htmlOutput("visualisation.text"),
      fluidRow(
        column(width = 12,
          uiOutput("visualisation.selection"),
          uiOutput("predictionplot.settings")
        )
      ),
      fluidRow(
        column(width = 12,
          verbatimTextOutput("confusion.matrix")
        )
      ),
      fluidRow(
        column(width = 12,
          plotOutput("prediction.plot")
        )
      )
    )
  )
)
