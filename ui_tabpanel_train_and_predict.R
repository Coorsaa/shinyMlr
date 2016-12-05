tabpanel.modelling = fluidRow(
  tabBox(width = 12,
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
            br()# ,
            # conditionalPanel(condition = "output.predoverview",
            #   
            # )
          )
        ),
        column(width = 9, align = "center",
          tabBox(id = "predict.tab", selected = "Test Set", side = "right", width = 12,
            tabPanel("Prediction Plots",
              fluidRow(
                column(width = 6,
                  selectInput("prediction.plot.sel", "Choose plot",
                    choices = c("prediction", "residuals", "ROC"),
                    selected = "prediction plot", width = 200),
                  uiOutput("predictionplot.learner.sel")
                ),
                column(width = 6,
                  uiOutput("predictionplot.settings")
                )
              ),
              fluidRow(
                plotOutput("prediction.plot")
              )
            ),
            tabPanel("Predictions", value = "pred.res",
              dataTableOutput("predoverview"),
              br(),
              br(),
              downloadButton("predict.download", "download predictions")
            ),
            tabPanel("Test Set", value = "test.set",
              dataTableOutput("import.pred.preview")
            )
          )
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
)
