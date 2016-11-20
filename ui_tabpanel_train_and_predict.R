tabpanel.modelling = tabBox(width = 12,
  tabPanel(title = "Train",
    fluidRow(
      box(width = 12,
        fluidRow(
          column(width = 12, align = "center",
            uiOutput("train.learner.sel"),
            actionButton("train.run", label = "Train")
          )
        ),
        br(),
        br(),
        fluidRow(
          verbatimTextOutput("model.overview")
        )
      )
    )
  ),
  tabPanel(title = "Predict",
    fluidRow(
      box(width = 12, align = "center",
        fluidRow(
          box(width = 3,
            uiOutput("import.pred.ui")
          ),
          box(width = 9,
            dataTableOutput("import.pred.preview")
          )
        ),
        fluidRow(
          box(width = 3,
            actionButton("predict.run", label = "Predict"),
            br(),
            br(),
            conditionalPanel(condition = "output.predoverview",
              downloadButton("predict.download", "download predictions")
            )
          ),
          box(width = 9,
            dataTableOutput("predoverview")
          )
        )
      )
    )
  ),
  tabPanel(title = "Performance",
    fluidRow(
      box(width = 12, align = "center",
        uiOutput("perf.measures.sel"),
        actionButton("performance.run", label = "Measure Performance"),
        br(),
        br(),
        uiOutput("performance.overview", align = "left")
      )
    )
  )  
)
