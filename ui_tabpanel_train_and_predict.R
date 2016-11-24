tabpanel.modelling = tabBox(width = 12,
  tabPanel(title = "Train",
    fluidRow(
      column(width = 12, align = "center",
        uiOutput("train.learner.sel"),
        actionButton("train.run", label = "Train")
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
        uiOutput("import.pred.ui")
      ),
      column(width = 9,
        dataTableOutput("import.pred.preview")
      )
    ),
    fluidRow(
      column(width = 3, align = "center",
        actionButton("predict.run", label = "Predict"),
        br(),
        br(),
        conditionalPanel(condition = "output.predoverview",
          downloadButton("predict.download", "download predictions")
        )
      ),
      column(width = 9,
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
