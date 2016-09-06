tabpanel.train = list(fluidRow(
  box(width = 12, align = "center", title = "Train",
    uiOutput("train.prob.sel"),
    uiOutput("train.learner.sel"),
    checkboxInput("hyppars", "Set hyperparameters of the learner manually"),
    conditionalPanel(
      condition = "input.hyppars == true",
      textInput("hypparslist", "Hyperparameters:", "list()")
    ),
    br(),
    actionButton("train.run", label = "Train")
  )
  ),
  fluidRow(
    box(width = 12, align = "center",
      textOutput("train.overview")
    )
  )
)

tabpanel.predict = list(
  h2("Predict"),
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
      actionButton("predict.run", label = "Predict")
    ),
    box(width = 9,
      dataTableOutput("pred.overview")
    )
  )
)

tabpanel.performance = list(
  fluidRow(
    box(align = "center", title = "Performance", width = 12,
      uiOutput("perf.measures.sel"),
      actionButton("performance.run", label = "Measure Performance")
    )
  ),
  fluidRow(
    box(width = 12, align = "center",
      tableOutput("performance.overview")
    )
  )
)

