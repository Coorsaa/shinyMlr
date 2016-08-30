tabpanel.train = tabPanel("Train", 
  titlePanel("Train"),
  fluidRow(
    uiOutput("train.prob.sel"),
    uiOutput("train.learner.sel"),
    checkboxInput("hyppars", "Set hyperparameters of the learner manually"),
    conditionalPanel(
      condition = "input.hyppars == true",
      textInput("hypparslist", "Hyperparameters:", "list()")
    ),
    br(),
    actionButton("train.run", label = "Train"),
    textOutput("train.overview")
  )
)

tabpanel.predict = 
  tabPanel("Predict", 
    titlePanel("Predict"),
    sidebarLayout(
      uiOutput("import.pred.ui"),
      mainPanel(
        dataTableOutput("import.pred.preview")
      )),
    
    sidebarLayout(
      sidebarPanel(actionButton("predict.run", label = "Predict"),
        "Extract Predictions"),
      mainPanel(
        dataTableOutput("pred.overview")
      )
    )
  ) 

tabpanel.performance = tabPanel("Performance", 
  titlePanel("Performance"),
  uiOutput("perf.measures.sel"),
  actionButton("performance.run", label = "Measure Performance"),
  tableOutput("performance.overview")
) 

