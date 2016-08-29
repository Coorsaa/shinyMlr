tabpanel.train = tabPanel("Train", 
  titlePanel("Training"),
  fluidRow(
    uiOutput("train.learner.sel"),
    "Set hyperpars of the learner",
    br(),
    actionButton("train.run", label = "Train"),
    textOutput("train.overview")
  )
)

tabpanel.predict = 
  tabPanel("Predict", 
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
  uiOutput("perf.measures.sel"),
  actionButton("performance.run", label = "Measure Performance"),
  dataTableOutput("performance.overview")
) 

