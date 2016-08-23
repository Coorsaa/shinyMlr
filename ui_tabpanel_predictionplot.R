tabpanel.predictionplot = tabPanel("Prediction Plots", 
  sidebarPanel(
    uiOutput("predictionplot.learner.sel"),
    uiOutput("predictionplot.x.sel")
  ),
  mainPanel(
    plotOutput("predictionplot")
  )
)


