tabpanel.predictionplot = list(
  fluidRow(
    box(width = 12, align = "center", collapsible = TRUE,
      uiOutput("predictionplot.learner.sel"),
      uiOutput("predictionplot.x.sel")
    )
  ),
  fluidRow(
    box(width = 12,
      plotOutput("predictionplot")
    )
  )
)
