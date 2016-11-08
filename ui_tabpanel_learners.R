tabpanel.learners = list(
  fluidRow(
    box(title = "Choose learners", width = 12, collapsible = TRUE,
      div(align = "center",
        uiOutput("learners.sel")
        #uiOutput("learners.choose")
      )
    )
  ),
  fluidRow(
    uiOutput("learners.ui")
  )
)


