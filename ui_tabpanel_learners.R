tabpanel.learners = list(fluidRow(
  box(collapsible = TRUE, width = 12, title = "Choose learners",
    uiOutput("learners.sel")
  )),
  fluidRow(
    box(title = "Set hyperparameters", width = 12,
      uiOutput("learners.sel.par.set")
    )
  )
)


