tabpanel.learners = list(
  fluidRow(
    box(title = "Choose learners", width = 12, collapsible = TRUE,
      div(align = "center",
        uiOutput("learners.sel"),
        uiOutput("learners.choose")
      )
    )
  ),
  fluidRow(
    box(title = "Set hyperparameters", width = 12,
      uiOutput("learners.sel.par.set"),
      div(align = "center", uiOutput("learners.constr"))
    )
  )
)


