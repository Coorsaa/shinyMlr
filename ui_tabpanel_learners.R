tabpanel.learners = list(fluidRow(
  box(title = "Choose learners", width = 12, collapsible = TRUE,
    div(align = "center",
      uiOutput("learners.sel"),
      actionButton("learners.choose", "choose learners")
    )
  )),
  fluidRow(
    box(title = "Set hyperparameters", width = 12,
      uiOutput("learners.sel.par.set")
    )
  ),
  fluidRow(
    box(width = 12,
      div(align = "center",
        actionButton("learners.constr", "construct learners")
      )
    )
  )
)


