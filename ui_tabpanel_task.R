tabpanel.task = list(fluidRow(
  box(collapsible = TRUE, width = 12, title = "Task",
    fluidRow(
      box(
        uiOutput("task.id")
      ),
      box(
        uiOutput("task.target")
      )
    )
  )
  ),
  fluidRow(
    box(width = 12,
      verbatimTextOutput("task.overview")
    )
  )
)

