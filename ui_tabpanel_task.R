tabpanel.task = list(fluidRow(
  box(collapsible = TRUE, width = 12, title = "Task",
    fluidRow(
      column(width = 6,
        uiOutput("task.id")
      ),
      column(width = 6,
        uiOutput("task.target")
      )
    ),
    fluidRow(
      div(align = "center",
        actionButton("create.task", "create task")
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

