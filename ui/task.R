tabpanel.task = fluidRow(
  box(width = 12, title = "Task",
    fluidRow(htmlOutput("task.text")),
    fluidRow(
      column(width = 3, align = "center",
        makeSidebar(
          uiOutput("task.id"),
          uiOutput("task.target"),
          uiOutput("task.weights"),
          tags$hr(),
          actionButton("create.task", "create task")
        )
      ),
      column(width = 9,
        fluidRow(uiOutput("task.overview"))
      )
    )
  )
)

