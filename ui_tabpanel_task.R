tabpanel.task = tabPanel("Task", 
  sidebarPanel(
    uiOutput("task.id"),
    uiOutput("task.target")
  ),
  mainPanel(
    verbatimTextOutput("task.overview")
  )
)

