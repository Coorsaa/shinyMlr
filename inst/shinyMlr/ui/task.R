tabpanel.task = fluidPage(
  sidebarLayout(
    sidebarPanel(width = 3,
      div(align = "center",
          uiOutput("task.id"),
          uiOutput("task.target"),
          uiOutput("task.weights"),
          br(),
          tags$hr(),
          bsButton("create.task", "create task",
              icon = icon("flag")),
          tags$hr()
      )
    ),
    mainPanel(width = 9,
      fluidRow(htmlOutput("task.text")),
      fluidRow(uiOutput("task.overview"))
    )
  )
)
