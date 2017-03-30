tabpanel.task = fluidPage(
  sidebarLayout(
    sidebarPanel(
      div(align = "center",
          uiOutput("task.id"),
          uiOutput("task.target"),
          uiOutput("task.weights"),
          tags$hr(),
          bsButton("create.task", "create task",
              icon = icon("flag")),
          tags$hr()
      )
    ),
    mainPanel(
      fluidRow(htmlOutput("task.text")),
      fluidRow(uiOutput("task.overview"))
    )
  )
)
#   box(width = 12, title = "Task",
#     fluidRow(htmlOutput("task.text")),
#     fluidRow(
#       column(width = 3, align = "center",
#         makeSidebar(

#         )
#       ),
#       column(width = 9,
#         fluidRow(uiOutput("task.overview"))
#       )
#     )
#   )
# )

