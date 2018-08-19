tabpanel.summary = bootstrapPage(theme = shinytheme("united"),
  # fluidPage(
    uiOutput("data.summary.box"),
    box(width = 12, title = "Variable Visualization", id = "summary.vis.box",
      fluidRow(
        column(12,
          uiOutput("summary.vis.hist")),
        column(12,
          plotlyOutput("summary.vis"),
          br(),
          br()
        )
      )
    )
#  )
)
