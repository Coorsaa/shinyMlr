tabpanel.summary = fluidRow(
  box(width = 12, title = "Data Summary",
    htmlOutput("summary.text"),
    br(),
    DT::dataTableOutput("summary.datatable")
  ),
  box(width = 12, title = "Variable Visualization", id = "summary.vis.box",
    fluidRow(
      column(12,
        uiOutput("summary.vis.hist.nbins")),
      column(12,
      plotOutput("summary.vis")
      )
    )
  )
)
