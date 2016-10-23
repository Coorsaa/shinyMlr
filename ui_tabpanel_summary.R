tabpanel.summary = fluidRow(
  box(width = 12, title = "Data Summary",
    dataTableOutput("summary.datatable")
  ),
  box(width = 12, title = "Data Visualization",
    fluidRow(
      column(6,
        uiOutput("summary.plots.var")),
      column(6,
        uiOutput("summary.plots.nbins"))
    )
  ),
  box(width = 12,
    plotOutput("summary.plots")
  )
)
