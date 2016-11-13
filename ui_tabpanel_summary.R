tabpanel.summary = fluidRow(
  box(width = 12, title = "Data Summary",
    dataTableOutput("summary.datatable")
  ),
  box(width = 12, title = "Variable Visualization",
    fluidRow(
      column(4,
        uiOutput("summary.vis.var")),
      column(8,
        uiOutput("summary.vis.hist.nbins")),
      plotOutput("summary.vis")
    )
  )
)
