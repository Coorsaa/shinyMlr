tabpanel.summary = fluidRow(
  box(width = 12, title = "Data Summary",
    DT::dataTableOutput("summary.datatable")
  ),
  box(width = 12, title = "Variable Visualization",
    fluidRow(
      column(4,
        uiOutput("summary.vis.var")),
      column(8,
        uiOutput("summary.vis.hist.nbins")),
      column(12,
      plotOutput("summary.vis")
      )
    )
  )
)
