tabpanel.summary = fluidRow(
  box(width = 12, title = "Data Summary",
    dataTableOutput("summary.datatable")
  ),
  box(width = 12, title = "Data Visualization",
    dataTableOutput("summary.plots")
  )
)


