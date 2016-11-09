tabpanel.browse.openml = fluidRow(
  box(width = 12, title = "Browse OpenML",
    column(12, dataTableOutput("import.browse.openml"))
  )
)