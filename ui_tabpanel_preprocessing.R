tabpanel.preprocessing = fluidRow(
  box(width = 12, title = "Data Preprocessing",
    fluidRow(
      column(6, uiOutput("preproc.var")),
      column(6, "")
    ),
    uiOutput("preproc"),
    fluidRow(
      box(width = 12,
        dataTableOutput("impute.datatable")
      )
    )
  )
)
