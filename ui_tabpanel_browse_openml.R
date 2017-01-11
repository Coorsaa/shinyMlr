tabpanel.browse.openml = fluidRow(
  box(width = 12, title = "Browse OpenML",
    # conditionalPanel("output.import_browse_openml)",
    #   div(id = "loading.message2", align = "center",
    #     h4("Loading datasets from OpenML")
    #   )
    # ),
    hidden(
      div(id = "loading.message2", align = "center",
        h4("Loading datasets from OpenML")
      )
    ),
    column(12, DT::dataTableOutput("import.browse.openml"))
  )
)