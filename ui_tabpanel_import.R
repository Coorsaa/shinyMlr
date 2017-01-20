tabpanel.import = list(fluidRow(
  box(width = 12, collapsible = TRUE, title = "Import",
    uiOutput("import.ui"),
    hidden(
      div(id = "loading.message", align = "center",
        h4("Loading datasets from OpenML")
      )
    )
  )),
  fluidRow(
    box(width = 12,
      DT::dataTableOutput("import.preview")
    )
  ),
  uiOutput("tabpanel.browse.openml")
)




