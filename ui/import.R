tabpanel.import = list(fluidRow(
  box(width = 12, title = "Import",
    htmlOutput("import.text"),
    br(),
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
  fluidRow(uiOutput("tabpanel.browse.openml"))
)




