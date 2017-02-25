tabpanel.import = list(
  fluidRow(
    box(width = 12, title = "Import",
      htmlOutput("import.text"),
      br(),
      column(width = 3,
        makeSidebar(uiOutput("import.ui"))
      ),
      column(width = 9,
        hidden(
          div(id = "loading.message", align = "center",
            h4("Loading datasets from OpenML")
          )
        ),
        DT::dataTableOutput("import.preview"),
        uiOutput("tabpanel.browse.openml")
      )
    )
  )
)
