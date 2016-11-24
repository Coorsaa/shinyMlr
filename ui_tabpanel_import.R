tabpanel.import = list(fluidRow(
  box(width = 12, collapsible = TRUE, title = "Import",
    uiOutput("import.ui")
    )
  ),
  fluidRow(
    box(width = 12,
      dataTableOutput("import.preview")
      )
    )
)




