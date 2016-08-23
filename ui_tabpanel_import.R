tabpanel.import = tabPanel("Import", 
      uiOutput("import.ui"),
  # sidebarPanel(
    # wellPanel(
    # fluidRow(column(10,
      # selectInput("import.type", "Type", selected = "mlr", choices = c("mlr", "CSV")),
    # ),
    # wellPanel(
      # uiOutput("import.ui")
    # )
    # ))
  # ),
  mainPanel(
    dataTableOutput("import.preview")
  )
)

