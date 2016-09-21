tabpanel.import = list(fluidRow(
  # box(width = 12,
  #  selectInput("import.type", "Type", selected = "mlr", choices = c("mlr", "CSV"))
  #  ),
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




# tabPanel("Import", 
  #    uiOutput("import.ui"),
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
  #mainPanel(
   # dataTableOutput("import.preview")
  #)
#)

