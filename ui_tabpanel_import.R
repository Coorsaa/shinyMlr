tabpanel.import = tabPanel("Load", 
  sidebarPanel(
    fileInput("import.file", "Choose CSV File",
      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    tags$hr(),
    checkboxInput("import.header", "Header", TRUE),
    radioButtons("import.sep", "Separator",
      c(Comma=",", Semicolon=";", Tab="\t"), ","),
    radioButtons("import.quote", "Quote", c(None="", "Double Quote"='"', "Single Quote"="'"), '"'),
    textInput("import.rownames", "Row Names", 1)
    # uiOutput("target")
  ),
  mainPanel(
    dataTableOutput("import.preview")
  )
)

