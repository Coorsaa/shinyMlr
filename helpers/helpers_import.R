makeImportSideBar = function(type) {
  imptype.sel.input = selectInput("import.type", "Type", selected = type, choices = c("mlr", "OpenML", "CSV", "ARFF"))
  switch(type,
    mlr = fluidRow(
      column(width = 3,
        imptype.sel.input
      ),
      column(width = 9,
        selectInput("import.mlr", "Choose mlr task", choices = c("iris.task", "bh.task", "sonar.task"))
      )
    ),
    OpenML = fluidRow(
      column(width = 3,
        imptype.sel.input
      ),
      column(width = 9,
        selectInput("import.OpenML", "Choose OpenML Data ID", selected = 61L, choices = listOMLDataSets()[,1], multiple = FALSE)
      )
    ),
    CSV = fluidRow(
      column(width = 4, height = 250,
        imptype.sel.input,
        fileInput("import.csv", "Choose CSV File",
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      # tags$hr(),
      column(width = 8, height = 250,
        checkboxInput("import.header", "Header", TRUE),
        selectInput("import.sep", "Separator", selected = ",",
          choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), width = 200),
        selectInput("import.quote", "Quote", selected = '"',
          choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
          width = 200)
      )#,
      #      textInput("import.rownames", "Row Names", NULL)
    ),
    ARFF = fluidRow(
      column(width = 4, height = 250,
        imptype.sel.input,
        fileInput("import.arff", "Choose ARFF File",
          accept = c(".arff"))
      )
    )
  )
}

