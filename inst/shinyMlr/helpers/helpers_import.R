makeImportSideBar = function(type) {
  imptype.sel.input = sidebarMenu(
    menuItem("Type"),
    selectInput("import.type", "", selected = type,
      choices = c("examples", "OpenML", "CSV", "ARFF"))
  )
  switch(type,
    examples = list(
      imptype.sel.input,
      sidebarMenu(
        menuItem("Choose example data"),
        selectInput("import.mlr", "", choices = c("iris.task", "bh.task", "sonar.task"))
      )
    ),
    OpenML = list(
      imptype.sel.input,
      sidebarMenu(
        menuItem("Choose OpenML Data ID"),
      selectInput("import.OpenML", "", selected = 61L,
        choices = listOMLDataSets()[,1], multiple = FALSE)
      )
    ),
    CSV = list(
      imptype.sel.input,
      sidebarMenu(
        menuItem("Import"),
      fileInput("import.csv", "Choose CSV File",
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      checkboxInput("import.header", "Header", TRUE),
      selectInput("import.sep", "Separator", selected = ",",
        choices = c(Comma = ",", Semicolon = ";", Tab = "\t")),
      selectInput("import.quote", "Quote", selected = '"',
        choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"))
      )
    ),
    ARFF = list(
      imptype.sel.input,
      sidebarMenu(
        menuItem("Choose ARFF File"),
        fileInput("import.arff", "", accept = c(".arff"))
      )
    )
  )
}

