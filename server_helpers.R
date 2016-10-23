sPlotHist = function(input, data) {
  pv = input$summary.plots.var
  y = data[, pv]
  hist(y, breaks = summary.plots.nbins)
}

# FIXME: mlr: create makeAutoTask or whatever depending on target? 
sMakeTask = function(input, data) {
  id = input$task.id
  tn = input$task.target
  y = data[,tn]
  if (is.numeric(y))
    makeRegrTask(id = id, data = data, target = tn)
  else if (is.factor(y))
    makeClassifTask(id = id, data = data, target = tn)
}


# FIXME: maybe we want this as a helper too in mlr directly plot pd plot for one feature??
sPlotPartialDep = function(input, task, learners) {
  lrn = input$partialdep.learner
  lrn = learners[[lrn]]
  mod = train(lrn, task)
  fn = input$partialdep.feature
  pd = generatePartialDependenceData(mod, task, features = fn)
  plotPartialDependence(pd)
}

makeImportSideBar = function(type) {
  imptype.sel.input = selectInput("import.type", "Type", selected = type, choices = c("mlr", "CSV", "OpenML"))
  switch(type,
    CSV = fluidRow(
      box(width = 4, height = 250,
        imptype.sel.input,
        fileInput("import.file", "Choose CSV File",
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      # tags$hr(),
      box(width = 8, height = 250,
        checkboxInput("import.header", "Header", TRUE),
        selectInput("import.sep", "Separator", selected = ",",
          choices = c(Comma = ",", Semicolon = ";", Tab = "\t")),
        selectInput("import.quote", "Quote", selected = '"',
          choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"))
      )#,
      #      textInput("import.rownames", "Row Names", NULL)
    ),
    mlr = fluidRow(
      box(width = 3,
        imptype.sel.input
      ),
      box(width = 9,
        selectInput("import.mlr", "Choose toy task", choices = c("iris.task", "bh.task", "sonar.task"))
      )
    ),
    OpenML = fluidRow(
      box(width = 3,
        imptype.sel.input
      ),
      box(width = 9,
        numericInput("import.OpenML", "Choose OpenML Task ID", value = 59L)
      )
    )
  )
}

makeImportPredSideBar = function(type) {
  imptype.sel.input = selectInput("import.pred.type", "Type", selected = type, choices = c("mlr", "NewData", "OpenML"))
  switch(type, 
    NewData = list(
      imptype.sel.input,
      fileInput("import.pred.file", "Choose CSV File",
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      tags$hr(),
      checkboxInput("import.pred.header", "Header", TRUE),
      selectInput("import.pred.sep", "Separator", selected = ",",
        choices = c(Comma=",", Semicolon=";", Tab="\t")),
      selectInput("import.pred.quote", "Quote", selected = '"',
        choices = c(None="", "Double Quote"='"', "Single Quote"="'"))#,
      # textInput("import.rownames", "Row Names", NULL)
    ),
    mlr = list(
      imptype.sel.input,
      selectInput("import.pred.mlr", "Choose toy task", choices = c("iris.task", "bh.task", "sonar.task"))
    ),
    OpenML = list(
      imptype.sel.input,
      numericInput("import.pred.OpenML", "Choose OpenML Task ID", value = 59L)
    )
  )
}
