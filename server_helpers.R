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


# FIXME: maybe we wnat this as a helper too in mlr directly plot pd plot for one feature??
sPlotPartialDep = function(input, task, learners) {
  lrn = input$partialdep.learner
  lrn = learners[[lrn]]
  mod = train(lrn, task)
  fn = input$partialdep.feature
  pd = generatePartialDependenceData(mod, task, features = fn)
  plotPartialDependence(pd)
}

makeImportSideBar = function(type) {
  imptype.sel.input = selectInput("import.type", "Type", selected = type, choices = c("mlr", "CSV"))
  switch(type, 
    CSV = sidebarPanel(
      imptype.sel.input,
      fileInput("import.file", "Choose CSV File",
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      tags$hr(),
      checkboxInput("import.header", "Header", TRUE),
      radioButtons("import.sep", "Separator",
        c(Comma=",", Semicolon=";", Tab="\t"), ","),
      selectInput("import.quote", "Quote", selected = '"',
        choices = c(None="", "Double Quote"='"', "Single Quote"="'")),
      textInput("import.rownames", "Row Names", 1)
      ),
    mlr = sidebarPanel(
      imptype.sel.input,
      selectInput("import.mlr", "Choose toy task", choices = c("iris.task", "bh.task"))
    )
  )
}

