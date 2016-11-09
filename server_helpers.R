# FIXME: mlr: create makeAutoTask or whatever depending on target? 
sMakeTask = function(id, target, data) {
  y = data[, target]
  if (is.numeric(y))
    makeRegrTask(id = id, data = data, target = target)
  else if (is.factor(y))
    makeClassifTask(id = id, data = data, target = target)
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
  imptype.sel.input = selectInput("import.type", "Type", selected = type, choices = c("mlr", "OpenML", "CSV", "ARFF"))
  switch(type,
    mlr = fluidRow(
      box(width = 3,
        imptype.sel.input
      ),
      box(width = 9,
        selectInput("import.mlr", "Choose mlr task", choices = c("iris.task", "bh.task", "sonar.task"))
      )
    ),
    OpenML = fluidRow(
      box(width = 3,
        imptype.sel.input
      ),
      box(width = 9,
        selectInput("import.OpenML", "Choose OpenML Data ID", selected = 61L, choices = listOMLDataSets()[,1], multiple = FALSE)
      )
    ),
    CSV = fluidRow(
      box(width = 4, height = 250,
        imptype.sel.input,
        fileInput("import.csv", "Choose CSV File",
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
    ARFF = fluidRow(
      box(width = 4, height = 250,
        imptype.sel.input,
        fileInput("import.arff", "Choose ARFF File",
          accept = c("text/arff", "text/comma-separated-values,text/plain", ".arff"))
      )
    )
  )
}

makeImportPredSideBar = function(type) {
  imptype.sel.input = selectInput("import.pred.type", "Type", selected = type, choices = c("mlr", "OpenML", "CSV", "ARFF"))
  switch(type, 
    mlr = list(
      imptype.sel.input,
      selectInput("import.pred.mlr", "Choose toy task", choices = c("iris.task", "bh.task", "sonar.task"))
    ),
    OpenML = list(
      imptype.sel.input,
      numericInput("import.pred.OpenML", "Choose OpenML Data ID", value = 61L)
    ),
    CSV = list(
      imptype.sel.input,
      fileInput("import.pred.csv", "Choose CSV File",
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      tags$hr(),
      checkboxInput("import.pred.header", "Header", TRUE),
      selectInput("import.pred.sep", "Separator", selected = ",",
        choices = c(Comma = ",", Semicolon = ";", Tab = "\t")),
      selectInput("import.pred.quote", "Quote", selected = '"',
        choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"))#,
      # textInput("import.rownames", "Row Names", NULL)
    ),
    ARFF = list(
      imptype.sel.input,
      fileInput("import.pred.arff", "Choose ARFF File",
        accept = c("text/arff", "text/comma-separated-values,text/plain", ".arff"))
    )
  )
}

makeLearnerConstructionUI = function(lrns) {
    lrns.names = lrns
    par.sets = lapply(lrns.names, getParamSet)
    lrn.tabs = mapply(function (par.set, lrn.name) {
      pars.tab = renderTable({ParamHelpers:::getParSetPrintData(par.set)},
        rownames = TRUE)
      pars.sel = textInput(paste("hypparslist", lrn.name, sep = "."),
        "Hyperparameters:", "list()")
      lrn.has.probs = hasLearnerProperties(lrn.name, props = "prob")
      tabPanel(title = lrn.name, width = 12,
        pars.tab,
        pars.sel,
        if (lrn.has.probs) {
          selectInput(paste("lrn.prob.sel", lrn.name, sep = "."),
            "Probability estimation:", choices = c("Yes", "No"),
            multiple = FALSE, selected = "Yes", width = 200
          )
        } else {
          NULL
        }
      )
    }, par.sets, lrns.names, SIMPLIFY = FALSE)

    do.call(tabBox, c(lrn.tabs, width = 12))
}


