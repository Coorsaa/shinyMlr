makeImportPredSideBar = function(type, newdata.type) {
  if (newdata.type == "task") {
    return(NULL)
  } else {
    switch(type, 
      mlr = list(
        selectInput("import.pred.mlr", "Choose toy task", choices = c("iris.task", "bh.task", "sonar.task"))
      ),
      OpenML = list(
        numericInput("import.pred.OpenML", "Choose OpenML Data ID", value = 61L)
      ),
      CSV = list(
        fileInput("import.pred.csv", "Choose CSV File",
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        tags$hr(),
        checkboxInput("import.pred.header", "Header", TRUE),
        selectInput("import.pred.sep", "Separator", selected = ",",
          choices = c(Comma = ",", Semicolon = ";", Tab = "\t")),
        selectInput("import.pred.quote", "Quote", selected = '"',
          choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"))
      ),
      ARFF = list(
        fileInput("import.pred.arff", "Choose ARFF File",
          accept = c("text/arff", "text/comma-separated-values,text/plain", ".arff"))
      )
    )
  }
}



makePerformanceUI = function(performance) {
  ms.names = names(performance)
  boxes = Map(function(perf, ms.name) {
    valueBox(ms.name, perf, color = "light-blue", width = 2)
  }, performance, ms.names)
  boxes
}

makePredictionPlot = function(tsk.type, plot.type, lrn, feats, preds, ms, resplot.type) {
  if (plot.type == "prediction") {
    validate(checkPlotLearnerPrediction(tsk.type, feats))
    q = plotLearnerPrediction(learner = lrn, features = feats, task = tsk, cv = 0)
  } else if (plot.type == "residuals") {
    req(resplot.type)
    resplot.type = switch(resplot.type,
      scatterplot = "scatterplot",
      "histogram" = "hist")
    q = plotResiduals(preds, type = resplot.type)
  } else if (plot.type == "confusion matrix") {
    q = NULL
  } else {
    checkPlotROCCurves(lrn)
      df = generateThreshVsPerfData(preds, measures = ms)
      q = plotROCCurves(df)
  }
  return(q)
}

makeConfusionMatrix = function(plot.type, preds) {
  conf = calculateConfusionMatrix(preds)
  return(conf$result)
}

makePredictionPlotSettingsUI = function(plot.type, fnames, ms.def, ms, width = 200) {
  if (plot.type == "prediction") {
    settings.inp = selectInput("predictionplot.feat.sel", "Select variables:",
      choices = fnames, multiple = TRUE, width = width)
    settings.ui = column(width = 4, settings.inp)
  } else if (plot.type == "residuals") {
      settings.inp = selectInput("residualplot.type", "Select type of plot:",
        choices = c("scatterplot", "histogram"), selected = "scatterplot",
        width = width)
      settings.ui = column(4, settings.inp)
  } else {
    settings.ui = column(4, NULL)
  }
  return(settings.ui)
}