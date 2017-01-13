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

determinePerformanceStatus = function(worst, best, perf) {
  worst = replaceInfiniteValues(worst)
  best = replaceInfiniteValues(best)
  if (is.na(perf)) {
    status = "primary"
    color = "color:black"
  } else {
    if (best == 0)
      best = 1e-16
    if (perf == 0)
      perf = 1e-16
    perf.rel = perf / best
    if (perf.rel <= 0.33) {
      status = "danger"
      color = "color:#dd4b39"
    } else {
      if (perf.rel <= 0.66) {
        status = "warning"
        color = "color:#f39c12"
      } else {
        status = "success"
        color = "color:#00a65a"
      }
    }    
  }
  return(list(status = status, color = color))
}

makePerformanceUI = function(measures, performances) {
  ms.ids = names(performances)
  ms.names = extractSubList(measures, "name")
  ms.worst = extractSubList(measures, "worst")
  ms.best = extractSubList(measures, "best")
  # ms.min = extractSubList(measures, "minimize")
  statuses = Map(function(worst, best, perf) {
     determinePerformanceStatus(worst, best, perf)
  }, ms.worst, ms.best, performances)
  
  boxes = Map(function(ms.id, ms.name, perf, worst, best, status) {
    box(title = ms.id, status = status$status, solidHeader = TRUE, width = 3, height = 200,
      fluidRow(
        column(width = 12, h5(ms.name), align = "center")
      ),
      fluidRow(
        column(width = 12, div(h4(strong(perf)), style = status$color), align = "center")
      ),
      fluidRow(
        column(width = 12, align = "center",
          makeInfoDescription("worst", worst, width = 6),
          makeInfoDescription("best", best, width = 6)
        )

      )
    )
  }, ms.ids, ms.names, performances, ms.worst, ms.best, statuses)
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
