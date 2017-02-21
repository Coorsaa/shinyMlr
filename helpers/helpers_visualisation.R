# FIXME: Should be done with validate/need
checkPlotLearnerPrediction = function(tsk.type, fnames, feats) {
  res = NULL
  validateNumFeatures(fnames)
  nfeats = length(feats)
  if (tsk.type == "regr") {
    if (nfeats %nin% 1:2)
      res = "You must choose one or two features to plot learner predictions."
  } else if (tsk.type == "classif") {
    if (nfeats != 2L)
      res = "You must choose exactly two features to plot learner predictions."
  }
  return(res)
}

checkPlotROCCurves = function(lrn) {
  validate(
    need(lrn$predict.type == "prob", "You must predict probabilities to plot ROC curves.")  
  )
}

checkPlotPartialDependency = function(tsk.type, lrn, fnames) {
  validateNumFeatures(fnames)
  if (tsk.type == "classif") {
    validate(
      need(lrn$predict.type == "prob", "You must predict probabilities to plot partial dependency plots.")  
    )
  }
}


# # FIXME: maybe we want this as a helper too in mlr directly plot pd plot for one feature??
# sPlotPartialDep = function(input, task, learners) {
#   lrn = input$partialdep.learner
#   lrn = learners[[lrn]]
#   mod = train(lrn, task)
#   fn = input$partialdep.feature
#   pd = generatePartialDependenceData(mod, task, features = fn)
#   plotPartialDependence(pd)
# }


makeVisualisationSelectionUI = function(tsk) {
  if (tsk$type == "classif") {
    if (length(getTaskClassLevels(tsk)) == 2) {
      vis.inp = selectInput("prediction.plot.sel", "Choose plot",
        choices = c("prediction", "residuals", "partial dependency", "confusion matrix", "ROC"),
        selected = "prediction plot", width = 200
      )
    } else {
      vis.inp = selectInput("prediction.plot.sel", "Choose plot",
        choices = c("prediction", "residuals", "partial dependency", "confusion matrix"),
        selected = "prediction plot", width = 200
      )
    }
  } else {
    vis.inp = selectInput("prediction.plot.sel", "Choose plot",
      choices = c("prediction", "residuals", "partial dependency"),
      selected = "prediction plot", width = 200
    )
  }
  return(vis.inp)
}

