##### visualization #####



##### benchmark plots #####

output$bmrplots = renderPlot({
  b = bmr(); if (is.null(b)) return(NULL)
  plotfun = switch(input$bmrplots.type,
    Beanplots = function(b) plotBMRBoxplots(b, style = "violin"),
    Boxplots = plotBMRBoxplots
  )
  plotfun(b)
})

##### prediction plot ####

output$visualisation.selection = renderUI({
  reqAndAssign(task(), "tsk")
  column(width = 4,
    makeVisualisationSelectionUI(tsk)
  )
})

output$predictionplot.x.sel = renderUI({
  fnames = task.feature.names() #FIXME
  selectInput("predictionplot.x.sel", "Select variables:", choices = fnames,
    multiple = TRUE)
})

output$predictionplot.settings = renderUI({
  reqAndAssign(pred(), "preds")
  fnames = task.numeric.feature.names()
  ms = measures.train.avail()
  ms.def = measures.default()
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  tsk.type = getTaskType(task())
  reqAndAssign(isolate(filter.methods()), "fm")
  lrn.sel = input$train.learner.sel
  lrn = isolate(learners())[[lrn.sel]]
  predict.type = lrn$predict.type
  makePredictionPlotSettingsUI(plot.type, fnames, ms.def, ms, tsk.type, fm, predict.type)
})

measures.plot = reactive({
  tsk = isolate(task())
  reqAndAssign(measures.default(), "ms.def")
  if (plot.type == "prediction") {
    ms = ms.def
  } else {
    if (plot.type == "ROC") {
      ms = c("fpr", "tpr")
    } else {
      ms = 1L
    }
  }
  listMeasures(tsk, create = TRUE)[ms]
})

output$prediction.plot = renderPlot({
  lrn.sel = input$train.learner.sel
  validateLearnerModel(model(), lrn.sel)
  validateTask(input$create.task, task.data(), data$data)
  reqAndAssign(isolate(task()), "tsk")
  reqAndAssign(isolate(model()), "mod")
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  lrn = learners()[[lrn.sel]]
  reqAndAssign(task.numeric.feature.names(), "fnames")
  preds = pred()
  ms = measures.plot()
  resplot.type = input$residualplot.type
  if (plot.type == "variable importance")
    reqAndAssign(input$vi.method, "vi.method")
  if (plot.type == "partial dependency") {
    if (lrn$predict.type == "se")
      ind = FALSE
    else
      ind = as.logical(input$pd.plot.ind)
    reqAndAssign(input$predictionplot.feat.sel, "feats")
  } else {
    feats = input$predictionplot.feat.sel
  }
  makePredictionPlot(mod, tsk, tsk.type, plot.type, lrn, fnames, feats, preds, ms,
    resplot.type, vi.method, ind)
})


output$confusion.matrix = renderPrint({
  reqAndAssign(isolate(pred()), "preds")
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  if (plot.type == "confusion matrix") {
    t = makeConfusionMatrix(plot.type, preds)
    print(t)
  } else {
    invisible(NULL)
  }
})

observeEvent(input$prediction.plot.sel, {
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  if (plot.type == "confusion matrix") {
    shinyjs::show("confusion.matrix", animType = "fade")
  } else {
    shinyjs::hide("confusion.matrix", anim = TRUE)
  }
})


filter.methods = reactive({
  listFilterMethods(tasks = TRUE)
})

