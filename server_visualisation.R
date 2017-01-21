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
  makePredictionPlotSettingsUI(plot.type, fnames, ms.def, ms, tsk.type, fm)
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
  reqAndAssign(task(), "tsk")
  tsk = task()
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  lrn = learners()[[lrn.sel]]
  feats = input$predictionplot.feat.sel
  preds = pred()
  ms = measures.plot()
  resplot.type = input$residualplot.type
  if (plot.type == "variable importance")
    reqAndAssign(input$vi.method, "vi.method")
  makePredictionPlot(tsk, tsk.type, plot.type, lrn, feats, preds, ms,
    resplot.type, vi.method)
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

##### partial dependency #####

output$partialdep.learner = renderUI({
  lrns = learners(); if (is.null(lrns)) return(NULL)
  lids = names(lrns)
  selectInput("partialdep.learner", "Choose a model:", choices = lids)
})

output$partialdep.feature = renderUI({
  selectInput("partialdep.feature", "Choose a feature:", getTaskFeatureNames(task()))
})

output$partialdep.plot = renderPlot({
  tt = task(); if (is.null(tt)) return(NULL)
  lrns = learners()
  sPlotPartialDep(input, tt, lrns)
})


