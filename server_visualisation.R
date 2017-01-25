##### visualization #####



##### benchmark plots #####

output$bmrplot.measures.sel = renderUI({
  ms = input$benchmark.measures.sel
  selectInput("plot.measures.sel", "Measures", choices = ms,
    selected = measures.default(), width = 200)
})

output$bmrplots = renderPlot({
  plot.type = switch(input$bmrplots.type,
    Beanplots = "violin",
    Boxplots = "box"
  )
  ms = measures.bmr()[[input$plot.measures.sel]]
  plotBMRBoxplots(bmr(), style = plot.type, measure = ms)
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
  feats = task.feature.names()
  ms = measures.train.avail()
  ms.def = measures.default()
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  tsk.type = getTaskType(task())
  reqAndAssign(isolate(filter.methods()), "fm")
  lrn.sel = input$train.learner.sel
  lrn = isolate(learners())[[lrn.sel]]
  predict.type = lrn$predict.type
  makePredictionPlotSettingsUI(plot.type, fnames, feats, ms.def, ms, tsk.type, fm, predict.type)
})

measures.plot = reactive({
  tsk = isolate(task())
  reqAndAssign(measures.default(), "ms.def")
  reqAndAssign(input$prediction.plot.sel, "plot.type")
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
  tsk.type = tsk$type
  reqAndAssign(isolate(model()), "mod")
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  lrn = learners()[[lrn.sel]]
  fnames = task.numeric.feature.names()
  feats = input$predictionplot.feat.sel
  preds = pred()
  ms = measures.plot()
  resplot.type = input$residualplot.type
  if (plot.type == "variable importance")
    reqAndAssign(input$vi.method, "vi.method")
  
  if (plot.type == "partial dependency" && lrn$predict.type == "se")
    ind = "FALSE"
  else
    ind = as.logical(input$pd.plot.ind)
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

