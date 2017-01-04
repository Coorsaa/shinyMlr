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

# output$predictionplot.learner.sel = renderUI({
#   reqAndAssign(learners(), "lrns")
#   lids = names(lrns)
#   selectInput("predictionplot.learner.sel", "Choose a learner:", choices = lids,
#     width = 200)
# })

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



# output$predictionplot = renderPlot({
#   feats = input$predictionplot.x.sel
#   lrn = input$predictionplot.learner.sel
#   ms = getFirst(measures())
#   if (length(feats) %in% 1:2) {
#     plotLearnerPrediction(learner = lrn, task = task(), features = feats, measures = ms, cv = 0)
#   }
# })

output$predictionplot.settings = renderUI({
  reqAndAssign(pred(), "preds")
  reqAndAssign(task.numeric.feature.names(), "fnames") #FIXME
  ms = measures.train.avail()
  ms.def = measures.default()
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  makePredictionPlotSettingsUI(plot.type, fnames, ms.def, ms)
})

measures.plot = reactive({
  tsk = isolate(task())
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  if (plot.type == "prediction") {
    ms = input$plot.measures.sel
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
  reqAndAssign(task(), "tsk")
  tsk.type = task.type()
  plot.type = input$prediction.plot.sel
  lrn = learners()[[input$train.learner.sel]]
  feats = input$predictionplot.feat.sel
  preds = pred()
  ms = measures.plot()
  num.levels = length(target.levels())
  resplot.type = input$residualplot.type
  makePredictionPlot(tsk.type, plot.type, lrn, feats, preds, ms,
    num.levels, resplot.type)
})

output$confusion.matrix = renderPrint({
  reqAndAssign(isolate(pred()), "preds")
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  if (plot.type == "confusion matrix") {
    t = makeConfusionMatrix(plot.type, preds)
    #dt = datatable(t, rownames = TRUE)
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


