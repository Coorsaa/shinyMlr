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

output$predictionplot.learner.sel = renderUI({
  lrns = learners(); if (is.null(lrns)) return(NULL)
  lids = names(lrns)
  selectInput("predictionplot.learner.sel", "Choose a learner:", choices = lids)
})

output$predictionplot.x.sel = renderUI({
  tt = task(); if (is.null(tt)) return(NULL)
  fnames = getTaskFeatureNames(tt)
  selectInput("predictionplot.x.sel", "Select two variables:", choices = fnames, multiple = TRUE)
})

output$predictionplot = renderPlot({
  feats = input$predictionplot.x.sel
  lrn = input$predictionplot.learner.sel
  ms = getFirst(measures())
  if (length(feats) %in% 1:2) {
    plotLearnerPrediction(learner = lrn, task = task(), features = feats, measures = ms, cv = 0)
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


