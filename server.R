library(tools)
library(mlr)
library(readr)
library(BBmisc)

source("server_helpers.R")

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output) {
  
  # input$file1 will be NULL initially. after user selection a df with cols:
  # 'size', 'type', and 'datapath'
  
  ##### data import #####
  
  output$import.ui = renderUI({
    type = input$import.type; 
    if (is.null(type)) 
      type = "mlr"
    makeImportSideBar(type)
  })
  
  data = reactive({
    if (is.null(input$import.type)) {
      return(NULL)
    } else if (input$import.type == "mlr") {
      return(getTaskData(get(input$import.mlr)))
    } else if (input$import.type == "CSV") {
      f = input$import.file$datapath
      if (is.null(f)) return(NULL)
      #rn = as.numeric(input$import.rownames)
      read.csv(f, header = input$import.header, sep = input$import.sep,
        quote = input$import.quote) #, row.names = rn)
    }
  })
  
  output$import.preview = renderDataTable({
    d = data(); if (is.null(d)) return(NULL)
    d
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
  )
  
  ##### data summary #####
  
  output$summary.datatable = renderDataTable({
    d = data(); if (is.null(d)) return(NULL)
    summarizeColumns(d)
  })
  
  ##### task #####
  
  output$task.id = renderUI({
    id = file_path_sans_ext(input$import.file$name)
    # FIXME: quickinit / remove later
    id = "iris"
    textInput("task.id", "Task ID", value = id)
  })
  
  output$task.target = renderUI({
    choices = as.list(colnames(data()))
    selectInput("task.target", "Choose a target:", choices = choices, selected = getLast(choices))
  })
  
  task = reactive({
    d = data() 
    if (is.null(d)) return(NULL)
    sMakeTask(input, d)
  })
  
  output$task.overview = renderPrint({
    tt = task(); if (is.null(tt)) return(NULL)
    print(tt)
  })
  
  ##### benchmark #####
  
  learners.avail = reactive({
    tt = task(); if (is.null(tt)) return(NULL)
    ls = listLearners(tt)
    return(ls)
  })
  
  learners.default = reactive({
    tt = getTaskType(task()); if (is.null(tt)) return(NULL)
    switch(tt, 
      classif =  c("classif.randomForest", "classif.svm", "classif.rpart"),
      regr = c("regr.randomForest", "regr.svm", "regr.rpart"))
  })
  
  output$benchmark.learners.sel = renderUI({
    ls = learners.avail(); if (is.null(ls)) return(NULL)
    ls.ids = ls$class
    selectInput("benchmark.learners.sel", "Learners", choices = ls.ids, multiple = TRUE, selected = learners.default())
  })
  
  learners = reactive({
    res = lapply(input$benchmark.learners.sel, makeLearner)
    setNames(res, input$benchmark.learners.sel)
  })
  
  rdesc = reactive({
    makeResampleDesc(input$benchmark.rdesctype, iters = input$benchmark.iters)
  })
  
  measures.avail = reactive({
    tt = task(); if (is.null(tt)) return(NULL)
    listMeasures(tt, create = FALSE)
  })
  
  measures.default = reactive({
    tt = getTaskType(task()); if (is.null(tt)) return(NULL)
    switch(tt, 
      classif = "acc",
      regr = "mse")
  })
  
  output$benchmark.measures.sel = renderUI({
    ms = measures.avail(); if (is.null(ms)) return(NULL)
    selectInput("benchmark.measures.sel", "Measures", choices = ms, multiple = TRUE, selected = measures.default())
  })
  
  measures = reactive({
    tt = task(); if (is.null(tt)) return(NULL)
    listMeasures(tt, create = TRUE)[input$benchmark.measures.sel]
  })
  
  bmr = eventReactive(input$benchmark.run, {
    tt = task(); if (is.null(tt)) return(NULL)
    ms = measures()
    lrns = learners()
    rd = rdesc()
    
    withCallingHandlers({
      benchmark(lrns, tt, rd, measures = ms, show.info = TRUE)
    },
      message = function(m) {
        shinyjs::html(id = "benchmark.text", html = m$message, add = FALSE)
      })
  })
  
  output$benchmark.overview = renderDataTable({
    b = bmr(); if (is.null(b)) return(NULL)
    getBMRAggrPerformances(b, as.df = TRUE)
  })
  
  ##### benchmark plots #####
  
  output$bmrplots = renderPlot({
    b = bmr(); if (is.null(b)) return(NULL)
    plotfun = switch(input$bmrplots.type,
      Beanplots = function(b) plotBMRBoxplots(b, style = "violin"),
      Boxplots = plotBMRBoxplots,
      Ranks = plotBMRSummary
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
  
  #### train and predict ####
  
  output$train.learner.sel = renderUI({
    ls = learners.avail(); if (is.null(ls)) return(NULL)
    ls.ids = ls$class
    selectInput("train.learner.sel", "Select a learner", choices = ls.ids, multiple = FALSE, selected = learners.default()[1])
  })
  
  trn = eventReactive(input$train.run, {
    tt = task(); if (is.null(tt)) return(NULL)
    lrn = makeLearner(input$train.learner.sel)
    train(lrn, tt)
  })
  
  output$train.overview = renderPrint({
    b = trn(); if (is.null(b)) return(NULL)
    ifelse(!is.null(b), return("Model was successfully trained"), return(NULL))
  })
  
  ##### prediction data import #####
  
  output$import.pred.ui = renderUI({
    type = input$import.pred.type; 
    if (is.null(type)) 
      type = "mlr"
    makeImportPredSideBar(type)
  })
  
  data.pred = reactive({
    if (is.null(input$import.pred.type)) {
      return(NULL)
    } else if (input$import.pred.type == "mlr") {
      return(getTaskData(get(input$import.pred.mlr)))
    } else if (input$import.pred.type == "NewData") {
      f = input$import.pred.file$datapath
      if (is.null(f)) return(NULL)
      #rn = as.numeric(input$import.rownames)
      read.csv(f, header = input$import.pred.header, sep = input$import.pred.sep,
        quote = input$import.pred.quote) #, row.names = rn)
    }
  })
  
  output$import.pred.preview = renderDataTable({
    d = data.pred(); if (is.null(d)) return(NULL)
    d
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
  )
  
  ##### predict on new data #####
  
  pred = eventReactive(input$predict.run, {
    model = trn()
    newdata = data.pred()
    predict(model, newdata = newdata)
  })
  
  output$pred.overview = renderDataTable({
    p = pred(); if (is.null(p)) return(NULL)
    p$data
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
  )
  
  #### performance on the test data ####
  
  output$perf.measures.sel = renderUI({
    ms = measures.avail(); if (is.null(ms)) return(NULL)
    selectInput("perf.measures.sel", "Choose performance measures", choices = ms, multiple = TRUE, selected = measures.default())
  })
  
  measures.perf = reactive({
    tt = task(); if (is.null(tt)) return(NULL)
    listMeasures(tt, create = TRUE)[input$perf.measures.sel]
  })
  
  perf = eventReactive(input$performance.run, {
    p = pred(); if (is.null(p)) return(NULL)
    ms = measures.perf()
    performance(p, measures = ms)
  })
  
  output$performance.overview = renderDataTable({
    p = perf(); if (is.null(p)) return(NULL)
    as.data.frame(t(p))
  })
  
})


