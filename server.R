library(tools)
library(mlr)
library(readr)
library(BBmisc)
library(farff)
library(OpenML)
library(ggplot2)

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
    } else if (input$import.type == "OpenML") {
      t = getOMLTask(task.id = input$import.OpenML)
      return(t$input$data.set$data)
    } else if (input$import.type == "CSV") {
      f = input$import.csv$datapath
      if (is.null(f)) return(NULL)
      #rn = as.numeric(input$import.rownames)
      read.csv(f, header = input$import.header, sep = input$import.sep,
        quote = input$import.quote) #, row.names = rn)
    } else if (input$import.type == "ARFF") {
      f = input$import.arff$datapath
      if (is.null(f)) return(NULL)
      readARFF(f)
    }
  })

  
  output$import.preview = renderDataTable({
    d = data(); if (is.null(d)) return(NULL)
    colnames(d) = make.names(colnames(d)) 
    d
  }, options = list(lengthMenu = c(5, 20, 50), pageLength = 5, scrollX = TRUE)
  )
  
  ##### data summary #####
  
  output$summary.datatable = renderDataTable({
    d = data(); if (is.null(d)) return(NULL)
    colnames(d) = make.names(colnames(d)) 
    summarizeColumns(d)
  }, options = list(lengthMenu = c(5, 20, 50), pageLength = 5)
  )
  
  
  output$summary.vis.hist.var = renderUI({
    d = data()
    nums <- sapply(d, is.numeric)
    choices = colnames(d[, nums])
    # choices = as.list(colnames(data()))
    selectInput("summary.vis.hist.var", "Choose a variable:", choices = choices, selected = getLast(choices), width = "95%")
  })
  
  output$summary.vis.hist.nbins = renderUI({
    sliderInput("summary.vis.hist.nbins", "Number of bins", min = 1L, max = 100L, value = 30L, step = 1L, width = "95%")
  })

  output$summary.vis.hist = renderPlot({
    d = data() 
    if (is.null(d)) return(NULL)
      ggplot(data = d, aes(x = as.numeric(d[,input$summary.vis.hist.var]))) + 
      geom_histogram(aes(y = ..density..), stat = "bin", bins = input$summary.vis.hist.nbins) + 
      geom_density() + xlab(input$summary.vis.hist.var)
   })    
  
  output$summary.vis.bp.var = renderUI({
    d = data()
    nums <- sapply(d, is.numeric)
    choices = colnames(d[, nums])
    # choices = as.list(colnames(data()))
    selectInput("summary.vis.bp.var", "Choose a variable:", choices = choices, selected = getLast(choices), width = "95%")
  })
  
  output$summary.vis.bp = renderPlot({
    d = data() 
    if (is.null(d)) return(NULL)
    ggplot(data = d, aes(y = as.numeric(d[,input$summary.vis.bp.var]), x = input$summary.vis.bp.var)) + 
      geom_boxplot() + ylab(input$summary.vis.bp.var) + xlab("")
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
    colnames(d) = make.names(colnames(d)) 
    sMakeTask(input, d)
  })
  
  output$task.overview = renderPrint({
    tt = task(); if (is.null(tt)) return(NULL)
    print(tt)
  })
  
  ##### benchmark #####
  
  learners.avail = reactive({
    tt = task(); if (is.null(tt)) return(NULL)
    if (getTaskType(tt) == "classif") {
      ls = listLearners(tt, properties = "prob") 
    } else {
      ls = listLearners(tt)
    }
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
    res = list()
    for (i in 1:length(input$benchmark.learners.sel)) {
      if (hasLearnerProperties(input$benchmark.learners.sel[i], props = "prob"))
        res[[i]] = makeLearner(input$benchmark.learners.sel[i], predict.type = "prob")
      else {
        res[[i]] = makeLearner(input$benchmark.learners.sel[i])
      }
    }
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
  }, options = list(lengthMenu = c(10, 20), pageLength = 10,
    scrollX = TRUE)
  )
  
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
  
  output$train.prob.sel = renderUI({
    tt = getTaskType(task()); if (is.null(tt)) return(NULL)
    if (tt == "classif")
      selectInput("train.prob.sel", "Probability estimation:", choices = c("Yes", "No"),
        multiple = FALSE, selected = "No", width = 200)
  })
  
  learners.train.avail = reactive({
    tt = task(); if (is.null(tt)) return(NULL)
    if (is.null(input$train.prob.sel)) {
      probs.dec = "No"
    } else {
      probs.dec = input$train.prob.sel
    }
    if (probs.dec == "Yes" && getTaskType(tt) == "classif") {
      ls = listLearners(tt, properties = "prob")
    } else {
      ls = listLearners(tt)
    }
    return(ls)
  })
  
  output$train.learner.sel = renderUI({
    ls = learners.train.avail(); if (is.null(ls)) return(NULL)
    ls.ids = ls$class
    selectInput("train.learner.sel", "Select a learner", choices = ls.ids, multiple = FALSE,
      selected = learners.default()[1], width = 200)
  })
  
  trn = eventReactive(input$train.run, {
    tt = task(); if (is.null(tt)) return(NULL)
    par.vals = eval(parse(text=input$hypparslist))
    
    if (input$train.prob.sel == "Yes" && getTaskType(tt) == "classif") {
      lrn = makeLearner(input$train.learner.sel, predict.type = "prob", par.vals = par.vals)
    }
    else {
      lrn = makeLearner(input$train.learner.sel, par.vals = par.vals)
    }
    train(lrn, tt)
  })
  

  
  getParamSet(makeLearner("classif.randomForest"))
  lrn = makeLearner("classif.randomForest")
  setHyperPars(lrn, par.vals = list(ntree = 100))
  
  
  
  output$train.overview = renderInfoBox({
    infoBox("", width = 5,
      ifelse(!is.null(trn()), "Model successfully trained",
        "Model was not trained yet")
      )
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
      f = input$import.pred.csv$datapath
      if (is.null(f)) return(NULL)
      #rn = as.numeric(input$import.rownames)
      read.csv(f, header = input$import.pred.header, sep = input$import.pred.sep,
        quote = input$import.pred.quote) #, row.names = rn)
    } else if (input$import.pred.type == "OpenML") {
      t = getOMLTask(task.id = input$import.pred.OpenML)
      return(t$input$data.set$data)
    } else if (input$import.type == "ARFF") {
      f = input$import.pred.arff$datapath
      if (is.null(f)) return(NULL)
      readARFF(f)
    }
  })
  
  output$import.pred.preview = renderDataTable({
    d = data.pred(); if (is.null(d)) return(NULL)
    colnames(d) = make.names(colnames(d))
    d
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
  )
  
  ##### predict on new data #####
  
  pred = eventReactive(input$predict.run, {
    model = trn()
    newdata = data.pred()
    colnames(newdata) = make.names(colnames(newdata)) 
    predict(model, newdata = newdata)
  })
  
  output$pred.overview = renderDataTable({
    p = pred(); if (is.null(p)) return(NULL)
    p$data
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
  )
  
  #### performance on the test data ####
  
  measures.train.avail = reactive({
    tt = task(); if (is.null(tt)) return(NULL)
    if (is.null(input$train.prob.sel)) {
      probs.dec = "No"
    } else {
      probs.dec = input$train.prob.sel
    }
    if (probs.dec == "Yes") {
      ls = listMeasures(tt)
    } else {
      prob.subset =  listMeasures(tt) %in% listMeasures(tt, properties = "req.prob")
      ls = listMeasures(tt)[!prob.subset]
    }
    return(ls)
  })
  
  output$perf.measures.sel = renderUI({
    ms = measures.train.avail(); if (is.null(ms)) return(NULL)
    selectInput("perf.measures.sel", "Choose performance measures", choices = ms, multiple = TRUE, selected = measures.default())
  })
  
  measures.perf = reactive({
    tt = task(); if (is.null(tt)) return(NULL)
    listMeasures(tt, create = TRUE)[input$perf.measures.sel]
  })
  
  perf = eventReactive(input$performance.run, {
    p = pred(); if (is.null(p)) return(NULL)
    model = trn()
    ms = measures.perf()
    performance(p, measures = ms, model = model)
  })
  
  output$performance.overview = renderTable({
    p = perf(); if (is.null(p)) return(NULL)
    data.frame(t(p), row.names = "")
  })
})


