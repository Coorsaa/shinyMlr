library(tools)
library(mlr)
library(readr)
library(BBmisc)
library(checkmate)
library(ParamHelpers)
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
      t = getOMLDataSet(data.id = as.integer(input$import.OpenML))
      return(t$data)
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

  output$import.browse.openml = renderDataTable({
    listOMLDataSets()[,c(1:5,10:12)]
  })

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
    # id = file_path_sans_ext(input$import.file$name)
    # FIXME: quickinit / remove later
    id = "iris"
    textInput("task.id", "Task ID", value = id)
  })
  
  output$task.target = renderUI({
    choices = as.list(colnames(data()))
    selectInput("task.target", "Choose a target:", choices = choices, selected = getLast(choices))
  })
  
  task = eventReactive(input$create.task, {
    d = isolate({data()})
    if (is.null(d)) return(NULL)
    colnames(d) = make.names(colnames(d)) 
    sMakeTask(input$task.id, input$task.target, d)
  })
  
  output$task.overview = renderPrint({
    validate(need(input$create.task != 0L, "you didn't create a task yet"))
    tt = task()# ; if (is.null(tt)) return(NULL)
    print(tt)
  })

  ##### learners #####

  learners.avail = reactive({
    validate(need(input$create.task != 0L,
      "create a task first to list suitable learners"))
    tt = task()
    listLearners(tt)
  })
  
  learners.default = reactive({
    req(task())
    tt = getTaskType(task())
    switch(tt, 
      classif =  c("classif.randomForest", "classif.svm", "classif.rpart"),
      regr = c("regr.randomForest", "regr.svm", "regr.rpart"))
  })
  
  output$learners.sel = renderUI({
    req(learners.avail())
    ls = learners.avail()
    ls.ids = ls$class
    selectInput("learners.sel", "", choices = ls.ids, multiple = TRUE,
      selected = learners.default())
  })

  learners.par.sets = reactive({
    lrns.sel = input$learners.sel
    par.sets = lapply(lrns.sel, getParamSet)
    names(par.sets) = lrns.sel
    return(par.sets)
  })

  learners.params.ui = reactive({
    req(learners.par.sets())
    req(learners.params())
    par.sets = learners.par.sets()
    params.inp = learners.params()
    makeLearnerParamUI(par.sets, params.inp)
  })

  output$learners.ui = renderUI({
    req(learners.params.ui)
    lrns.sel = input$learners.sel
    par.sets = isolate(learners.par.sets())
    params = isolate(learners.params.ui())
    makeLearnerConstructionUI(lrns.sel, par.sets, params)
  })

  learners.pred.types = reactive({
    lrns = input$learners.sel
    lrns.pred.types = vcapply(lrns, function(lrn) {
      pred.type = pasteDot("lrn.prob.sel", lrn)
      pred.type = input[[pred.type]]
      if (is.null(pred.type)) {
        "response"
      }
      else {
        if (pred.type == "Yes") {
          "prob"
        } else {
          "response"
        }
      }
    })
    lrns.pred.types
  })

  learners.threshold = reactive({
    lrns = input$learners.sel
    lrns.threshold = vcapply(lrns, function(lrn) {
      threshold = pasteDot("lrn.threshold", lrn)
      threshold = input[[threshold]]
      if (is.null(threshold) || threshold == "")
        return(NULL)
      return(threshold)
    })
    lrns.threshold
  })

  learners.params = reactive({
    req(learners.par.sets())
    par.sets = learners.par.sets()
    lrns.names = names(par.sets)
    params = extractSubList(par.sets, "pars")
    params.names = lapply(params, names)
    lrns.params = Map(function(lrn.name, pars) {
      par.names = params.names[[lrn.name]]
      lrn.params = Map(function(param.name, lrn.par) {
        par.name = pasteDot(lrn.name, param.name)
        par = input[[par.name]]
        par = convertParamForLearner(lrn.par, par)
      }, par.names, pars)
      names(lrn.params) = par.names
      lrn.params
    }, lrns.names, params)
    names(lrns.params) = lrns.names
    lrns.params = lapply(lrns.params, function(pars) {
      par.keep = !(unlist(lapply(pars, is.null)))
      pars[par.keep]
    })
    lrns.params
  })

  learners = reactive({ 
    req(learners.params())
    lrns.params = learners.params()
    lrns.sel = isolate({input$learners.sel})
    pred.types = isolate({learners.pred.types()})
    lrns = Map(function(lrn, pars, pred.type) {
      makeLearner(lrn, predict.type = pred.type,
        par.vals = pars)
    }, lrns.sel, lrns.params, pred.types)
    setNames(lrns, lrns.sel)
  })

  #### train and predict ####

  output$train.learner.sel = renderUI({
    ls = learners(); if (is.null(ls)) return(NULL)
    ls.ids = names(ls)
    selectInput("train.learner.sel", "Learners", choices = ls.ids)
  })
  
  trn = eventReactive(input$train.run, {
    tt = isolate({task()}); if (is.null(tt)) return(NULL)
    lrns = isolate({learners()})
    lrn = lrns[[isolate({input$train.learner.sel})]]
    train(lrn, tt)
  })
  
  output$train.overview = renderInfoBox({
    infoBox("", width = 5,
      ifelse(!is.null(trn()), "Model successfully trained",
        #FIXME: if not trained nothing shows
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
    } else if (input$import.pred.type == "CSV") {
      f = input$import.pred.csv$datapath
      if (is.null(f)) return(NULL)
      #rn = as.numeric(input$import.rownames)
      read.csv(f, header = input$import.pred.header, sep = input$import.pred.sep,
        quote = input$import.pred.quote) #, row.names = rn)
    } else if (input$import.pred.type == "OpenML") {
      t = getOMLDataSet(data.id = input$import.pred.OpenML)
      return(t$data)
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
  
  ##### benchmark #####
  
  output$benchmark.learners.sel = renderUI({
    req(learners())
    ls.ids = names(learners())
    selectInput("benchmark.learners.sel", "Learners", choices = ls.ids,
      multiple = TRUE, selected = ls.ids)
  })
  
  strat = reactive({
    req(learners())
    ls = learners()
    ls[1L]$type
    # print(ls)
  })

  output$stratText = renderText({
    req(strat())
    paste(strat())
  })

  observeEvent(strat(), {
    if (strat() != "classif") {
      shinyjs::hide("benchmark.stratification")
    } else {
      shinyjs::show("benchmark.stratification")
    }
  })
  
  rdesctype = reactive(input$benchmark.rdesctype)
  output$rdesctypeText = renderText(paste(rdesctype()))
  
  observeEvent(rdesctype(), {
    if (rdesctype() %in% c("LOO", "RepCV", "Holdout")) {
      shinyjs::hide("benchmark.iters")
    } else {
      shinyjs::show("benchmark.iters")
    }
  })
  
  rdesc = reactive({
    if (input$benchmark.rdesctype %in% c("CV", "Subsample", "Bootstrap")) {
      makeResampleDesc(input$benchmark.rdesctype, iters = input$benchmark.iters)
    } else {
      makeResampleDesc(input$benchmark.rdesctype)
    }
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
    lrns = learners()[input$benchmark.learners.sel]
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
  
})


