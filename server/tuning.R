##### tuning ####

output$tuning.sel = renderUI({
  req(task())
  validateLearner(learners(), req = TRUE)
  validateLearner(learners(), req = TRUE, check = "err")
  reqAndAssign(measures.tuning.avail(), "ms")
  reqAndAssign(measures.default(), "ms.def")
  lrns = learners()
  lrns.ids = names(lrns)
  list(
    selectInput("tuning.learner.sel", "Choose learner to tune", choices = lrns.ids),
    selectInput("tuning.method", "Choose tuning method", 
      choices = c("Grid", "Random", "irace"), selected = "Grid"),
    uiOutput("tuning.iters"),
    numericInput("tuning.cv", "No. of CV folds", min = 1L, max = Inf, value = 3L, step = 1L),
    selectInput("tuning.measure", "Choose performance measure", choices = ms,
      selected = ms.def, multiple = TRUE),
    uiOutput("tuning.parallel.ui")
  )
})

output$tuning.validation = renderUI({
    validateTask(input$create.task, task.data(), data$data,
    task.weights = input$task.weights, req = TRUE)
  validateLearner(lrns = learner$learner)
  validateLearner(lrns = learners(), check = "err")
})

measures.tuning.avail = reactive({
  reqAndAssign(task(), "tsk")
  req(learners())
  listMatchingMeasures(tsk, learners())
})

tuning.measures.perf = reactive({
  tsk = isolate(task())
  listMeasures(tsk, create = TRUE)[input$tuning.measure]
})


output$tuning.iters = renderUI({
  reqAndAssign(input$tuning.method, "method")
  if (method == "Random")
    numericInput("tuning.iters", "No. of iterations", min = 1L, max = Inf, value = 100L, step = 1L)
  else if (method == "Grid")
    numericInput("tuning.res", "No. of steps (resolution)", min = 1L, max = Inf, value = 10L, step = 1L)
  else if (method == "irace")
    numericInput("tuning.max.exp", "No. of maximal Experiments", min = 1L, max = Inf, value = 200L, step = 1L)
})

output$tuning.parallel.ui = renderUI({
  list(
    radioButtons("tuning.parallel", "Parallel tuning?", choices = c("Yes", "No"), selected = "No"),
    numericInput("tuning.parallel.nc", "No. of cores", min = 1L, max = Inf, value = 2L, step = 1L)
  )
})

observeEvent(input$tuning.parallel, {
  if (input$tuning.parallel == "No") {
    shinyjs::hide("tuning.parallel.nc", animType = "fade")
  } else {
    shinyjs::show("tuning.parallel.nc", anim = TRUE)
  }
})


tuning.learner = reactive({
  reqAndAssign(input$tuning.learner.sel, "lrn.sel")
  lrns = isolate({learners()})
  lrn = lrns[[isolate({lrn.sel})]]
  lrn
})


tuning.par.set = reactive({
  reqAndAssign(input$tuning.learner.sel, "lrn")
  par.sets = learners.par.sets()
  tuning.par.set = filterParams(par.sets[[lrn]], tunable = TRUE, 
    type = c("integer", "numeric", "discrete"))
  tuning.par.set
})

output$tuning.table = DT::renderDataTable({
  reqAndAssign(tuning.par.set(), "par.set")
  dt = ParamHelpers:::getParSetPrintData(par.set)
  dt
}, options = list(scrollX = TRUE, paging = FALSE, searching = FALSE, bInfo = FALSE),
  caption = "Click on params you want to tune and go to 'Param settings' tab afterwards")


output$tuning.learner.params = renderUI({
  reqAndAssign(tuning.par.set(), "par.set")
  param.ids = getParamIds(par.set)[input$tuning.table_rows_selected]
  param.types = getParamTypes(par.set)[input$tuning.table_rows_selected]
  makeTuningParameterUI(par.set, param.ids, param.types)
})


tuning = eventReactive(input$tune.run, {
  reqAndAssign(isolate(learners()), "lrns")
  reqAndAssign(tuning.par.set(), "par.set")
  reqAndAssign(tuning.learner(), "lrn")
  reqAndAssign(task(), "tsk")
  reqAndAssign(input$tuning.method, "method")
  reqAndAssign(input$tuning.cv, "cv")
  reqAndAssign(tuning.measures.perf(), "ms")
  reqAndAssign(input$tuning.parallel, "parallel")
  
  param.ids = getParamIds(par.set)[input$tuning.table_rows_selected]
  param.types = getParamTypes(par.set)[input$tuning.table_rows_selected]
  param.defs = Map(function(param) {par.set$pars[[param]]$default}, param.ids)
  
  ps = do.call("makeParamSet",
    Map(function(param, param.type, param.def) {
    
      if (param.type == "numeric") {
        
        validate(
          need(input[[paste0("tune.par.lower.", param)]], paste0("No lower value set for ", param, "!")),
          need(input[[paste0("tune.par.upper.", param)]], paste0("No upper value set for ", param, "!"))
        )
        
        param.low = input[[paste0("tune.par.lower.", param)]]
        param.up = input[[paste0("tune.par.upper.", param)]]
        param.trafo = input[[paste0("tune.par.trafo.", param)]]
        
        if (param.trafo == "linear")
          makeNumericParam(id = param, lower = param.low, upper = param.up)
        else if (param.trafo == "log2")
          makeNumericParam(id = param, lower = param.low, upper = param.up, trafo = function (x) 2^x)
        else if (param.trafo == "log10")
          makeNumericParam(id = param, lower = param.low, upper = param.up, trafo = function (x) 10^x)
      } else if (param.type == "integer") {
        
        validate(
          need(input[[paste0("tune.par.lower.", param)]], paste0("No lower value set for ", param, "!")),
          need(input[[paste0("tune.par.upper.", param)]], paste0("No upper value set for ", param, "!"))
        )
        
        param.low = input[[paste0("tune.par.lower.", param)]]
        param.up = input[[paste0("tune.par.upper.", param)]]
        param.trafo = input[[paste0("tune.par.trafo.", param)]]
        
        if (param.trafo == "linear")
          makeIntegerParam(id = param, lower = param.low, upper = param.up)
        else if (param.trafo == "log2")
          makeIntegerParam(id = param, lower = param.low, upper = param.up, trafo = function (x) 2^x)
        else if (param.trafo == "log10")
          makeIntegerParam(id = param, lower = param.low, upper = param.up, trafo = function (x) 10^x)
      } else if (param.type == "discrete") {
        
        validate(
          need(input[[paste0("tune.par.checkbox", param)]], paste0("No values selected for ", param, "!"))
        )
        
        param.box = input[[paste0("tune.par.checkbox", param)]]
        if (suppressWarnings(!any(is.na(as.numeric(param.box)))))
          param.box = as.integer(param.box)
        makeDiscreteParam(id = param, values = param.box)
      }
    }, param.ids, param.types, param.defs)
  )

  rdesc = makeResampleDesc("CV", iters = input$tuning.cv)

  if (method == "Grid") {
    res = input$tuning.res
    ctrl = makeTuneControlGrid(resolution = res)
  } else if (method == "Random") {
    iters = input$tuning.iters
    ctrl = makeTuneControlRandom(maxit = iters)
  } else if (method == "irace") {
    max.exp = input$tuning.max.exp
    ctrl = makeTuneControlIrace(maxExperiments = max.exp)
  }
  
  if (parallel == "No") {
    withCallingHandlers({
    res = tryCatch(tuneParams(lrn, task = tsk, resampling = rdesc, par.set = ps,
      control = ctrl, measures = ms), error = errAsString)
    },
      message = function(m) {
        shinyjs::html(id = "tuning.text", html = m$message, add = FALSE)
    })
  } else {
    parallelStartSocket(cpus = input$tuning.parallel.nc, level = "mlr.tuneParams")
    withCallingHandlers({
    res = tryCatch(tuneParams(lrn, task = tsk, resampling = rdesc, par.set = ps,
      control = ctrl, measures = ms), error = errAsString)
    },
      message = function(m) {
        shinyjs::html(id = "tuning.text", html = m$message, add = FALSE)
      })
    parallelStop()
  }
  
  # configureMlr()

  if (!is.character(res)) {
    tuned.lrn = setHyperPars(lrn, par.vals = res$x)
    lrns[[lrn$id]] = tuned.lrn
    learner$tuned.learner = lrns    
  }
  return(res)
})


output$print.tuning.ps = renderPrint({
  validateExperiment(tuning(), "TuneResult")
  return(tuning())
})


observe({
  if (is.null(learner$tuned.learner)) {
    shinyjs::hide("tune.set.hp")
  } else {
    shinyjs::show("tune.set.hp")
  }
})

observeEvent(input$tune.set.hp, {
  reqAndAssign(learner$tuned.learner, "lrns")
  learner$learner = lrns
  return(lrns)
})


output$transfer.info.box = renderInfoBox({
  infoBox("Success", "Hyper parameters successfully transfered to learner!",
    icon = icon("info-circle"), width = 12)
})


observeEvent(input$tune.set.hp, {
  req(tuning())
  shinyjs::show("transfer.info.box")
})

observeEvent(input$tune.run, {
  shinyjs::hide("transfer.info.box")
})



