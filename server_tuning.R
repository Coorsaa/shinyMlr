output$tuning.sel = renderUI({
  validateTask(input$create.task, task.data(), data$data, req = TRUE)
  validateLearner(input$learners.sel)
  reqAndAssign(measures.avail(), "ms")
  reqAndAssign(measures.default(), "ms.def")
  lrns = learners()
  lrns.ids = names(lrns)
  list(
    column(6, align = "center",
      selectInput("tuning.learner.sel", "Choose learner to tune", choices = lrns.ids)
    ),
    column(6, align = "center",
      selectInput("tuning.method", "Choose tuning method", 
        choices = c("Grid", "Random", "irace"), selected = "Grid")
    ),
    column(width = 4,
      uiOutput("tuning.iters")
    ),
    column(width = 4,
      numericInput("tuning.cv", "No. of CV folds", min = 1L, max = Inf, value = 3L, step = 1L)
    ),
    column(width = 4,
      selectInput("tuning.measure", "Choose performance measure", choices = ms,
        selected = ms.def, multiple = TRUE)
    ),
    uiOutput("tuning.parallel.ui")
  )
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
  fluidRow(
    column(width = 6, align = "center",
      radioButtons("tuning.parallel", "Parallel tuning?", choices = c("Yes", "No"), selected = "No")
    ),
    column(width = 6, align = "center",
      numericInput("tuning.parallel.nc", "No. of cores", min = 1L, max = Inf, value = 2L, step = 1L)
    )
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
}, caption = "Click on params you want to tune and go to 'Param settings' tab afterwards")


output$tuning.params.table = DT::renderDataTable({
  reqAndAssign(tuning.par.set(), "par.set")
  req(input$tuning.table_rows_selected)
  ParamHelpers:::getParSetPrintData(par.set)[input$tuning.table_rows_selected,]
}, caption = "Below your chosen params you want to tune:")



output$tuning.learner.params = renderUI({
  reqAndAssign(tuning.par.set(), "par.set")
  param.ids = getParamIds(par.set)[input$tuning.table_rows_selected]
  param.types = getParamTypes(par.set)[input$tuning.table_rows_selected]
  makeTuningParameterUI(par.set, param.ids, param.types)
})


tuning = eventReactive(input$tune.run, {
  reqAndAssign(isolate(learners()), "lrns")
  reqAndAssign(tuning.par.set(), "par.set")
  reqAndAssign(input$tuning.learner.sel, "lrn")
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
        param.low = input[[paste0("tune.par.lower.", param)]]
        param.up = input[[paste0("tune.par.upper.", param)]]
        makeNumericParam(id = param, lower = param.low, upper = param.up)#, default = param.def) #FIXME is default necessary somehow?
      } else if (param.type == "integer") {
        param.low = input[[paste0("tune.par.lower.", param)]]
        param.up = input[[paste0("tune.par.upper.", param)]]
        makeIntegerParam(id = param, lower = param.low, upper = param.up)#, default = param.def)
      } else if (param.type == "discrete") {
        param.box = input[[paste0("tune.par.checkbox", param)]]
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
    res = tuneParams(lrn, task = tsk, resampling = rdesc, par.set = ps,
        control = ctrl, measures = ms)
    },
      message = function(m) {
        shinyjs::html(id = "tuning.text", html = m$message, add = FALSE)
    })
  } else {
    parallelStartSocket(input$tuning.parallel.nc)
    withCallingHandlers({
    res = tuneParams(lrn, task = tsk, resampling = rdesc, par.set = ps,
        control = ctrl, measures = ms)
    },
      message = function(m) {
        shinyjs::html(id = "tuning.text", html = m$message, add = FALSE)
      })
    parallelStop()
  }
  
  lrn.sel = lrns[[lrn]]
  tuned.lrn = setHyperPars(lrn.sel, par.vals = res$x)
  lrns[[lrn]] = tuned.lrn
  learner$tuned.learner = lrns
  return(res)
})


output$print.tuning.ps = renderPrint({
  reqAndAssign(tuning(), "result")
  return(result)
})


observe({
  if (is.null(learner$tuned.learner)) {
    shinyjs::hide("tune.set.hp")
  } else {
    shinyjs::show("tune.set.hp")
  }
})

transfer.learners = observeEvent(input$tune.set.hp, {
  reqAndAssign(learner$tuned.learner, "lrns")
  learner$learner = lrns
  return(lrns)
})

