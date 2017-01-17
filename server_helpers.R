#### general helpers ####

pasteDot = function(...) {
  paste(..., sep = ".")
}

writeBold = function(chr) {
  tags$b(chr)
}

makeInfoDescription = function(header, body, width) {
  column(width = width, align = "center", writeBold(header), h5(body)) 
}

replaceInfiniteValues = function(val) {
  if (is.infinite(val)) {
    if (val > 0) {
      val = 1e10
    } else {
      val = -1e10
    }
  }
  val
}

#### needy functions

reqAndAssign = function(obj, name) {
  req(obj)
  assign(name, obj, pos = 1L)
}

validateTask = function(tsk.button, tsk.df, df, req = FALSE) {
  validate(need(tsk.button != 0L, "you didn't create a task yet"))
  state.ok = identical(tsk.df, df)
  if (req) {
    req(state.ok)
  } else {
    validate(need(state.ok, "data refreshed, create new task..."))
  }
}

validateLearner = function(lrns.sel) {
  validate(need(length(lrns.sel) != 0L, "you didn't create a learner yet"))
}

# FIXME: Should be done with validate/need
checkPlotLearnerPrediction = function(tsk.type, feats) {
  res = NULL
  nfeats = length(feats)
  if (tsk.type == "regr") {
    if (nfeats %nin% 1:2)
      res = "You must choose one or two features to plot learner predictions."
  } else if (tsk.type == "classif") {
    if (nfeats != 2L)
      res = "You must choose exactly two features to plot learner predictions."
  }
  return(res)
}

checkPlotROCCurves = function(lrn) {
  validate(
    need(lrn$predict.type == "prob", "You must predict probabilities to plot ROC curves.")  
  )
}

validateLearnerModel = function(mod, lrn) {
  mod.lrn = mod$learner$id
  validate(need(mod.lrn == lrn, "Learner changed. Train new model."))
}

  

# FIXME: mlr: create makeAutoTask or whatever depending on target? 
sMakeTask = function(id, target, data) {
  y = data[, target]
  validate(need(all(!is.na(y)), "Target can't have missing values"))
  if (is.numeric(y) | is.integer(y))
    makeRegrTask(id = id, data = data, target = target)
  else if (is.factor(y) | is.character(y))
    makeClassifTask(id = id, data = data, target = target)
}


# FIXME: maybe we want this as a helper too in mlr directly plot pd plot for one feature??
sPlotPartialDep = function(input, task, learners) {
  lrn = input$partialdep.learner
  lrn = learners[[lrn]]
  mod = train(lrn, task)
  fn = input$partialdep.feature
  pd = generatePartialDependenceData(mod, task, features = fn)
  plotPartialDependence(pd)
}

makeImportSideBar = function(type) {
  imptype.sel.input = selectInput("import.type", "Type", selected = type, choices = c("mlr", "OpenML", "CSV", "ARFF"))
  switch(type,
    mlr = fluidRow(
      column(width = 3,
        imptype.sel.input
      ),
      column(width = 9,
        selectInput("import.mlr", "Choose mlr task", choices = c("iris.task", "bh.task", "sonar.task"))
      )
    ),
    OpenML = fluidRow(
      column(width = 3,
        imptype.sel.input
      ),
      column(width = 9,
        selectInput("import.OpenML", "Choose OpenML Data ID", selected = 61L, choices = listOMLDataSets()[,1], multiple = FALSE)
      )
    ),
    CSV = fluidRow(
      column(width = 4, height = 250,
        imptype.sel.input,
        fileInput("import.csv", "Choose CSV File",
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      # tags$hr(),
      column(width = 8, height = 250,
        checkboxInput("import.header", "Header", TRUE),
        selectInput("import.sep", "Separator", selected = ",",
          choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), width = 200),
        selectInput("import.quote", "Quote", selected = '"',
          choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
          width = 200)
      )#,
      #      textInput("import.rownames", "Row Names", NULL)
    ),
    ARFF = fluidRow(
      column(width = 4, height = 250,
        imptype.sel.input,
        fileInput("import.arff", "Choose ARFF File",
          accept = c("text/arff", "text/comma-separated-values,text/plain", ".arff"))
      )
    )
  )
}

makeVisualisationSelectionUI = function(tsk) {
  if (tsk$type == "classif") {
    if (length(getTaskClassLevels(tsk)) == 2) {
      vis.inp = selectInput("prediction.plot.sel", "Choose plot",
        choices = c("prediction", "residuals", "confusion matrix", "ROC"),
        selected = "prediction plot", width = 200
      )
    } else {
      vis.inp = selectInput("prediction.plot.sel", "Choose plot",
        choices = c("prediction", "residuals", "confusion matrix"),
        selected = "prediction plot", width = 200
      )
    }
  } else {
    vis.inp = selectInput("prediction.plot.sel", "Choose plot",
      choices = c("prediction", "residuals"),
      selected = "prediction plot", width = 200
    )
  }
  return(vis.inp)
}



makeTuningParameterUI = function(par.set, param.ids, param.types) {
  param.lows = Map(function(param) {par.set$pars[[param]]$lower}, param.ids)
  param.ups = Map(function(param) {par.set$pars[[param]]$upper}, param.ids)
  params.vals = Map(function(param) {par.set$pars[[param]]$values}, param.ids)
  
  
  param.ui = Map(function(param, param.type, param.low, param.up, param.vals) {
    
    if (param.type %in% c("numeric", "integer")) {
      
      if (param.type == "numeric") {
        tune.par.lower = numericInput(inputId = paste0("tune.par.lower.", param), label = paste("Lower value for", param),
          value = param.low, min = param.low, max = param.up)
        tune.par.upper = numericInput(inputId = paste0("tune.par.upper.", param), label = paste("Upper value for", param),
          value = param.up, min = param.low, max = param.up)
      } else if (param.type == "integer") {
        tune.par.lower = numericInput(inputId = paste0("tune.par.lower.", param), label = paste("Lower value for", param),
          value = param.low, min = param.low, max = param.up, step = 1L)
        tune.par.upper = numericInput(inputId = paste0("tune.par.upper.", param), label = paste("Upper value for", param),
          value = param.up, min = param.low, max = param.up, step = 1L)
      }
      
      pars1 = fluidRow(width = 12,
        column(6, tune.par.lower),
        column(6, tune.par.upper)
      )
      
      return(pars1)
      
    } else if (param.type == "discrete") {
      param.vals = lapply(param.vals, as.character)
      discrete.box = checkboxGroupInput(inputId = paste0("tune.par.checkbox", param), label = param,
        choices = param.vals, selected = param.vals)
      
      pars2 = fluidRow(width = 12,
        column(4, discrete.box)
      )

      return(pars2)
      
    }

  }, param.ids, param.types, param.lows, param.ups, params.vals)
  
  column(width = 12,
    param.ui
  )
}




# makeTuningParameterSet = function(par.set, param.ids, param.types){
#   
#   param.defs = Map(function(param) {par.set$pars[[param]]$default}, param.ids)
#   
#   
#   ps = Map(function(param, param.type, param.def) {
#   
#     if (param.type == "numeric") {
#       return(input[[paste0("tune.par.lower.", param)]])
#       # param.low = as.numeric(input[[paste0("tune.par.lower.", param)]])
#       # param.up = as.numeric(input[[paste0("tune.par.upper.", param)]])
#       # makeNumericParam(id = param, lower = param.low, upper = param.up, default = param.def)
#     } else if (param.type == "integer") {
#       return(input[[paste0("tune.par.lower.", param)]])
#       # param.low = as.numeric(input[[paste0("tune.par.lower.", param)]])
#       # param.up = as.numeric(input[[paste0("tune.par.upper.", param)]])
#       # makeIntegerParam(id = param, lower = param.low, upper = param.up, default = param.def)
#     } else if (param.type == "discrete") {
#       return(input[[paste0("tune.par.checkbox", param)]])
#       # param.box = input[[paste0("tune.par.checkbox", param)]]
#       # makeDiscreteParam(id = param, values = param.box)
#     }
#   # return(c(param.low, param.up, param.box))
#   }, param.ids, param.types, param.defs)
#   # ps
#   return(ps)
# }



