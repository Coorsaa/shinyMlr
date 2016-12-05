#### general helpers ####

pasteDot = function(...) {
  paste(..., sep = ".")
}

reqAndAssign = function(obj, name) {
  req(obj)
  assign(name, obj, pos = 1L)
}

#### needy functions

validateTask = function(tsk.button, tsk.df, df, req = FALSE) {
  validate(need(tsk.button != 0L, "you didn't create a task yet"))
  state.ok = identical(tsk.df, df)
  if (req) {
    req(state.ok)
  } else {
    validate(need(state.ok, "data refreshed, create new task..."))
  }
}

validatePlotLearnerPrediction = function(tsk.type, feats) {
  res = NULL
  nfeats = length(feats)
  if (tsk.type == "regr") {
    if (nfeats %nin% 1:2)
      res = "You must choose one or two features to plot learner predictions."
  } else {
    if (tsk.type == "classif") {
      if (nfeats != 2L)
        res = "You must choose exactly two features to plot learner predictions."
    }
  }
  return(res)
}

  

# FIXME: mlr: create makeAutoTask or whatever depending on target? 
sMakeTask = function(id, target, data) {
  y = data[, target]
  if (is.numeric(y))
    makeRegrTask(id = id, data = data, target = target)
  else if (is.factor(y))
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

makeImportPredSideBar = function(type, newdata.type) {
  if (newdata.type == "task") {
    return(NULL)
  } else {
    switch(type, 
      mlr = list(
        selectInput("import.pred.mlr", "Choose toy task", choices = c("iris.task", "bh.task", "sonar.task"))
      ),
      OpenML = list(
        numericInput("import.pred.OpenML", "Choose OpenML Data ID", value = 61L)
      ),
      CSV = list(
        fileInput("import.pred.csv", "Choose CSV File",
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        tags$hr(),
        checkboxInput("import.pred.header", "Header", TRUE),
        selectInput("import.pred.sep", "Separator", selected = ",",
          choices = c(Comma = ",", Semicolon = ";", Tab = "\t")),
        selectInput("import.pred.quote", "Quote", selected = '"',
          choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"))
      ),
      ARFF = list(
        fileInput("import.pred.arff", "Choose ARFF File",
          accept = c("text/arff", "text/comma-separated-values,text/plain", ".arff"))
      )
    )
  }
}

makeLearnerParamUI = function(par.sets, params.inp, inp.width = 200) {
  params = Map(function(par.set, lrn.name) {
    Map(function(par, par.name) {
      par.type = par$type
      par.id = pasteDot(lrn.name, par.name)
      par.inp = params.inp[[lrn.name]][[par.name]]
      if (is.null(par.inp)) {
        if (is.null(par$default))
          par.inp = NA
        else
          par.inp = par$default
      }

      if (par.type %in% c("numeric", "integer")) {
        if (par.type == "integer")
          step = 1L
        else
          step = NA
        
        if (is.null(par$lower))
          par$lower = NA
        if (is.null(par$upper))
          par$upper = NA
          
        numericInput(par.id, value = par.inp, par.name, min = par$lower,
          max = par$upper, step = step, width = inp.width)
      } else {
        if (par.type %in% c("logical", "discrete")) {
          radioButtons(par.id, par.name, par$values, par.inp, width = inp.width)
        } else {
          textInput(par.id, par.name, par.inp, width = inp.width)
        }
      }
    }, par.set$pars, names(par.set$pars))
  }, par.sets, names(par.sets))
  names(params) = NULL
  return(params)
}

makeLearnerPredTypesUI = function(lrns.names, pred.types.inp) {
  Map(function(lrn.name, pred.type.inp){
    lrn.has.probs = hasLearnerProperties(lrn.name, props = "prob")
    if (lrn.has.probs) {
      if(pred.type.inp == "prob") {
        pred.type.inp = "Yes"
      } else {
        pred.type.inp = "No"
      }
      radioButtons(paste("lrn.prob.sel", lrn.name, sep = "."),
        "Probability estimation:", choices = c("Yes", "No"),
        selected = pred.type.inp)
    } else {
      NULL
    }
  }, lrns.names, pred.types.inp)
}

makeLearnerThresholdUI = function(lrns.names, pred.types.inp, threshs.inp,
  target.levels, inp.width = 100) {
  Map(function(lrn.name, thresh.inp, pred.type.inp) {
    if (pred.type.inp == "prob") {
      if(is.null(thresh.inp))
        thresh.inp = rep(NA, length(target.levels))
      Map(function(target.level, trsh.inp) {
        id = pasteDot(lrn.name, "threshold", target.level)
        numericInput(id, label = target.level, value = trsh.inp, min = 0,
          max = 1, width = inp.width)
      },target.levels, thresh.inp)
    } else {
      NULL
    }
  }, lrns.names, threshs.inp, pred.types.inp)
}

makeLearnerConstructionUI = function(lrns.names, par.sets, params, pred.types,
  thresholds, tab.box.sel) {
  lrn.tabs = Map(function (par.set, lrn.name, hyppar, pred.type, threshold) {
    par.tab = renderTable({ParamHelpers:::getParSetPrintData(par.set)},
     rownames = TRUE)

    hyppar = split(hyppar, ceiling(seq_along(hyppar) / (length(hyppar) / 3)))
    hyppar = lapply(hyppar, function(sub.pars) {
        column(sub.pars, width = 4)
    })
    threshold = lapply(threshold, function(thresh) {
      column(thresh, width = 2)
    })
    
    tabPanel(title = lrn.name, width = 12,
      fluidRow(column(pred.type, width = 6, align = "center")),
      fluidRow(div(align = "center", threshold)),
      h4("Hyperparameters"),
      fluidRow(div(align = "center", hyppar)),
      fluidRow(div(align = "center", par.tab))
    )
  }, par.sets, lrns.names, params, pred.types, thresholds)

  names(lrn.tabs) = NULL
  do.call(tabBox, c(lrn.tabs, width = 12, id = "learners.tabBox",
    selected = tab.box.sel))
}


stringToParamValue = function (par, x) {
  assertClass(par, "Param")
  assertCharacter(x)
  
  type = par$type

  if (x == "") {
    res = NULL
  } else {
    if (type %in% c("numeric", "integer", "logical")) {
      res = do.call(paste0("as.", type), list(x))
    }

    if (type %in% c("numericvector", "integervector", "logicalvector",
      "charactervector", "discretevector")) {
      x = gsub("c|[(]|[)]|L", "", x)
      res = do.call(paste0("as.", gsub("vector", "", type)), strsplit(x, ","))
    }

    if (type == "character")
      res = x
    if (type == "discrete") 
      res = discreteNameToValue(par, x)
    if (type == "function")
      res = eval(parse(text = x))
    if (type == "untyped")
      #FIXME: We need to figure out how we should handle this
      res = x
  }

  return(res)
}

convertParamForLearner = function(lrn.par, value) {

  if (!is.null(value)) {
    if (is.na(value)) {
      value = NULL
    } else {
      value = stringToParamValue(lrn.par, as.character(value))
    }
  }
  return(value)
}

determinePredType = function(pred.type) {
  if (is.null(pred.type)) {
    "response"
  } else {
    if (pred.type == "Yes") {
      "prob"
    } else {
      "response"
    }
  }
}

makePerformanceUI = function(performance) {
  ms.names = names(performance)
  boxes = Map(function(perf, ms.name) {
    valueBox(ms.name, perf, color = "light-blue", width = 2)
  }, performance, ms.names)
  boxes
}

makePredictionPlot = function(tsk.type, plot.type, lrn, feats, preds, ms, num.levels, resplot.type) {
  if (plot.type == "prediction") {
    validate(validatePlotLearnerPrediction(tsk.type, feats))
    q = plotLearnerPrediction(learner = lrn, task = tsk, features = feats,
      measures = ms, cv = 0)
  } else {
    if (plot.type == "residuals") {
      resplot.type = switch(resplot.type,
        scatterplot = "scatterplot",
        "histogram" = "hist")
      q = plotResiduals(preds, type = resplot.type)
    } else {
      validate(need(tsk.type == "classif" && num.levels == 2L,
        "Task needs to be a binary classification problem to plot ROC curves."))
      df = generateThreshVsPerfData(preds, measures = ms)
      q = plotROCCurves(df)
    }
  }
  return(q)
}


