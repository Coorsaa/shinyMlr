filterParSetsForUI = function(par.sets) {
  allowed.types = c("integer", "numeric", "integervector", "numericvector",
    "logical", "discrete")
  par.sets = lapply(par.sets, function(par.set) {
    filterParams(par.set, type = allowed.types)
  })
  return(par.sets)
}

makeLearnerParamInfoUI = function(par) {
  par.type = par$type
  par.def = par$default
  if (is.null(par.def))
    par.def = "-"
  par.tun = par$tunable
  if (par.tun) {
    par.tun = "yes"
  } else {
    par.tun = "no"
  }

  info.ui = list(makeInfoDescription("type", par.type, width = 2),
    makeInfoDescription("default", par.def, width = 4),
    makeInfoDescription("tunable", par.tun, width = 2)
  )
  if (par.type %in% c("numeric", "integer", "numericvector", "integervector")) {
    par.lower = par$lower
    par.upper = par$upper
    if (is.null(par.lower))
      par.lower = "-"
    if (is.null(par.upper))
      par.upper = "-"
    
    info.ui = list(info.ui,
      makeInfoDescription("lower", par.lower, width = 2),
      makeInfoDescription("upper", par.upper, width = 2)
    )
  }

  return(info.ui)
}

makeLearnerParamUI = function(par.sets, params.inp, inp.width = 150) {
  lab.val = "value"
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
          
        inp = numericInput(par.id, value = par.inp, min = par$lower,
          max = par$upper, step = step, width = inp.width, label = lab.val)
      } else {
        if (par.type %in% c("logical", "discrete")) {
          inp = radioButtons(par.id, par$values, par.inp, inline = TRUE, label = lab.val)
        } else {
          inp = textInput(par.id, par.inp, width = inp.width, label = lab.val)
        }
      }
      par.info.ui = makeLearnerParamInfoUI(par)
      par.ui = box(width = 12, height = 130, title = par.name, solidHeader = TRUE, status = "primary",
        fluidRow(
          column(width = 5, div(height = "130px", inp)),
          column(width = 7, div(height = "130px", par.info.ui))
        )
      )
      return(par.ui)
    }, par.set$pars, names(par.set$pars))
  }, par.sets, names(par.sets))
  names(params) = NULL
  return(params)
}

makeLearnerPredTypesInputs = function(lrns.names, pred.types.inp, tsk.type) {
  if (tsk.type == "classif") {
    prop = "prob"
    inp.header = "Probability estimation:"
  } else {
    prop = "se"
    inp.header = "Standard error estimation:"
  }
  Map(function(lrn.name, pred.type.inp){
    lrn.has.props = hasLearnerProperties(lrn.name, props = prop)
    if (lrn.has.props) {
      if(pred.type.inp %in% c("prob", "se")) {
        pred.type.inp = "Yes"
      } else {
        pred.type.inp = "No"
      }
      inp = radioButtons(paste("lrn.prob.sel", lrn.name, sep = "."),
        inp.header, choices = c("Yes", "No"),
        selected = pred.type.inp, inline = TRUE)
    } else {
      NULL
    }
  }, lrns.names, pred.types.inp)
}

makeLearnerThresholdInputs = function(lrns.names, pred.types.inp, threshs.inp,
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

makeLearnerPredTypesUI = function(pred.types, thresholds) {
  Map(function(pred.type, thresh) {
    if (is.null(pred.type)) {
      NULL
    } else {
      thresh = lapply(thresh, function(thrsh) {
        column(thrsh, width = 2)
      })
      box(width = 12, title = "Predict type:", status = "warning", solidHeader = TRUE,
        column(pred.type, width = 6, align = "center"),
        column(width = 6, div(align = "center", thresh))
      )
    }
  }, pred.types, thresholds)
}

makeLearnerConstructionUI = function(lrns.names, par.sets, params, pred.types, tab.box.sel) {
  lrn.tabs = Map(function (par.set, lrn.name, hyppar, pred.type) {
    
    tabPanel(title = lrn.name, width = 12,
      fluidRow(
        pred.type
      ),
      h3("Hyperparameters:"),
      br(),
      fluidRow(
        column(width = 12, hyppar)
      )
    )
  }, par.sets, lrns.names, params, pred.types)

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

determinePredType = function(pred.type, tsk.type) {
  if (is.null(pred.type)) {
    "response"
  } else {
    if (pred.type == "Yes") {
      if (tsk.type == "classif") {
        "prob"
      } else {
        "se"
      }
    } else {
      "response"
    }
  }
}
