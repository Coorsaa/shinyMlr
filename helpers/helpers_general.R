#### general helpers ####

pasteDot = function(...) {
  paste(..., sep = ".")
}

writeBold = function(chr) {
  tags$b(chr)
}

makeInfoDescription = function(header, body, width, inline = TRUE) {
  if (inline) {
      column(width = width, align = "center", div(class = "padded-text", h5(body)))
  } else {
    column(width = width, align = "center", writeBold(header), h5(body))
  }
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

#### error handlers ####
errAsString = function(err) {
  err$message
}

#### needy functions ####

reqAndAssign = function(obj, name) {
  req(obj)
  assign(name, obj, pos = 1L)
}

validateData = function(df) {
  validate(need(class(df) == "data.frame", "You didn't import any data."))
}

validateTask = function(tsk.button, tsk.df, df, req = FALSE, task.weights = NULL) {
  validate(need(tsk.button != 0L, "you didn't create a task yet"))
  if (!is.null(task.weights) & task.weights != "-" ) {
      df[, task.weights] = NULL
      df = df[, colnames(tsk.df)]
  }
  state.ok = identical(tsk.df, df)
  if (req) {
    req(state.ok)
  } else {
    validate(need(state.ok, "data refreshed, create new task..."))
  }
}

validateLearner = function(lrns = NULL, req = FALSE, check = "exists") {
  if (req) {
    mess = NULL
  } else {
    if (check == "exists") {
      mess = "You didn't create a learner yet." 
    } else {
      mess = "Constructing the learner failed with the following error:" 
    }
  }

  if (check == "exists") {
    cond = !(is.null(lrns))
    res = validate(need(cond, mess))
  } else {
    checks = lapply(lrns, function(lrn) {
      cond = ("RLearner" %in% class(lrn))
      validate(need(cond, stri_paste(mess, lrn, sep = " \n")))
    })
    res = unique(unlist(checks))
  }
  return(res)
}


validateLearnerModel = function(mod, lrn = NULL, req = FALSE) {
  cond = (class(mod) == "WrappedModel")
  if (req) {
    req(cond)
  } else {
    validate(need(cond,
      stri_paste("Training the model failed with the following error:",
        mod, sep = "\n")))
    if (!is.null(lrn)) {
      mod.lrn = mod$learner$id
      validate(need(mod.lrn == lrn, "Learner changed. Train new model."))
    }
  }
}

validateNumFeatures = function(nfeats) {
  validate(need(length(nfeats) != 0L, "There are no numeric features to select."))
}

validatePreprocData = function(df, type) {
  err.mess = ifelse(type == "training set", "",
    "You didn't upload a test set yet. Click on the 'train and predict' panel to do so.")
  validate(need(df, err.mess))
}

validateExperiment = function(res, cl) {
  validate(need(class(res) %in% cl,
    stri_paste("Computing failed with the following error:", res, sep = "\n")))
}


# FIXME: mlr: create makeAutoTask or whatever depending on target? 
sMakeTask = function(id, target, df, weights = NULL) {
  tsk.weights = NULL
  if (!is.null(weights) & weights != "-") {
    tsk.weights = df[, weights]
    df[, weights] = NULL
  }
  y = df[, target]
  validate(need(all(!is.na(y)), "Target can't have missing values"))
  if (is.numeric(y) | is.integer(y))
    makeRegrTask(id = id, data = df, target = target, weights = tsk.weights)
  else if (is.factor(y) | is.character(y))
    makeClassifTask(id = id, data = df, target = target, weights = tsk.weights)
}

listMatchingMeasures = function(task, lrns) {
  ls = listMeasures(task)
  pred.types = lapply(lrns, getLearnerPredictType)
  if (any(pred.types != "prob")) {
    prob.subset = listMeasures(task) %in% listMeasures(task, properties = "req.prob")
    ls = listMeasures(task)[!prob.subset]
  }
  return(ls)
}

makeResampleDescUI = function(rdesc.type) {
  inps = list()
  if (rdesc.type %in% c("CV", "Bootstrap", "Subsample")) {
    inps$iters = numericInput("benchmark.iters", label = "Iterations", value = 10L,
      min = 1L, max = 100L, step = 1L)
  }
  if (rdesc.type == "RepCV") {
    inps$reps = numericInput("benchmark.reps", label = "Repeats", value = 10L,
      min = 1L, max = 100L, step = 1L)
    inps$folds = numericInput("benchmark.folds", label = "Folds", value = 10L,
      min = 1L, max = 100L, step = 1L)
  }
  if (rdesc.type %in% c("Subsample", "Holdout")) {
    inps$split = numericInput("benchmark.split", label = "Split", value = 0.66,
      min = 0, max = 1, step = 0.01)
  }
  inps
}

makePreprocUI = function(help, ...) {
  confs = list(...)
  confs = lapply(confs, function(conf) {
    column(6, conf)
  })
  list(fluidRow(help), fluidRow(confs))
}

printTaskOverviewUI = function(tsk) {
  dsc = tsk$task.desc
  n.feat = t(as.data.frame(dsc$n.feat))
  rownames(n.feat) = NULL
  box(width = 12, title = "Task overview", status = "primary", solidHeader = TRUE,
    makeTaskDescLine("Supervised task: ", dsc$id),
    makeTaskDescLine("Type: ", dsc$type),
    makeTaskDescLine("Observations: ", as.character(dsc$size)),
    makeTaskDescLine("Has missings: ", ifelse(dsc$has.missings, "yes", "no")),
    makeTaskDescLine("Has weights: ", ifelse(dsc$has.weights, "yes", "no")),
    h4("Features:"),
    column(width = 3, align = "center",
      renderSimpleDataTable(n.feat)
    ),
    column(width = 9, NULL)
  )
}

makeTaskDescLine = function(title, body) {
  if (is.character(body))
    body = h4(body)
  fluidRow(
    column(3, h4(title)),
    column(9, body)
  )
}

renderSimpleDataTable = function(df, ...) {
  dt = datatable(df, options = list(paging = FALSE, searching = FALSE,
    bInfo = FALSE, ordering = FALSE, width = "200px"))
  renderDataTable(dt)
}

makeRecodeLevelUI = function(levs) {
  lapply(levs, function(lev) {
    # div(
    #   column(width = 6,
    #     NULL
    #   ),
    #   column(width = 6,
        textInput(paste("recode_", lev), lev, lev)
    #   )
    # )
  })
}




