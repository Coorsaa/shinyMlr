#### general helpers ####

pasteDot = function(...) {
  paste(..., sep = ".")
}

writeBold = function(chr) {
  tags$b(chr)
}

makeInfoDescription = function(header, body, width, inline = FALSE) {
  if (inline) {
    fluidRow(
      column(width = width, align = "center", h5(header)),
      column(width = width, align = "center", h5(body))
    )
  } else {
    column(width = width, align = "center", div(class = "padded-text", h5(body)))  
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


validateLearnerModel = function(mod, lrn = NULL) {
  validate(need(class(mod) == "WrappedModel",
    stri_paste("Training the model failed with the following error:",
      mod, sep = "\n")))
  if (!is.null(lrn)) {
    mod.lrn = mod$learner$id
    validate(need(mod.lrn == lrn, "Learner changed. Train new model."))
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


# FIXME: mlr: create makeAutoTask or whatever depending on target? 
sMakeTask = function(id, target, data) {
  y = data[, target]
  validate(need(all(!is.na(y)), "Target can't have missing values"))
  if (is.numeric(y) | is.integer(y))
    makeRegrTask(id = id, data = data, target = target)
  else if (is.factor(y) | is.character(y))
    makeClassifTask(id = id, data = data, target = target)
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
  # go = column(12, align = "center", go)
  list(fluidRow(help), fluidRow(confs))
}





