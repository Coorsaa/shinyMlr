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


validateLearnerModel = function(mod, lrn) {
  mod.lrn = mod$learner$id
  validate(need(mod.lrn == lrn, "Learner changed. Train new model."))
}

validateNumFeatures = function(nfeats) {
  validate(need(length(nfeats) != 0L, "There are no numeric features to select."))
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


