##### learners #####

learners.avail = reactive({
  validateTask(input$create.task, task.data(), data$data)
  tsk = task()
  listLearners(tsk)
})

learners.default = reactive({
  req(task())
  tsk = getTaskType(task())
  switch(tsk, 
    classif =  c("classif.randomForest", "classif.svm", "classif.rpart"),
    regr = c("regr.randomForest", "regr.svm", "regr.rpart"))
})

output$learners.sel = renderUI({
  reqAndAssign(learners.avail(), "ls")
  ls.ids = ls$class
  selectInput("learners.sel", "", choices = ls.ids, multiple = TRUE,
    selected = learners.default())
})

learners.par.sets = reactive({
  reqAndAssign(input$learners.sel, "lrns.sel")
  par.sets = lapply(lrns.sel, getParamSet)
  par.sets = filterParSetsForUI(par.sets)
  names(par.sets) = lrns.sel
  return(par.sets)
})

learners.params = reactive({
  reqAndAssign(learners.par.sets(), "par.sets")
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

learners.params.ui = reactive({
  reqAndAssign(learners.par.sets(), "par.sets")
  params.inp = isolate({learners.params()})
  makeLearnerParamUI(par.sets, params.inp)
})

learners.pred.types = reactive({
  lrns = input$learners.sel
  tsk.type = isolate(task.type())
  lrns.pred.types = vcapply(lrns, function(lrn) {
    pred.type = pasteDot("lrn.prob.sel", lrn)
    pred.type = input[[pred.type]]
    pred.type = determinePredType(pred.type, tsk.type)
  })
  lrns.pred.types
})

learners.pred.types.inputs = reactive({
  reqAndAssign(learners.pred.types(), "pred.types")
  lrns.sel = input$learners.sel
  tsk.type = task.type()
  makeLearnerPredTypesInputs(lrns.sel, pred.types, tsk.type)
})

learners.threshold = reactive({
  lrns = input$learners.sel
  target.levels = target.levels()
  lrns.threshold = lapply(lrns, function(lrn) {
    threshold = sapply(target.levels, function(target.level) {
      thresh.id = pasteDot(lrn, "threshold", target.level)
      thresh.inp = input[[thresh.id]]
      if (is.null(thresh.inp))
        thresh.inp = NA
      thresh.inp
    })
    names(threshold) = target.levels
    return(threshold)
  })
  return(lrns.threshold)
})

learners.threshold.ui = reactive({
  reqAndAssign(learners.threshold(), "threshs")
  lrns.sel = input$learners.sel
  pred.types = learners.pred.types()
  tsk = isolate({task()})
  target.levels = target.levels()
  makeLearnerThresholdInputs(lrns.sel, pred.types, threshs, target.levels)
})

learners.pred.types.ui = reactive({
  reqAndAssign(learners.pred.types.inputs(), "pred.types")
  threshs = isolate(learners.threshold.ui())
  makeLearnerPredTypesUI(pred.types, threshs)
})

output$learners.ui = renderUI({ 
  req(task.is.consistent())
  lrns.sel = input$learners.sel
  par.sets = isolate(learners.par.sets())
  params = learners.params.ui()
  pred.types = learners.pred.types.ui()
  lrns.tab.box.sel = isolate(input$learners.tabBox)
  makeLearnerConstructionUI(lrns.sel, par.sets, params, pred.types, lrns.tab.box.sel)
})


learner = reactiveValues(learner = NULL, tuned.learner = NULL)

observe({
  reqAndAssign(learners.params(), "lrns.params")
  lrns.sel = input$learners.sel
  pred.types = learners.pred.types()
  threshs = learners.threshold()
  lrns = Map(function(lrn, pars, pred.type, thresh) {
    # FIXME: this is ugly, should be handled in learners.threshold()
    # didnt find easy way to do it
    if (any(is.na(thresh)) | length(thresh) == 0L)
      thresh = NULL

    makeLearner(lrn, predict.type = pred.type,
      par.vals = pars, predict.threshold = thresh)
  }, lrns.sel, lrns.params, pred.types, threshs)
  learner$learner = setNames(lrns, lrns.sel)
})

learners = reactive({
  learner$learner
})
