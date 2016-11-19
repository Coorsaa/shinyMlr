##### learners #####

learners.avail = reactive({
  validate(need(input$create.task != 0L,
    "create a task first to list suitable learners"))
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

learners.params.ui = reactive({
  req(learners.par.sets())
  par.sets = learners.par.sets()
  params.inp = isolate({learners.params()})
  makeLearnerParamUI(par.sets, params.inp)
})

learners.pred.types = reactive({
  lrns = input$learners.sel
  lrns.pred.types = vcapply(lrns, function(lrn) {
    pred.type = pasteDot("lrn.prob.sel", lrn)
    pred.type = input[[pred.type]]
    pred.type = determinePredType(pred.type)
  })
  lrns.pred.types
})

learners.pred.types.ui = reactive({
  req(learners.pred.types())
  lrns.sel = input$learners.sel
  pred.types = learners.pred.types()
  makeLearnerPredTypesUI(lrns.sel, pred.types)
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
  
  lrns.threshold
})

learners.threshold.ui = reactive({
  req(learners.threshold())
  lrns.sel = input$learners.sel
  threshs = learners.threshold()
  pred.types = learners.pred.types()
  tsk = isolate({task()})
  target.levels = target.levels()
  makeLearnerThresholdUI(lrns.sel, pred.types, threshs, target.levels)
})

output$learners.ui = renderUI({
  req(learners.params.ui)
  lrns.sel = input$learners.sel
  par.sets = isolate(learners.par.sets())
  params = learners.params.ui()
  pred.types = learners.pred.types.ui()
  thresholds = learners.threshold.ui()
  makeLearnerConstructionUI(lrns.sel, par.sets, params, pred.types, thresholds)
})

learners = reactive({ 
  req(learners.params())
  lrns.params = learners.params()
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
  setNames(lrns, lrns.sel)
})