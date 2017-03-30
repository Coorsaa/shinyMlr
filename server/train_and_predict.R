#### train ####

output$train.learner.sel = renderUI({
  reqAndAssign(learners(), "lrns")
  lrns.ids = names(lrns)
  sel.inp = selectInput("train.learner.sel", "", choices = lrns.ids)
  tr.button = bsButton("train.run", label = "train",
    icon = icon("graduation-cap"))
  sidebarMenu(
    menuItem("Learners"),
    sel.inp,
    tr.button
  )
})

train.learner = reactive({
  req(input$train.learner.sel)
  lrns = learners()
  lrn = lrns[[isolate({input$train.learner.sel})]]
  lrn
})

model = eventReactive(input$train.run, {
  lrn = train.learner()
  tsk = isolate({task()})
  mod = tryCatch(train(lrn, tsk), error = errAsString)
  mod
})


model.ov = reactive({
  input$train.run
  mod = isolate(model())
  makeModelUI(mod, task())
})

output$model_overview = renderUI({
  validateLearnerModel(model(), input$train.learner.sel, req = TRUE)
  model.ov()[[1L]]
})

output$model.params = renderUI({
  validateData(data$data)
  validateTask(input$create.task, task.data(), data$data,
    task.weights = input$task.weights)
  validateLearner(learner$learner)
  validateLearner(list(train.learner()), check = "err")
  validateLearnerModel(model(), input$train.learner.sel)
  model.ov()[[2L]]
})


##### prediction data import #####

output$import.pred.ui = renderUI({
  newdata.type = input$newdatatype
  imp.type = input$import.type
  type = input$import.pred.type
  if (is.null(type))
    type = imp.type
  makeImportPredSideBar(newdata.type, type)
})

observe({
  req(task())
  reqAndAssign(input$newdatatype, "newdata.type")
  import.pred.type = input$import.pred.type
  if (is.null(import.pred.type))
    import.pred.type = "mlr"
  if (newdata.type == "task") {
    df.test = task.data()
  } else if (import.pred.type == "mlr") {
    mlr.imp = input$import.mlr
    df.test = getTaskData(get(mlr.imp))
  } else if (import.pred.type == "CSV") {
    df = input$import.pred.csv$datapath
    if (is.null(df))
      return(NULL)
    df.test = read.csv(df, header = input$import.pred.header, sep = input$import.pred.sep,
    quote = input$import.pred.quote)
  } else if (import.pred.type == "OpenML") {
    imp.status = need(!is.null(input$import.pred.OpenML), "")
    if (is.null(imp.status)) {
      if (is.na(input$import.pred.OpenML))
        return(NULL)
      data.id = as.integer(input$import.pred.OpenML)
    } else {
    data.id = 61L
    }
    t = getOMLDataSet(data.id = data.id)
    df.test = t$data
  } else if (input$import.pred.type == "ARFF") {
    df = input$import.pred.arff$datapath
    if (is.null(df))
      df.test = NULL
    else
      df.test = foreign::read.arff(df)
  }
  data$data.test = df.test
})

output$import.pred.preview = renderDataTable({
    validateTask(input$create.task, task.data(), data$data,
    task.weights = input$task.weights, req = TRUE)
  validateLearnerModel(model(), input$train.learner.sel)
  d = data$data.test
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 30, 50), pageLength = 5, scrollX = TRUE, pagingType = "simple"))


##### predict on new data #####

pred = eventReactive(input$predict.run, {
    validateTask(input$create.task, task.data(), data$data,
    task.weights = input$task.weights, req = TRUE)
  model = model()
  validate(need(!is.null(model), "Train a model first to make predictions"))
  newdata = data$data.test
  colnames(newdata) = make.names(colnames(newdata))
  feat.names = task.feature.names()
  validate(need(all(feat.names %in% colnames(newdata)),
    sprintf("Column names %s must be present in data",
      paste(feat.names, collapse = " "))))
  preds = tryCatch(predict(model, newdata = newdata), error = errAsString)
  preds
})

observeEvent(input$predict.run, {
  updateTabItems(session, "predict.tab", "pred.res")
})

output$predoverview = renderDataTable({
    validateTask(input$create.task, task.data(), data$data,
    task.weights = input$task.weights, req = TRUE)
  validateLearnerModel(model(), input$train.learner.sel)
  p = pred()
  validate(need("Prediction" %in% class(p),
    stri_paste("Predicting failed with the following error:", p, sep = "\n")))
  p$data
}, options = list(scrollX = TRUE, lengthMenu = c(5, 30), pageLength = 5)
)

output$predict.download = downloadHandler(
  filename = function() {
    tsk = task()
    lrn.name = input$train.learner.sel
    pasteDot(getTaskId(tsk), lrn.name, "csv")
  },
  content = function(file) {
    pred = pred()
    write.csv(pred$data, file)
  }
)

#### performance on the test data ####

measures.train.avail = reactive({
  lrn = train.learner()
  tsk = isolate(task())
  listMatchingMeasures(tsk, list(lrn))
})

output$perf.measures.sel = renderUI({
  ms = measures.train.avail()
  selectInput("perf.measures.sel", "Choose performance measures",
    choices = ms, multiple = TRUE, selected = measures.default())
})

measures.perf = reactive({
  tsk = isolate(task())
  listMeasures(tsk, create = TRUE)[input$perf.measures.sel]
})

perf = eventReactive(input$performance.run, {
  p = pred()
  model = model()
  ms = measures.perf()
  perf = performance(p, measures = ms, model = model)
  round(perf, digits = 4L)
})

output$performance.overview = renderUI({
  input$performance.run
  req(perf())
    validateTask(input$create.task, task.data(), data$data,
    task.weights = input$task.weights, req = TRUE)
  validateLearnerModel(model(), input$train.learner.sel)
  ms = isolate(measures.perf())
  perf = isolate(perf())
  makePerformanceUI(ms, perf)
})




##### prediction plot ####

output$visualisation.selection = renderUI({
  reqAndAssign(task(), "tsk")
  column(width = 4,
    makeVisualisationSelectionUI(tsk)
  )
})

output$predictionplot.x.sel = renderUI({
  fnames = task.feature.names() #FIXME
  selectInput("predictionplot.x.sel", "Select variables:", choices = fnames,
    multiple = TRUE)
})

output$predictionplot.settings = renderUI({
  reqAndAssign(pred(), "preds")
  fnames = task.numeric.feature.names()
  feats = task.feature.names()
  ms = measures.train.avail()
  ms.def = measures.default()
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  reqAndAssign(task(), "tsk")
  tsk.type = getTaskType(tsk)
  reqAndAssign(isolate(filter.methods()), "fm")
  lrn.sel = input$train.learner.sel
  lrn = isolate(learners())[[lrn.sel]]
  predict.type = lrn$predict.type
  help.texts = input$show.help
  makePredictionPlotSettingsUI(plot.type, fnames, feats, ms.def, ms, tsk.type, fm, predict.type, help.texts, tsk)
})

measures.plot = reactive({
  tsk = isolate(task())
  reqAndAssign(measures.default(), "ms.def")
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  if (plot.type == "prediction") {
    ms = ms.def
  } else {
    if (plot.type == "ROC") {
      ms = c("fpr", "tpr")
    } else {
      ms = 1L
    }
  }
  listMeasures(tsk, create = TRUE)[ms]
})

prediction.plot.out = reactive({
  lrn.sel = input$train.learner.sel
  validateLearnerModel(model(), lrn.sel)
  validateTask(input$create.task, task.data(), data$data,
    task.weights = input$task.weights)
  reqAndAssign(isolate(task()), "tsk")
  tsk.type = tsk$type
  reqAndAssign(isolate(model()), "mod")
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  lrn = learners()[[lrn.sel]]
  fnames = task.numeric.feature.names()
  feats = input$predictionplot.feat.sel
  preds = pred()
  ms = measures.plot()
  resplot.type = input$residualplot.type

  if (plot.type == "partial dependency" && lrn$predict.type == "se")
    ind = "FALSE"
  else
    ind = as.logical(input$pd.plot.ind)
  makePredictionPlot(mod, tsk, tsk.type, plot.type, lrn, fnames, feats, preds, ms,
    resplot.type, ind)
})

output$prediction.plot = renderPlot({
  pred.plot = prediction.plot.out()
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  if (plot.type == "confusion matrix")
    return(NULL)
  else
    pred.plot
})

prediction.plot.collection = reactiveValues(plot.titles = NULL, pred.plots = NULL)

observeEvent(prediction.plot.out(), {
  q = prediction.plot.out()
  plot.title = isolate(input$prediction.plot.sel)
  # prediction.plot.collection$plot.titles = c(prediction.plot.collection$plot.titles, plot.title)
  prediction.plot.collection$pred.plots[[plot.title]] = q
})

output$confusion.matrix = renderPrint({
  reqAndAssign(isolate(pred()), "preds")
  reqAndAssign(task(), "tsk")
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  rel.conf = as.logical(input$confusion.matrix.relative)
  if (plot.type == "confusion matrix") {
    t = makeConfusionMatrix(plot.type, preds, tsk, rel.conf)
    print(t)
  } else {
    invisible(NULL)
  }
})

observeEvent(input$prediction.plot.sel, {
  reqAndAssign(input$prediction.plot.sel, "plot.type")
  if (plot.type == "confusion matrix") {
    shinyjs::show("confusion.matrix", animType = "fade")
  } else {
    shinyjs::hide("confusion.matrix", anim = TRUE)
  }
})
