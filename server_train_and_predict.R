#### train and predict ####

output$train.learner.sel = renderUI({
  ls = learners(); if (is.null(ls)) return(NULL)
  ls.ids = names(ls)
  selectInput("train.learner.sel", "Learners", choices = ls.ids)
})

trn = eventReactive(input$train.run, {
  tt = isolate({task()}); if (is.null(tt)) return(NULL)
  lrns = isolate({learners()})
  lrn = lrns[[isolate({input$train.learner.sel})]]
  train(lrn, tt)
})

output$train.overview = renderInfoBox({
  infoBox("", width = 5,
    ifelse(!is.null(trn()), "Model successfully trained",
      #FIXME: if not trained nothing shows
      "Model was not trained yet")
  )
})

##### prediction data import #####

output$import.pred.ui = renderUI({
  type = input$import.pred.type; 
  if (is.null(type)) 
    type = "mlr"
  makeImportPredSideBar(type)
})

data.pred = reactive({
  if (is.null(input$import.pred.type)) {
    return(NULL)
  } else if (input$import.pred.type == "mlr") {
    return(getTaskData(get(input$import.pred.mlr)))
  } else if (input$import.pred.type == "CSV") {
    f = input$import.pred.csv$datapath
    if (is.null(f)) return(NULL)
    #rn = as.numeric(input$import.rownames)
    read.csv(f, header = input$import.pred.header, sep = input$import.pred.sep,
      quote = input$import.pred.quote) #, row.names = rn)
  } else if (input$import.pred.type == "OpenML") {
    t = getOMLDataSet(data.id = input$import.pred.OpenML)
    return(t$data)
  } else if (input$import.type == "ARFF") {
    f = input$import.pred.arff$datapath
    if (is.null(f)) return(NULL)
    readARFF(f)
  }
})

output$import.pred.preview = renderDataTable({
  d = data.pred(); if (is.null(d)) return(NULL)
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
)

##### predict on new data #####

pred = eventReactive(input$predict.run, {
  model = trn()
  newdata = data.pred()
  colnames(newdata) = make.names(colnames(newdata)) 
  predict(model, newdata = newdata)
})

output$pred.overview = renderDataTable({
  p = pred(); if (is.null(p)) return(NULL)
  p$data
}, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
)

#### performance on the test data ####

measures.train.avail = reactive({
  tt = task(); if (is.null(tt)) return(NULL)
  if (is.null(input$train.prob.sel)) {
    probs.dec = "No"
  } else {
    probs.dec = input$train.prob.sel
  }
  if (probs.dec == "Yes") {
    ls = listMeasures(tt)
  } else {
    prob.subset =  listMeasures(tt) %in% listMeasures(tt, properties = "req.prob")
    ls = listMeasures(tt)[!prob.subset]
  }
  return(ls)
})

output$perf.measures.sel = renderUI({
  ms = measures.train.avail(); if (is.null(ms)) return(NULL)
  selectInput("perf.measures.sel", "Choose performance measures", choices = ms, multiple = TRUE, selected = measures.default())
})

measures.perf = reactive({
  tt = task(); if (is.null(tt)) return(NULL)
  listMeasures(tt, create = TRUE)[input$perf.measures.sel]
})

perf = eventReactive(input$performance.run, {
  p = pred(); if (is.null(p)) return(NULL)
  model = trn()
  ms = measures.perf()
  performance(p, measures = ms, model = model)
})

output$performance.overview = renderTable({
  p = perf(); if (is.null(p)) return(NULL)
  data.frame(t(p), row.names = "")
})