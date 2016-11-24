#### train ####

output$train.learner.sel = renderUI({
  req(learners())
  lrns = learners()
  lrns.ids = names(lrns)
  selectInput("train.learner.sel", "Learners",
    choices = lrns.ids, width = 250)
})

train.learner = reactive({
  req(input$train.learner.sel)
  lrns = isolate({learners()})
  lrn = lrns[[isolate({input$train.learner.sel})]]
  lrn
})

model = eventReactive(input$train.run, {
  req(train.learner())
  lrn = train.learner()
  tsk = isolate({task()})
  train(lrn, tsk)
})

output$model.overview = renderPrint({
  validate(need(input$train.run != 0L, "No model trained yet"))
  input$train.run
  mod = isolate(model())
  print(mod)
})



##### prediction data import #####

output$import.pred.ui = renderUI({
  type = input$import.pred.type; 
  if (is.null(type)) 
    type = "mlr"
  makeImportPredSideBar(type)
})

data.pred = reactive({
  req(input$import.pred.type)
  if (input$import.pred.type == "mlr") {
    return(getTaskData(get(input$import.pred.mlr)))
  } else {
    if (input$import.pred.type == "CSV") {
      f = input$import.pred.csv$datapath
      if (is.null(f))
        return(NULL)
      read.csv(f, header = input$import.pred.header, sep = input$import.pred.sep,
        quote = input$import.pred.quote)
    } else {
      if (input$import.pred.type == "OpenML") {
        t = getOMLDataSet(data.id = input$import.pred.OpenML)
        return(t$data)
      } else {
        if (input$import.type == "ARFF") {
          f = input$import.pred.arff$datapath
          if (is.null(f))
            return(NULL)
          readARFF(f)
        }
      }
    }
  }
})

output$import.pred.preview = renderDataTable({
  d = data.pred()
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))


##### predict on new data #####

pred = eventReactive(input$predict.run, {
  model = model()
  newdata = data.pred()
  colnames(newdata) = make.names(colnames(newdata)) 
  predict(model, newdata = newdata)
})

output$predoverview = renderDataTable({
  p = pred()
  p$data
}, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
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
  pred.type = lrn$predict.type
  tsk = isolate(task())
  if (pred.type == "prob") {
    ls = listMeasures(tsk)
  } else {
    prob.subset =  listMeasures(tsk) %in% listMeasures(tsk, properties = "req.prob")
    ls = listMeasures(tsk)[!prob.subset]
  }
  return(ls)
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
  perf = perf()
  makePerformanceUI(perf)
})
