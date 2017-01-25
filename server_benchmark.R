##### benchmark #####

output$benchmark.learners.sel = renderUI({
  # validateLearner(input$lrns.sel)
  lrns = learners()
  validateTask(input$create.task, task.data(), data$data, req = TRUE)
  lrn.ids = names(lrns)
  selectInput("benchmark.learners.sel", NULL, choices = lrn.ids,
    multiple = TRUE, selected = lrn.ids)
})

bmr.learners = reactive({
  learners()[input$benchmark.learners.sel]
})

observeEvent(task.type(), {
  if (task.type() != "classif") {
    shinyjs::hide("benchmark.stratification", animType = "slide")
  } else {
    shinyjs::show("benchmark.stratification", anim = TRUE)
  }
})

rdesc.type = reactive(input$benchmark.rdesc.type)

output$benchmark.rdesc.config = renderUI({
  makeResampleDescUI(rdesc.type())
})

rdesc = reactive({
  rdesc.type = rdesc.type()
  args = list()
  if (rdesc.type %in% c("CV", "Bootstrap", "Subsample")) {
    args$iters = input$benchmark.iters
  }
  if (rdesc.type == "RepCV") {
    args$reps = input$benchmark.reps
    args$folds = input$benchmark.folds
  }
  if (rdesc.type %in% c("Subsample", "Holdout")) {
    args$split = input$benchmark.split
  }
  args$method = rdesc.type
  do.call("makeResampleDesc", args)
})


measures.bmr.avail = reactive({
  reqAndAssign(task(), "tsk")
  listMatchingMeasures(tsk, bmr.learners())
})

measures.default = reactive({
  reqAndAssign(task.type(), "tsk.type")
  switch(tsk.type, 
    classif = "acc",
    regr = "mse")
})

output$benchmark.measures.sel = renderUI({
  reqAndAssign(measures.bmr.avail(), "ms")
  selectInput("benchmark.measures.sel", "Measures", choices = ms,
    multiple = TRUE, selected = measures.default())
})

measures.bmr = reactive({
  req(input$benchmark.measures.sel)
  listMeasures(task(), create = TRUE)[input$benchmark.measures.sel]
})

bmr = eventReactive(input$benchmark.run, {
  req(learners())
  ms = measures.bmr()
  lrns = bmr.learners()
  rd = rdesc()
  tsk = task()
  
  withCallingHandlers({
    benchmark(lrns, tsk, rd, measures = ms, show.info = TRUE)
  },
    message = function(m) {
      shinyjs::html(id = "benchmark.text", html = m$message, add = FALSE)
    })
})

output$benchmark.overview = renderDataTable({
  reqAndAssign(bmr(), "b")
  getBMRAggrPerformances(b, as.df = TRUE)
}, options = list(lengthMenu = c(10, 20), pageLength = 10,
  scrollX = TRUE)
)
