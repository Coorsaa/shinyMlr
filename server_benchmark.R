##### benchmark #####

output$benchmark.learners.sel = renderUI({
  reqAndAssign(learners(), "lrns")
  validateTask(input$create.task, task.data(), data$data, req = TRUE)
  lrn.ids = names(lrns)
  selectInput("benchmark.learners.sel", "Learners", choices = lrn.ids,
    multiple = TRUE, selected = lrn.ids)
})

output$stratText = renderText({
  req(strat())
  paste(strat())
})

observeEvent(task.type(), {
  if (task.type() != "classif") {
    shinyjs::hide("benchmark.stratification", animType = "slide")
  } else {
    shinyjs::show("benchmark.stratification", anim = TRUE)
  }
})

rdesctype = reactive(input$benchmark.rdesctype)
output$rdesctypeText = renderText(paste(rdesctype()))

observeEvent(rdesctype(), {
  if (rdesctype() %in% c("LOO", "RepCV", "Holdout")) {
    shinyjs::hide("benchmark.iters")
  } else {
    shinyjs::show("benchmark.iters")
  }
})

rdesc = reactive({
  if (input$benchmark.rdesctype %in% c("CV", "Subsample", "Bootstrap")) {
    makeResampleDesc(input$benchmark.rdesctype, iters = input$benchmark.iters)
  } else {
    makeResampleDesc(input$benchmark.rdesctype)
  }
})


measures.avail = reactive({
  reqAndAssign(task(), "tsk")
  listMeasures(tsk, create = FALSE)
})

measures.default = reactive({
  reqAndAssign(task.type(), "tsk.type")
  switch(tsk.type, 
    classif = "acc",
    regr = "mse")
})

output$benchmark.measures.sel = renderUI({
  reqAndAssign(measures.avail(), "ms")
  selectInput("benchmark.measures.sel", "Measures", choices = ms, multiple = TRUE, selected = measures.default())
})

measures = reactive({
  req(input$benchmark.measures.sel)
  listMeasures(task(), create = TRUE)[input$benchmark.measures.sel]
})

bmr = eventReactive(input$benchmark.run, {
  req(learners())
  ms = measures()
  lrns = learners()[input$benchmark.learners.sel]
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
